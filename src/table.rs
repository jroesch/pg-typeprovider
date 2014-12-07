extern crate syntax;
extern crate rustc;
extern crate postgres;

use self::syntax::ast::{Item, TokenTree, CrateConfig};
use self::syntax::ast::TokenTree::TtToken;
use self::syntax::ptr::P;
use self::syntax::parse::{parse_item_from_source_str, ParseSess};
use self::syntax::parse::token::get_ident;
use self::syntax::parse::token::Token::Ident;
use self::syntax::ext::base::{ExtCtxt, MacResult, DummyResult, MacItems};
use self::syntax::codemap::Span;
use self::rustc::plugin::Registry;
use self::postgres::{Connection, SslMode};

use std::collections::HashMap;

use util::Joinable;

use self::PgType::{PgInt, PgBool, PgString, PgTime};
use self::TableKind::{Full, Insert, Search, Update};

#[deriving(Show, PartialEq)]
enum PgType {
    PgInt,
    PgBool,
    PgString,
    PgTime
}

impl PgType {
    pub fn from_data_type(s: &String) -> PgType {
        match s.as_slice() {
            "integer" => PgInt,
            "boolean" => PgBool,
            "character varying" | "text" => PgString,
            "timestamp without time zone" => PgTime,
            s => panic!("type {} not yet supported", s)
        }
    }

    pub fn to_rust_type(&self) -> String {
        match self {
            &PgInt => "i32".to_string(),
            &PgBool => "bool".to_string(),
            &PgString => "String".to_string(),
            &PgTime => "Timespec".to_string()
        }
    }
}

// There are three kinds of struct definitions for a table T:
// -T (Full): a record returned from the database.  This has an id field we
//  should not be able to modify.
// -TInsert: A record we want to go into the database.  This is missing
//  the id field, which will be put in by the database. (Insert)
// -TSearch: A way to search the database for a given kind of record.
//  This has all the fields as a T, except they are all Option.  If we have
//  Somes, then we search for something like that. (Search)

#[deriving(Show, PartialEq)]
enum TableKind {
    Full,
    Insert,
    Search,
    Update
}

struct TableDefinition<'a> {
    table_name: String,
    full_name: String,
    insert_name: String,
    search_name: String,
    update_name: String,
    schema: HashMap<String, PgType>,
    cfg: &'a CrateConfig,
    session: &'a ParseSess
}
    
// de-plural and capitialize name (ActiveRecord name convention).
fn pretty_name(name: &str) -> String {
    let retval: String = name.chars().enumerate().filter_map(|(i, c)| {
        if i == 0 {
            Some(c.to_uppercase())
        } else if i == name.len() - 1 {
            None
        } else {
            Some(c.clone())
        }
    }).collect();
    retval
}

fn schema_for(conn: Connection, table_name: &str) -> HashMap<String, PgType> {
    let query = format!("select column_name, \
                                data_type, \
                                character_maximum_length \
                         from INFORMATION_SCHEMA.COLUMNS \
                         where table_name = '{}';", table_name);

    let rows = conn.prepare(query.as_slice()).unwrap();

    let mut schema = HashMap::new();

    for row in rows.query(&[]).unwrap() {
        schema.insert(row.get(0), PgType::from_data_type(&row.get(1)));
    }

    schema
}

impl<'a> TableDefinition<'a> {
    fn new<'a>(connect_str: &'a str, actual_name: &'a str, cfg: &'a CrateConfig, sess: &'a ParseSess) -> TableDefinition<'a> {
        let conn = Connection::connect(connect_str, &SslMode::None).unwrap();
        let schema = schema_for(conn, actual_name);
        assert!(schema.contains_key(&"id".to_string()));
        assert_eq!(schema.get(&"id".to_string()).unwrap(), &PgInt);
        let pretty = pretty_name(actual_name);

        TableDefinition {
            table_name: actual_name.to_string(),
            insert_name: format!("{}Insert", pretty.as_slice()),
            search_name: format!("{}Search", pretty.as_slice()),
            update_name: format!("{}Update", pretty.as_slice()),
            full_name: pretty,
            schema: schema,
            cfg: cfg,
            session: sess
        }
    }

    fn name_for_kind(&self, kind: &TableKind) -> &str {
        match kind {
            &Full => self.full_name.as_slice(),
            &Insert => self.insert_name.as_slice(),
            &Search => self.search_name.as_slice(),
            &Update => self.update_name.as_slice(),
        }
    }

    // Given a field name in a table and the kind of table it is for, returns
    // triples for:
    // -If the field is public in the struct
    // -The name of the field in the struct
    // -The type of the struct field, as a String
    fn field_projection(&self, field_name: &str, kind: &TableKind) -> Vec<(bool, String, String)> {
        assert!(self.schema.contains_key(field_name));
        
        let name = field_name.to_string();
        let typ = self.schema.get(field_name).unwrap().to_rust_type();
        let op_typ = format!("Option<{}>", typ.as_slice());

        match kind {
            &Full =>
                vec!((field_name != "id", name, typ)),
            &Insert => 
                if field_name != "id" {
                    vec!((true, name, typ))
                } else {
                    vec!()
                },
            &Search =>
                vec!((true, name, op_typ)),
            &Update =>
                vec!((true, format!("{}_to", name.as_slice()), op_typ.clone()),
                     (true, format!("where_{}", name), op_typ))
        }
    }

    fn struct_body(&self, kind: &TableKind) -> String {
        let mut fields: Vec<String> = vec!();

        for key in self.schema.keys() {
            let add_fields = self.field_projection(key.as_slice(), kind);
            for (is_pub, name, typ) in add_fields.into_iter() {
                fields.push(
                    format!("{} {}: {}",
                            if is_pub { "pub" } else { "" },
                            name,
                            typ));
            }
        }

        fields.iter().join(", ")
    }
                            
    fn struct_definition_for(&self, kind: &TableKind) -> P<Item> {
        parse_item_from_source_str(
            "structgen".to_string(),
            format!("pub struct {} {{ {} }}",
                    self.name_for_kind(kind),
                    self.struct_body(kind).as_slice()),
            self.cfg.clone(),
            self.session).unwrap()
    }

    fn schema_keys_filter(&self, f: |&str| -> bool) -> Vec<&String> {
        self.schema.keys().filter(|k| f(k.as_slice())).collect()
    }

    fn insert_implementation_body(&self) -> String {
        let keys = self.schema_keys_filter(|k| k != "id");
        let query_base =
            format!("\"INSERT INTO {} ({}) VALUES ({})\"",
                    self.table_name.as_slice(),
                    keys.iter().join(", "),
                    range(1, keys.len() + 1).map(|i| {
                        format!("${}", i)
                    }).join(", "));
        let values = keys.iter().map(|k| {
            format!("&self.{}", k)
        }).join(", ");

        format!(
            "pub fn insert(self, conn: &Connection) {{\
                conn.execute({}, &[{}]).unwrap();\
            }}",
            query_base,
            values)
    }

    fn search_implementation_body(&self) -> String {
        let keys = self.schema_keys_filter(|_| true);
        let base_query =
            format!("\"SELECT {} FROM {}\"",
                    keys.iter().join(", "),
                    self.table_name.as_slice());

        fn check_key(key: &str) -> String {
            format!(
                "if self.{0}.is_some() {{\
                    constraints.push((\"{0}\", &self.{0}));\
                }}", key)
        }

        format!(
            // TODO: should return an iterator, but this is getting complex
            "pub fn search(&self, conn: &Connection, limit: Option<uint>) -> Vec<{0}> {{\
                let mut constraints: Vec<(&str, &ToSql)> = vec!();
                {1}\
                let mut query = {2}.to_string();\
                let len = constraints.len();\
                if len > 0 {{\
                    query.push_str(\" WHERE \");\
                    query.push_str(
                        constraints.iter().zip(range(1, len + 1)).map(\
                            |(&(k, _), i)| format!(\
                                \"{{}} = ${{}}\",\
                                k, i)).join(\" AND \").as_slice());\
                }}\
                limit.map(|l| query.push_str(format!(\" LIMIT {{}}\", l).as_slice()));\
                let rows = conn.prepare(query.as_slice()).unwrap();\
                let values_vec: Vec<&ToSql> = \
                    constraints.iter().map(|&(_, v)| v).collect();\
                rows.query(values_vec.as_slice()).unwrap().map(|row| {{\
                    {0} {{ {3} }}\
                }}).collect()
            }}",
            self.full_name.as_slice(),
            keys.iter().map(|k| check_key(k.as_slice())).join("\n"),
            base_query,
            keys.iter().zip(range(0, keys.len())).map(|(k, i)| {
                format!("{}: row.get({})", k.as_slice(), i)
            }).join(", "))
    }

    fn struct_implementation_for(&self, kind: &TableKind) -> P<Item> {
        let imp = 
            format!("impl {} {{ {} }}", 
                    self.name_for_kind(kind),
                    match kind {
                        &Insert => self.insert_implementation_body(),
                        &Search => self.search_implementation_body(),
                        _ => panic!("Not yet implemented")
                    });

        parse_item_from_source_str(
            "implgen".to_string(),
            imp,
            self.cfg.clone(),
            self.session).unwrap()
    }
} // TableDefinition

fn expand_pg_table(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
    // Get the table name.
    let table_name_string = match args {
        [TtToken(_, Ident(s, _))] => get_ident(s).to_string(),
        _ => {
            cx.span_err(sp, "argument should be a single identifier");
            return DummyResult::any(sp);
        }
    };

    let table_def = TableDefinition::new(
        "postgres://jroesch@localhost/gradr-production",
        table_name_string.as_slice(),
        &cx.cfg,
        cx.parse_sess);
    
    MacItems::new(
        vec!(table_def.struct_definition_for(&Full),
             table_def.struct_definition_for(&Insert),
             table_def.struct_definition_for(&Search),
             table_def.struct_implementation_for(&Insert),
             table_def.struct_implementation_for(&Search)).into_iter())
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("pg_table", expand_pg_table);
}

