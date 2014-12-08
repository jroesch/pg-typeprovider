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
            &Update => {
                let mut retval =
                    vec!((true, format!("where_{}", name), op_typ.clone()));
                if field_name != "id" {
                    retval.push(
                        (true, format!("{}_to", name.as_slice()), op_typ));
                }
                retval
            }
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

    fn all_schema_keys(&self) -> Vec<&String> {
        self.schema_keys_filter(|_| true)
    }

    fn schema_keys_filter(&self, f: |&str| -> bool) -> Vec<&String> {
        self.schema.keys().filter(|k| f(k.as_slice())).collect()
    }

    fn full_implementation_body(&self) -> String {
        "pub fn get_id(&self) -> i32 { self.id }".to_string()
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

    // constraints: The name of a constructed Vec<(&str, &ToSql)>
    fn build_multi_constraint_push(keys: &Vec<&String>,
                                   constraints: &str,
                                   as_struct_key: |&str| -> String) -> String {
        keys.iter().map(|k| {
            TableDefinition::build_constraint_push(
                constraints,
                as_struct_key(k.as_slice()).as_slice(),
                k.as_slice())
        }).join("\n")
    }

    // constraints: The name of a constructed Vec<(&str, &ToSql)>
    fn build_constraint_push(constraints: &str,
                             struct_key: &str,
                             table_key: &str) -> String {
        format!(
            "if self.{0}.is_some() {{\
                {1}.push((\"{2}\", self.{0}.as_ref().unwrap()));\
            }}",
            struct_key,
            constraints,
            table_key)
    }

    // constraints: The name of a constructed Vec<(&str, &ToSql)>
    fn build_clause(constraints: &str,
                    range_start: &str,
                    range_end: &str,
                    join_on: &str) -> String {
        format!(
            "{0}.iter().zip(range({1}, {2})).map(\
                |(&(k, _), i)| format!(\
                    \"{{}} = ${{}}\",\
                    k, i)).join(\"{3}\")",
            constraints,
            range_start,
            range_end,
            join_on)
    }

    fn search_implementation_body(&self) -> String {
        let keys = self.all_schema_keys();
        let base_query =
            format!("\"SELECT {} FROM {}\"",
                    keys.iter().join(", "),
                    self.table_name.as_slice());
        format!(
            // TODO: should return an iterator, but this is getting complex
            "pub fn search(&self, conn: &Connection, limit: Option<uint>) -> Vec<{0}> {{\
                let mut constraints: Vec<(&str, &ToSql)> = vec!();\
                {1}\
                let mut query = {2}.to_string();\
                let len = constraints.len();\
                if len > 0 {{\
                    query.push_str(\" WHERE \");\
                    query.push_str({3}.as_slice());\
                }}\
                limit.map(|l| query.push_str(format!(\" LIMIT {{}}\", l).as_slice()));\
                let rows = conn.prepare(query.as_slice()).unwrap();\
                let values_vec: Vec<&ToSql> = \
                    constraints.iter().map(|&(_, v)| v).collect();\
                rows.query(values_vec.as_slice()).unwrap().map(|row| {{\
                    {0} {{ {4} }}\
                }}).collect()
            }}",
            self.full_name.as_slice(),
            TableDefinition::build_multi_constraint_push(
                &keys,
                "constraints",
                |k| k.to_string()),
            base_query,
            TableDefinition::build_clause(
                "constraints", "1", "len + 1", " AND "),
            keys.iter().zip(range(0, keys.len())).map(|(k, i)| {
                format!("{}: row.get({})", k.as_slice(), i)
            }).join(", "))
    }

    fn update_implementation_body(&self) -> String {
        let set_keys = self.schema_keys_filter(|k| k != "id");
        let where_keys = self.all_schema_keys();

        let base_query =
            format!("\"UPDATE {} SET \"", self.table_name.as_slice());

        format!(
            "pub fn update(&self, conn: &Connection) -> uint {{\
                let mut set_cons: Vec<(&str, &ToSql)> = vec!();\
                let mut where_cons: Vec<(&str, &ToSql)> = vec!();\
                {0}\
                {1}\
                let set_cons_len = set_cons.len();
                if set_cons_len > 0 {{\
                    let mut query = {2}.to_string();\
                    query.push_str({3}.as_slice());\
                    let where_cons_len = where_cons.len();
                    if where_cons_len > 0 {{\
                        query.push_str(\" WHERE \");\
                        query.push_str({4}.as_slice());\
                    }}\

                    let mut values: Vec<&ToSql> = \
                        set_cons.iter().map(|&(_, v)| v).collect();\
                    let where_values: Vec<&ToSql> = \
                        where_cons.iter().map(|&(_, v)| v).collect();\
                    values.push_all(where_values.as_slice());\
                    conn.execute(query.as_slice(), values.as_slice()).unwrap()\
                }} else {{ 0 }}\
            }}",
            TableDefinition::build_multi_constraint_push(
                &set_keys,
                "set_cons",
                |k| format!("{}_to", k)),
            TableDefinition::build_multi_constraint_push(
                &where_keys,
                "where_cons",
                |k| format!("where_{}", k)),
            base_query,
            TableDefinition::build_clause(
                "set_cons", "1", "set_cons_len + 1", ", "),
            TableDefinition::build_clause(
                "where_cons", "set_cons_len + 1",
                "set_cons_len + 1 + where_cons_len", " AND "))
    } // update_implementation_body

    fn all_items(&self) -> Vec<P<Item>> {
        let mut retval = vec!();
        for kind in vec!(Full, Insert, Search, Update).iter() {
            retval.push_all(self.items_for(kind).as_slice());
        }
        retval
    }

    fn items_for(&self, kind: &TableKind) -> Vec<P<Item>> {
        let mut retval = vec!();
        retval.push(self.struct_definition_for(kind));
        self.struct_implementation_for(kind).map(|i| retval.push(i));
        retval
    }

    fn implementation_body(&self, kind: &TableKind) -> Option<String> {
        match kind {
            &Full => Some(self.full_implementation_body()),
            &Insert => Some(self.insert_implementation_body()),
            &Search => Some(self.search_implementation_body()),
            &Update => Some(self.update_implementation_body())
        }
    }

    fn struct_implementation_for(&self, kind: &TableKind) -> Option<P<Item>> {
        self.implementation_body(kind).map(|body| {
            let name = self.name_for_kind(kind);
            parse_item_from_source_str(
                format!("implgen-{}", name.as_slice()),
                format!("impl {} {{ {} }}", name, body),
                self.cfg.clone(),
                self.session).unwrap()
        })
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
    
    MacItems::new(table_def.all_items().into_iter())
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("pg_table", expand_pg_table);
}

