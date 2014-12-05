#![crate_type="dylib"]
#![feature(plugin_registrar, phase)]

extern crate syntax;
extern crate rustc;
extern crate postgres;

use syntax::ast::{Item, TokenTree};
use syntax::ast::TokenTree::TtToken;
use syntax::ptr::P;
use syntax::parse::{new_parse_sess, parse_item_from_source_str};
use syntax::parse::token::get_ident;
use syntax::parse::token::Token::Ident;
use syntax::ext::base::{ExtCtxt, MacResult, DummyResult, MacItems};
use syntax::codemap::Span;
use std::collections::HashMap;
use rustc::plugin::Registry;

use postgres::{Connection, SslMode};

use self::PgType::{PgInt, PgBool, PgString, PgTime};
use self::TableKind::{Full, Insert, Search};

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
    Search
}

struct TableDefinition {
    table_name: String,
    full_name: String,
    insert_name: String,
    search_name: String,
    schema: HashMap<String, PgType>
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

trait Joinable {
    fn join(&mut self, delim: &str) -> String;
}

impl<'a, A : Str, T : Iterator<A> + 'a> Joinable for T  {
    fn join(&mut self, delim: &str) -> String {
        let mut retval = "".to_string();
        let mut prev = self.next();

        if prev.is_none() {
            return retval;
        }

        let mut cur = self.next();

        while cur.is_some() {
            retval.push_str(prev.unwrap().as_slice());
            retval.push_str(delim);
            prev = cur;
            cur = self.next();
        }
                
        retval.push_str(prev.unwrap().as_slice());
        retval
    } // join
}

impl TableDefinition {
    fn new(connect_str: &str, actual_name: &str) -> TableDefinition {
        let conn = Connection::connect(connect_str, &SslMode::None).unwrap();
        let schema = schema_for(conn, actual_name);
        assert!(schema.contains_key(&"id".to_string()));
        assert_eq!(schema.get(&"id".to_string()).unwrap(), &PgInt);
        let pretty = pretty_name(actual_name);

        TableDefinition {
            table_name: actual_name.to_string(),
            insert_name: format!("{}Insert", pretty.as_slice()),
            search_name: format!("{}Search", pretty.as_slice()),
            full_name: pretty,
            schema: schema
        }
    }

    fn name_for_kind(&self, kind: &TableKind) -> &str {
        match kind {
            &Full => self.full_name.as_slice(),
            &Insert => self.insert_name.as_slice(),
            &Search => self.search_name.as_slice()
        }
    }

    // returns whether or not the field should be in the record, and if
    // so, if the field is public (true) and what the type of the field
    // should be (as a string)
    fn field_projection(&self, field_name: &str, kind: &TableKind) -> Option<(bool, String)> {
        assert!(self.schema.contains_key(field_name));

        let is_pub = 
            match kind {
                &Full => Some(field_name != "id"),
                &Insert => 
                    if field_name != "id" { Some(true) } else { None },
                &Search => Some(true)
            };
        is_pub.map(|p| {
            let base_typ = self.schema.get(field_name).unwrap().to_rust_type();
            let typ_str =
                if kind == &Search { 
                    format!("Option<{}>", base_typ)
                } else {
                    base_typ
                };
            (p, typ_str)
        })
    } // field_projection

    fn struct_body(&self, kind: &TableKind) -> String {
        let mut fields: Vec<String> = vec!();

        for key in self.schema.keys() {
            self.field_projection(key.as_slice(), kind).map(|(is_pub, typ)| {
                fields.push(
                    format!("{} {}: {}", 
                            if is_pub { "pub" } else { "" },
                            key.as_slice(),
                            typ));
            });
        }

        fields.iter().join(", ")
    }
                            
    fn struct_definition_for(&self, kind: &TableKind) -> P<Item> {
        parse_item_from_source_str(
            "structgen".to_string(),
            format!("pub struct {} {{ {} }}",
                    self.name_for_kind(kind),
                    self.struct_body(kind).as_slice()),
            vec!(),
            &new_parse_sess()).unwrap()
    }

    fn insert_implementation_body(&self) -> String {
        let keys: Vec<&String> =
            self.schema.keys().filter(|k| k.as_slice() != "id").collect();
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
                println!({});\
                conn.execute({}, &[{}]);\
            }}",
            query_base.as_slice(),
            query_base,
            values)
    }
            
    fn struct_implementation_for(&self, kind: &TableKind) -> P<Item> {
        let imp = 
            format!("impl {} {{ {} }}", 
                    self.name_for_kind(kind),
                    match kind {
                        &Insert => self.insert_implementation_body(),
                        _ => panic!("Not yet implemented")
                    });
        parse_item_from_source_str(
            "implgen".to_string(),
            imp,
            vec!(),
            &new_parse_sess()).unwrap()
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
        table_name_string.as_slice());
    
    MacItems::new(
        vec!(table_def.struct_definition_for(&Full),
             table_def.struct_definition_for(&Insert),
             table_def.struct_definition_for(&Search),
             table_def.struct_implementation_for(&Insert)).into_iter())
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("pg_table", expand_pg_table);
}

