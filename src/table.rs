#![crate_type="dylib"]
#![feature(plugin_registrar, phase, quote)]

extern crate syntax;
extern crate rustc;
extern crate postgres;
extern crate time;
// extern crate regex;
// #[phase(plugin)] extern crate regex_macros;

use syntax::codemap::{Span, Spanned};
use syntax::parse::token;
use syntax::ast::{TokenTree, TtToken};
use syntax::ast;
use syntax::ext::base::{ExtCtxt, MacResult, DummyResult, MacItems};
use syntax::ext::build::AstBuilder;  // trait for expr_uint
use syntax::ptr::P;
use syntax::parse::token::intern;
use rustc::plugin::Registry;

use std::collections::HashMap;
use postgres::{Connection, SslMode};

use self::PgType::{PgInt, PgBool, PgString, PgTime};
use self::TableKind::{Full, Insert, Search};

fn expand_pg_table(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {

    // Get the table name.
    let table_name_string = match args {
        [TtToken(_, token::Ident(s, _))] => token::get_ident(s).to_string(),
        _ => {
            cx.span_err(sp, "argument should be a single identifier");
            return DummyResult::any(sp);
        }
    };

    let table_name = table_name_string.as_slice();

    // Open a connection to PG.
    let conn = Connection::connect("postgres://jroesch@localhost/gradr-production", &SslMode::None)
            .unwrap();

    let schema: &HashMap<String, PgType> = &schema_for(conn, table_name);

    let base_struct_name_string = base_name(table_name);
    let base_struct_name = base_struct_name_string.as_slice();

    let full_def =
        struct_item_for(cx, sp, schema, base_struct_name, &Full).map(
            |mut f| { f.vis = ast::Inherited; f });
    let insert_def = struct_item_for(cx, sp, schema, base_struct_name, &Insert);
    let search_def = struct_item_for(cx, sp, schema, base_struct_name, &Search);
    
    let insert_impl_item = insert_impl(cx, sp, table_name,
                                       &insert_def.ident, schema);

    MacItems::new(
        vec![full_def, insert_def, search_def, insert_impl_item].into_iter())
}

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

    pub fn to_rust_type(&self, cx: &ExtCtxt, sp: Span) -> P<ast::Ty> {
        match self {
            &PgInt => cx.ty_ident(sp, ast::Ident::new(intern("int"))),
            &PgBool => cx.ty_ident(sp, ast::Ident::new(intern("bool"))),
            &PgString => cx.ty_ident(sp, ast::Ident::new(intern("String"))),
            &PgTime => cx.ty_ident(sp, ast::Ident::new(intern("Timespec")))
        }
    }
}

fn schema_for(conn: Connection, table_name: &str) -> HashMap<String, PgType> {
    let query = format!("select column_name, \
                                data_type, \
                                character_maximum_length \
                         from INFORMATION_SCHEMA.COLUMNS \
                         where table_name = '{}';", table_name);

    let rows = conn.prepare(query.as_slice()).unwrap();

    let mut schema = HashMap::new();

    for row in rows.query([]).unwrap() {
        schema.insert(row.get(0), PgType::from_data_type(&row.get(1)));
    }

    schema
}

fn join(vec: &Vec<&str>, delim: &str) -> String {
    let mut retval = "".to_string();
    let mut cur = 0;
    let len = vec.len();

    for s in vec.iter() {
        retval.push_str(*s);
        if cur < len - 1 {
            retval.push_str(delim);
        }
        cur += 1;
    }

    retval
}
        
fn insert_line_and_keys<'a>(table_name: &'a str, schema: &'a HashMap<String, PgType>) -> (String, Vec<&'a str>) {
    let mut keys: Vec<&str> = Vec::new();
    let mut values: Vec<String> = Vec::new();
    let mut i = 1i;

    for (name, _) in schema.iter() {
        let key = name.as_slice();
        if name.as_slice() != "id" {
            keys.push(key);
            values.push(format!("${}", i));
            i += 1;
        }
    }

    let query = format!("INSERT INTO {} ({}) VALUES ({})",
                        table_name,
                        join(&keys, ", "),
                        join(&values.iter().map(|s| s.as_slice()).collect(), ", "));
    (query, keys)
}

// There are three kinds of struct definitions for a table T:
// -T (Full): a record returned from the database.  This has an id field we should
//  not be able to modify.
// -TInsert: A record we want to go into the database.  This is missing
//  the id field, which will be put in by the database. (Insert)
// -TSearch: A way to search the database for a given kind of record.
//  This has all the fields as a T, except they are all Option.  If we have
//  Somes, then we search for something like that. (Search)

enum TableKind { Full, Insert, Search }

impl TableKind {
    fn name(&self, base_name: &str) -> String {
        match *self {
            Full => base_name.to_string(),
            Insert => format!("{}Insert", base_name),
            Search => format!("{}Search", base_name)
        }
    }

    fn name_ident(&self, base_name: &str) -> ast::Ident {
        ast::Ident::new(intern(self.name(base_name).as_slice()))
    }

    fn struct_field_for(&self, cx: &ExtCtxt, sp: Span, field_name: &str, ty: &PgType) -> Option<ast::StructField> {
        let is_id = field_name == "id";

        let make_field = |wrap: |P<ast::Ty>| -> P<ast::Ty>, visibility| {
            Spanned {
                node: ast::StructField_ {
                    kind: ast::NamedField(ast::Ident::new(intern(field_name)), visibility),
                    id: ast::DUMMY_NODE_ID,
                    ty: wrap(ty.to_rust_type(cx, sp)),
                    attrs: Vec::new()
                },
                span: sp
            }
        };
                         
        match *self {
            Full => Some(make_field(
                |x| x,
                if is_id { ast::Inherited } else { ast:: Public })),
            Insert if !is_id => Some(make_field(|x| x, ast::Public)),
            Search => Some(make_field(|x| cx.ty_option(x), ast::Public)),
            _ => None
        }
    }
}

fn insert_impl(cx: &ExtCtxt, sp: Span, table_name: &str, struct_name: &ast::Ident, schema: &HashMap<String, PgType>) -> P<ast::Item> {
    let (query, keys) = insert_line_and_keys(table_name, schema);
    let r_exp = cx.expr_ident(sp, ast::Ident::new(intern("r")));
    let values = cx.expr_vec_slice(
        sp, keys.iter().map(
            |k| cx.expr_addr_of(
                sp, cx.expr_field_access(
                    sp, r_exp.clone(),
                    ast::Ident::new(intern(*k))))).collect());
    let method = quote_method!(cx, fn insert(self, conn: &Connection) {
        let r = &self;
        conn.execute($query, $values);
    });
    
    cx.item(
        sp, struct_name.clone(), vec!(),
        ast::Item_::ItemImpl(ast::Generics {
            lifetimes: vec!(),
            ty_params: syntax::owned_slice::OwnedSlice::empty(),
            where_clause: ast::WhereClause {
                id: ast::DUMMY_NODE_ID,
                predicates: vec!()
            }
        },
                             None,
                             cx.ty_ident(sp, struct_name.clone()),
                             vec!(ast::ImplItem::MethodImplItem(method))))
}

fn struct_item_for(cx: &ExtCtxt, sp: Span, schema: &HashMap<String, PgType>, base_name: &str, kind: &TableKind) -> P<ast::Item> {
    cx.item_struct(
        sp,
        kind.name_ident(base_name),
        struct_def_for(cx, sp, schema, kind))
}

// Returns a struct definition for the given kind of struct we are interested in
fn struct_def_for(cx: &ExtCtxt, span: Span, schema: &HashMap<String, PgType>, kind: &TableKind) -> ast::StructDef {
    assert!(schema.get(&"id".to_string()).map(|t| t == &PgInt).unwrap_or(false),
            "Need an id field with type int");

    let mut fields = Vec::new();

    for (name, ty) in schema.iter() {
        kind.struct_field_for(cx, span, name.as_slice(), ty).map(|f| fields.push(f));
    }

    ast::StructDef { fields: fields, ctor_id: None }
}

// de-plural and capitialize name (ActiveRecord name convention).
fn base_name(name: &str) -> String {
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

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("pg_table", expand_pg_table);
}
