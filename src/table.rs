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

fn expand_pg_table(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {

    // Get the table name.
    let table_name = match args {
        [TtToken(_, token::Ident(s, _))] => token::get_ident(s).to_string(),
        _ => {
            cx.span_err(sp, "argument should be a single identifier");
            return DummyResult::any(sp);
        }
    };

    // Open a connection to PG.
    let conn = Connection::connect("postgres://jroesch@localhost/gradr-production", &SslMode::None)
            .unwrap();

    let schema: HashMap<String, PgType> = schema_for(conn, &table_name);

    // for (name, ty) in schema.iter() {
    //     debug!("FieldName: {}; Type: {}", name, ty)
    // }

    let (template_def, full_def) = struct_defs_for(cx, sp, &schema);

    let (template_name, full_name) = structify(&table_name);

    let template_item = cx.item_struct(sp, template_name, template_def);
    let full_item = cx.item_struct(sp, full_name, full_def);
    let full_item = full_item.map(|mut f| { f.vis = ast::Inherited; f } );

    let (query, keys) = insert_line_and_keys(table_name.as_slice(), &schema);
    
    let r_exp = cx.expr_ident(sp, ast::Ident::new(intern("r")));
    let values = cx.expr_vec_slice(sp, keys.iter().map(|k| cx.expr_addr_of(sp, cx.expr_field_access(sp, r_exp.clone(), ast::Ident::new(intern(*k))))).collect());

    let method = quote_method!(cx, fn insert(self, conn: Connection) {
        let r = &self;
        conn.execute($query, $values);
    });

    let imp = cx.item(
        sp, template_name.clone(), vec!(),
        ast::Item_::ItemImpl(ast::Generics {
            lifetimes: vec!(),
            ty_params: syntax::owned_slice::OwnedSlice::empty(),
            where_clause: ast::WhereClause {
                id: ast::DUMMY_NODE_ID,
                predicates: vec!()
            }
        },
                             None,
                             cx.ty_ident(sp, template_name.clone()),
                             vec!(ast::ImplItem::MethodImplItem(method))));
    
    MacItems::new(vec![template_item, full_item, imp].into_iter())
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
            "character varying" => PgString,
            "timestamp without time zone" => PgTime,
            s => panic!("type {} not yet supported", s)
        }
    }

    pub fn to_rust_type(&self, cx: &mut ExtCtxt, sp: Span) -> P<ast::Ty> {
        match self {
            &PgInt => cx.ty_ident(sp, ast::Ident::new(intern("int"))),
            &PgBool => cx.ty_ident(sp, ast::Ident::new(intern("bool"))),
            &PgString => cx.ty_ident(sp, ast::Ident::new(intern("String"))),
            &PgTime => cx.ty_ident(sp, ast::Ident::new(intern("Timespec")))
        }
    }
}

fn schema_for(conn: Connection, table_name: &String) -> HashMap<String, PgType> {
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
    
// Returns a pair of template, full struct
// Templates are used strictly for making new records, and are missing the
// all-important id field.  The full version corresponds to something in the
// database
fn struct_defs_for(ecx: &mut ExtCtxt, span: Span, schema: &HashMap<String, PgType>) -> (ast::StructDef, ast::StructDef) {
    assert!(schema.get(&"id".to_string()).map(|t| t == &PgInt).unwrap_or(false),
            "Need an id field with type int");
    
    let mut template_fields = Vec::new();
    let mut full_fields = Vec::new();

    for (name, ty) in schema.iter() {
        let field = struct_field_for(ecx, span, name.as_slice(), ty);
        full_fields.push(field.clone());
        if name.as_slice() != "id" {
            template_fields.push(field);
        }
    }

   (ast::StructDef { fields: template_fields, ctor_id: None },
    ast::StructDef { fields: full_fields, ctor_id: None })
}

fn struct_field_for(ecx: &mut ExtCtxt, sp: Span, field_name: &str, ty: &PgType) -> ast::StructField {
    let visibility =
        if field_name == "id" { ast::Inherited } else { ast::Public };

    let struct_field_ = ast::StructField_ {
        kind: ast::NamedField(ast::Ident::new(intern(field_name)), visibility),
        id: ast::DUMMY_NODE_ID,
        ty: ty.to_rust_type(ecx, sp),
        attrs: Vec::new()
    };

    Spanned { node: struct_field_, span: sp }
}

// de-plural and capitialize name (ActiveRecord name convention).
// Returns the template name and the full name
fn structify(name: &String) -> (ast::Ident, ast::Ident) {
    let base_name: String = name.as_slice().chars().enumerate().filter_map(|(i, c)| {
        if i == 0 {
            Some(c.to_uppercase())
        } else if i == name.len() - 1 {
            None
        } else {
            Some(c.clone())
        }
    }).collect();

    let mut template_name = base_name.clone();
    template_name.push_str("Template");
    (ast::Ident::new(intern(template_name.as_slice())),
     ast::Ident::new(intern(base_name.as_slice())))
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("pg_table", expand_pg_table);
}
