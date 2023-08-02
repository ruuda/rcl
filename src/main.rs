mod ast;
mod runtime;

use std::rc::Rc;
use std::collections::BTreeMap;

use ast::{BinOp, Compr, Expr, Ident, Seq, UnOp};
use runtime::{Env, Value};

/// Helpers for constructing AST in code.
pub fn var(name: Ident) -> Expr {
    Expr::Var(name)
}

pub fn string(value: &'static str) -> Expr {
    Expr::StringLit(value.to_string())
}

pub fn field(field: Ident, obj: Expr) -> Expr {
    Expr::Field(field, Box::new(obj))
}

pub fn singleton(key: Ident, value: Expr) -> Vec<Seq> {
    vec![Seq::Elem(Expr::Assoc(Box::new(string(key)), Box::new(value)))]
}

pub fn neg(x: Expr) -> Expr {
    Expr::UnOp(UnOp::Neg, Box::new(x))
}

pub fn union(x: Expr, y: Expr) -> Expr {
    Expr::BinOp(BinOp::Union, Box::new(x), Box::new(y))
}

pub fn call(f: Expr, args: Vec<Expr>) -> Expr {
    Expr::Call(Box::new(f), args)
}

pub fn let_(name: Ident, value: Expr, in_: Expr) -> Expr {
    Expr::Let(name, Box::new(value), Box::new(in_))
}

pub fn assoc(key: Expr, value: Expr) -> Expr {
    Expr::Assoc(Box::new(key), Box::new(value))
}

fn example_ast() -> Expr {
    let inner_if = Compr::If {
        condition: Box::new(call(
            field(
                "contains",
                call(
                    field("get", field("group_devices", var("var"))),
                    vec![
                        var("group"),
                        Expr::MapLit(vec![]),
                    ],
                ),
            ),
            vec![var("host")],
        )),
        body: Box::new(Seq::Compr(Compr::For {
            collection: Box::new(var("tags")),
            elements: vec!["tag"],
            body: Box::new(Seq::Elem(var("tag"))),
        })),
    };

    let body = let_(
        "device_tags",
        call(
            field("get", field("device_tags", var("var"))),
            vec![var("host"), Expr::ListLit(vec![])],
        ),
        let_(
            "group_tags",
            Expr::MapLit(vec![
                Seq::Compr(
                    Compr::For {
                        collection: Box::new(field("group_tags", var("var"))),
                        elements: vec!["group", "tags"],
                        body: Box::new(Seq::Compr(inner_if)),
                    }
                )
            ]),
            assoc(
                var("host"),
                union(var("group_tags"), var("device_tags")),
            ),
        )
    );

    Expr::MapLit(singleton(
        "tags",
        Expr::MapLit(vec![
            Seq::Compr(
                Compr::For {
                    collection: Box::new(field("hosts", var("var"))),
                    elements: vec!["host"],
                    body: Box::new(Seq::Compr(
                        Compr::If {
                            condition: Box::new(neg(
                                call(
                                    field("contains", field("excluded_devices", var("var"))),
                                    vec![var("host")],
                                )
                            )),
                            body: Box::new(Seq::Elem(body)),
                        }
                    ))
                }
            )
        ]),
    ))
}

fn val_string(v: &str) -> Rc<Value> {
    Rc::new(Value::String(v.to_string()))
}

fn val_list(vs: &[&Rc<Value>]) -> Rc<Value> {
    Rc::new(Value::List(vs.iter().copied().cloned().collect()))
}

fn example_input() -> Env {
    let mut env = Env::new();

    let ams01 = val_string("ams01");
    let ams02 = val_string("ams02");
    let fra01 = val_string("fra01");
    let fra02 = val_string("fra02");
    let fra03 = val_string("fra03");
    let lax01 = val_string("lax01");
    let zrh01 = val_string("zrh01");
    let zrh02 = val_string("zrh02");

    let tag_ch = val_string("tag:ch");
    let tag_de = val_string("tag:de");
    let tag_nl = val_string("tag:nl");
    let tag_us = val_string("tag:us");

    let icelake = val_string("tag:icelake");
    let skylake = val_string("tag:skylake");
    let znver2 = val_string("tag:znver2");
    let znver3 = val_string("tag:znver3");
    let primary = val_string("tag:primary");
    let standby = val_string("tag:standby");

    let hosts = val_list(&[
        &ams01,
        &ams02,
        &fra01,
        &fra02,
        &fra03,
        &lax01,
        &zrh01,
        &zrh02,
    ]);
    env.push("hosts", hosts);

    let excluded_devices = val_list(&[&lax01]);
    env.push("excluded_devices", excluded_devices);

    let mut device_tags = BTreeMap::new();
    device_tags.insert(ams01.clone(), val_list(&[&znver2, &primary]));
    device_tags.insert(ams02.clone(), val_list(&[&znver3, &standby]));
    device_tags.insert(fra01.clone(), val_list(&[&icelake]));
    device_tags.insert(fra02.clone(), val_list(&[&icelake]));
    device_tags.insert(fra03.clone(), val_list(&[&icelake]));
    device_tags.insert(lax01.clone(), val_list(&[&skylake]));
    device_tags.insert(zrh01.clone(), val_list(&[&znver3, &standby]));
    device_tags.insert(zrh02.clone(), val_list(&[&znver3, &primary]));
    env.push("device_tags", Rc::new(Value::Map(device_tags)));

    env
}

fn main() {
    println!("{:#?}", example_ast());
}
