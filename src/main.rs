mod ast;

use ast::{BinOp, Compr, Expr, Ident, Seq, UnOp};

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

fn example() -> Expr {
    let inner_if = Compr::If {
        condition: Box::new(var("TODO")),
        body: Box::new(Seq::Elem(var("TODO"))),
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

fn main() {
    println!("{:#?}", example());
}
