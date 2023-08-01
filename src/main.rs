mod ast;

use ast::{Compr, Expr, Ident, Seq, UnOp};

/// Helpers for constructing AST in code.
pub fn var(name: Ident) -> Expr {
    Expr::Var(name)
}

pub fn field(obj: Expr, field: Ident) -> Expr {
    Expr::Field(Box::new(obj), field)
}

pub fn singleton(key: Ident, value: Expr) -> Vec<Seq> {
    vec![Seq::Field(key, value)]
}

pub fn neg(x: Expr) -> Expr {
    Expr::UnOp(UnOp::Neg, Box::new(x))
}

pub fn call(f: Expr, args: Vec<Expr>) -> Expr {
    Expr::Call(Box::new(f), args)
}

fn example() -> Expr {
    Expr::MapLit(singleton(
        "tags",
        Expr::MapLit(vec![
            Seq::Compr(
                Compr::For {
                    collection: Box::new(field(var("var"), "hosts")),
                    element: "host",
                    body: Box::new(Seq::Compr(
                        Compr::If {
                            condition: Box::new(neg(
                                call(
                                    field(field(var("var"), "excluded_devices"), "contains"),
                                    vec![var("host")],
                                )
                            )),
                            body: Box::new(Seq::Elem(var("TODO"))),
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
