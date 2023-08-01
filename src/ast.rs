use std::collections::{HashSet, HashMap};

#[derive(Debug)]
pub struct TypeVar(u32);

#[derive(Debug)]
pub enum SimpleType {
    Var(TypeVar),
    Bool,
    Int,
    String,
    Vec(Box<SimpleType>),
    Set(Box<SimpleType>),
    Map(Box<SimpleType>, Box<SimpleType>),
    Fun(Box<SimpleType>, Box<SimpleType>),
}

#[derive(Debug)]
pub enum Type {
    Simple(SimpleType),
    ForAll(TypeVar, Box<Type>),
}

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    // TODO: Should be a bigint.
    Int(i64),
    String(String),
    List(Vec<Value>),
    // TODO: Should preserve insertion order.
    Set(HashSet<Value>),
    // TODO: Should preserve insertion order.
    Map(HashMap<Value, Value>),
}

pub type Ident = &'static str;

/// A unary operator.
#[derive(Debug)]
pub enum UnOp {
    /// Negate a boolean.
    Neg
}

/// A binary operator.
#[derive(Debug)]
pub enum BinOp {
    /// `|`: Union two collections
    Union,

    /// `+`: Add two numbers.
    Add,
}

#[derive(Debug)]
pub enum Expr {
    /// A map or set literal, depending on the element types.
    MapLit(Vec<Seq>),

    /// A list literal.
    ListLit(Vec<Seq>),

    /// A for-comprehension.
    Compr(Box<Compr>),

    /// An conditional choice (if, then, else).
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),

    /// Access a variable.
    Var(Ident),

    /// Access a field or key.
    Field(Box<Expr>, Ident),

    /// Call a function.
    Call(Box<Expr>, Vec<Expr>),

    /// Define a function.
    Lam(Vec<Ident>, Box<Expr>),

    /// Apply a unary operator.
    UnOp(UnOp, Box<Expr>),

    /// Apply a binary operator.
    BinOp(BinOp, Box<Expr>),
}

/// One or more elements of a sequence.
#[derive(Debug)]
pub enum Seq {
    /// A single element.
    Elem(Expr),
    
    /// A `key = value` mapping, where the key is syntactially an identifier.
    Field(Ident, Expr),

    /// A `key: value` mapping, where the key is an expression.
    Assoc(Expr, Expr),

    /// A comprehension that yields elements or mappings.
    Compr(Compr),
}

/// A for-comprehension.
#[derive(Debug)]
pub enum Compr {
    /// Loop over the collection.
    For {
        collection: Box<Expr>,
        element: Ident,
        body: Box<Seq>,
    },

    /// Enter the loop only if the condition is true.
    If {
        condition: Box<Expr>,
        body: Box<Seq>,
    },
}
