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

    /// A string literal.
    StringLit(String),

    /// A for-comprehension.
    Compr(Box<Compr>),

    /// An conditional choice (if, then, else).
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),

    /// Access a variable.
    Var(Ident),

    /// A let-binding. First is the bound value, then the result expression.
    Let(Ident, Box<Expr>, Box<Expr>),

    /// Access a field or key.
    Field(Ident, Box<Expr>),

    /// A `key: value` mapping.
    Assoc(Box<Expr>, Box<Expr>),

    /// Call a function.
    Call(Box<Expr>, Vec<Expr>),

    /// Define a function.
    Lam(Vec<Ident>, Box<Expr>),

    /// Apply a unary operator.
    UnOp(UnOp, Box<Expr>),

    /// Apply a binary operator.
    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

/// One or more elements of a sequence.
#[derive(Debug)]
pub enum Seq {
    /// A single element.
    Elem(Expr),

    /// A comprehension that yields elements or mappings.
    Compr(Compr),
}

/// A for-comprehension.
#[derive(Debug)]
pub enum Compr {
    /// Loop over the collection.
    For {
        collection: Box<Expr>,
        elements: Vec<Ident>,
        body: Box<Seq>,
    },

    /// Enter the loop only if the condition is true.
    If {
        condition: Box<Expr>,
        body: Box<Seq>,
    },
}
