// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Representations of values and scopes at runtime.

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ast::{CallArg, Expr};
use crate::decimal::Decimal;
use crate::error::{IntoError, PathElement, Result};
use crate::eval::Evaluator;
use crate::fmt_rcl::format_rcl;
use crate::fmt_type::format_type;
use crate::pprint::{concat, indent, Doc};
use crate::source::Span;
use crate::type_diff::{Mismatch, TypeDiff};
use crate::type_source::Source;
use crate::types;
use crate::types::{Side, SourcedType, Type};

/// The arguments to a function call at runtime.
pub struct FunctionCall<'a> {
    /// The opening paren for the call.
    pub call_open: Span,

    /// The closing paren for the call.
    pub call_close: Span,

    /// The arguments and their spans in the source code.
    pub args: &'a [CallArg<Value>],
}

/// The arguments to a method call at runtime.
pub struct MethodCall<'a> {
    /// The source code span of the receiver of the method call.
    ///
    /// In `widget.len()`, the receiver is `widget`.
    pub receiver_span: Span,

    /// The receiver of the call.
    pub receiver: &'a Value,

    /// The span of the method being called.
    ///
    /// In `widget.len()`, the method is `len`.
    pub method_span: Span,

    /// Arguments to the call.
    pub call: FunctionCall<'a>,
}

/// A built-in function.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub struct BuiltinFunction {
    pub name: &'static str,
    pub type_: fn() -> types::Function,
    pub f: for<'a> fn(&'a mut Evaluator, FunctionCall<'a>) -> Result<Value>,
}

/// A built-in method.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub struct BuiltinMethod {
    pub name: &'static str,
    pub type_: fn() -> types::Function,
    pub f: for<'a> fn(&'a mut Evaluator, MethodCall<'a>) -> Result<Value>,
}

/// A method and its receiver.
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct MethodInstance {
    /// The span where we refer to the method, e.g. the `keys` in `{}.keys()`.
    pub method_span: Span,
    /// The method to be called.
    pub method: &'static BuiltinMethod,
    /// The span of the receiving expression, e.g. the `{}` in `{}.keys()`.
    pub receiver_span: Span,
    /// The receiver of the call.
    pub receiver: Value,
}

impl std::fmt::Debug for BuiltinFunction {
    // coverage:off -- Debug is needed for assert, not expected to be called.
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}@{:p}", self.name, self.f)
    }
    // coverage:on
}

impl std::fmt::Debug for BuiltinMethod {
    // coverage:off -- Debug is needed for assert, not expected to be called.
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}@{:p}", self.name, self.f)
    }
    // coverage:on
}

#[derive(Debug)]
pub struct Function {
    /// Source location of lambda, including args, `=>`, and body.
    ///
    /// This span is used to identify the function for comparison and equality,
    /// so we don't have to inspect its AST.
    pub span: Span,

    /// Captured environment at the time of the call.
    ///
    /// TODO: It might be nicer to capture only the variables that are needed,
    /// but then we need to inspect the body AST when the lambda is produced.
    pub env: Env,
    pub body: Rc<Expr>,

    /// The type of this function, including its arguments.
    pub type_: Rc<types::Function>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Function) -> bool {
        // What matters for the identity of the lambda is where in the source
        // code it was produced. If that is the same, then the args and body are
        // necessarily the same. But the captured environment could be different,
        // so we take that into account too.
        (self.span, &self.env) == (other.span, &other.env)
    }
}

impl Eq for Function {}

impl PartialOrd for Function {
    // coverage:off -- All callers use `Ord`, not `PartialOrd`.
    fn partial_cmp(&self, other: &Function) -> Option<Ordering> {
        Some(self.cmp(other))
    }
    // coverage:on
}

impl Ord for Function {
    fn cmp(&self, other: &Function) -> Ordering {
        let lhs = (self.span, &self.env);
        let rhs = (other.span, &other.env);
        lhs.cmp(&rhs)
    }
}

/// A value.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Value {
    Null,

    Bool(bool),

    Number(Decimal),

    String(Rc<str>),

    List(Rc<Vec<Value>>),

    // TODO: Should preserve insertion order.
    Set(Rc<BTreeSet<Value>>),

    // TODO: Should preserve insertion order.
    Dict(Rc<BTreeMap<Value, Value>>),

    Function(Rc<Function>),

    BuiltinFunction(&'static BuiltinFunction),

    BuiltinMethod(Rc<MethodInstance>),
}

impl Value {
    pub fn int(i: i64) -> Value {
        Value::Number(Decimal::from(i))
    }

    /// Extract an integer if the value is a number that is integral.
    #[inline]
    pub fn to_i64(&self) -> Option<i64> {
        match self {
            Value::Number(d) => d.to_i64(),
            _ => None,
        }
    }

    /// Extract a number if the value is a number, panic otherwise.
    #[inline]
    pub fn expect_number(&self) -> &Decimal {
        match self {
            Value::Number(d) => d,
            other => panic!("Expected Number but got {other:?}."),
        }
    }

    /// Extract the dict if it is one, panic otherwise.
    #[inline]
    pub fn expect_dict(&self) -> &BTreeMap<Value, Value> {
        match self {
            Value::Dict(inner) => inner,
            other => panic!("Expected Dict but got {other:?}."),
        }
    }

    /// Extract the list if it is one, panic otherwise.
    #[inline]
    pub fn expect_list(&self) -> &[Value] {
        match self {
            Value::List(inner) => inner.as_ref(),
            other => panic!("Expected List but got {other:?}."),
        }
    }

    /// Extract the list if it is one, panic otherwise.
    #[inline]
    pub fn expect_set(&self) -> &BTreeSet<Value> {
        match self {
            Value::Set(inner) => inner,
            other => panic!("Expected Set but got {other:?}."),
        }
    }

    /// Extract the string if it is one, panic otherwise.
    #[inline]
    pub fn expect_string(&self) -> &str {
        match self {
            Value::String(inner) => inner.as_ref(),
            other => panic!("Expected String but got {other:?}."),
        }
    }

    /// As [`Value::expect_string`], but make an owned `Rc` copy instead of borrowing.
    #[inline]
    pub fn expect_string_clone(&self) -> Rc<str> {
        match self {
            Value::String(inner) => inner.clone(),
            other => panic!("Expected String but got {other:?}."),
        }
    }

    /// Dynamically check that the value fits the required type.
    pub fn is_instance_of(&self, at: Span, type_: &SourcedType) -> Result<()> {
        let req_type = match &type_.type_ {
            Type::Any => return Ok(()),
            t => t,
        };
        match (req_type, self) {
            // For the primitive types, we just check for matching values.
            (Type::Null, Value::Null) => return Ok(()),
            (Type::Bool, Value::Bool(..)) => return Ok(()),
            (Type::Number, Value::Number(..)) => return Ok(()),
            (Type::String, Value::String(..)) => return Ok(()),

            // For compound types, we descend into them to check.
            (Type::List(elem_type), Value::List(elems)) => {
                for (i, elem) in elems.iter().enumerate() {
                    elem.is_instance_of(at, elem_type)
                        .map_err(|err| err.with_path_element(PathElement::Index(i)))?;
                }
                return Ok(());
            }
            (Type::Set(elem_type), Value::Set(elems)) => {
                for (i, elem) in elems.iter().enumerate() {
                    elem.is_instance_of(at, elem_type).map_err(|err|
                        // Even though sets don't strictly have indexes,
                        // they do have an order, so report the index to
                        // clarify that this is a nested error.
                        err.with_path_element(PathElement::Index(i)))?;
                }
                return Ok(());
            }
            (Type::Dict(dict), Value::Dict(kvs)) => {
                for (k, v) in kvs.iter() {
                    k.is_instance_of(at, &dict.key)
                        .map_err(|err| err.with_path_element(PathElement::Key(k.clone())))?;
                    v.is_instance_of(at, &dict.value)
                        .map_err(|err| err.with_path_element(PathElement::Key(k.clone())))?;
                }
                return Ok(());
            }

            (Type::Union(types), value) => {
                // For a union, if it's an instance of any member, then it's
                // okay, if not, we fall through to the generic error at the end.
                for member in types.members.iter() {
                    match value.is_instance_of(at, member) {
                        Ok(()) => return Ok(()),
                        Err(..) => continue,
                    }
                }
            }

            (Type::Function(fn_type), Value::Function(fn_val)) => {
                let error = match fn_val.type_.is_subtype_of(fn_type) {
                    TypeDiff::Ok(..) => return Ok(()),
                    // If we encounter a defer, if that happens statically at
                    // typecheck time then we can insert a runtime check. But
                    // now we are at runtime, and we can't guarantee that these
                    // types are compatible, so treat that as an error.
                    TypeDiff::Defer(..) => TypeDiff::Error(Mismatch::Atom {
                        actual: SourcedType {
                            type_: Type::Function(fn_val.type_.clone()),
                            source: Source::None,
                        },
                        expected: SourcedType {
                            type_: Type::Function(fn_type.clone()),
                            source: Source::None,
                        },
                    }),
                    error => error,
                };
                error.check(at)?;
                unreachable!("The above ? fails.")
            }

            _ => {}
        }

        let mut error = at.error("Type mismatch.").with_body(concat! {
            "Expected a value that fits this type:"
            Doc::HardBreak Doc::HardBreak
            indent! { format_type(req_type).into_owned() }
            Doc::HardBreak Doc::HardBreak
            "But got this value:"
            Doc::HardBreak Doc::HardBreak
            indent! { format_rcl(self).into_owned() }
        });
        type_.explain_error(Side::Expected, &mut error);
        error.err()
    }

    /// Return the depth of the most deeply nested primitive value.
    ///
    /// This is expensive to compute as it traverses the entire value. Its goal
    /// is to prevent very deep values where the `Ord` implementation is very
    /// expensive.
    pub fn depth(&self) -> u32 {
        match self {
            Value::Null => 0,
            Value::Bool(..) => 0,
            Value::Number(..) => 0,
            Value::String(..) => 0,
            Value::List(xs) => 1 + xs.iter().map(Value::depth).max().unwrap_or(0),
            Value::Set(xs) => 1 + xs.iter().map(Value::depth).max().unwrap_or(0),
            Value::Dict(kv) => {
                1 + u32::max(
                    kv.keys().map(Value::depth).max().unwrap_or(0),
                    kv.values().map(Value::depth).max().unwrap_or(0),
                )
            }
            Value::Function(f) => 1 + f.env.map_fold_max(Value::depth),
            Value::BuiltinFunction(..) => 0,
            Value::BuiltinMethod(m) => 1 + m.receiver.depth(),
        }
    }
}

impl<'a> From<&'a str> for Value {
    #[inline]
    fn from(value: &'a str) -> Self {
        Value::String(value.into())
    }
}

/// An environment binds names to values.
pub type Env = crate::env::Env<Value>;

/// Create a new environment with an initialized standard library.
pub fn prelude() -> Env {
    let mut env = Env::new();
    env.push("std".into(), crate::stdlib::initialize());
    env
}

macro_rules! builtin_function {
    (
        $rcl_name:expr,
        ( $( $arg_name:ident: $arg_type:tt ),* ) -> $result:tt,
        const $rust_const:ident,
        $rust_name:ident
    ) => {
        pub const $rust_const: crate::runtime::BuiltinFunction = crate::runtime::BuiltinFunction {
            name: $rcl_name,
            type_: || {
                #[allow(unused_imports)]
                use crate::types::{Type, Dict, Function, FunctionArg, builtin, make_function, make_type};
                crate::types::make_function!( ($( $arg_name: $arg_type ),*) -> $result)
            },
            f: $rust_name,
        };
    };
}
pub(crate) use builtin_function;

macro_rules! builtin_method {
    (
        $rcl_name:expr,
        ( $( $arg_name:ident: $arg_type:tt ),* ) -> $result:tt,
        const $rust_const:ident,
        $rust_name:ident
    ) => {
        pub const $rust_const: crate::runtime::BuiltinMethod = crate::runtime::BuiltinMethod {
            name: $rcl_name,
            type_: || {
                #[allow(unused_imports)]
                use crate::types::{Type, Dict, Function, FunctionArg, builtin, make_function, make_type};
                crate::types::make_function!( ($( $arg_name: $arg_type ),*) -> $result)
            },
            f: $rust_name,
        };
    };
}
pub(crate) use builtin_method;
