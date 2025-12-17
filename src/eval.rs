// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Evaluation turns ASTs into values.

use std::collections::HashMap;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ast::{BinOp, CallArg, Expr, FormatFragment, Seq, Stmt, UnOp, Yield};
use crate::error::{Error, IntoError, PathElement, Result};
use crate::fmt_rcl::{self, format_rcl};
use crate::loader::Loader;
use crate::pprint::{concat, indent, Doc};
use crate::runtime::{
    self, BuiltinFunction, BuiltinMethod, Env, Function, FunctionCall, MethodCall, MethodInstance,
    Value,
};
use crate::source::{DocId, Span};
use crate::stdlib;
use crate::tracer::Tracer;
use crate::typecheck;
use crate::types;

/// An entry on the evaluation stack.
pub struct EvalContext {
    /// The document that we are evaluating.
    pub doc: DocId,
    /// The source that caused evaluation of this document.
    pub imported_from: Option<Span>,
}

/// A limiter to catch infinite loops.
///
/// Some programs diverge, and instead of hanging, we want to report an error.
/// But some inputs are just very large, and we don't want to put an arbitrary
/// low limit on the number of evaluation steps. To get around this, we also
/// track the span where we are evaluating. As long as it keeps increasing, we
/// make progress. But when we keep visiting the same span, then the limit kicks
/// in.
///
/// An example divergent program:
/// ```text
/// let f = g => g(g(h => k => g(g(h)))); f(f)
/// ```
pub struct EvalCount {
    /// The maximum visited location.
    pub span: Span,
    /// The number of times the evaluation depth was incremented at that location.
    pub count: u32,
}

impl EvalCount {
    pub fn new() -> EvalCount {
        EvalCount {
            span: Span::new(DocId(0), 0, 0),
            count: 0,
        }
    }

    /// Increment the count, return an error if the budget is exceeded.
    #[inline]
    pub fn inc(&mut self, at: Span) -> Result<()> {
        if at > self.span {
            self.span = at;
            self.count = 0;
        }

        // The maximum number of evaluation steps is a bit of a difficult limit.
        // If we set it too low, it becomes impossible to evaluate legitimate
        // programs that are large. This also applies to trying to do too much
        // general-purpose programming in RCL, it is possible, but not the
        // intended use case. So for release builds, we set a reasonably
        // generous limit.
        //
        // For debug builds, because we do need to test this error, and we don't
        // want the tests to be super slow, set a more aggressive limit. And for
        // fuzzing, set a more aggressive limit still, because doing 2000 more
        // iterations of whatever it can do in 200 iterations is very unlikely
        // to discover new code paths, but it does make the fuzzer less
        // effective.
        #[cfg(fuzzing)]
        let max_steps = 250;

        #[cfg(all(not(fuzzing), debug_assertions))]
        let max_steps = 10_000;

        #[cfg(all(not(fuzzing), not(debug_assertions)))]
        let max_steps = 10_000_000;

        self.count += 1;

        if self.count >= max_steps {
            return at
                .error(concat! {
                    "Evaluation budget exceeded. "
                    "This expression exceeds the maximum of "
                    max_steps.to_string()
                    " steps."
                })
                .err();
        }

        Ok(())
    }
}

pub struct Evaluator<'a> {
    pub loader: &'a mut Loader,
    pub tracer: &'a mut dyn Tracer,
    pub import_stack: Vec<EvalContext>,

    /// The single instance of the standard library.
    ///
    /// Because it is immutable, it can be reused across evaluations.
    pub stdlib: Value,

    /// The types of the built-in functions.
    ///
    /// Types need to be constructed at runtime because they contain heap objects.
    /// But the types of built-ins does not change. So we only do it once, and
    /// then cache them here.
    function_type_cache: HashMap<fn() -> types::Function, types::Function>,

    /// The depth of the evaluation stack.
    ///
    /// Used to error before we overflow the native stack.
    pub eval_depth: u32,

    /// The number of times the evaluation depth was incremented.
    ///
    /// This is used to break infinite loops.
    pub eval_count: EvalCount,
}

impl<'a> Evaluator<'a> {
    pub fn new(loader: &'a mut Loader, tracer: &'a mut dyn Tracer) -> Evaluator<'a> {
        Evaluator {
            loader,
            tracer,
            import_stack: Vec::new(),
            stdlib: stdlib::initialize(),
            function_type_cache: HashMap::new(),
            eval_depth: 0,
            eval_count: EvalCount::new(),
        }
    }

    /// Return the type of a builtin function, cached.
    fn get_builtin_function_type(&mut self, builtin: &BuiltinFunction) -> &types::Function {
        self.function_type_cache
            .entry(builtin.type_)
            .or_insert_with(|| (builtin.type_)())
    }

    /// Return the type of a builtin method, cached.
    fn get_builtin_method_type(&mut self, builtin: &BuiltinMethod) -> &types::Function {
        self.function_type_cache
            .entry(builtin.type_)
            .or_insert_with(|| (builtin.type_)())
    }

    #[inline]
    fn inc_eval_depth(&mut self, at: Span) -> Result<()> {
        // Error out when the call stack gets too deep, instead of waiting for
        // the native call stack to overflow. In practice, unless you are doing
        // recursion, the call stack shouldn't be extremely deep, and for
        // recursion we need a better solution, so set a fairly low limit.
        let max_eval_depth = 150;
        self.eval_depth += 1;

        if self.eval_depth >= max_eval_depth {
            return at
                .error(concat! {
                    "Evaluation budget exceeded. "
                    "This expression exceeds the maximum evaluation depth of "
                    max_eval_depth.to_string()
                    "."
                })
                .err();
        }

        self.eval_count.inc(at)
    }

    #[inline]
    fn dec_eval_depth(&mut self) {
        self.eval_depth -= 1;
    }

    /// Evaluate a document as the entry point of evaluation.
    pub fn eval_doc(
        &mut self,
        type_env: &mut typecheck::Env,
        value_env: &mut Env,
        doc: DocId,
    ) -> Result<Value> {
        debug_assert!(self.import_stack.is_empty());
        let expr = self.loader.get_typechecked_ast(type_env, doc)?;
        let ctx = EvalContext {
            doc,
            imported_from: None,
        };
        self.import_stack.push(ctx);
        let result = self.eval_expr(value_env, &expr)?;
        self.import_stack.pop().expect("Push/pop are balanced.");
        Ok(result)
    }

    /// Evaluate a document for an import.
    fn eval_import(&mut self, doc: DocId, imported_from: Span) -> Result<Value> {
        // Before we allow the import, check that this would not create a cycle.
        let mut error: Option<Error> = None;
        for ctx in &self.import_stack {
            if ctx.doc == doc {
                debug_assert!(
                    error.is_none(),
                    // coverage:off -- Error message not hit when there is no error.
                    "There should not be a cycle in the stack already.",
                    // coverage:on
                );
                let err = imported_from.error("This import creates a cycle.");
                error = Some(err);
            }
            if let Some(error) = error.as_mut() {
                if let Some(src) = ctx.imported_from {
                    error.add_note(src, "Imported here.");
                }
            }
        }
        if let Some(mut err) = error {
            // Reverse the notes: we print the error itself first, and then we
            // want to print what the offending file was imported from, etc.
            err.notes.reverse();
            return Err(err.into());
        }

        // Evaluate the import in its own clean environment, it should not be
        // affected by the surrounding environment of the import statement.
        let mut type_env = typecheck::prelude();
        let mut value_env = runtime::prelude();

        let expr = self.loader.get_typechecked_ast(&mut type_env, doc)?;
        let ctx = EvalContext {
            doc,
            imported_from: Some(imported_from),
        };

        self.import_stack.push(ctx);
        let result = self.eval_expr(&mut value_env, &expr)?;
        self.import_stack.pop().expect("Push/pop are balanced.");

        Ok(result)
    }

    fn eval_expr(&mut self, env: &mut Env, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Import {
                path_span,
                path: path_expr,
            } => {
                // We could allow an arbitrary expression that evaluates to a
                // string, but for now we limit ourselves to string literals. It
                // ensures that the import graph can be statically known by
                // traversing the AST without evaluating it, and it ensures
                // filenames can be grepped, there are no "magic" imports. This
                // requirement is artificial, we can relax it later if that turns
                // out to be useful.
                let path = match path_expr.as_ref() {
                    Expr::StringLit(path) => path,
                    Expr::Format(..) => {
                        return path_span
                            .error("Import path must be a string literal without holes.")
                            .err()
                    }
                    _ => {
                        return path_span
                            .error("Import path must be a string literal.")
                            .err()
                    }
                };
                let from = Some(path_span.doc());
                let doc = self
                    .loader
                    .load_path(path.as_ref(), from)
                    .map_err(|mut err| {
                        if err.origin.is_none() {
                            err.origin = Some(*path_span);
                        }
                        err
                    })?;
                self.eval_import(doc, *path_span)
            }

            // coverage:off -- Code not expected to be reached.
            Expr::BraceLit { .. } => {
                unreachable!("The typechecker replaces `BraceLit` with `SetLit` or `DictLit`.")
            }
            // coverage:on

            // Brackets are syntactically lists, we already know that.
            Expr::BracketLit { open, elements } => {
                let mut out = Vec::with_capacity(elements.len());
                self.inc_eval_depth(*open)?;
                for seq in elements {
                    self.eval_seq(env, seq, &mut |v| out.push(v), &mut |_, _| {
                        unreachable!("Typechecker ensures scalar elements.")
                    })?;
                }
                self.dec_eval_depth();
                Ok(Value::List(Rc::new(out)))
            }

            Expr::SetLit { open, elements } => {
                let mut out = BTreeSet::new();
                self.inc_eval_depth(*open)?;
                for seq in elements {
                    self.eval_seq(env, seq, &mut |v| _ = out.insert(v), &mut |_, _| {
                        unreachable!("Typechecker ensures scalar elements.")
                    })?;
                }
                self.dec_eval_depth();
                Ok(Value::Set(Rc::new(out)))
            }

            Expr::DictLit { open, elements } => {
                let mut out = BTreeMap::new();
                self.inc_eval_depth(*open)?;
                for seq in elements {
                    self.eval_seq(
                        env,
                        seq,
                        &mut |_| unreachable!("Typechecker ensures assoc elements."),
                        &mut |k, v| _ = out.insert(k, v),
                    )?;
                }
                self.dec_eval_depth();
                Ok(Value::Dict(Rc::new(out)))
            }

            Expr::NullLit => Ok(Value::Null),

            Expr::BoolLit(b) => Ok(Value::Bool(*b)),

            Expr::NumberLit(d) => Ok(Value::Number(*d)),

            Expr::StringLit(s) => Ok(Value::String(s.clone())),

            Expr::Format(fragments) => self.eval_format(env, fragments),

            Expr::IfThenElse {
                condition_span,
                condition,
                body_then,
                body_else,
                ..
            } => {
                self.inc_eval_depth(*condition_span)?;
                let cond = self.eval_expr(env, condition)?;
                let result = match cond {
                    Value::Bool(true) => self.eval_expr(env, body_then),
                    Value::Bool(false) => self.eval_expr(env, body_else),
                    _ => unreachable!("The typechecker ensures the condition is a Bool."),
                };
                self.dec_eval_depth();
                result
            }

            Expr::Var { ident, .. } => match env.lookup(ident) {
                Some(value) => Ok(value.clone()),
                // TODO: The typechecker could replace all variable lookups with
                // an index into the bindings stack, then we can skip the lookup,
                // and we don't have to handle this case (except though implicit
                // bounds checks).
                None => unreachable!("If it passed the typechecker, the variable exists."),
            },

            Expr::Field {
                field_span,
                field: field_name,
                inner_span,
                inner: inner_expr,
            } => {
                self.inc_eval_depth(*inner_span)?;
                let inner = self.eval_expr(env, inner_expr)?;
                self.dec_eval_depth();
                let field_name_value = Value::String(field_name.0.clone());

                let builtin = match (&inner, field_name.as_ref()) {
                    (Value::String(_), "chars") => Some(&stdlib::STRING_CHARS),
                    (Value::String(_), "contains") => Some(&stdlib::STRING_CONTAINS),
                    (Value::String(_), "ends_with") => Some(&stdlib::STRING_ENDS_WITH),
                    (Value::String(_), "len") => Some(&stdlib::STRING_LEN),
                    (Value::String(_), "parse_int") => Some(&stdlib::STRING_PARSE_INT),
                    (Value::String(_), "parse_number") => Some(&stdlib::STRING_PARSE_NUMBER),
                    (Value::String(_), "remove_prefix") => Some(&stdlib::STRING_REMOVE_PREFIX),
                    (Value::String(_), "remove_suffix") => Some(&stdlib::STRING_REMOVE_SUFFIX),
                    (Value::String(_), "replace") => Some(&stdlib::STRING_REPLACE),
                    (Value::String(_), "split") => Some(&stdlib::STRING_SPLIT),
                    (Value::String(_), "split_lines") => Some(&stdlib::STRING_SPLIT_LINES),
                    (Value::String(_), "starts_with") => Some(&stdlib::STRING_STARTS_WITH),
                    (Value::String(_), "to_lowercase") => Some(&stdlib::STRING_TO_LOWERCASE),
                    (Value::String(_), "to_uppercase") => Some(&stdlib::STRING_TO_UPPERCASE),

                    (Value::Number(_), "round") => Some(&stdlib::NUMBER_ROUND),

                    (Value::Dict(_), "contains") => Some(&stdlib::DICT_CONTAINS),
                    (Value::Dict(_), "except") => Some(&stdlib::DICT_EXCEPT),
                    (Value::Dict(_), "get") => Some(&stdlib::DICT_GET),
                    (Value::Dict(_), "keys") => Some(&stdlib::DICT_KEYS),
                    (Value::Dict(_), "len") => Some(&stdlib::DICT_LEN),
                    (Value::Dict(_), "values") => Some(&stdlib::DICT_VALUES),
                    (Value::Dict(fields), _field_name) => {
                        // If it wasn't a builtin, look for a key in the dict.
                        return match fields.get(&field_name_value) {
                            Some(v) => Ok(v.clone()),
                            None => {
                                return field_span
                                    .error("Unknown field.")
                                    .with_note(
                                        *inner_span,
                                        concat! {
                                            // TODO: Printing the full value may be overkill,
                                            // the full value could be very large. We
                                            // could print the dict keys here.
                                            "On value: " format_rcl(&inner).into_owned()
                                        },
                                    )
                                    .err();
                            }
                        };
                    }

                    (Value::List(_), "all") => Some(&stdlib::LIST_ALL),
                    (Value::List(_), "any") => Some(&stdlib::LIST_ANY),
                    (Value::List(_), "contains") => Some(&stdlib::LIST_CONTAINS),
                    (Value::List(_), "enumerate") => Some(&stdlib::LIST_ENUMERATE),
                    (Value::List(_), "filter") => Some(&stdlib::LIST_FILTER),
                    (Value::List(_), "flat_map") => Some(&stdlib::LIST_FLAT_MAP),
                    (Value::List(_), "fold") => Some(&stdlib::LIST_FOLD),
                    (Value::List(_), "group_by") => Some(&stdlib::LIST_GROUP_BY),
                    (Value::List(_), "join") => Some(&stdlib::LIST_JOIN),
                    (Value::List(_), "key_by") => Some(&stdlib::LIST_KEY_BY),
                    (Value::List(_), "len") => Some(&stdlib::LIST_LEN),
                    (Value::List(_), "map") => Some(&stdlib::LIST_MAP),
                    (Value::List(_), "reverse") => Some(&stdlib::LIST_REVERSE),
                    (Value::List(_), "sort") => Some(&stdlib::LIST_SORT),
                    (Value::List(_), "sort_by") => Some(&stdlib::LIST_SORT_BY),
                    (Value::List(_), "sum") => Some(&stdlib::LIST_SUM),
                    (Value::List(_), "to_set_dedup") => Some(&stdlib::LIST_TO_SET_DEDUP),
                    (Value::List(_), "to_set_unique") => Some(&stdlib::LIST_TO_SET_UNIQUE),

                    (Value::Set(_), "all") => Some(&stdlib::SET_ALL),
                    (Value::Set(_), "any") => Some(&stdlib::SET_ANY),
                    (Value::Set(_), "contains") => Some(&stdlib::SET_CONTAINS),
                    (Value::Set(_), "except") => Some(&stdlib::SET_EXCEPT),
                    (Value::Set(_), "filter") => Some(&stdlib::SET_FILTER),
                    (Value::Set(_), "flat_map_dedup") => Some(&stdlib::SET_FLAT_MAP_DEDUP),
                    (Value::Set(_), "group_by") => Some(&stdlib::SET_GROUP_BY),
                    (Value::Set(_), "key_by") => Some(&stdlib::SET_KEY_BY),
                    (Value::Set(_), "len") => Some(&stdlib::SET_LEN),
                    (Value::Set(_), "map_dedup") => Some(&stdlib::SET_MAP_DEDUP),
                    (Value::Set(_), "sort") => Some(&stdlib::SET_SORT),
                    (Value::Set(_), "sort_by") => Some(&stdlib::SET_SORT_BY),
                    (Value::Set(_), "sum") => Some(&stdlib::SET_SUM),
                    (Value::Set(_), "to_list") => Some(&stdlib::SET_TO_LIST),
                    (Value::Set(_), "transitive_closure") => Some(&stdlib::SET_TRANSITIVE_CLOSURE),

                    _other => None,
                };
                match builtin {
                    Some(b) => {
                        let instance = MethodInstance {
                            receiver_span: *inner_span,
                            receiver: inner,
                            method_span: *field_span,
                            method: b,
                        };
                        Ok(Value::BuiltinMethod(Rc::new(instance)))
                    }
                    None => {
                        field_span
                            .error("Unknown field.")
                            .with_note(
                                *inner_span,
                                concat! {
                                    // TODO: Printing the full value may be overkill,
                                    // the full value could be very large.
                                    "On value: " format_rcl(&inner).into_owned()
                                },
                            )
                            .err()
                    }
                }
            }

            Expr::Stmt { stmt, body, .. } => {
                let ck = env.checkpoint();
                self.eval_stmt(env, stmt)?;
                let result = self.eval_expr(env, body)?;
                env.pop(ck);
                Ok(result)
            }

            Expr::Call {
                open,
                close,
                function_span,
                function: fun_expr,
                args: args_exprs,
            } => {
                // We do strict evaluation, all arguments get evaluated before we go
                // into the call.
                self.inc_eval_depth(*function_span)?;
                let fun = self.eval_expr(env, fun_expr)?;
                let args = args_exprs
                    .iter()
                    .map(|call_arg| {
                        Ok(CallArg {
                            span: call_arg.span,
                            value: self.eval_expr(env, &call_arg.value)?,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                self.dec_eval_depth();

                let call = FunctionCall {
                    call_open: *open,
                    call_close: *close,
                    args: &args[..],
                };
                self.eval_call(*function_span, &fun, call)
            }

            Expr::Index {
                open,
                collection_span,
                collection: collection_expr,
                index: index_expr,
                index_span,
                ..
            } => {
                let collection = self.eval_expr(env, collection_expr)?;
                let index = self.eval_expr(env, index_expr)?;
                self.eval_index(*open, collection, *collection_span, index, *index_span)
            }

            // coverage:off -- Not covered if it's really unreachable.
            Expr::Function { .. } => unreachable!(
                "The typechecker replaces all Expr::Function with Expr::TypedFunction.",
            ),
            // coverage:on
            Expr::TypedFunction {
                span,
                body_span: _,
                body,
                type_,
            } => {
                let result = Function {
                    span: *span,
                    env: env.clone(),
                    body: Rc::new((**body).clone()),
                    type_: type_.clone(),
                };
                Ok(Value::Function(Rc::new(result)))
            }

            Expr::UnOp {
                op,
                op_span,
                body: value_expr,
                ..
            } => {
                self.inc_eval_depth(*op_span)?;
                let value = self.eval_expr(env, value_expr)?;
                let result = self.eval_unop(*op, *op_span, value)?;
                self.dec_eval_depth();
                Ok(result)
            }

            Expr::BinOp {
                op,
                op_span,
                lhs: lhs_expr,
                rhs: rhs_expr,
                ..
            } => {
                self.inc_eval_depth(*op_span)?;
                let lhs = self.eval_expr(env, lhs_expr)?;
                let rhs = self.eval_expr(env, rhs_expr)?;
                let result = self.eval_binop(*op, *op_span, lhs, rhs)?;
                self.dec_eval_depth();
                Ok(result)
            }

            Expr::CheckType { span, type_, body } => {
                let v = self.eval_expr(env, body)?;
                v.is_instance_of(*span, type_)?;
                Ok(v)
            }
        }
    }

    /// Evaluate a call to any callable.
    ///
    /// This function adds a call frame. For calls made from builtins, the call
    /// frame may not be very clear. Then, in case of error, the caller of
    /// `eval_call` can replace the call frame with a more descriptive one.
    pub fn eval_call(
        &mut self,
        callee_span: Span,
        callee: &Value,
        call: FunctionCall,
    ) -> Result<Value> {
        let call_open = call.call_open;
        self.inc_eval_depth(call_open)?;

        let result = match callee {
            Value::BuiltinMethod(instance) => {
                let method = instance.method;
                let fn_type = self.get_builtin_method_type(method);
                fn_type.check_arity(Some(method.name), call.args, call.call_close)?;
                // TODO: Also check the type, while we're at it!

                let method_call = MethodCall {
                    call,
                    method_span: instance.method_span,
                    receiver_span: instance.receiver_span,
                    receiver: &instance.receiver,
                };

                (method.f)(self, method_call).map_err(|err| {
                    err.with_call_frame(
                        call_open,
                        concat! { "In call to method '" Doc::highlight(method.name) "'." },
                    )
                    .into()
                })
            }
            Value::BuiltinFunction(f) => {
                let fn_type = self.get_builtin_function_type(f);
                fn_type.check_arity(Some(f.name), call.args, call.call_close)?;
                // TODO: Also check the type, while we're at it!

                (f.f)(self, call).map_err(|err| {
                    err.with_call_frame(
                        call_open,
                        concat! { "In call to function '" Doc::highlight(f.name) "'." },
                    )
                    .into()
                })
            }
            Value::Function(fun) => {
                fun.type_.check_arity(None, call.args, call.call_close)?;
                // TODO: Also perform typechecks of the arguments.

                self.eval_function_call(fun, call).map_err(|err| {
                    err.with_call_frame(call_open, "In call to function.")
                        .into()
                })
            }
            _ => callee_span
                .error("This is not a function, it cannot be called.")
                .err(),
        };

        self.dec_eval_depth();

        result
    }

    /// Evaluate a call to a lambda function.
    ///
    /// This does not perform all required checks; use [`eval_call`] to evaluate
    /// a general call to any callable value.
    fn eval_function_call(&mut self, fun: &Function, call: FunctionCall) -> Result<Value> {
        // TODO: If we could stack multiple layers of envs, then we would not
        // have to clone the full thing.
        let mut env = fun.env.clone();
        for (arg, CallArg { value, .. }) in fun.type_.args.iter().zip(call.args) {
            let arg_name = arg
                .name
                .as_ref()
                .expect("Types attached to functions have arg names.");
            env.push(arg_name.clone(), value.clone());
        }

        self.eval_expr(&mut env, fun.body.as_ref())
    }

    /// While joining values for string formatting, push one fragment.
    ///
    /// This powers both format strings and `List.join`.
    pub fn push_format_fragment(out: &mut Vec<Rc<str>>, span: Span, value: &Value) -> Result<()> {
        match value {
            Value::Bool(b) => out.push((if *b { "true" } else { "false" }).into()),
            Value::Number(d) => out.push(d.format().into()),
            Value::Null => out.push("null".into()),
            Value::String(s) => out.push(s.clone()),
            not_formattable => {
                return span
                    .error(concat! {
                        "This value cannot be interpolated into a string:"
                        Doc::HardBreak
                        Doc::HardBreak
                        indent! { format_rcl(not_formattable).into_owned() }
                    })
                    .err();
            }
        }
        Ok(())
    }

    /// Join fragments pushed by [`Evaluator::push_format_fragment`] into one string.
    pub fn join_format_fragments(fragments: Vec<Rc<str>>) -> Value {
        let mut result = String::with_capacity(fragments.iter().map(|s| s.len()).sum());

        for s in fragments {
            result.push_str(s.as_ref());
        }

        Value::String(result.into())
    }

    /// Evaluate a format string.
    pub fn eval_format(&mut self, env: &mut Env, fragments: &[FormatFragment]) -> Result<Value> {
        let mut results = Vec::new();

        for fragment in fragments {
            let value = self.eval_expr(env, &fragment.body)?;
            Evaluator::push_format_fragment(&mut results, fragment.span, &value)?;
        }

        Ok(Evaluator::join_format_fragments(results))
    }

    fn eval_index(
        &mut self,
        open_span: Span,
        collection: Value,
        collection_span: Span,
        index: Value,
        index_span: Span,
    ) -> Result<Value> {
        match collection {
            Value::List(xs) => self.eval_index_list(&xs, index, index_span),
            Value::Dict(dict) => self.eval_index_dict(&dict, collection_span, index, index_span),
            // TODO: Implement indexing into strings.
            Value::String(..) => open_span
                .error("Indexing into a string is not yet supported.")
                .with_note(collection_span, "This is a string.")
                .err(),
            not_indexable => {
                let note = concat! {
                    "Expected a dict or list, but found: "
                    format_rcl(&not_indexable).into_owned()
                    "."
                };
                open_span
                    .error("Indexing is not supported here.")
                    .with_note(collection_span, note)
                    .err()
            }
        }
    }

    fn eval_index_list(&mut self, list: &[Value], index: Value, index_span: Span) -> Result<Value> {
        let i_signed = match index.to_i64() {
            Some(i) => i,
            _ => {
                return index_span
                    .error(concat! {
                        "Expected list index to be an integer, but got "
                        format_rcl(&index).into_owned()
                        "."
                    })
                    .err()
            }
        };

        let i = match i_signed {
            _ if i_signed >= 0 && (i_signed as usize) < list.len() => i_signed as usize,
            _ if i_signed >= -(list.len() as i64) && i_signed < 0 => {
                list.len() - (-i_signed as usize)
            }
            _ => {
                let error = concat! {
                    "Index "
                    i_signed.to_string()
                    " is out of bounds for list of length "
                    list.len().to_string()
                    "."
                };
                return index_span.error(error).err();
            }
        };

        Ok(list[i].clone())
    }

    fn eval_index_dict(
        &mut self,
        dict: &BTreeMap<Value, Value>,
        dict_span: Span,
        index: Value,
        index_span: Span,
    ) -> Result<Value> {
        match dict.get(&index) {
            None => index_span
                .error(concat! {
                    "Dict does not have a key "
                    format_rcl(&index).into_owned()
                    "."
                })
                .with_note(
                    dict_span,
                    concat! {
                        "On value: "
                        fmt_rcl::dict(dict.iter()).into_owned()
                    },
                )
                .err(),
            Some(v) => Ok(v.clone()),
        }
    }

    fn eval_unop(&mut self, op: UnOp, op_span: Span, v: Value) -> Result<Value> {
        match (op, v) {
            (UnOp::Not, Value::Bool(x)) => Ok(Value::Bool(!x)),
            (UnOp::Neg, Value::Number(d)) => match d.checked_neg() {
                Some(nd) => Ok(Value::Number(nd)),
                None => {
                    let err = concat! {
                        "Negation of " d.format() " would overflow."
                    };
                    op_span.error(err).err()
                }
            },
            _ => unreachable!("Invalid cases are prevented by the typechecker."),
        }
    }

    fn eval_binop(&mut self, op: BinOp, op_span: Span, lhs: Value, rhs: Value) -> Result<Value> {
        match (op, lhs, rhs) {
            (BinOp::Union, Value::Dict(xs), Value::Dict(ys)) => {
                let mut result = (*xs).clone();
                for (k, v) in ys.iter() {
                    result.insert(k.clone(), v.clone());
                }
                Ok(Value::Dict(Rc::new(result)))
            }
            (BinOp::Union, Value::Set(xs), Value::Set(ys)) => {
                let result = xs.union(ys.as_ref()).cloned().collect();
                Ok(Value::Set(Rc::new(result)))
            }
            (BinOp::Union, Value::Set(xs), Value::List(ys)) => {
                let mut result = (*xs).clone();
                result.extend(ys.iter().cloned());
                Ok(Value::Set(Rc::new(result)))
            }
            (BinOp::Union, _, _) => {
                // We could make a nicer error and include the values, but I plan
                // to remove | in favor of unpack, so I'm not going to bother.
                op_span
                    .error(concat! {
                        "Union operator " Doc::highlight("|")
                        " is not supported between these values. "
                    })
                    .with_help("The left-hand side must be a dict or set.")
                    .err()
            }
            // TODO: Could evaluate these boolean expressions lazily, if the
            // language is really pure. But if I enable external side effects like
            // running a program to read its input, that would be questionable to do.
            (BinOp::And, Value::Bool(x), Value::Bool(y)) => Ok(Value::Bool(x && y)),
            (BinOp::Or, Value::Bool(x), Value::Bool(y)) => Ok(Value::Bool(x || y)),
            (BinOp::Add, Value::Number(x), Value::Number(y)) => match x.checked_add(&y) {
                Some(z) => Ok(Value::Number(z)),
                None => {
                    let err = concat! {
                        "Addition " x.format() " + " y.format() " would overflow."
                    };
                    op_span.error(err).err()
                }
            },
            (BinOp::Sub, Value::Number(x), Value::Number(y)) => match x.checked_sub(&y) {
                Some(z) => Ok(Value::Number(z)),
                None => {
                    let err = concat! {
                        "Subtraction " x.format() " - " y.format() " would overflow."
                    };
                    op_span.error(err).err()
                }
            },
            (BinOp::Mul, Value::Number(x), Value::Number(y)) => match x.checked_mul(&y) {
                Some(z) => Ok(Value::Number(z)),
                None => {
                    let err = concat! {
                        "Multiplication " x.format() " * " y.format() " would overflow."
                    };
                    op_span.error(err).err()
                }
            },
            (BinOp::Div, Value::Number(x), Value::Number(y)) => {
                if y.mantissa == 0 {
                    op_span.error("Division by zero.").err()
                } else {
                    match x.checked_div_exact(&y) {
                        Some(z) => Ok(Value::Number(z)),
                        None => {
                            let err = concat! {
                                x.format() " / " y.format()
                                " cannot be represented exactly. "
                                "Lossy arithmetic is not supported at this time."
                            };
                            op_span.error(err).err()
                        }
                    }
                }
            }
            // We allow comparing any two values, even if they are not of the
            // same type. I would prefer to make nonsensical comparisons a type
            // error (e.g. `1 < "2"` should return "Number and String incomparable",
            // but due to the type lattice, there is no such thing as "same type".
            // We could enforce that the value discriminant is the same, and then
            // we can rule out `1 < "2"`, but not `[1] < ["2"]`. So let's just
            // allow comparing anything then.
            (BinOp::Lt, x, y) => Ok(Value::Bool(x < y)),
            (BinOp::Gt, x, y) => Ok(Value::Bool(x > y)),
            (BinOp::LtEq, x, y) => Ok(Value::Bool(x <= y)),
            (BinOp::GtEq, x, y) => Ok(Value::Bool(x >= y)),
            (BinOp::Eq, x, y) => Ok(Value::Bool(x == y)),
            (BinOp::Neq, x, y) => Ok(Value::Bool(x != y)),
            _ => unreachable!("Invalid cases are prevented by the typechecker."),
        }
    }

    fn eval_stmt(&mut self, env: &mut Env, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Let { ident, value, .. } => {
                // Note, this is not a recursive let, the variable is not bound
                // when we evaluate the expression. Even if the let binding has
                // a type annotation, we don't check it here; the typechecker
                // inserts a dedicated `CheckType` node when needed.
                let v = self.eval_expr(env, value)?;
                env.push(ident.clone(), v);
            }
            Stmt::Assert {
                condition_span,
                condition,
                message: message_expr,
                ..
            } => {
                match self.eval_expr(env, condition)? {
                    Value::Bool(true) => {
                        // Ok, the assertion passed, nothing else to do.
                    }
                    Value::Bool(false) => {
                        let message = self.eval_expr(env, message_expr)?;
                        let body: Doc = match &message {
                            // If the message is a string, then we include it directly,
                            // not pretty-printed as a value.
                            Value::String(msg) => Doc::lines(msg),
                            // Otherwise, we pretty-print it as an RCL value.
                            _ => format_rcl(&message),
                        };
                        return condition_span
                            .error("Assertion failed.")
                            .with_body(body.into_owned())
                            .err();
                    }
                    _ => unreachable!("The typechecker ensures the condition is a Bool."),
                }
            }
            Stmt::Trace {
                message_span,
                message: message_expr,
            } => {
                let message = self.eval_expr(env, message_expr)?;
                self.tracer
                    .trace(&self.loader.as_inputs(), *message_span, &message);
            }
        }
        Ok(())
    }

    fn eval_seq<OnScalar, OnAssoc>(
        &mut self,
        env: &mut Env,
        seq: &Seq,
        on_scalar: &mut OnScalar,
        on_assoc: &mut OnAssoc,
    ) -> Result<()>
    where
        OnScalar: FnMut(Value),
        OnAssoc: FnMut(Value, Value),
    {
        match seq {
            Seq::Yield(Yield::Elem {
                value: value_expr, ..
            }) => {
                let value = self.eval_expr(env, value_expr)?;
                on_scalar(value);
                Ok(())
            }
            Seq::Yield(Yield::Assoc {
                key: key_expr,
                value: value_expr,
                ..
            }) => {
                let key = self.eval_expr(env, key_expr)?;
                let value = self.eval_expr(env, value_expr)?;
                on_assoc(key, value);
                Ok(())
            }
            Seq::Yield(Yield::UnpackElems {
                collection,
                collection_span,
                check_elem_type,
                ..
            }) => {
                match (self.eval_expr(env, collection)?, check_elem_type) {
                    (Value::List(xs), None) => xs.iter().cloned().for_each(on_scalar),
                    (Value::Set(xs), None) => xs.iter().cloned().for_each(on_scalar),
                    (Value::List(xs), Some(elem_type)) => {
                        for (i, x) in xs.iter().enumerate() {
                            // We report the error at the collection span, not
                            // the full unpack span, because the error includes
                            // the index, so we get "error in value at index n",
                            // and it's index n of the list, I think that makes
                            // a bit more sense than blaming the full unpack.
                            x.is_instance_of(*collection_span, elem_type)
                                .map_err(|err| err.with_path_element(PathElement::Index(i)))?;
                            on_scalar(x.clone());
                        }
                    }
                    (Value::Set(xs), Some(elem_type)) => {
                        // For the set, we report the index of the error like
                        // with lists. Even though set elements have no real
                        // index, sets do have an iteration order.
                        for (i, x) in xs.iter().enumerate() {
                            x.is_instance_of(*collection_span, elem_type)
                                .map_err(|err| err.with_path_element(PathElement::Index(i)))?;
                            on_scalar(x.clone());
                        }
                    }
                    (Value::Dict(..), _) => {
                        return collection_span
                            .error("Expected a list or set to unpack, but this is a dict.")
                            .err()
                    }
                    _ => {
                        return collection_span
                            .error("Expected a list or set to unpack; this is not iterable.")
                            .err()
                    }
                }
                Ok(())
            }
            Seq::Yield(Yield::UnpackAssocs {
                collection,
                collection_span,
                ..
            }) => {
                match self.eval_expr(env, collection)? {
                    Value::Dict(xs) => {
                        for (key, value) in xs.iter() {
                            on_assoc(key.clone(), value.clone());
                        }
                    }
                    Value::List(..) => {
                        return collection_span
                            .error("Expected a dict to unpack, but this is a list.")
                            .err()
                    }
                    Value::Set(..) => {
                        return collection_span
                            .error("Expected a dict to unpack, but this is a set.")
                            .err()
                    }
                    _ => {
                        return collection_span
                            .error("Expected a dict to unpack, but this is not a dict.")
                            .err()
                    }
                }
                Ok(())
            }
            Seq::For {
                idents_span,
                idents,
                collection_span,
                collection,
                body,
            } => {
                let collection_value = self.eval_expr(env, collection)?;
                match (&idents[..], collection_value) {
                    ([name], Value::List(xs)) => {
                        for x in xs.iter() {
                            let ck = env.push(name.clone(), x.clone());
                            self.eval_seq(env, body, on_scalar, on_assoc)?;
                            env.pop(ck);
                        }
                        Ok(())
                    }
                    (_names, Value::List(..)) => {
                        let err = idents_span.error("Expected a single variable.").with_note(
                            *collection_span,
                            "This is a list, it yields one element per iteration.",
                        );
                        Err(err.into())
                    }
                    ([name], Value::Set(xs)) => {
                        for x in xs.iter() {
                            let ck = env.push(name.clone(), x.clone());
                            self.eval_seq(env, body, on_scalar, on_assoc)?;
                            env.pop(ck);
                        }
                        Ok(())
                    }
                    (_names, Value::Set(..)) => {
                        let err = idents_span.error("Expected a single variable.").with_note(
                            *collection_span,
                            "This is a set, it yields one element per iteration.",
                        );
                        Err(err.into())
                    }
                    ([k_name, v_name], Value::Dict(xs)) => {
                        for (k, v) in xs.iter() {
                            let ck = env.checkpoint();
                            env.push(k_name.clone(), k.clone());
                            env.push(v_name.clone(), v.clone());
                            self.eval_seq(env, body, on_scalar, on_assoc)?;
                            env.pop(ck);
                        }
                        Ok(())
                    }
                    (_names, Value::Dict(..)) => {
                        let err = idents_span
                            .error("Expected two variables in dict iteration.")
                            .with_note(
                                *collection_span,
                                "This is a dict, it yields a key and value per iteration.",
                            );
                        Err(err.into())
                    }
                    _ => Err(collection_span.error("This is not iterable.").into()),
                }
            }
            Seq::If {
                condition, body, ..
            } => {
                let cond = self.eval_expr(env, condition)?;
                match cond {
                    Value::Bool(true) => self.eval_seq(env, body, on_scalar, on_assoc),
                    Value::Bool(false) => Ok(()),
                    _ => unreachable!("The typechecker ensures the condition is a Bool."),
                }
            }
            Seq::Stmt { stmt, body } => {
                let ck = env.checkpoint();
                self.eval_stmt(env, stmt)?;
                self.eval_seq(env, body, on_scalar, on_assoc)?;
                env.pop(ck);
                Ok(())
            }
        }
    }
}
