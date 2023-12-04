// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Implementation of the standard library.

use std::collections::BTreeMap;
use std::rc::Rc;

use crate::error::{IntoError, Result};
use crate::eval::Evaluator;
use crate::fmt_rcl::format_rcl;
use crate::markup::Markup;
use crate::pprint::{concat, indent, Doc};
use crate::runtime::{builtin_function, builtin_method, CallArg, FunctionCall, MethodCall, Value};

builtin_function!(
    "std.read_file_utf8",
    const STD_READ_FILE_UTF8,
    builtin_std_read_file_utf8
);
fn builtin_std_read_file_utf8(eval: &mut Evaluator, call: FunctionCall) -> Result<Rc<Value>> {
    call.check_arity_static("std.read_file_utf8", &["path"])?;
    let arg_span = call.args[0].span;
    let path = match call.args[0].value.as_ref() {
        Value::String(s) => s,
        _not_string => {
            // TODO: Add proper typechecking and a proper type error.
            return arg_span
                .error("Expected a String here, but got a different type.")
                .err();
        }
    };
    let from = eval.import_stack.last().map(|ctx| ctx.doc);
    let doc = eval
        .loader
        .load_path(path, from)
        .map_err(|err| err.with_origin(arg_span))?;
    Ok(Rc::new(eval.loader.get_doc(doc).data.into()))
}

builtin_function!(
    "std.range",
    const STD_RANGE,
    builtin_std_range
);
fn builtin_std_range(_eval: &mut Evaluator, call: FunctionCall) -> Result<Rc<Value>> {
    call.check_arity_static("std.range", &["lower", "upper"])?;
    let lower: i64 = match call.args[0].value.as_ref() {
        Value::Int(i) => *i,
        _not_string => {
            // TODO: Add proper typechecking and a proper type error.
            return call.args[0]
                .span
                .error("Expected an Int here, but got a different type.")
                .err();
        }
    };
    let upper: i64 = match call.args[1].value.as_ref() {
        Value::Int(i) => *i,
        _not_string => {
            // TODO: Add proper typechecking and a proper type error.
            return call.args[1]
                .span
                .error("Expected an Int here, but got a different type.")
                .err();
        }
    };

    let range = lower..upper;

    // Because we materialize the entire list, it's easy to cause out of memory
    // with a single call to `range`. To prevent that, put an upper limit on the
    // size. We use a lower limit when fuzzing because it runs with less memory,
    // and also to keep the fuzzer fast. For production, empirically, a limit of
    // 1e6 takes about a second to evaluate, so calling `std.range` with much
    // larger values makes little sense anyway.
    #[cfg(fuzzing)]
    let max_len = 500;
    #[cfg(not(fuzzing))]
    let max_len = 1_000_000;

    if upper.saturating_sub(lower) > max_len {
        return call
            .call_close
            .error(concat! {
                "Range "
                Doc::string(lower.to_string()).with_markup(Markup::Number)
                ".."
                Doc::string(upper.to_string()).with_markup(Markup::Number)
                " exceeds the maximum length of "
                Doc::string(max_len.to_string()).with_markup(Markup::Number)
                ". The list would require too much memory."
            })
            .err();
    }

    let values: Vec<_> = range.map(|i| Rc::new(Value::Int(i))).collect();
    Ok(Rc::new(Value::List(values)))
}

/// Initialize the standard library.
pub fn initialize() -> Rc<Value> {
    let mut builtins: BTreeMap<Rc<Value>, Rc<Value>> = BTreeMap::new();

    builtins.insert(
        Rc::new("range".into()),
        Rc::new(Value::BuiltinFunction(STD_RANGE)),
    );
    builtins.insert(
        Rc::new("read_file_utf8".into()),
        Rc::new(Value::BuiltinFunction(STD_READ_FILE_UTF8)),
    );

    Rc::new(Value::Dict(builtins))
}

builtin_method!("Dict.len", const DICT_LEN, builtin_dict_len);
fn builtin_dict_len(_eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    call.call.check_arity_static("Dict.len", &[])?;
    let dict = call.receiver.expect_dict();
    Ok(Rc::new(Value::Int(dict.len() as _)))
}

builtin_method!("List.len", const LIST_LEN, builtin_list_len);
fn builtin_list_len(_eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    call.call.check_arity_static("List.len", &[])?;
    let list = call.receiver.expect_list();
    Ok(Rc::new(Value::Int(list.len() as _)))
}

builtin_method!("Set.len", const SET_LEN, builtin_set_len);
fn builtin_set_len(_eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    call.call.check_arity_static("Set.len", &[])?;
    let set = call.receiver.expect_set();
    Ok(Rc::new(Value::Int(set.len() as _)))
}

builtin_method!("String.len", const STRING_LEN, builtin_string_len);
fn builtin_string_len(_eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    call.call.check_arity_static("String.len", &[])?;
    let string = call.receiver.expect_string();
    Ok(Rc::new(Value::Int(string.len() as _)))
}

builtin_method!("Dict.contains", const DICT_CONTAINS, builtin_dict_contains);
fn builtin_dict_contains(_eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    call.call.check_arity_static("Dict.contains", &["key"])?;
    let dict = call.receiver.expect_dict();
    let needle = &call.call.args[0].value;
    Ok(Rc::new(Value::Bool(dict.contains_key(needle))))
}

builtin_method!("List.contains", const LIST_CONTAINS, builtin_list_contains);
fn builtin_list_contains(_eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    call.call
        .check_arity_static("List.contains", &["element"])?;
    let list = call.receiver.expect_list();
    let needle = &call.call.args[0].value;
    Ok(Rc::new(Value::Bool(list.contains(needle))))
}

builtin_method!("Set.contains", const SET_CONTAINS, builtin_set_contains);
fn builtin_set_contains(_eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    call.call.check_arity_static("Set.contains", &["element"])?;
    let set = call.receiver.expect_set();
    let needle = &call.call.args[0].value;
    Ok(Rc::new(Value::Bool(set.contains(needle))))
}

builtin_method!("Dict.get", const DICT_GET, builtin_dict_get);
fn builtin_dict_get(_eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    call.call
        .check_arity_static("Dict.get", &["key", "default"])?;
    let dict = call.receiver.expect_dict();
    let key = &call.call.args[0].value;
    let default = &call.call.args[1].value;
    match dict.get(key) {
        Some(v) => Ok(v.clone()),
        None => Ok(default.clone()),
    }
}

fn builtin_group_by_impl<'a, I: IntoIterator<Item = &'a Rc<Value>>>(
    eval: &mut Evaluator,
    call: MethodCall,
    name: &'static str,
    elements: I,
) -> Result<BTreeMap<Rc<Value>, Vec<Rc<Value>>>> {
    // TODO: Add static type checks. Right now, if you call `group_by` on an empty
    // collection, you can provide a completely bogus get_key, and it will never
    // be called, so that doesn't fail.
    call.call.check_arity_static(name, &["get_key"])?;

    let get_key = &call.call.args[0].value;
    let get_key_span = call.call.args[0].span;

    let mut groups: BTreeMap<Rc<Value>, Vec<Rc<Value>>> = BTreeMap::new();

    for x in elements {
        // The call that we construct here is internal, there is no span in the
        // source code that we could point at. Add one nonetheless, we'll replace
        // the error below if needed.
        let void_span = get_key_span.take(0);
        let args = [CallArg {
            span: void_span,
            value: x.clone(),
        }];
        let call = FunctionCall {
            call_open: void_span,
            call_close: void_span,
            args: &args,
        };
        let key = eval
            .eval_call(get_key_span, get_key.as_ref(), call)
            .map_err(|err| {
                err.with_prefix(
                    get_key_span,
                    concat! {
                        "In call to key selector in '"
                        Doc::highlight(name)
                        "':"
                    },
                )
            })?;
        groups.entry(key).or_default().push(x.clone());
    }

    Ok(groups)
}

builtin_method!("List.group_by", const LIST_GROUP_BY, builtin_list_group_by);
fn builtin_list_group_by(eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    let list = call.receiver.expect_list();
    let result = builtin_group_by_impl(eval, call, "List.group_by", list)?
        .into_iter()
        .map(|(k, vs)| (k, Rc::new(Value::List(vs))))
        .collect();
    Ok(Rc::new(Value::Dict(result)))
}

builtin_method!("Set.group_by", const SET_GROUP_BY, builtin_set_group_by);
fn builtin_set_group_by(eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    let set = call.receiver.expect_set();
    let result = builtin_group_by_impl(eval, call, "Set.group_by", set)?
        .into_iter()
        .map(|(k, vs)| (k, Rc::new(Value::Set(vs.into_iter().collect()))))
        .collect();
    Ok(Rc::new(Value::Dict(result)))
}

fn builtin_key_by_impl<'a, I: IntoIterator<Item = &'a Rc<Value>>>(
    eval: &mut Evaluator,
    call: MethodCall,
    name: &'static str,
    elements: I,
) -> Result<Rc<Value>> {
    let method_span = call.method_span;
    let groups = builtin_group_by_impl(eval, call, name, elements)?;
    let mut result = BTreeMap::new();
    for (k, mut vs) in groups.into_iter() {
        if vs.len() > 1 {
            return method_span
                .error(concat! {
                    "The key " format_rcl(k.as_ref()).into_owned() " is not unique."
                })
                .with_body(concat! {
                    "The following values use this key:"
                    Doc::HardBreak
                    Doc::HardBreak
                    indent! {
                        Doc::join(
                            vs.iter().map(|v| format_rcl(v).into_owned()),
                            Doc::HardBreak,
                        )
                    }
                })
                .err();
        }
        result.insert(k, vs.pop().expect("Groups have at least one element."));
    }

    Ok(Rc::new(Value::Dict(result)))
}

builtin_method!("List.key_by", const LIST_KEY_BY, builtin_list_key_by);
fn builtin_list_key_by(eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    let list = call.receiver.expect_list();
    builtin_key_by_impl(eval, call, "List.key_by", list)
}

builtin_method!("Set.key_by", const SET_KEY_BY, builtin_set_key_by);
fn builtin_set_key_by(eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    let set = call.receiver.expect_set();
    builtin_key_by_impl(eval, call, "Set.key_by", set)
}

builtin_method!("String.split", const STRING_SPLIT, builtin_string_split);
fn builtin_string_split(_eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    call.call
        .check_arity_static("String.split", &["separator"])?;
    let string = call.receiver.expect_string();

    let sep_arg = &call.call.args[0];
    let sep = match sep_arg.value.as_ref() {
        Value::String(sep) => sep.as_ref(),
        _ => return sep_arg.span.error("Separator must be a string.").err(),
    };

    let result: Vec<Rc<Value>> = string
        .split(sep)
        .map(|part| Rc::new(Value::from(part)))
        .collect();

    Ok(Rc::new(Value::List(result)))
}

builtin_method!("String.split_lines", const STRING_SPLIT_LINES, builtin_string_split_lines);
fn builtin_string_split_lines(_eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    call.call.check_arity_static("String.split_lines", &[])?;
    let string = call.receiver.expect_string();

    let result: Vec<Rc<Value>> = string
        .lines()
        .map(|part| Rc::new(Value::from(part)))
        .collect();

    Ok(Rc::new(Value::List(result)))
}

builtin_method!("String.parse_int", const STRING_PARSE_INT, builtin_string_parse_int);
fn builtin_string_parse_int(_eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    use std::str::FromStr;

    call.call.check_arity_static("String.parse_int", &[])?;
    let string = call.receiver.expect_string();

    match i64::from_str(string.as_ref()) {
        Ok(i) => Ok(Rc::new(Value::Int(i))),
        Err(..) => call
            .receiver_span
            .error("Failed to parse as integer:")
            .with_body(format_rcl(call.receiver).into_owned())
            .err(),
    }
}

builtin_method!("List.fold", const LIST_FOLD, builtin_list_fold);
fn builtin_list_fold(eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    // TODO: Add static type checks. Right now you could provide a bogus
    // function to fold over an empty list and that doesn't fail.
    call.call
        .check_arity_static("List.fold", &["seed", "reduce"])?;

    let list = call.receiver.expect_list();
    let seed = &call.call.args[0];
    let reduce = &call.call.args[1];

    let mut acc = seed.value.clone();

    for element in list.iter() {
        // The call that we construct here is internal, there is no span in the
        // source code that we could point at. Add one nonetheless, we'll
        // replace the error below if needed.
        let void_span = reduce.span.take(0);
        let args = [
            CallArg {
                span: void_span,
                value: acc,
            },
            CallArg {
                span: void_span,
                value: element.clone(),
            },
        ];
        let call = FunctionCall {
            call_open: void_span,
            call_close: void_span,
            args: &args,
        };
        acc = eval
            .eval_call(reduce.span, reduce.value.as_ref(), call)
            .map_err(|err| {
                // TODO: This is also not ideal, if you pass a function whose
                // body is a BinOp, then if that fails, it can pinpoint the
                // exact location in the source tree where it fails, but now we
                // are masking the value. But for other errors, e.g. an arity
                // error, we don't have a span we can point at, and overriding
                // is good. How to fix this?
                err.with_prefix(
                    reduce.span,
                    concat! {
                        "In call to reduce in '"
                        Doc::highlight("List.fold")
                        "':"
                    },
                )
            })?;
    }

    Ok(acc)
}

builtin_method!("List.reverse", const LIST_REVERSE, builtin_list_reverse);
fn builtin_list_reverse(_eval: &mut Evaluator, call: MethodCall) -> Result<Rc<Value>> {
    call.call.check_arity_static("List.reverse", &[])?;
    let list = call.receiver.expect_list();
    let reversed = list.iter().rev().cloned().collect();
    Ok(Rc::new(Value::List(reversed)))
}
