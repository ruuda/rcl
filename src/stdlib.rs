// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Implementation of the standard library.

use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ast::CallArg;
use crate::error::{IntoError, PathElement, Result};
use crate::eval::Evaluator;
use crate::fmt_rcl::format_rcl;
use crate::markup::Markup;
use crate::pprint::{concat, indent, Doc};
use crate::runtime::{builtin_function, builtin_method, FunctionCall, MethodCall, Value};
use crate::types::AsTypeName;

builtin_function!(
    "std.read_file_utf8",
    (path: String) -> String,
    const STD_READ_FILE_UTF8,
    builtin_std_read_file_utf8
);
fn builtin_std_read_file_utf8(eval: &mut Evaluator, call: FunctionCall) -> Result<Value> {
    let arg_span = call.args[0].span;
    let path = match &call.args[0].value {
        Value::String(s) => s.as_ref(),
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
    Ok(eval.loader.get_doc(doc).data.into())
}

builtin_function!(
    "std.range",
    (lower: Int, upper: Int) -> [Int],
    const STD_RANGE,
    builtin_std_range
);
fn builtin_std_range(_eval: &mut Evaluator, call: FunctionCall) -> Result<Value> {
    let lower: i64 = match &call.args[0].value {
        Value::Int(i) => *i,
        _not_string => {
            // TODO: Add proper typechecking and a proper type error.
            return call.args[0]
                .span
                .error("Expected an Int here, but got a different type.")
                .err();
        }
    };
    let upper: i64 = match &call.args[1].value {
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
        let args_span = call.args[0].span.union(call.args[1].span);
        return args_span
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

    let values: Vec<_> = range.map(Value::Int).collect();
    Ok(Value::List(Rc::new(values)))
}

/// Initialize the standard library.
pub fn initialize() -> Value {
    let mut builtins: BTreeMap<Value, Value> = BTreeMap::new();

    builtins.insert("empty_set".into(), Value::Set(Rc::new(BTreeSet::new())));
    builtins.insert("range".into(), Value::BuiltinFunction(&STD_RANGE));
    builtins.insert(
        "read_file_utf8".into(),
        Value::BuiltinFunction(&STD_READ_FILE_UTF8),
    );

    Value::Dict(Rc::new(builtins))
}

builtin_method!("Dict.len", () -> Int, const DICT_LEN, builtin_dict_len);
fn builtin_dict_len(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let dict = call.receiver.expect_dict();
    Ok(Value::Int(dict.len() as _))
}

builtin_method!("List.len", () -> Int, const LIST_LEN, builtin_list_len);
fn builtin_list_len(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    Ok(Value::Int(list.len() as _))
}

builtin_method!("Set.len", () -> Int, const SET_LEN, builtin_set_len);
fn builtin_set_len(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    Ok(Value::Int(set.len() as _))
}

builtin_method!("String.len", () -> Int, const STRING_LEN, builtin_string_len);
fn builtin_string_len(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();
    Ok(Value::Int(string.chars().count() as _))
}

builtin_method!(
    "Dict.contains",
    (key: Any) -> Bool,
    const DICT_CONTAINS,
    builtin_dict_contains
);
fn builtin_dict_contains(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let dict = call.receiver.expect_dict();
    let needle = &call.call.args[0].value;
    Ok(Value::Bool(dict.contains_key(needle)))
}

builtin_method!(
    "List.contains",
    (element: Any) -> Bool,
    const LIST_CONTAINS,
    builtin_list_contains
);
fn builtin_list_contains(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    let needle = &call.call.args[0].value;
    Ok(Value::Bool(list.contains(needle)))
}

builtin_method!(
    "Set.contains",
    (element: Any) -> Bool,
    const SET_CONTAINS,
    builtin_set_contains
);
fn builtin_set_contains(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    let needle = &call.call.args[0].value;
    Ok(Value::Bool(set.contains(needle)))
}

builtin_method!(
    "Dict.get",
    (key: Any, default: Any) -> Any,
    const DICT_GET,
    builtin_dict_get
);
fn builtin_dict_get(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let dict = call.receiver.expect_dict();
    let key = &call.call.args[0].value;
    let default = &call.call.args[1].value;
    match dict.get(key) {
        Some(v) => Ok(v.clone()),
        None => Ok(default.clone()),
    }
}

builtin_method!(
    "Dict.keys",
    () -> {Any},
    const DICT_KEYS,
    builtin_dict_keys
);
fn builtin_dict_keys(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let result = call.receiver.expect_dict().keys().cloned().collect();
    Ok(Value::Set(Rc::new(result)))
}

builtin_method!(
    "Dict.values",
    () -> [Any],
    const DICT_VALUES,
    builtin_dict_values
);
fn builtin_dict_values(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let result = call.receiver.expect_dict().values().cloned().collect();
    Ok(Value::List(Rc::new(result)))
}

builtin_method!(
    "Dict.except",
    (key: Any) -> {Any: Any},
    const DICT_EXCEPT,
    builtin_dict_except
);
fn builtin_dict_except(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let mut result = call.receiver.expect_dict().clone();
    let key = &call.call.args[0].value;
    result.remove(key);
    Ok(Value::Dict(Rc::new(result)))
}

builtin_method!(
    "Set.except",
    (element: Any) -> {Any},
    const SET_EXCEPT,
    builtin_set_except
);
fn builtin_set_except(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let mut result = call.receiver.expect_set().clone();
    let element = &call.call.args[0].value;
    result.remove(element);
    Ok(Value::Set(Rc::new(result)))
}

fn builtin_group_by_impl<'a, I: IntoIterator<Item = &'a Value>>(
    eval: &mut Evaluator,
    call: MethodCall,
    name: &'static str,
    elements: I,
) -> Result<BTreeMap<Value, Vec<Value>>> {
    let get_key = &call.call.args[0].value;
    let get_key_span = call.call.args[0].span;

    let mut groups: BTreeMap<Value, Vec<Value>> = BTreeMap::new();

    for x in elements {
        // The call that we construct here is internal, there is no span in the
        // source code that we could point at. Point at the argument so we still
        // have something to highlight.
        let args = [CallArg {
            span: get_key_span,
            value: x.clone(),
        }];
        let call = FunctionCall {
            call_open: get_key_span,
            call_close: get_key_span,
            args: &args,
        };
        let key = eval
            .eval_call(get_key_span, get_key, call)
            .map_err(|mut err| {
                // If the call includes a call frame for this call, then replace
                // it with a more descriptive message, since the span is a bit
                // misleading.
                err.replace_call_frame(
                    get_key_span,
                    concat! { "In internal call to key selector from '" Doc::highlight(name) "'." },
                );
                err
            })?;
        groups.entry(key).or_default().push(x.clone());
    }

    Ok(groups)
}

builtin_method!(
    "List.group_by",
    // TODO: Add type variables so we can describe this more accurately.
    (get_key: (fn (element: Any) -> Any)) -> {Any: [Any]},
    const LIST_GROUP_BY,
    builtin_list_group_by
);
fn builtin_list_group_by(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    let result = builtin_group_by_impl(eval, call, "List.group_by", list)?
        .into_iter()
        .map(|(k, vs)| (k, Value::List(Rc::new(vs))))
        .collect();
    Ok(Value::Dict(Rc::new(result)))
}

builtin_method!(
    "Set.group_by",
    // TODO: Add type variables so we can describe this more accurately.
    (get_key: (fn (element: Any) -> Any)) -> {Any: {Any}},
    const SET_GROUP_BY,
    builtin_set_group_by
);
fn builtin_set_group_by(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    let result = builtin_group_by_impl(eval, call, "Set.group_by", set)?
        .into_iter()
        .map(|(k, vs)| (k, Value::Set(Rc::new(vs.into_iter().collect()))))
        .collect();
    Ok(Value::Dict(Rc::new(result)))
}

fn builtin_key_by_impl<'a, I: IntoIterator<Item = &'a Value>>(
    eval: &mut Evaluator,
    call: MethodCall,
    name: &'static str,
    elements: I,
) -> Result<Value> {
    let method_span = call.method_span;
    let groups = builtin_group_by_impl(eval, call, name, elements)?;
    let mut result = BTreeMap::new();
    for (k, mut vs) in groups.into_iter() {
        if vs.len() > 1 {
            return method_span
                .error(concat! {
                    "The key " format_rcl(&k).into_owned() " is not unique."
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

    Ok(Value::Dict(Rc::new(result)))
}

builtin_method!(
    "List.key_by",
    // TODO: Add type variables so we can describe this more accurately.
    (get_key: (fn (element: Any) -> Any)) -> {Any: Any},
    const LIST_KEY_BY,
    builtin_list_key_by
);
fn builtin_list_key_by(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    builtin_key_by_impl(eval, call, "List.key_by", list)
}

builtin_method!(
    "Set.key_by",
    // TODO: Add type variables so we can describe this more accurately.
    (get_key: (fn (element: Any) -> Any)) -> {Any: Any},
    const SET_KEY_BY,
    builtin_set_key_by
);
fn builtin_set_key_by(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    builtin_key_by_impl(eval, call, "Set.key_by", set)
}

/// A generic building block to help implement map, filter, and flatmap.
///
/// The acceptor function receives the original value, and the mapped value.
fn builtin_generic_map_impl<
    'a,
    I: IntoIterator<Item = &'a Value>,
    F: FnMut(&Value, Value) -> Result<()>,
>(
    eval: &mut Evaluator,
    call: MethodCall,
    fn_description: &'static str,
    name: &'static str,
    elements: I,
    mut accept: F,
) -> Result<()> {
    let map_element = &call.call.args[0].value;
    let map_element_span = call.call.args[0].span;

    for x in elements {
        // The call that we construct here is internal, there is no span in the
        // source code that we could point at. Point at the argument so we still
        // have something to highlight.
        let args = [CallArg {
            span: map_element_span,
            value: x.clone(),
        }];
        let call = FunctionCall {
            call_open: map_element_span,
            call_close: map_element_span,
            args: &args,
        };
        let mapped_value = eval
            .eval_call(map_element_span, map_element, call)
            .map_err(|mut err| {
                // If the call includes a call frame for this call, then replace
                // it with a more descriptive message, since the span is a bit
                // misleading.
                err.replace_call_frame(
                    map_element_span,
                    concat! { "In internal call to " fn_description " from '" Doc::highlight(name) "'." },
                );
                err
            })?;
        accept(x, mapped_value)?;
    }
    Ok(())
}

fn builtin_map_impl<'a, I: IntoIterator<Item = &'a Value>, F: FnMut(Value)>(
    eval: &mut Evaluator,
    call: MethodCall,
    name: &'static str,
    elements: I,
    mut accept: F,
) -> Result<()> {
    builtin_generic_map_impl(
        eval,
        call,
        "mapping function",
        name,
        elements,
        |_orig, mapped| {
            accept(mapped);
            Ok(())
        },
    )
}

fn builtin_filter_impl<'a, I: IntoIterator<Item = &'a Value>, F: FnMut(Value)>(
    eval: &mut Evaluator,
    call: MethodCall,
    name: &'static str,
    elements: I,
    mut accept: F,
) -> Result<()> {
    let predicate_span = call.call.args[0].span;
    builtin_generic_map_impl(eval, call, "predicate", name, elements, |orig, result| {
        match result {
            Value::Bool(true) => accept(orig.clone()),
            Value::Bool(false) => {}
            not_bool => {
                return predicate_span
                    .error("Type mismatch.")
                    .with_body(concat! {
                        "Expected the predicate to return "
                        "Bool".format_type()
                        ", but it returned "
                        format_rcl(&not_bool).into_owned()
                        "."
                    })
                    .err();
            }
        }
        Ok(())
    })
}

fn builtin_flat_map_impl<'a, I: IntoIterator<Item = &'a Value>, F: FnMut(Value)>(
    eval: &mut Evaluator,
    call: MethodCall,
    name: &'static str,
    elements: I,
    mut accept: F,
) -> Result<()> {
    let predicate_span = call.call.args[0].span;
    builtin_generic_map_impl(
        eval,
        call,
        "mapping function",
        name,
        elements,
        |_orig, mapped| {
            match mapped {
                Value::List(xs) => {
                    for x in xs.iter() {
                        accept(x.clone());
                    }
                }
                Value::Set(xs) => {
                    for x in xs.iter() {
                        accept(x.clone());
                    }
                }
                not_collection => {
                    return predicate_span
                        .error("Type mismatch.")
                        .with_body(concat! {
                        "Expected the mapping function to return a list or set, but it returned "
                        format_rcl(&not_collection).into_owned()
                        "."
                    })
                        .err();
                }
            }
            Ok(())
        },
    )
}

builtin_method!(
    "List.map",
    // TODO: Add type variables so we can describe this more accurately.
    (map_element: (fn (element: Any) -> Any)) -> [Any],
    const LIST_MAP,
    builtin_list_map
);
fn builtin_list_map(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    let mut result = Vec::with_capacity(list.len());
    builtin_map_impl(eval, call, "List.map", list, |v| result.push(v))?;
    Ok(Value::List(Rc::new(result)))
}

builtin_method!(
    "List.flat_map",
    // TODO: Add type variables so we can describe this more accurately.
    (map_element: (fn (element: Any) -> [Any])) -> [Any],
    const LIST_FLAT_MAP,
    builtin_list_flat_map
);
fn builtin_list_flat_map(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    let mut result = Vec::with_capacity(list.len());
    builtin_flat_map_impl(eval, call, "List.flat_map", list, |v| result.push(v))?;
    Ok(Value::List(Rc::new(result)))
}

builtin_method!(
    "List.filter",
    // TODO: Add type variables so we can describe this more accurately.
    (predicate: (fn (element: Any) -> Bool)) -> [Any],
    const LIST_FILTER,
    builtin_list_filter
);
fn builtin_list_filter(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    let mut result = Vec::new();
    builtin_filter_impl(eval, call, "List.filter", list, |v| result.push(v))?;
    Ok(Value::List(Rc::new(result)))
}

builtin_method!(
    "Set.map",
    // TODO: Add type variables so we can describe this more accurately.
    (map_element: (fn (element: Any) -> Any)) -> {Any},
    const SET_MAP,
    builtin_set_map
);
fn builtin_set_map(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    let mut result = BTreeSet::new();
    builtin_map_impl(eval, call, "Set.map", set, |v| {
        result.insert(v);
    })?;
    Ok(Value::Set(Rc::new(result)))
}

builtin_method!(
    "Set.flat_map",
    // TODO: Add type variables so we can describe this more accurately.
    (map_element: (fn (element: Any) -> {Any})) -> {Any},
    const SET_FLAT_MAP,
    builtin_set_flat_map
);
fn builtin_set_flat_map(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    let mut result = BTreeSet::new();
    builtin_flat_map_impl(eval, call, "Set.flat_map", set, |v| {
        result.insert(v);
    })?;
    Ok(Value::Set(Rc::new(result)))
}

builtin_method!(
    "Set.filter",
    // TODO: Add type variables so we can describe this more accurately.
    (predicate: (fn (element: Any) -> Bool)) -> {Any},
    const SET_FILTER,
    builtin_set_filter
);
fn builtin_set_filter(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    let mut result = BTreeSet::new();
    builtin_filter_impl(eval, call, "Set.filter", set, |v| {
        result.insert(v);
    })?;
    Ok(Value::Set(Rc::new(result)))
}

fn builtin_sum_impl<'a>(
    call: MethodCall,
    xs: impl IntoIterator<Item = &'a Value>,
) -> Result<Value> {
    let mut acc: i64 = 0;
    for x in xs {
        match x {
            Value::Int(n) => match acc.checked_add(*n) {
                Some(m) => acc = m,
                None => {
                    let err = concat! {
                        "Addition " acc.to_string() " + " n.to_string() " would overflow."
                    };
                    return call.method_span.error(err).err();
                }
            },
            not_int => {
                let err = concat! {
                    "Expected integers to add, but found " format_rcl(not_int).into_owned() "."
                };
                return call.receiver_span.error(err).err();
            }
        }
    }

    Ok(Value::Int(acc))
}

builtin_method!("List.sum", () -> Int, const LIST_SUM, builtin_list_sum);
fn builtin_list_sum(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    builtin_sum_impl(call, list)
}

builtin_method!("Set.sum", () -> Int, const SET_SUM, builtin_set_sum);
fn builtin_set_sum(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    builtin_sum_impl(call, set)
}

builtin_method!(
    "String.split",
    (separator: String) -> [String],
    const STRING_SPLIT,
    builtin_string_split
);
fn builtin_string_split(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();

    let sep_arg = &call.call.args[0];
    let sep = match &sep_arg.value {
        Value::String(sep) => sep.as_ref(),
        _ => return sep_arg.span.error("Separator must be a string.").err(),
    };

    if sep.is_empty() {
        return sep_arg.span.error("Cannot split on empty separator.").err();
    }

    let result: Vec<Value> = string.split(sep).map(Value::from).collect();

    Ok(Value::List(Rc::new(result)))
}

builtin_method!(
    "String.split_lines",
    () -> [String],
    const STRING_SPLIT_LINES,
    builtin_string_split_lines
);
fn builtin_string_split_lines(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();

    let result: Vec<Value> = string.lines().map(Value::from).collect();

    Ok(Value::List(Rc::new(result)))
}

builtin_method!(
    "String.parse_int",
    () -> Int,
    const STRING_PARSE_INT,
    builtin_string_parse_int
);
fn builtin_string_parse_int(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    use std::str::FromStr;

    let string = call.receiver.expect_string();

    match i64::from_str(string) {
        Ok(i) => Ok(Value::Int(i)),
        Err(..) => call
            .receiver_span
            .error("Failed to parse as integer:")
            .with_body(format_rcl(call.receiver).into_owned())
            .err(),
    }
}

builtin_method!(
    "String.starts_with",
    (prefix: String) -> Bool,
    const STRING_STARTS_WITH,
    builtin_string_starts_with
);
fn builtin_string_starts_with(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();
    let prefix_arg = &call.call.args[0];
    let prefix = match &prefix_arg.value {
        Value::String(s) => s.as_ref(),
        _ => return prefix_arg.span.error("Prefix must be a string.").err(),
    };
    Ok(Value::Bool(string.starts_with(prefix)))
}

builtin_method!(
    "String.ends_with",
    (suffix: String) -> Bool,
    const STRING_ENDS_WITH,
    builtin_string_ends_with
);
fn builtin_string_ends_with(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();
    let suffix_arg = &call.call.args[0];
    let suffix = match &suffix_arg.value {
        Value::String(s) => s.as_ref(),
        _ => return suffix_arg.span.error("Suffix must be a string.").err(),
    };
    Ok(Value::Bool(string.ends_with(suffix)))
}

builtin_method!(
    "String.contains",
    (needle: String) -> Bool,
    const STRING_CONTAINS,
    builtin_string_contains
);
fn builtin_string_contains(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();
    let needle_arg = &call.call.args[0];
    let needle = match &needle_arg.value {
        Value::String(s) => s.as_ref(),
        _ => return needle_arg.span.error("Needle must be a string.").err(),
    };
    Ok(Value::Bool(string.contains(needle)))
}

builtin_method!(
    "String.chars",
    () -> [String],
    const STRING_CHARS,
    builtin_string_chars
);
fn builtin_string_chars(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();

    // Copy each of the code points (chars) into its own string, return that
    // as a list of characters.
    let mut result = Vec::with_capacity(string.len());
    let mut i = 0;
    for (j, _ch) in string.char_indices().skip(1) {
        result.push(Value::from(&string[i..j]));
        i = j;
    }

    if !string[i..].is_empty() {
        result.push(Value::from(&string[i..]));
    }

    Ok(Value::List(Rc::new(result)))
}

builtin_method!(
    "String.replace",
    (needle: String, replacement: String) -> String,
    const STRING_REPLACE,
    builtin_string_replace
);
fn builtin_string_replace(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();
    let needle_arg = &call.call.args[0];
    let needle = match &needle_arg.value {
        Value::String(s) => s.as_ref(),
        _ => return needle_arg.span.error("Needle must be a string.").err(),
    };
    let replacement_arg = &call.call.args[1];
    let replacement = match &replacement_arg.value {
        Value::String(s) => s.as_ref(),
        _ => {
            return replacement_arg
                .span
                .error("Replacement must be a string.")
                .err()
        }
    };
    Ok(Value::String(string.replace(needle, replacement).into()))
}

builtin_method!(
    "String.remove_prefix",
    (prefix: String) -> String,
    const STRING_REMOVE_PREFIX,
    builtin_string_remove_prefix
);
fn builtin_string_remove_prefix(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();
    let prefix_arg = &call.call.args[0];
    let prefix = match &prefix_arg.value {
        Value::String(s) => s.as_ref(),
        _ => return prefix_arg.span.error("Prefix must be a string.").err(),
    };
    match string.strip_prefix(prefix) {
        Some(s) => Ok(Value::String(s.into())),
        None => prefix_arg
            .span
            .error("Cannot remove this prefix.")
            .with_body(concat! {
                format_rcl(call.receiver).into_owned()
                " does not start with "
                format_rcl(&prefix_arg.value).into_owned()
                "."
            })
            .err(),
    }
}

builtin_method!(
    "String.remove_suffix",
    (suffix: String) -> String,
    const STRING_REMOVE_SUFFIX,
    builtin_string_remove_suffix
);
fn builtin_string_remove_suffix(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();
    let suffix_arg = &call.call.args[0];
    let suffix = match &suffix_arg.value {
        Value::String(s) => s.as_ref(),
        _ => return suffix_arg.span.error("Suffix must be a string.").err(),
    };
    match string.strip_suffix(suffix) {
        Some(s) => Ok(Value::String(s.into())),
        None => suffix_arg
            .span
            .error("Cannot remove this suffix.")
            .with_body(concat! {
                format_rcl(call.receiver).into_owned()
                " does not end with "
                format_rcl(&suffix_arg.value).into_owned()
                "."
            })
            .err(),
    }
}

builtin_method!(
    "String.to_lowercase",
    () -> String,
    const STRING_TO_LOWERCASE,
    builtin_string_to_lowercase
);
fn builtin_string_to_lowercase(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();
    Ok(Value::String(string.to_lowercase().into()))
}

builtin_method!(
    "String.to_uppercase",
    () -> String,
    const STRING_TO_UPPERCASE,
    builtin_string_to_uppercase
);
fn builtin_string_to_uppercase(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();
    Ok(Value::String(string.to_uppercase().into()))
}

builtin_method!(
    "List.fold",
    (
        seed: Any,
        reduce: (fn (accumulator: Any, element: Any) -> Any)
    ) -> Any,
    const LIST_FOLD,
    builtin_list_fold
);
fn builtin_list_fold(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    // TODO: Add static type checks. Right now you could provide a bogus
    // function to fold over an empty list and that doesn't fail.
    // TODO: Confirm that that is resolved.

    let list = call.receiver.expect_list();
    let seed = &call.call.args[0];
    let reduce = &call.call.args[1];

    let mut acc = seed.value.clone();

    for element in list.iter() {
        // The call that we construct here is internal, there is no span in the
        // source code that we could point at. To have something to pin errors
        // to, we'll take the entire span of the 'reduce' argument.
        let args = [
            CallArg {
                span: reduce.span,
                value: acc,
            },
            CallArg {
                span: reduce.span,
                value: element.clone(),
            },
        ];
        let call = FunctionCall {
            call_open: reduce.span,
            call_close: reduce.span,
            args: &args,
        };
        acc = eval.eval_call(reduce.span, &reduce.value, call).map_err(|mut err| {
            // If the call includes a call frame for this call, then replace
            // it with a more descriptive message, since the span is a bit
            // misleading.
            err.replace_call_frame(
                reduce.span,
                concat! { "In internal call to reduce function from '" Doc::highlight("List.fold") "'." }
            );
            err
        })?;
    }

    Ok(acc)
}

builtin_method!(
    "List.join",
    (separator: Any) -> String,
    const LIST_JOIN,
    builtin_list_join
);
fn builtin_list_join(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    let separator = &call.call.args[0];

    // The join method behaves the same as a format string, and in fact we
    // implement it that way. Build a list of fragments first.
    let mut fragments = Vec::new();

    for (i, elem) in list.iter().enumerate() {
        Evaluator::push_format_fragment(&mut fragments, call.receiver_span, elem)?;

        if i + 1 < list.len() {
            Evaluator::push_format_fragment(&mut fragments, separator.span, &separator.value)?;
        }
    }

    Ok(Evaluator::join_format_fragments(fragments))
}

builtin_method!(
    "List.reverse",
    () -> [Any],
    const LIST_REVERSE,
    builtin_list_reverse
);
fn builtin_list_reverse(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    let reversed = list.iter().rev().cloned().collect();
    Ok(Value::List(Rc::new(reversed)))
}

builtin_method!(
    "List.sort",
    () -> [Any],
    const LIST_SORT,
    builtin_list_sort
);
fn builtin_list_sort(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    let mut sorted: Vec<_> = list.to_vec();
    sorted.sort();
    Ok(Value::List(Rc::new(sorted)))
}

builtin_method!(
    "List.enumerate",
    () -> {Int: Any},
    const LIST_ENUMERATE,
    builtin_list_enumerate
);
fn builtin_list_enumerate(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    let kv: BTreeMap<_, _> = list
        .iter()
        .zip(0..)
        .map(|(v, i)| (Value::Int(i), v.clone()))
        .collect();
    Ok(Value::Dict(Rc::new(kv)))
}

/// Shared implementation for `List.all` and `List.any`.
///
/// Note, we could have a specialized impl for both, and then short-circuit them,
/// but I think type safety is more important than performance here, and the list
/// is materialized anyway, so traversing the entire list is not such a big deal.
/// For e.g. `[true, false, "bobcat"].any()`, we want to report an error rather
/// than evaluating to `true`, because this is likely a bug.
fn builtin_list_all_impl(call: MethodCall, name: &'static str) -> Result<bool> {
    let list = call.receiver.expect_list();
    let mut all_true = true;
    for (i, elem) in list.iter().enumerate() {
        match elem {
            Value::Bool(true) => continue,
            Value::Bool(false) => all_true = false,
            not_bool => {
                return call
                    .receiver_span
                    .error("Type mismatch.")
                    .with_path(vec![PathElement::Index(i)])
                    .with_body(concat! {
                        "'" Doc::highlight(name) "' takes a list of "
                        "Bool".format_type()
                        ", but found "
                        format_rcl(not_bool).into_owned()
                        "."
                    })
                    .err();
            }
        }
    }
    Ok(all_true)
}

builtin_method!("List.all", () -> Bool, const LIST_ALL, builtin_list_all);
fn builtin_list_all(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    Ok(Value::Bool(builtin_list_all_impl(call, "List.all")?))
}

builtin_method!("List.any", () -> Bool, const LIST_ANY, builtin_list_any);
fn builtin_list_any(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    Ok(Value::Bool(!builtin_list_all_impl(call, "List.any")?))
}
