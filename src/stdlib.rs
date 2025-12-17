// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Implementation of the standard library.

use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ast::CallArg;
use crate::decimal::Decimal;
use crate::error::{IntoError, Result};
use crate::eval::Evaluator;
use crate::fmt_rcl::format_rcl;
use crate::markup::Markup;
use crate::pprint::{concat, indent, Doc};
use crate::runtime::{builtin_function, builtin_method, FunctionCall, MethodCall, Value};
use crate::source::Span;
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
    "std.format_json",
    (value: Any) -> String,
    const STD_FORMAT_JSON,
    builtin_std_format_json
);
fn builtin_std_format_json(_eval: &mut Evaluator, call: FunctionCall) -> Result<Value> {
    let arg = &call.args[0];
    let doc = crate::fmt_json::format_json(arg.span, &arg.value)?;
    let result = doc.print_wide().to_string_no_markup();
    Ok(Value::String(result.into()))
}

/// Extract an i64 from a call argument, or return an error if it's not an integer.
fn expect_arg_i64(arg: &CallArg<Value>, arg_name: &'static str) -> Result<i64> {
    match arg.value.to_i64() {
        Some(n) => Ok(n),
        None => arg
            .span
            .error(concat! {
                "Expected "
                arg_name
                " to be integer, but got "
                format_rcl(&arg.value).into_owned()
                "."
            })
            .err(),
    }
}

builtin_function!(
    "std.range",
    (lower: Number, upper: Number) -> [Number],
    const STD_RANGE,
    builtin_std_range
);
fn builtin_std_range(_eval: &mut Evaluator, call: FunctionCall) -> Result<Value> {
    let lower: i64 = expect_arg_i64(&call.args[0], "lower bound")?;
    let upper: i64 = expect_arg_i64(&call.args[1], "upper bound")?;
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

    let values: Vec<_> = range.map(Value::int).collect();
    Ok(Value::List(Rc::new(values)))
}

/// Initialize the standard library.
pub fn initialize() -> Value {
    let mut builtins: BTreeMap<Value, Value> = BTreeMap::new();

    builtins.insert("empty_set".into(), Value::Set(Rc::new(BTreeSet::new())));
    builtins.insert(
        "format_json".into(),
        Value::BuiltinFunction(&STD_FORMAT_JSON),
    );
    builtins.insert("range".into(), Value::BuiltinFunction(&STD_RANGE));
    builtins.insert(
        "read_file_utf8".into(),
        Value::BuiltinFunction(&STD_READ_FILE_UTF8),
    );

    Value::Dict(Rc::new(builtins))
}

builtin_method!("Dict.len", () -> Number, const DICT_LEN, builtin_dict_len);
fn builtin_dict_len(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let dict = call.receiver.expect_dict();
    Ok(Value::int(dict.len() as _))
}

builtin_method!("List.len", () -> Number, const LIST_LEN, builtin_list_len);
fn builtin_list_len(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    Ok(Value::int(list.len() as _))
}

builtin_method!("Set.len", () -> Number, const SET_LEN, builtin_set_len);
fn builtin_set_len(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    Ok(Value::int(set.len() as _))
}

builtin_method!("String.len", () -> Number, const STRING_LEN, builtin_string_len);
fn builtin_string_len(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let string = call.receiver.expect_string();
    Ok(Value::int(string.chars().count() as _))
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

/// Shared implementation for applying a `get_key` function to each element of a collection.
fn builtin_loop_get_key_impl<'a, I: Iterator<Item = &'a Value>, F: FnMut(Value, &Value)>(
    eval: &mut Evaluator,
    call: MethodCall,
    name: &'static str,
    elements: I,
    mut handle_key_value: F,
) -> Result<()> {
    let get_key = &call.call.args[0].value;
    let get_key_span = call.call.args[0].span;

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

        handle_key_value(key, x);
    }

    Ok(())
}

fn builtin_group_by_impl<'a, I: IntoIterator<Item = &'a Value>>(
    eval: &mut Evaluator,
    call: MethodCall,
    name: &'static str,
    elements: I,
) -> Result<BTreeMap<Value, Vec<Value>>> {
    let mut groups: BTreeMap<Value, Vec<Value>> = BTreeMap::new();

    builtin_loop_get_key_impl(eval, call, name, elements.into_iter(), |key, value| {
        groups.entry(key).or_default().push(value.clone());
    })?;

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

fn builtin_sort_by_impl<'a, I: IntoIterator<Item = &'a Value>>(
    eval: &mut Evaluator,
    call: MethodCall,
    name: &'static str,
    elements: I,
) -> Result<Value> {
    let elements = elements.into_iter();
    let n = elements.size_hint().0;
    let mut i = 0;
    let mut tuples: Vec<(Value, usize, Value)> = Vec::with_capacity(n);

    builtin_loop_get_key_impl(eval, call, name, elements, |key, value| {
        tuples.push((key, i, value.clone()));
        i += 1;
    })?;

    // We include the index in the tuples, but then we do an unstable sort. This
    // ensures that the sort is stable with respect to the input list, and also
    // that we never compare the values themselves, only the keys, and if those
    // are equal, the indices which are unique. Comparing the values may be
    // expensive when they are e.g. large dicts.
    tuples.sort_unstable();
    let result: Vec<_> = tuples.into_iter().map(|(_key, _i, value)| value).collect();

    Ok(Value::List(Rc::new(result)))
}

builtin_method!(
    "List.sort_by",
    // TODO: Add type variables so we can describe this more accurately.
    (get_key: (fn (element: Any) -> Any)) -> [Any],
    const LIST_SORT_BY,
    builtin_list_sort_by
);
fn builtin_list_sort_by(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    builtin_sort_by_impl(eval, call, "List.sort_by", list)
}

builtin_method!(
    "Set.sort_by",
    // TODO: Add type variables so we can describe this more accurately.
    (get_key: (fn (element: Any) -> Any)) -> [Any],
    const SET_SORT_BY,
    builtin_set_sort_by
);
fn builtin_set_sort_by(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    builtin_sort_by_impl(eval, call, "Set.sort_by", set)
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
    "Set.sort",
    () -> [Any],
    const SET_SORT,
    builtin_set_sort
);
fn builtin_set_sort(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    // Note, sets are currently implemented as sorted sets, so there is
    // no additional sorting to be done here, just collecting in a vec.
    let sorted: Vec<_> = set.iter().cloned().collect();
    Ok(Value::List(Rc::new(sorted)))
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
    "Set.map_dedup",
    // TODO: Add type variables so we can describe this more accurately.
    (map_element: (fn (element: Any) -> Any)) -> {Any},
    const SET_MAP_DEDUP,
    builtin_set_map_dedup
);
fn builtin_set_map_dedup(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    let mut result = BTreeSet::new();
    builtin_map_impl(eval, call, "Set.map_dedup", set, |v| {
        result.insert(v);
    })?;
    Ok(Value::Set(Rc::new(result)))
}

builtin_method!(
    "Set.flat_map_dedup",
    // TODO: Add type variables so we can describe this more accurately.
    (map_element: (fn (element: Any) -> {Any})) -> {Any},
    const SET_FLAT_MAP_DEDUP,
    builtin_set_flat_map_dedup
);
fn builtin_set_flat_map_dedup(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    let mut result = BTreeSet::new();
    builtin_flat_map_impl(eval, call, "Set.flat_map_dedup", set, |v| {
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

builtin_method!(
    "Set.to_list",
    () -> [Any],
    const SET_TO_LIST,
    builtin_set_to_list
);
fn builtin_set_to_list(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    let mut result = Vec::with_capacity(set.len());
    result.extend(set.iter().cloned());
    Ok(Value::List(Rc::new(result)))
}

builtin_method!(
    "List.to_set_unique",
    () -> {Any},
    const LIST_TO_SET_UNIQUE,
    builtin_list_to_set_unique
);
fn builtin_list_to_set_unique(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    let mut result = BTreeSet::new();

    for elem in list.iter() {
        let is_new = result.insert(elem.clone());
        if !is_new {
            return call
                .receiver_span
                .error(concat! {
                    "Expected unique elements to convert to set, but got a duplicate: "
                    format_rcl(elem).into_owned()
                })
                .with_help(concat! {
                    "Use '" Doc::highlight("to_set_dedup") "' to discard duplicates."
                })
                .err();
        }
    }

    Ok(Value::Set(Rc::new(result)))
}

builtin_method!(
    "List.to_set_dedup",
    () -> {Any},
    const LIST_TO_SET_DEDUP,
    builtin_list_to_set_dedup
);
fn builtin_list_to_set_dedup(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    let mut result = BTreeSet::new();
    for elem in list.iter() {
        result.insert(elem.clone());
    }
    Ok(Value::Set(Rc::new(result)))
}

/// Confirm that the value is not too deeply nested.
///
/// Functions that can recursively build values, such as fold and transitive
/// closure, can generate values that have a "size" that grows at runtime,
/// beyond anything proportional to the input. E.g. transitive closure with
/// expansion `x => [[x]]`. Those deeply nested objects are problematic to put
/// in a `BTreeSet`, because the `Ord` implementation has to descend all the
/// way into the value, and for larger sets like what transitive closure builds,
/// this slows everything to a crawl. The fuzzer *will* find pathological cases
/// like this and hang on them, so we ban them.
fn check_value_depth(error_span: Span, description: &'static str, value: &Value) -> Result<()> {
    #[cfg(any(fuzzing, debug_assertions))]
    let max_depth = 10;

    #[cfg(all(not(fuzzing), not(debug_assertions)))]
    let max_depth = 250;

    if value.depth() < max_depth {
        Ok(())
    } else {
        let msg = concat! { description " exceeds the maximum nesting depth:" };
        let body = format_rcl(value).into_owned();
        error_span.error(msg).with_body(body).err()
    }
}

builtin_method!(
    "Set.transitive_closure",
    (expand: (fn (element: Any) -> {Any})) -> {Any},
    const SET_TRANSITIVE_CLOSURE,
    builtin_set_transitive_closure
);
fn builtin_set_transitive_closure(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let mut closed = BTreeSet::new();
    let mut frontier = call.receiver.expect_set().clone();

    let expand = &call.call.args[0].value;
    let expand_span = call.call.args[0].span;
    let method_span = call.method_span;

    while let Some(elem) = frontier.pop_first() {
        // Prevent hangs from deeply nested values that are expensive to put in
        // the BTreeSet.
        check_value_depth(method_span, "A value in the transitive closure", &elem)?;

        // See also the comments in `builtin_generic_map_impl` for how we handle
        // spans of internal calls.
        let args = [CallArg {
            span: expand_span,
            value: elem.clone(),
        }];
        let call = FunctionCall {
            call_open: expand_span,
            call_close: expand_span,
            args: &args,
        };
        let expansion = eval
            .eval_call(expand_span, expand, call)
            .map_err(|mut err| {
                // If the call includes a call frame for this call, then replace
                // it with a more descriptive message, since the span is a bit
                // misleading.
                err.replace_call_frame(
                    expand_span,
                    concat! {
                        "In internal call to the expansion function from '"
                        Doc::highlight("transitive_closure")
                        "'."
                    },
                );
                err
            })?;

        closed.insert(elem);

        match expansion {
            Value::List(xs) => xs.iter().for_each(|x| {
                if !closed.contains(x) {
                    frontier.insert(x.clone());
                }
            }),
            Value::Set(xs) => xs.iter().for_each(|x| {
                if !closed.contains(x) {
                    frontier.insert(x.clone());
                }
            }),
            not_collection => {
                return expand_span
                    .error("Type mismatch.")
                    .with_body(concat! {
                        "Expected the expand function to return a list or set, but it returned "
                        format_rcl(&not_collection).into_owned()
                        "."
                    })
                    .err();
            }
        }
    }

    Ok(Value::Set(Rc::new(closed)))
}

fn builtin_sum_impl<'a>(
    call: MethodCall,
    xs: impl IntoIterator<Item = &'a Value>,
) -> Result<Value> {
    let mut acc = Decimal::from(0);
    for x in xs {
        match x {
            Value::Number(n) => match acc.checked_add(n) {
                Some(m) => acc = m,
                None => {
                    let err = concat! {
                        "Addition " acc.format() " + " n.format() " would overflow."
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

    Ok(Value::Number(acc))
}

builtin_method!("List.sum", () -> Number, const LIST_SUM, builtin_list_sum);
fn builtin_list_sum(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    builtin_sum_impl(call, list)
}

builtin_method!("Set.sum", () -> Number, const SET_SUM, builtin_set_sum);
fn builtin_set_sum(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let set = call.receiver.expect_set();
    builtin_sum_impl(call, set)
}

builtin_method!(
    "Number.round",
    (n_decimals: Number) -> Number,
    const NUMBER_ROUND,
    builtin_number_round
);
fn builtin_number_round(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let arg = &call.call.args[0];
    let n_decimals = expect_arg_i64(arg, "number of decimals")?;

    if n_decimals < 0 {
        return arg
            .span
            .error("Cannot round to negative decimals, decimals must be at least 0.")
            .err();
    }
    if n_decimals > 100 {
        // The representation allows up to 255 decimals, but we also have this
        // property that RCL should be able to read its own output, and we have
        // a limit on the length of number literals. If we allow 255 decimals,
        // the formatted number can exceed that limit. Given that we have an
        // arbitrary limit anyway, and that even 50 decimals would be silly, I
        // think a limit of 100 decimals should be fine. RCL is not an arbitrary
        // precision calculator, use a different language if you need that many
        // decimals.
        return arg
            .span
            .error("Number of decimals can be at most 100.")
            .err();
    }
    match call.receiver.expect_number().round(n_decimals as u8) {
        Some(d) => Ok(Value::Number(d)),
        None => call
            .method_span
            .error("Overflow while rounding number.")
            .err(),
    }
}

/// Which function to implement in [`builtin_any_all_impl`].
enum AllAny {
    All,
    Any,
}

/// Shared implementation for `{List,Set}.{any,all}`.
#[inline(always)]
fn builtin_all_any_impl<'a>(
    eval: &'a mut Evaluator,
    call: MethodCall,
    name: &'static str,
    mode: AllAny,
    xs: impl IntoIterator<Item = &'a Value>,
) -> Result<bool> {
    let predicate = &call.call.args[0].value;
    let predicate_span = call.call.args[0].span;

    for x in xs {
        // The call that we construct here is internal, there is no span in the
        // source code that we could point at. Point at the argument so we still
        // have something to highlight.
        let args = [CallArg {
            span: predicate_span,
            value: x.clone(),
        }];
        let call = FunctionCall {
            call_open: predicate_span,
            call_close: predicate_span,
            args: &args,
        };
        let result = eval
            .eval_call(predicate_span, predicate, call)
            .map_err(|mut err| {
                // If the call includes a call frame for this call, then replace
                // it with a more descriptive message, since the span is a bit
                // misleading.
                err.replace_call_frame(
                    predicate_span,
                    concat! { "In internal call to predicate from '" Doc::highlight(name) "'." },
                );
                err
            })?;
        match result {
            Value::Bool(true) => match mode {
                AllAny::All => continue,
                AllAny::Any => return Ok(true),
            },
            Value::Bool(false) => match mode {
                AllAny::All => return Ok(false),
                AllAny::Any => continue,
            },
            not_bool => {
                return predicate_span
                    .error("Type mismatch.")
                    .with_body(concat! {
                        "Expected predicate for '" Doc::highlight(name) "' to return "
                        "Bool".format_type()
                        ", but it returned "
                        format_rcl(&not_bool).into_owned()
                        "."
                    })
                    .err();
            }
        }
    }
    match mode {
        AllAny::All => Ok(true),
        AllAny::Any => Ok(false),
    }
}

builtin_method!(
    "List.all",
    (predicate: (fn (element: Any) -> Bool)) -> Bool,
    const LIST_ALL,
    builtin_list_all
);
fn builtin_list_all(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    Ok(Value::Bool(builtin_all_any_impl(
        eval,
        call,
        "List.all",
        AllAny::All,
        list,
    )?))
}

builtin_method!(
    "List.any",
    (predicate: (fn (element: Any) -> Bool)) -> Bool,
    const LIST_ANY,
    builtin_list_any
);
fn builtin_list_any(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    Ok(Value::Bool(builtin_all_any_impl(
        eval,
        call,
        "List.any",
        AllAny::Any,
        list,
    )?))
}

builtin_method!(
    "Set.all",
    (predicate: (fn (element: Any) -> Bool)) -> Bool,
    const SET_ALL,
    builtin_set_all
);
fn builtin_set_all(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let elems = call.receiver.expect_set();
    Ok(Value::Bool(builtin_all_any_impl(
        eval,
        call,
        "Set.all",
        AllAny::All,
        elems,
    )?))
}

builtin_method!(
    "Set.any",
    (predicate: (fn (element: Any) -> Bool)) -> Bool,
    const SET_ANY,
    builtin_set_any
);
fn builtin_set_any(eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let elems = call.receiver.expect_set();
    Ok(Value::Bool(builtin_all_any_impl(
        eval,
        call,
        "Set.any",
        AllAny::Any,
        elems,
    )?))
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
    () -> Number,
    const STRING_PARSE_INT,
    builtin_string_parse_int
);
fn builtin_string_parse_int(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    use std::str::FromStr;

    let string = call.receiver.expect_string();

    match i64::from_str(string) {
        Ok(i) => Ok(Value::int(i)),
        Err(..) => call
            .receiver_span
            .error("Failed to parse as integer:")
            .with_body(format_rcl(call.receiver).into_owned())
            .err(),
    }
}

/// Run the regular lexer on the input string, and return the matching token.
///
/// To be able to share the number parsing implementation between the language
/// itself, and `String.parse_number`, we run the full lexer. We need to lex,
/// because `Decimal::parse_str` on its own is too lenient. For example, it
/// accepts `-` anywhere in the string to flip the sign. We expect the lexer to
/// produce a single token that matches the entire input. Running the full lexer
/// is a bit inefficient, but it's okay for now.
fn builtin_string_parse_number_lex(string: &str) -> Option<crate::lexer::Token> {
    let dummy_doc_id = crate::source::DocId(0);
    let lexemes = crate::lexer::lex(dummy_doc_id, string).ok()?;

    // We need an exact match, so there can only be a single token,
    // and it needs to span the entire input.
    let (token, span) = match lexemes.len() {
        1 => lexemes[0],
        _ => return None,
    };

    if span.start() != 0 || span.len() != string.len() {
        return None;
    }

    Some(token)
}

builtin_method!(
    "String.parse_number",
    () -> Number,
    const STRING_PARSE_NUMBER,
    builtin_string_parse_number
);
fn builtin_string_parse_number(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    use crate::lexer::Token;

    let string = call.receiver.expect_string();

    // We abuse the `loop` construct to emulate "goto error".
    #[allow(clippy::never_loop)]
    let error = loop {
        // If there is a leading minus sign, cut it off here, because if we feed
        // it to the lexer below, it would parse it as a separate token instead.
        let (sign, str) = match string.as_bytes().first() {
            // Empty string is not a valid number.
            None => break "Failed to parse as number:",
            Some(b'-') => (-1, &string[1..]),
            _ => (1, string),
        };

        let opt_result: Option<Decimal> = match builtin_string_parse_number_lex(str) {
            Some(Token::NumBinary) => {
                // Remove the "0b" prefix, strip numeric underscores.
                let num_str = str[2..].replace('_', "");
                i64::from_str_radix(&num_str, 2).ok().map(Decimal::from)
            }
            Some(Token::NumHexadecimal) => {
                let num_str = str[2..].replace('_', "");
                i64::from_str_radix(&num_str, 16).ok().map(Decimal::from)
            }
            Some(Token::NumDecimal) => Decimal::parse_str(str).map(Decimal::from),
            _ => break "Failed to parse as number:",
        };

        // If any of the above three cases failed, that means it was an
        // overflow, because the lexer already validated that the input
        // follows the right format, so it can't fail due to wrong format.
        let mut result = match opt_result {
            Some(num) => num,
            None => break "Overflow while parsing number:",
        };

        // If there was a leading `-`, flip the sign. This does not overflow,
        // because abs(i64::MIN) > abs(i64::MAX). It does mean that we can't
        // parse i64::MIN itself, but I think that's acceptable for now.
        result.mantissa *= sign;

        return Ok(Value::Number(result));
    };

    call.receiver_span
        .error(error)
        .with_body(format_rcl(call.receiver).into_owned())
        .err()
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
    let method_span = call.method_span;

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

        // Avoid hangs from very deeply nested values.
        check_value_depth(method_span, "Accumulator", &acc)?;
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
    "List.enumerate",
    () -> {Number: Any},
    const LIST_ENUMERATE,
    builtin_list_enumerate
);
fn builtin_list_enumerate(_eval: &mut Evaluator, call: MethodCall) -> Result<Value> {
    let list = call.receiver.expect_list();
    let kv: BTreeMap<_, _> = list
        .iter()
        .zip(0..)
        .map(|(v, i)| (Value::int(i), v.clone()))
        .collect();
    Ok(Value::Dict(Rc::new(kv)))
}
