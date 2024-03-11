// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

// The names of the rules here, and the general structure, are modelled after
// the simpler Bison grammar in //grammar/bison/grammar.y.

module.exports = grammar({
  name: "rcl",

  word: $ => $.ident,

  conflicts: $ => [
    [ $.function_args, $._expr_term ],
  ],

  rules: {
    source_file: $ => seq(
      repeat($._prefix),
      $._expr,
      repeat($._prefix)
    ),

    // A blank is whitespace with two or more newlines. This token is distinct
    // from the regular whitespace, that Tree-sitter by default allows anywhere.
    // Though if we do include this token, then yes we get the node, but for
    // syntax highlighting we don't really care and it breaks parsing in other
    // places, so we just omit it.
    // blank: $ => /[ \t\r\f]*\n[ \t\r\f]*\n[ \t\r\n\f]*/,
    comment: $ => /\/\/[^\n]*\n/,
    shebang: $ => /#![^\n]*\n/,
    _prefix: $ => choice($.comment, $.shebang),

    ident: $ => /[_A-Za-z][-_A-Za-z0-9]*/,

    string: $ => choice(
      $.fstring_double,
      $.fstring_triple,
      $.string_double,
      $.string_triple,
    ),
    string_escape: $ => choice(
      /\\./,
      /\\u[0-9a-fA-F]{4}/,
      seq("\\u{", /[0-9a-fA-F]*/, "}"),
    ),
    string_hole: $ => seq(
      "{",
      repeat($._prefix), $._expr, repeat($._prefix),
      "}"
    ),
    _string_char: $ => /[^\\{"]/,
    string_double: $ => seq(
      "\"",
      repeat(choice($._string_char, $.string_escape, "{")),
      "\"",
    ),
    string_triple: $ => seq(
      "\"\"\"",
      repeat(choice($._string_char, $.string_escape, "\"", "{")),
      "\"\"\"",
    ),
    fstring_double: $ => seq(
      "f\"",
      repeat(choice($._string_char, $.string_escape, $.string_hole)),
      "\"",
    ),
    fstring_triple: $ => seq(
      "f\"\"\"",
      repeat(choice($._string_char, $.string_escape, $.string_hole, "\"")),
      "\"\"\"",
    ),

    number: $ => choice($.num_binary, $.num_hexadecimal, $.num_decimal),
    num_binary: $ => /0b[01_]*/,
    num_hexadecimal: $ => /0x[0-9a-fA-F_]*/,
    num_decimal: $ => /(0|[1-9][0-9_]*)(\.[0-9][0-9_]*)?([eE][-+]?[0-9][0-9_]*)?/,

    unop_keyword: $ => choice("not"),
    binop_keyword: $ => choice("and", "or"),

    unop: $ => choice($.unop_keyword, "-"),
    binop: $ => choice(
      $.binop_keyword,
      "|",
      "*",
      "+",
      "-",
      "/",
      "<",
      "<=",
      ">",
      ">=",
      "==",
      "!=",
    ),

    _expr: $ => choice(
      $.expr_stmt,
      $._expr_op,
    ),
    expr_stmt: $ => seq($._stmt, ";", repeat($._prefix), $._expr),

    _expr_op: $ => choice(
      $.expr_function,
      $.expr_unop,
      $.expr_binop,
      $._expr_not_op,
    ),

    expr_function: $ => seq(
      field("args", $.function_args),
      "=>",
      field("body", $._expr),
    ),

    function_args: $ => choice(
      $.ident,
      seq("(", ")"),
      seq("(", $.ident, repeat(seq(",", $.ident)), optional(","), ")"),
    ),

    expr_unop: $ => choice(
      seq($.unop, $._expr_not_op),
      seq($.unop, $.expr_unop),
    ),

    // In the parser in RCL, arbitrary combinations of binops without parens
    // are not allowed, they must be the same. But even though that error is
    // implemented in the parser, it is more akin to a logic error, and it's
    // only disallowed to force code to be unambiguous for humans. For this
    // parser, nothing prevents us from just parsing it.
    expr_binop: $ => seq($._expr_not_op, repeat1(seq($.binop, $._expr_not_op))),

    _expr_not_op: $ => choice(
      $._expr_term,
      $.expr_call,
      $.expr_index,
      $.expr_field,
    ),
    expr_call: $ => seq(
      field("function", $._expr_not_op),
      "(",
      field("args", optional($._call_args)),
      ")",
    ),
    expr_index: $ => seq(
      field("collection", $._expr_not_op),
      "[",
      field("index", $._expr),
      "]",
    ),
    expr_field: $ => seq(
      field("inner", $._expr_not_op),
      ".",
      field("field", $.ident),
    ),

    _call_args: $ => choice(
      seq(repeat($._prefix), $._expr),
      seq(repeat($._prefix), $._expr, ",", choice(optional($._call_args), repeat($._prefix))),
    ),

    _expr_term: $ => choice(
      $.expr_term_braces,
      $.expr_term_brackets,
      $.expr_term_parens,
      $.string,
      $.number,
      $.ident,
    ),
    expr_term_braces:   $ => seq("{", optional($._seqs), "}"),
    expr_term_brackets: $ => seq("[", optional($._seqs), "]"),
    expr_term_parens:   $ => seq("(", $._expr, ")"),

    _stmt: $ => seq(choice($.stmt_let)),
    stmt_let: $ => seq(
      "let",
      field("ident", $.ident),
      "=",
      repeat($._prefix),
      field("value", $._expr),
    ),

    // One or more `seq`s with an optional trailing comma. The use site has to
    // wrap it in `optional` as Tree-sitter does not support rules that match
    // the empty string.
    _seqs: $ => choice(
      seq(repeat($._prefix), $._seq),
      seq(repeat($._prefix), $._seq, ",", choice(optional($._seqs), repeat($._prefix))),
    ),

    _seq: $ => choice(
      $.seq_elem,
      $.seq_assoc_expr,
      $.seq_assoc_ident,
      $.seq_stmt,
      $.seq_for,
      $.seq_if,
    ),
    seq_elem: $ => $._expr_op,
    seq_assoc_expr: $ => seq(
      field("field", $._expr_op),
      ":",
      field("value", $._expr),
    ),
    seq_assoc_ident: $ => seq(
      field("field", $.ident),
      "=",
      field("value", $._expr),
    ),
    // TODO: I need to allow a prefix here. Will the fuzzer find it?
    seq_stmt: $ => seq($._stmt, ";", $._seq),
    seq_for: $ => seq(
      "for",
      field("idents", $._idents),
      "in",
      field("collection", $._expr),
      ":",
      field("body", $._seq),
    ),
    seq_if: $ => seq(
      "if",
      field("condition", $._expr),
      ":",
      field("body", $._seq),
    ),

    // One or more identifiers separated by comma, no trailing comma allowed.
    _idents: $=> seq($.ident, repeat(seq(",", $.ident))),
  }
});
