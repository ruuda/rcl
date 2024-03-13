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

  extras: $ => [
    $.comment,
    $.shebang,
    /[\s\x0c]/,
  ],

  conflicts: $ => [
    [ $.function_args, $._expr_term ],
  ],

  rules: {
    source_file: $ => $._expr,

    comment: $ => /\/\/[^\n]*\n/,
    shebang: $ => /#![^\n]*\n/,

    ident: $ => /[_A-Za-z][-_A-Za-z0-9]*/,

    string: $ => choice(
      $.fstring_double,
      $.fstring_triple,
      $.string_double,
      $.string_triple,
    ),
    string_escape: $ => token.immediate(seq(
      "\\",
      choice(
        /[^u]/,
        /u[0-9a-fA-F]{4}/,
        /u\{[0-9a-fA-F]*\}/,
      ),
    )),
    string_hole: $ => seq("{", $._expr, "}"),
    // Note, the prec 1 is crucial here. It ensures that string fragments have
    // higher precedence than comments and whitespace. Without it, the immediate
    // doesn't work and we can get comments inside strings.
    _string_char: $ => token.immediate(prec(1, /[^\\"]+/)),
    _fstring_char: $ => token.immediate(prec(1, /[^\\{"]+/)),

    string_double: $ => seq(
      "\"",
      repeat(choice(
        $._string_char,
        $.string_escape,
      )),
      "\"",
    ),
    string_triple: $ => seq(
      "\"\"\"",
      repeat(choice(
        $._string_char,
        $.string_escape,
        "\"",
      )),
      "\"\"\"",
    ),
    fstring_double: $ => seq(
      "f\"",
      repeat(choice(
        $._fstring_char,
        $.string_escape,
        $.string_hole,
      )),
      "\"",
    ),
    fstring_triple: $ => seq(
      "f\"\"\"",
      repeat(choice(
        $._fstring_char,
        $.string_escape,
        $.string_hole,
        "\"",
      )),
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
    expr_stmt: $ => seq($._stmt, ";", $._expr),

    _expr_op: $ => choice(
      $.expr_import,
      $.expr_function,
      $.expr_unop,
      $.expr_binop,
      $._expr_not_op,
    ),

    expr_import: $ => seq("import", $._expr),

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

    _call_args: $ => seq($._call_args_inner, optional(",")),
    _call_args_inner: $ => choice(
      $._expr,
      seq($._call_args_inner, ",", $._expr),
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

    _stmt: $ => choice($.stmt_let, $.stmt_assert, $.stmt_trace),
    stmt_let: $ => seq(
      "let",
      field("ident", $.ident),
      "=",
      field("value", $._expr),
    ),
    stmt_assert: $ => seq(
      "assert",
      field("condition", $._expr),
      ",",
      field("message", $._expr),
    ),
    stmt_trace: $ => seq(
      "trace",
      field("message", $._expr),
    ),

    // One or more `seq`s with an optional trailing comma. The use site has to
    // wrap it in `optional` as Tree-sitter does not support rules that match
    // the empty string.
    _seqs: $ => seq($._seqs_inner, optional(",")),
    _seqs_inner: $ => choice(
      $._seq,
      seq($._seqs_inner, ",", $._seq),
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
