// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

module.exports = grammar({
  name: "rcl",

  word: $ => $.ident,

  rules: {
    source_file: $ => seq(repeat($._prefix), $._expr),

    // A blank is whitespace with two or more newlines. This token is distinct
    // from the regular whitespace, that Tree-sitter by default allows anywhere.
    blank: $ => /[ \t\r\f]*\n[ \t\r\f]*\n[ \t\r\n\f]*/,
    comment: $ => seq("//", /[^\n]*\n/),
    _prefix: $ => choice($.blank, $.comment),

    ident: $ => /[_A-Za-z][-_A-Za-z0-9]*/,

    // TODO: Implement the custom lexer to handle string literals.
    string: $ => /"[^"]*"/,

    _expr: $ => choice(
      $.expr_stmt,
      $._expr_op,
    ),
    expr_stmt: $ => seq($._stmt, ";", repeat($._prefix), $._expr),

    _expr_op: $ => choice($._expr_not_op),

    _expr_not_op: $ => choice(
      $._expr_term,
      // TODO: Add call, add index.
      $.expr_field,
    ),
    expr_field: $ => seq(
      field("inner", $._expr_not_op),
      ".",
      field("field", $.ident),
    ),

    _expr_term: $ => choice(
      $.expr_term_braces,
      $.expr_term_brackets,
      $.expr_term_parens,
      $.string,
      $.ident,
    ),
    expr_term_braces:   $ => seq("{", optional($._seqs), "}"),
    expr_term_brackets: $ => seq("[", optional($._seqs), "]"),
    expr_term_parens:   $ => seq("(", optional($._seqs), ")"),

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
      $._expr_op,
      seq($._expr_op, ":", $._expr),
      seq($.ident, "=", $._expr),
      // TODO: I need to allow a prefix here. Will the fuzzer find it?
      seq($._stmt, ";", $._seq),
      seq("for", $._idents, "in", $._expr, ":", $._seq),
      seq("if", $._expr, ":", $._seq),
    ),

    // One or more identifiers separated by comma, no trailing comma allowed.
    _idents: $=> seq($.ident, repeat(seq(",", $.ident))),
  }
});
