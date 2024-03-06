// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

module.exports = grammar({
  name: "rcl",

  rules: {
    source_file: $ => $._prefixed_expr,

    // A blank is whitespace with two or more newlines.
    // Space allows arbitrary newlines, but they are not preserved in the CST.
    blank: $ => /[ \t\r\f]*\n[ \t\r\f]*\n[ \t\r\n\f]*/,
    _space: $ => /[ \t\r\n\f]+/,
    comment: $ => seq("//", /[^\n]*\n/),
    _prefix: $ => choice($.blank, $.comment, $._space),

    ident: $ => /[_A-Za-z][-_A-Za-z0-9]*/,

    _prefixed_expr: $ => seq(repeat($._prefix), $._expr),
    _expr: $ => choice($.expr_stmt),
    expr_stmt: $ => seq($._stmt, $._space, ";", $._prefixed_expr),

    _stmt: $ => choice($.stmt_let),
    stmt_let: $ => seq("let", $._space, $.ident, $._space, "=", $._prefixed_expr),
  }
});
