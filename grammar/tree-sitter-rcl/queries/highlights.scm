[
  "for"
  "if"
  "in"
  "let"
  (unop_keyword)
  (binop_keyword)
] @keyword

(comment) @comment
(number) @constant.numeric
[
  (string_double)
  (string_triple)
] @string
(string_escape) @string.escape

(seq_assoc_expr field: (string) @property)
(seq_assoc_ident field: (ident) @property)
