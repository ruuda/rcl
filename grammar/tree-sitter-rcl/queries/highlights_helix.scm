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
(string_escape) @string.special

(seq_assoc_expr field: (string) @attribute)
(seq_assoc_ident field: (ident) @attribute)
