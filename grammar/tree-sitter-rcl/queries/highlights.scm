[
  "for"
  "if"
  "in"
  "let"
  (unop_keyword)
  (binop_keyword)
] @keyword

(comment) @comment
(number) @number
(string) @string
(string_escape) @string.special

(seq_assoc_expr field: (string) @property)
(seq_assoc_ident field: (ident) @property)
