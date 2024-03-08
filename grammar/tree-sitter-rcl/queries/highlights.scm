[
  "for"
  "if"
  "in"
  "let"
  (unop_keyword)
  (binop_keyword)
] @keyword

(comment) @comment
(string) @string
(number) @constant.numeric

(seq_assoc_expr field: (string) @property)
(seq_assoc_ident field: (ident) @property)
