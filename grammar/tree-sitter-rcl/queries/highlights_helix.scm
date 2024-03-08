[
  "for"
  "if"
  "in"
  "let"
] @keyword

(comment) @comment
(string) @string
(number) @constant.numeric

(seq_assoc_expr field: (string) @attribute)
(seq_assoc_ident field: (ident) @attribute)
