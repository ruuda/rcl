[
  "for"
  "if"
  "import"
  "in"
  "let"
  (unop_keyword)
  (binop_keyword)
] @keyword

(comment) @comment
(number) @number

(string) @string
(string_escape) @string.escape
(string_hole (["{" "}"] @string.escape))

(seq_assoc_expr field: (string) @property)
(seq_assoc_ident field: (ident) @property)
