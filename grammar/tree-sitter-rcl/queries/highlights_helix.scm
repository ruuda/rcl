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
(number) @constant.numeric

(string) @string
(string_escape) @string.special
(string_hole (["{" "}"] @string.special))

(seq_assoc_expr field: (string) @attribute)
(seq_assoc_ident field: (ident) @attribute)
