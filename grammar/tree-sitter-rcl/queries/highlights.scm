["for" "in"] @keyword
["assert" "trace"] @keyword
["if" "else"] @keyword
["import"] @keyword
["let"] @keyword
(unop_keyword) @keyword
(binop_keyword) @keyword

(comment) @comment
(shebang) @keyword
(number) @number
(bool) @constant
(null) @constant

(string) @string
(string_escape) @string.special
(string_hole (["{" "}"] @string.special))

(seq_assoc_expr field: (string) @property)
(seq_assoc_ident field: (ident) @property)

(type_term) @type
