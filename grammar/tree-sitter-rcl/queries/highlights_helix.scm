["for" "in"] @keyword.control.repeat
["assert" "trace"] @keyword.exception
["if"] @keyword.control.conditional
["import"] @keyword.control.import
["let"] @keyword.storage.let
(unop_keyword) @keyword.operator
(binop_keyword) @keyword.operator

(comment) @comment
(number) @constant.numeric
(bool) @constant.builtin.boolean
(null) @constant.builtin

(string) @string
(string_escape) @string.special
(string_hole (["{" "}"] @string.special))

(seq_assoc_expr field: (string) @attribute)
(seq_assoc_ident field: (ident) @attribute)

(type_term) @type
