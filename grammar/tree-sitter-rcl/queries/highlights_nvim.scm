["for" "in"] @keyword.repeat
["assert" "trace"] @keyword.debug
["if" "else"] @keyword.conditional
["import"] @keyword.import
["let"] @keyword.storage
(unop_keyword) @keyword.operator
(binop_keyword) @keyword.operator

(comment) @comment
(shebang) @keyword.directive
(number) @number
(bool) @boolean
(null) @constant.builtin

(string) @string
(string_escape) @string.escape
(string_hole (["{" "}"] @string.escape))

(seq_assoc_expr field: (string) @property)
(seq_assoc_ident field: (ident) @property)

(type_term) @type
