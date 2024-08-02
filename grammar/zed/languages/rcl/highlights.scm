["for" "in"] @keyword
["assert" "trace"] @keyword
["if" "else"] @keyword
["import"] @keyword
["let"] @keyword
(binop) @operator
(unop) @operator
(unop_keyword) @keyword
(binop_keyword) @keyword

["=>" "->"] @operator
["{" "}" "[" "]" "(" ")"] @punctuation.bracket
["." "," ";" ":" "="] @punctuation.delimiter

(comment) @comment
(number) @number
(bool) @boolean
(null) @constant

(string) @string
(string_escape) @string.escape
(string_hole (["{" "}"] @string.escape))

(seq_assoc_expr field: (string) @property)
(seq_assoc_ident field: (ident) @property)

(type_term) @type

(ERROR) @error
