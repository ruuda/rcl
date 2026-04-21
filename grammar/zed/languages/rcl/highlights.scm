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
(shebang) @preproc
(number) @number
(bool) @boolean
(null) @constant

(string) @string
(string_escape) @string.escape
(string_hole (["{" "}"] @string.escape))

; The key in key-value pairs, even in expression form.
(seq_assoc_expr field: (string) @property)
(seq_assoc_ident field: (ident) @property)

; Highlight the callee in function calls.
(expr_call function: (ident) @function)
(expr_call function: (expr_field field: (ident) @function))

(type_term) @type

(ERROR) @error
