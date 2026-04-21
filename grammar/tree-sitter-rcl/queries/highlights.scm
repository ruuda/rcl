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

; The key in key-value pairs, even in expression form.
(seq_assoc_expr field: (string) @property)
(seq_assoc_ident field: (ident) @property)

; Highlight the callee in function calls.
(expr_call function: (ident) @function)
(expr_call function: (expr_field field: (ident) @function))

(type_term) @type
