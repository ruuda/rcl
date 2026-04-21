["for" "in"] @keyword.control.repeat
["assert" "trace"] @keyword.exception
["if" "else"] @keyword.control.conditional
["import"] @keyword.control.import
["let"] @keyword.storage.let
(unop_keyword) @keyword.operator
(binop_keyword) @keyword.operator

(comment) @comment
(shebang) @keyword.directive
(number) @constant.numeric
(bool) @constant.builtin.boolean
(null) @constant.builtin

(string) @string
(string_escape) @string.special
(string_hole (["{" "}"] @string.special))

; The key in key-value pairs, even in expression form.
(seq_assoc_expr field: (string) @attribute)
(seq_assoc_ident field: (ident) @attribute)

; Highlight the callee in function calls.
; TODO: Include a list of builtins so we can highlight them as @function.builtin.
(expr_call function: (ident) @function)
(expr_call function: (expr_field field: (ident) @function))

(type_term) @type
