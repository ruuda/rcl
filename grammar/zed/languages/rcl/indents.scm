(_ "{" "}" @end) @indent
(_ "[" "]" @end) @indent
(_ "(" ")" @end) @indent

; If you create a newline inside a let, that indents, we want to hang the
; body. It already indents if you enter before the `=`, but that's okay.
(stmt_let) @indent

; In addition to these, there are indent regexes in `config.toml` it seems
; that both are necessary to make it work, I don't understand why.
(expr_if) @start.if
(expr_if "else" @start.else)
