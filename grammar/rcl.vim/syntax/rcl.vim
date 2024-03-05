" RCL -- A reasonable configuration language.
" Copyright 2023 Ruud van Asseldonk
"
" Licensed under the Apache License, Version 2.0 (the "License");
" you may not use this file except in compliance with the License.
" A copy of the License has been included in the root of the repository.

syn keyword rclNull         null
syn keyword rclBoolean      true false
syn keyword rclConditional  if else
syn keyword rclRepeat       for
syn keyword rclOperator     and not or
syn keyword rclKeyword      in let import
syn keyword rclException    assert trace
syn cluster rclKeyword      contains=rclBoolean,rclConditional,rclRepeat,rclOperator,rclKeyword,rclException

syn match rclOperator '<='
syn match rclOperator '>='
syn match rclOperator '=='
syn match rclOperator '!='
syn match rclOperator '=>'
syn match rclOperator '->'
syn match rclOperator '|'
syn match rclOperator '+'
syn match rclOperator '-'
syn match rclOperator '*'
syn match rclOperator '/'

syn keyword rclTodo FIXME NOTE TODO HACK contained
syn match   rclComment '//.*' contains=rclTodo,@Spell

" TODO: Something in the float is not yet working right.
syn match   rclFloat       '\<[0-9_]\+\.[0-9_]\+\([eE][+-]\?[0-9_]\+\)\?\>'
syn match   rclHexadecimal '\<0x[0-9a-fA-F_]\+\>'
syn match   rclBinary      '\<0b[01_]\+\>'
syn match   rclDecimal     '\<[0-9_]\+\>'
syn cluster rclNumber      contains=rclFloat,rclHexadecimal,rclBinary,rclDecimal

syn region  rclStringDouble start='"' end='"' skip='\\"'
syn region  rclStringTriple start='"""' end='"""' skip='\\"'

syn region  rclInterpolation matchgroup=rclInterpolationDelimiter start="{" end="}" contained contains=@rclExpr
syn region  rclFormatDouble  start='f"'   end='"'   skip='\\"\|\\{' contains=rclInterpolation
syn region  rclFormatTriple  start='f"""' end='"""' skip='\\"\|\\{' contains=rclInterpolation

" See also https://vi.stackexchange.com/questions/5966/ for why the `contains`
" needs to end in `[]`.
syn keyword rclBuiltin chars contains[] ends_with except fold get group_by join key_by keys len parse_int remove_prefix remove_suffix replace reverse split split_lines starts_with std to_lowercase to_uppercase values

syn match   rclType '\<\(Any\|Bool\|Dict\|Int\|List\|Null\|Set\|String\|Void\)\>'

syn cluster rclString contains=rclStringDouble,rclStringTriple,rclFormatDouble,rclFormatTriple
highlight link rclStringDouble rclString
highlight link rclStringTriple rclString
highlight link rclFormatDouble rclString
highlight link rclFormatTriple rclString

syn cluster rclExpr contains=@rclKeyword,rclOperator,@rclNumber,rclComment,rclBuiltin,rclType,@rclString

highlight link rclBoolean     Boolean
highlight link rclConditional Conditional
highlight link rclRepeat      Repeat
highlight link rclOperator    Operator
highlight link rclNull        Keyword
highlight link rclKeyword     Keyword
highlight link rclException   Keyword
highlight link rclComment     Comment
highlight link rclTodo        Todo
highlight link rclBuiltin     Function
highlight link rclType        Type
highlight link rclString      String
highlight link rclFloat       Float
highlight link rclHexadecimal Number
highlight link rlcBinary      Number
highlight link rclDecimal     Number

highlight link rclInterpolationDelimiter Delimiter
