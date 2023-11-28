%{
// This file contains a Bison grammar for RCL. It is not intended to be used
// directly, there is a hand-written recursive descent parser in src/parser.rs
// instead. However to inform the structure of the hand-written one, it is nice
// to have the grammar in a concise format that is readable by humans and
// machines. In particular, Bison will warn about ambiguities.

// The grammar aims to be parsable with a recursive descent parser, but it's not
// strictly LL(k) because some rules contain left recursion. All of the cases
// that do though, have a clear token immediately following to still decide the
// production.

// Inspect with: bison --feature=syntax-only -Wcounterexamples src/grammar.y
%}

%token IDENT STRING NUMBER UNOP BINOP FSTRING_OPEN FSTRING_INNER FSTRING_CLOSE

%%

expr
  : expr_op
  | expr_stmt
  | expr_if
  ;

expr_stmt: stmt expr;
expr_if: "if" expr "then" expr "else" expr;
expr_import: "import" expr;

// There is no operator precedence, so if there is an operator, its args must
// not themselves contain operators. One exception is that if the operator is
// the same, it may be repeated more than once, e.g. "a + b + c" does not need
// to be written as "(a + b) + c". But "a + b * c" does need to be written as
// "a + (b * c)".
expr_op
  : expr_import
  | expr_lambda
  | UNOP expr_unop
  | expr_not_op BINOP expr_binop
  | expr_not_op
  ;

expr_lambda: lambda_args "=>" expr;

lambda_args
  : IDENT
  | '(' lambda_args_inner ')'
  ;

lambda_args_inner
  : %empty
  // TODO: If I allow the ident without the comma, I get a reduce/reduce
  // conflict? Is that very bad? I don't understand the counterexample.
  // | IDENT
  | IDENT ',' lambda_args_inner
  ;

expr_unop: expr_not_op | UNOP expr_unop;

// This rule for binop is simplified here. In reality, there should be a
// dedicated one for every binop, so you can repeat the same binop without
// parens, but you cannot mix multiple different ones. To keep this file simple,
// we don't expand it out here and we have just one generic binop.
expr_binop: expr_not_op | expr_not_op BINOP expr_binop;

// Because we disallow confusing operator combinations without parens, the nodes
// of an operator are "not binary operator", "not binop" for short.
expr_not_op
  : expr_term
  | expr_not_op '(' call_args ')'
  | expr_not_op '[' expr ']'
  | expr_not_op '.' IDENT
  ;

call_args
  : %empty
  | expr
  | expr ',' call_args
  ;

expr_term
  : '{' seqs '}'
  | '[' seqs ']'
  | '(' expr ')'
  | FSTRING_OPEN fstring
  | STRING
  | NUMBER
  | IDENT
  ;

fstring
  : expr FSTRING_CLOSE
  | expr FSTRING_INNER fstring
  ;

stmt
  : "let" IDENT '=' expr ';'
  | "assert" expr ',' expr ';'
  | "trace" expr ';'
  ;

seqs
  : %empty
  | seq
  | seq ',' seqs
  ;

// Note, here we use expr_op instead of expr to avoid a conflict with the let
// and for from the comprehension.
seq
  : expr_op
  | expr_op ':' expr
  | IDENT '=' expr ',' seq
  | stmt seq
  | "for" idents "in" expr ':' seq
  | "if" expr ':' seq
  ;

idents: IDENT | idents ',' IDENT;
