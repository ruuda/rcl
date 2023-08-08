%{
// This file contains a Bison grammar for RCL. It is not intended to be used
// directly, there is a hand-written recursive descent parser in src/parser.rs
// instead. However to inform the structure of the hand-written one, it is nice
// to have the grammar in a concise format that is readable by humans and
// machines. In particular, Bison will warn about ambiguities.

// Inspect with: bison --feature=syntax-only -Wcounterexamples src/grammar.y
%}

%token IDENT STRING UNOP 

%%

expr
  : expr_op
  | expr_let
  | expr_if
  ;

expr_let: "let" IDENT '=' expr ';' expr;
expr_if: "if" expr "then" expr "else" expr;

// There is no operator precedence, so if there is an operator, its args must
// not themselves contain operators. One exception is that if the operator is
// the same, it may be repeated more than once, e.g. "a + b + c" does not need
// to be written as "(a + b) + c". But "a + b * c" does need to be written as
// "a + (b * c)".
expr_op
  : expr_notop
  | UNOP expr_notop
  | expr_ops_pipe '|' expr_notop
  | expr_ops_plus '+' expr_notop
  | expr_ops_and  "and" expr_notop
  | expr_ops_or   "or"  expr_notop
  ;

expr_ops_pipe: expr_notop | expr_ops_pipe '|'   expr_notop;
expr_ops_plus: expr_notop | expr_ops_plus '+'   expr_notop;
expr_ops_and:  expr_notop | expr_ops_and  "and" expr_notop;
expr_ops_or:   expr_notop | expr_ops_or   "or"  expr_notop;

expr_notop
  : expr_term
  | expr_call
  ;

expr_term
  : '{' seqs '}'
  | '[' seqs ']'
  | '(' expr ')'
  | STRING
  | IDENT
  | expr_field
  ;

expr_field: expr_notop '.' IDENT;

expr_call
  : expr_term '(' ')'
  // TODO: Accept list.
  | expr_term '(' expr ')'
  ;

seqs
  : %empty
  // Note, here we use expr_op instead of expr to avoid a conflict with the let
  // and for from the comprehension.
  | expr_op ',' seqs
  | expr_op ':' expr ',' seqs
  | IDENT '=' expr ';' seqs
  | "let" IDENT '=' expr ';' seqs
  | "for" idents "in" expr ':' seqs
  | "if" expr ':' seqs
  ;

idents: IDENT | idents ',' IDENT;
