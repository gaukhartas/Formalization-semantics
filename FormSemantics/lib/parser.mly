
%token <string> IDENTIFIER
%token <Lang.tp> TP
%token <bool> BCONSTANT
%token <int> INTCONSTANT
%token BLAND BLOR
%token EQ LE 
%token ADD MUL DIV 
%token LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN 
%token DOT COMMA COLON
%token EOF

%start<Lang.expr> main

%left BLOR
%left BLAND
%left EQ LE 
%left ADD 
%left MUL DIV 

%{ open Lang %}

%%

main: expr EOF { $1 }


/* Expressions */

primary_expr:
| v = IDENTIFIER 
     { Var(v) }
| c = INTCONSTANT
     { Const(IntV(c)) }
| LPAREN e = expr RPAREN
     { e }

/* TODO: to be completed */
expr:
| a = primary_expr { a }
| e1  = primary_expr; ADD; e2  = primary_expr { BinOp(ArithOp Add, e1, e2) }


%%
