
%{
open Ast
%}
%token <int> INT
%token <string> ID
%token <bool> BOOL
%token PLUS MINUS TIMES DIV
%token LE GE LT GT EQ DIFF
%token AND OR NOT
%token COLON COLONEQUAL COMMA SEMICOLON PERIOD
%token BEGIN END
%token LPAREN RPAREN LBRACKET RBRACKET
%token IF THEN ELSE WHILE DO REPEAT UNTIL
%token VAR FUNCTION PROCEDURE PROGRAM NEW
%token WRITE WRITELN READLN
%token INTEGER BOOLEAN ARRAY OF


%left DIFF GE LE LT GT EQ
%left PLUS MINUS
%left TIMES DIV
%left OR
%left AND
%right NOT
%nonassoc UMINUS
%nonassoc LBRACKET

%start program
%type <Ast.program> program
%%

program:
  PROGRAM vars fun_proc instruction PERIOD
    { {globalVars = $2; fun_proc = $3; main = $4 } }
;

instruction:
      ID LPAREN arguments RPAREN         { ProcCall ($1, $3) }
    | ID COLONEQUAL expression           { SetVar ($1, $3) }
    | expression LBRACKET expression RBRACKET COLONEQUAL expression
                                            { SetArr ($1, $3, $6) }
    | IF condition THEN instruction ELSE instruction
                                            { If ($2, $4, $6) }
    | WHILE condition DO instruction        { While ($2, $4) }
    | BEGIN bloc END                        { Sequence $2 }
    | REPEAT bloc UNTIL condition
                                            { Sequence [Sequence $2; While($4,Sequence $2)] }
    | WRITE LPAREN arguments RPAREN         { Write ($3) }
    | WRITELN LPAREN arguments RPAREN       { Writeln ($3) }
;

expression:
      LPAREN expression RPAREN              { $2 }
    | INT                                   { Int $1 }
    | BOOL                                  { Bool $1 }
    | ID                                    { Var $1 }
    | MINUS expression %prec UMINUS         { UMinus ($2) }
    | expression PLUS expression            { Bin (Plus, $1, $3) }
    | expression MINUS expression           { Bin (Minus, $1, $3) }
    | expression TIMES expression           { Bin (Times, $1, $3) }
    | expression DIV expression             { Bin (Div, $1, $3) }

    | expression LT expression              { Bin (Lt, $1, $3) }
    | expression LE expression              { Bin (Le, $1, $3) }
    | expression GT expression              { Bin (Gt, $1, $3) }
    | expression GE expression              { Bin (Ge, $1, $3) }
    | expression EQ expression              { Bin (Eq, $1, $3) }
    | expression DIFF expression            { Bin (Diff, $1, $3) }

    | ID LPAREN arguments RPAREN            { FunctionCall ($1, $3) }
    | expression LBRACKET expression RBRACKET
                                            { GetArr ($1, $3) }
    | NEW ARRAY OF varType LBRACKET expression RBRACKET
                                            { NewArr($4, $6)}
    | READLN LPAREN RPAREN                  { Readln }
;

varType:
      INTEGER                              { Integer }
    | BOOLEAN                              { Boolean }
    | ARRAY OF varType                     { Array ($3) }
;

condition:
      expression                            { Expr $1 }
    | LPAREN condition RPAREN               { $2 }
    | NOT condition                         { Not $2 }
    | condition OR condition                { Or($1, $3) }
    | condition AND condition               { And($1, $3) }
;

arguments:
      expression                            { [$1] }
    | expression COMMA arguments            { $1 :: $3}
    |                                       { [] }
;

vars:
      VAR varList                 { $2 }
    |                                       { [] }
;
varList:
      var SEMICOLON varList                  { $1::$3 }
    | var SEMICOLON                                  { [$1] }
var:
     idList COLON varType                   { $1, $3 }
;

idList:
      ID                                    { [$1] }
    | ID COMMA idList                       { $1::$3 }
;

fun_proc:
      def SEMICOLON fun_proc                { $1 :: $3 }
    |                                       { [] }
;

def:
      FUNCTION ID LPAREN args RPAREN COLON varType SEMICOLON vars instruction
                                            { $2, { arguments = $4; result = Some $7; locals = $9; body = $10 } }
    | PROCEDURE ID LPAREN args RPAREN SEMICOLON vars instruction
                                            { $2, { arguments = $4; result = None; locals = $7; body = $8 } }
;
args:
      var argsCont                          { $1 :: $2 }
    |                                       { [] }
;
argsCont:
      SEMICOLON var argsCont                { $2 :: $3}
    |                                       { [] }
;
bloc:
      instruction blocCont                  { $1 :: $2 }
    |                                       { [] }
;
blocCont:
      SEMICOLON bloc                        { $2 }
    |                                       { [] }
;
