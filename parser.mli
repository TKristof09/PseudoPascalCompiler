type token =
  | INT of (int)
  | ID of (string)
  | BOOL of (bool)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LE
  | GE
  | LT
  | GT
  | EQ
  | DIFF
  | AND
  | OR
  | NOT
  | COLON
  | COLONEQUAL
  | COMMA
  | SEMICOLON
  | PERIOD
  | BEGIN
  | END
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | REPEAT
  | UNTIL
  | VAR
  | FUNCTION
  | PROCEDURE
  | PROGRAM
  | NEW
  | WRITE
  | WRITELN
  | READLN
  | INTEGER
  | BOOLEAN
  | ARRAY
  | OF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
