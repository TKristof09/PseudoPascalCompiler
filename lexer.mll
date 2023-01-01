{
    open Parser


exception Eof
exception LexError

let keywords = Hashtbl.create 25
let _ =
  List.iter (function keyword, token -> Hashtbl.add keywords keyword token)
    [
  "var", VAR;
  "integer",  INTEGER;
  "boolean",  BOOLEAN;
  "false",  BOOL false;
  "true",  BOOL true;
  "array",  ARRAY;
  "of",  OF;

  "or", OR;
  "and", AND;
  "not", NOT;

  "if",  IF;
  "then",  THEN;
  "else",  ELSE;

  "while",  WHILE;
  "do",  DO;

  "repeat", REPEAT;
  "until", UNTIL;

  "function",  FUNCTION;
  "procedure",  PROCEDURE;

  "new", NEW;

  "program",  PROGRAM;
  "begin",  BEGIN;
  "end",  END;

  "readln",  READLN;
  "write",  WRITE;
  "writeln",  WRITELN;
]
let id_or_keyword s =
  try Hashtbl.find keywords s with Not_found -> ID s


}

let comment = '{'[^'}']*'}'
let id = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let number = ['0'-'9']+

rule token = parse
        [' ' '\t' '\n']     { token lexbuf } (*skip*)
    | comment               { token lexbuf } (*skip*)
    | id             { id_or_keyword (Lexing.lexeme lexbuf) }
    | number         { INT(int_of_string (Lexing.lexeme lexbuf)) }
    | ":="           { COLONEQUAL }
    | "<>"           { DIFF }
    | "<="           { LE }
    | ">="           { GE }
    | '<'            { LT }
    | '>'            { GT }
    | ";"            { SEMICOLON }
    | ","            { COMMA }
    | ':'            { COLON }
    | '='            { EQ }
    | '-'            { MINUS }
    | '+'            { PLUS }
    | '*'            { TIMES }
    | '/'            { DIV }
    | '('            { LPAREN }
    | ')'            { RPAREN }
    | '['            { LBRACKET }
    | ']'            { RBRACKET }
    | '.'            { PERIOD }
    | eof            { raise Eof }
    | _              { raise LexError }



