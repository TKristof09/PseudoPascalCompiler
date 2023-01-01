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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
open Ast
# 54 "parser.ml"
let yytransl_const = [|
  260 (* PLUS *);
  261 (* MINUS *);
  262 (* TIMES *);
  263 (* DIV *);
  264 (* LE *);
  265 (* GE *);
  266 (* LT *);
  267 (* GT *);
  268 (* EQ *);
  269 (* DIFF *);
  270 (* AND *);
  271 (* OR *);
  272 (* NOT *);
  273 (* COLON *);
  274 (* COLONEQUAL *);
  275 (* COMMA *);
  276 (* SEMICOLON *);
  277 (* PERIOD *);
  278 (* BEGIN *);
  279 (* END *);
  280 (* LPAREN *);
  281 (* RPAREN *);
  282 (* LBRACKET *);
  283 (* RBRACKET *);
  284 (* IF *);
  285 (* THEN *);
  286 (* ELSE *);
  287 (* WHILE *);
  288 (* DO *);
  289 (* REPEAT *);
  290 (* UNTIL *);
  291 (* VAR *);
  292 (* FUNCTION *);
  293 (* PROCEDURE *);
  294 (* PROGRAM *);
  295 (* NEW *);
  296 (* WRITE *);
  297 (* WRITELN *);
  298 (* READLN *);
  299 (* INTEGER *);
  300 (* BOOLEAN *);
  301 (* ARRAY *);
  302 (* OF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ID *);
  259 (* BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\009\000\009\000\009\000\
\007\000\007\000\007\000\007\000\007\000\005\000\005\000\005\000\
\002\000\002\000\010\000\010\000\011\000\012\000\012\000\003\000\
\003\000\013\000\013\000\014\000\014\000\015\000\015\000\008\000\
\008\000\016\000\016\000\000\000"

let yylen = "\002\000\
\005\000\004\000\003\000\006\000\006\000\004\000\003\000\004\000\
\004\000\004\000\003\000\001\000\001\000\001\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\004\000\007\000\004\000\001\000\001\000\003\000\
\001\000\003\000\002\000\003\000\003\000\001\000\003\000\000\000\
\002\000\000\000\003\000\002\000\003\000\001\000\003\000\003\000\
\000\000\010\000\008\000\002\000\000\000\003\000\000\000\002\000\
\000\000\002\000\000\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\060\000\000\000\000\000\000\000\041\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\000\000\013\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\047\000\043\000\030\000\031\000\000\000\
\045\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\048\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\056\000\007\000\011\000\035\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\052\000\
\000\000\000\000\000\000\000\000\000\000\000\000\058\000\034\000\
\037\000\000\000\000\000\006\000\000\000\000\000\009\000\010\000\
\029\000\000\000\000\000\000\000\000\000\039\000\026\000\027\000\
\000\000\000\000\000\000\054\000\000\000\000\000\005\000\000\000\
\000\000\000\000\051\000\028\000\000\000\050\000"

let yydgoto = "\002\000\
\004\000\006\000\013\000\048\000\079\000\034\000\054\000\049\000\
\041\000\008\000\009\000\010\000\014\000\076\000\112\000\084\000"

let yysindex = "\050\000\
\020\255\000\000\029\255\000\000\069\255\240\254\062\255\000\000\
\063\255\070\255\088\255\090\255\146\255\075\255\069\255\069\255\
\228\254\074\255\082\255\000\000\001\255\000\000\218\255\146\255\
\218\255\118\255\118\255\146\255\065\255\084\255\091\255\094\255\
\101\255\214\001\240\254\000\000\000\000\000\000\000\000\079\255\
\000\000\069\255\069\255\218\255\218\255\103\255\105\255\110\255\
\115\255\168\001\118\255\118\255\224\001\253\254\038\255\116\255\
\093\255\218\255\218\255\218\255\000\000\218\255\218\255\218\255\
\218\255\218\255\218\255\218\255\218\255\218\255\218\255\218\255\
\000\000\228\254\134\255\131\255\133\255\224\001\136\255\191\001\
\218\255\218\255\146\255\000\000\000\000\000\000\000\000\168\001\
\041\255\118\255\118\255\146\255\146\255\118\255\228\254\138\255\
\139\255\140\255\002\255\002\255\105\255\105\255\107\255\107\255\
\107\255\107\255\107\255\107\255\124\001\000\000\069\255\000\000\
\142\255\147\255\000\000\218\255\144\255\134\001\000\000\000\000\
\000\000\162\255\148\255\000\000\071\255\154\255\000\000\000\000\
\000\000\163\255\134\255\228\254\029\255\000\000\000\000\000\000\
\146\255\218\255\218\255\000\000\164\255\146\255\000\000\158\001\
\224\001\029\255\000\000\000\000\146\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\060\255\000\000\000\000\170\255\165\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\104\255\
\000\000\000\000\000\000\000\000\247\001\000\000\000\000\160\255\
\000\000\000\000\000\000\155\255\000\000\000\000\000\000\000\000\
\000\000\000\000\170\255\000\000\000\000\000\000\000\000\000\000\
\000\000\166\255\166\255\000\000\168\255\001\000\032\000\246\254\
\000\000\000\000\000\000\000\000\224\255\000\000\000\000\000\000\
\000\000\168\255\168\255\168\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\175\255\000\000\000\000\132\255\000\000\177\255\
\168\255\000\000\044\255\000\000\000\000\000\000\000\000\089\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\125\000\156\000\063\000\094\000\214\000\241\000\
\012\001\039\001\066\001\093\001\000\000\000\000\000\000\000\000\
\000\000\000\000\187\000\168\255\000\000\000\000\000\000\000\000\
\000\000\000\002\000\000\000\000\181\000\000\000\000\000\000\000\
\000\000\001\002\175\255\000\000\194\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\002\194\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\137\255\155\000\243\255\229\255\234\255\239\255\230\255\
\200\255\188\000\220\255\190\000\000\000\163\000\076\000\000\000"

let yytablesize = 817
let yytable = "\033\000\
\047\000\056\000\050\000\053\000\053\000\075\000\075\000\064\000\
\065\000\055\000\090\000\091\000\059\000\142\000\038\000\039\000\
\040\000\110\000\044\000\011\000\012\000\078\000\080\000\059\000\
\045\000\092\000\149\000\082\000\053\000\088\000\096\000\097\000\
\098\000\087\000\089\000\080\000\080\000\080\000\126\000\099\000\
\100\000\101\000\102\000\103\000\104\000\105\000\106\000\107\000\
\108\000\109\000\001\000\090\000\091\000\117\000\090\000\091\000\
\119\000\003\000\080\000\118\000\042\000\042\000\042\000\005\000\
\042\000\120\000\057\000\053\000\053\000\093\000\007\000\053\000\
\121\000\122\000\131\000\141\000\125\000\057\000\123\000\124\000\
\015\000\042\000\016\000\042\000\090\000\091\000\017\000\042\000\
\134\000\018\000\042\000\019\000\042\000\080\000\035\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\033\000\033\000\
\044\000\043\000\044\000\058\000\044\000\057\000\062\000\063\000\
\064\000\065\000\059\000\144\000\145\000\060\000\020\000\046\000\
\022\000\061\000\023\000\143\000\074\000\044\000\081\000\044\000\
\147\000\083\000\082\000\044\000\082\000\051\000\044\000\150\000\
\044\000\085\000\095\000\044\000\044\000\052\000\044\000\044\000\
\044\000\044\000\020\000\021\000\022\000\094\000\023\000\003\000\
\003\000\111\000\003\000\113\000\029\000\114\000\132\000\032\000\
\115\000\003\000\127\000\128\000\129\000\003\000\133\000\024\000\
\135\000\025\000\049\000\049\000\049\000\026\000\049\000\090\000\
\027\000\137\000\028\000\138\000\139\000\046\000\057\000\146\000\
\029\000\030\000\031\000\032\000\057\000\073\000\053\000\049\000\
\040\000\049\000\042\000\042\000\042\000\049\000\042\000\055\000\
\049\000\038\000\049\000\037\000\036\000\077\000\140\000\000\000\
\049\000\049\000\049\000\049\000\000\000\000\000\000\000\042\000\
\000\000\042\000\020\000\046\000\022\000\042\000\023\000\000\000\
\042\000\000\000\042\000\000\000\000\000\000\000\000\000\000\000\
\042\000\042\000\042\000\042\000\000\000\033\000\033\000\000\000\
\000\000\025\000\000\000\033\000\033\000\000\000\033\000\000\000\
\033\000\000\000\000\000\000\000\033\000\033\000\000\000\033\000\
\029\000\033\000\000\000\032\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\000\000\000\000\000\000\014\000\014\000\014\000\000\000\014\000\
\000\000\014\000\014\000\014\000\000\000\014\000\014\000\000\000\
\014\000\000\000\014\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\000\000\
\000\000\000\000\015\000\015\000\015\000\000\000\015\000\000\000\
\015\000\000\000\015\000\000\000\015\000\015\000\000\000\015\000\
\000\000\015\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\000\000\000\000\
\000\000\018\000\018\000\018\000\000\000\018\000\000\000\018\000\
\000\000\018\000\000\000\018\000\018\000\000\000\018\000\000\000\
\018\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\000\000\000\000\000\000\
\019\000\019\000\019\000\000\000\019\000\000\000\019\000\000\000\
\019\000\000\000\019\000\019\000\000\000\019\000\000\000\019\000\
\016\000\016\000\000\000\000\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\000\000\000\000\000\000\016\000\
\016\000\016\000\000\000\016\000\000\000\016\000\000\000\016\000\
\000\000\016\000\016\000\000\000\016\000\000\000\016\000\017\000\
\017\000\000\000\000\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\000\000\000\000\000\000\017\000\017\000\
\017\000\000\000\017\000\000\000\017\000\000\000\017\000\000\000\
\017\000\017\000\000\000\017\000\000\000\017\000\026\000\026\000\
\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
\008\000\008\000\000\000\008\000\000\000\000\000\002\000\002\000\
\000\000\002\000\008\000\000\000\026\000\000\000\008\000\000\000\
\002\000\000\000\000\000\000\000\002\000\021\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\000\000\000\000\000\000\
\021\000\021\000\021\000\000\000\021\000\000\000\021\000\000\000\
\021\000\000\000\021\000\021\000\000\000\021\000\000\000\021\000\
\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\000\000\000\000\000\000\023\000\023\000\023\000\000\000\023\000\
\000\000\023\000\000\000\023\000\000\000\023\000\023\000\000\000\
\023\000\000\000\023\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\000\000\000\000\000\000\020\000\020\000\
\020\000\000\000\020\000\000\000\020\000\000\000\020\000\000\000\
\020\000\020\000\000\000\020\000\000\000\020\000\022\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\000\000\000\000\
\000\000\022\000\022\000\022\000\000\000\022\000\000\000\022\000\
\000\000\022\000\000\000\022\000\022\000\000\000\022\000\000\000\
\022\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
\024\000\000\000\000\000\000\000\024\000\024\000\024\000\000\000\
\024\000\000\000\024\000\000\000\024\000\000\000\024\000\024\000\
\000\000\024\000\000\000\024\000\025\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\000\000\000\000\000\000\025\000\
\025\000\025\000\000\000\025\000\000\000\025\000\000\000\025\000\
\000\000\025\000\025\000\000\000\025\000\000\000\025\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\000\000\000\000\082\000\130\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\082\000\
\136\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\000\000\000\000\082\000\
\148\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\086\000\082\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\000\000\000\000\000\000\000\000\
\000\000\116\000\000\000\000\000\000\000\000\000\000\000\000\000\
\082\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\000\000\000\000\072\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\082\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\036\000\000\000\
\014\000\000\000\000\000\036\000\036\000\000\000\036\000\000\000\
\036\000\000\000\027\000\000\000\036\000\036\000\000\000\036\000\
\000\000\036\000\004\000\004\000\000\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\
\004\000"

let yycheck = "\013\000\
\023\000\028\000\025\000\026\000\027\000\042\000\043\000\006\001\
\007\001\027\000\014\001\015\001\023\001\133\000\043\001\044\001\
\045\001\074\000\018\001\036\001\037\001\044\000\045\000\034\001\
\024\001\029\001\146\000\026\001\051\000\052\000\058\000\059\000\
\060\000\051\000\052\000\058\000\059\000\060\000\095\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\072\000\001\000\014\001\015\001\081\000\014\001\015\001\
\083\000\038\001\081\000\082\000\001\001\002\001\003\001\035\001\
\005\001\025\001\023\001\090\000\091\000\032\001\002\001\094\000\
\090\000\091\000\111\000\132\000\094\000\034\001\092\000\093\000\
\019\001\022\001\020\001\024\001\014\001\015\001\017\001\028\001\
\116\000\002\001\031\001\002\001\033\001\116\000\020\001\036\001\
\037\001\024\001\039\001\040\001\041\001\042\001\014\001\015\001\
\001\001\024\001\003\001\024\001\005\001\045\001\004\001\005\001\
\006\001\007\001\024\001\138\000\139\000\024\001\001\001\002\001\
\003\001\021\001\005\001\137\000\046\001\022\001\024\001\024\001\
\142\000\020\001\026\001\028\001\026\001\016\001\031\001\149\000\
\033\001\023\001\046\001\036\001\037\001\024\001\039\001\040\001\
\041\001\042\001\001\001\002\001\003\001\034\001\005\001\020\001\
\021\001\020\001\023\001\025\001\039\001\025\001\017\001\042\001\
\025\001\030\001\025\001\025\001\025\001\034\001\020\001\022\001\
\025\001\024\001\001\001\002\001\003\001\028\001\005\001\014\001\
\031\001\030\001\033\001\026\001\018\001\017\001\023\001\020\001\
\039\001\040\001\041\001\042\001\034\001\035\000\025\001\022\001\
\025\001\024\001\001\001\002\001\003\001\028\001\005\001\025\001\
\031\001\025\001\033\001\016\000\015\000\043\000\131\000\255\255\
\039\001\040\001\041\001\042\001\255\255\255\255\255\255\022\001\
\255\255\024\001\001\001\002\001\003\001\028\001\005\001\255\255\
\031\001\255\255\033\001\255\255\255\255\255\255\255\255\255\255\
\039\001\040\001\041\001\042\001\255\255\014\001\015\001\255\255\
\255\255\024\001\255\255\020\001\021\001\255\255\023\001\255\255\
\025\001\255\255\255\255\255\255\029\001\030\001\255\255\032\001\
\039\001\034\001\255\255\042\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\019\001\020\001\021\001\255\255\023\001\
\255\255\025\001\026\001\027\001\255\255\029\001\030\001\255\255\
\032\001\255\255\034\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\255\255\019\001\020\001\021\001\255\255\023\001\255\255\
\025\001\255\255\027\001\255\255\029\001\030\001\255\255\032\001\
\255\255\034\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\255\255\019\001\020\001\021\001\255\255\023\001\255\255\025\001\
\255\255\027\001\255\255\029\001\030\001\255\255\032\001\255\255\
\034\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\019\001\020\001\021\001\255\255\023\001\255\255\025\001\255\255\
\027\001\255\255\029\001\030\001\255\255\032\001\255\255\034\001\
\004\001\005\001\255\255\255\255\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\019\001\
\020\001\021\001\255\255\023\001\255\255\025\001\255\255\027\001\
\255\255\029\001\030\001\255\255\032\001\255\255\034\001\004\001\
\005\001\255\255\255\255\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\255\255\019\001\020\001\
\021\001\255\255\023\001\255\255\025\001\255\255\027\001\255\255\
\029\001\030\001\255\255\032\001\255\255\034\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\020\001\021\001\255\255\023\001\255\255\255\255\020\001\021\001\
\255\255\023\001\030\001\255\255\026\001\255\255\034\001\255\255\
\030\001\255\255\255\255\255\255\034\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\019\001\020\001\021\001\255\255\023\001\255\255\025\001\255\255\
\027\001\255\255\029\001\030\001\255\255\032\001\255\255\034\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\019\001\020\001\021\001\255\255\023\001\
\255\255\025\001\255\255\027\001\255\255\029\001\030\001\255\255\
\032\001\255\255\034\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\255\255\019\001\020\001\
\021\001\255\255\023\001\255\255\025\001\255\255\027\001\255\255\
\029\001\030\001\255\255\032\001\255\255\034\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\255\255\019\001\020\001\021\001\255\255\023\001\255\255\025\001\
\255\255\027\001\255\255\029\001\030\001\255\255\032\001\255\255\
\034\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\255\255\255\255\255\255\019\001\020\001\021\001\255\255\
\023\001\255\255\025\001\255\255\027\001\255\255\029\001\030\001\
\255\255\032\001\255\255\034\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\019\001\
\020\001\021\001\255\255\023\001\255\255\025\001\255\255\027\001\
\255\255\029\001\030\001\255\255\032\001\255\255\034\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\255\255\255\255\026\001\027\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\026\001\
\027\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\255\255\255\255\026\001\
\027\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\025\001\026\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\255\255\255\255\255\255\255\255\
\255\255\019\001\255\255\255\255\255\255\255\255\255\255\255\255\
\026\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\255\255\255\255\026\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\026\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\015\001\255\255\
\026\001\255\255\255\255\020\001\021\001\255\255\023\001\255\255\
\025\001\255\255\026\001\255\255\029\001\030\001\255\255\032\001\
\255\255\034\001\020\001\021\001\255\255\023\001\255\255\255\255\
\255\255\255\255\255\255\255\255\030\001\255\255\255\255\255\255\
\034\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LE\000\
  GE\000\
  LT\000\
  GT\000\
  EQ\000\
  DIFF\000\
  AND\000\
  OR\000\
  NOT\000\
  COLON\000\
  COLONEQUAL\000\
  COMMA\000\
  SEMICOLON\000\
  PERIOD\000\
  BEGIN\000\
  END\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  DO\000\
  REPEAT\000\
  UNTIL\000\
  VAR\000\
  FUNCTION\000\
  PROCEDURE\000\
  PROGRAM\000\
  NEW\000\
  WRITE\000\
  WRITELN\000\
  READLN\000\
  INTEGER\000\
  BOOLEAN\000\
  ARRAY\000\
  OF\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'vars) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'fun_proc) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'instruction) in
    Obj.repr(
# 35 "parser.mly"
    ( {globalVars = _2; fun_proc = _3; main = _4 } )
# 470 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 39 "parser.mly"
                                         ( ProcCall (_1, _3) )
# 478 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 40 "parser.mly"
                                         ( SetVar (_1, _3) )
# 486 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 42 "parser.mly"
                                            ( SetArr (_1, _3, _6) )
# 495 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'instruction) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'instruction) in
    Obj.repr(
# 44 "parser.mly"
                                            ( If (_2, _4, _6) )
# 504 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'instruction) in
    Obj.repr(
# 45 "parser.mly"
                                            ( While (_2, _4) )
# 512 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bloc) in
    Obj.repr(
# 46 "parser.mly"
                                            ( Sequence _2 )
# 519 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'bloc) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 48 "parser.mly"
                                            ( Sequence [Sequence _2; While(_4,Sequence _2)] )
# 527 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 49 "parser.mly"
                                            ( Write (_3) )
# 534 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 50 "parser.mly"
                                            ( Writeln (_3) )
# 541 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 54 "parser.mly"
                                            ( _2 )
# 548 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 55 "parser.mly"
                                            ( Int _1 )
# 555 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 56 "parser.mly"
                                            ( Bool _1 )
# 562 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                                            ( Var _1 )
# 569 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 58 "parser.mly"
                                            ( UMinus (_2) )
# 576 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 59 "parser.mly"
                                            ( Bin (Plus, _1, _3) )
# 584 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 60 "parser.mly"
                                            ( Bin (Minus, _1, _3) )
# 592 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 61 "parser.mly"
                                            ( Bin (Times, _1, _3) )
# 600 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 62 "parser.mly"
                                            ( Bin (Div, _1, _3) )
# 608 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 64 "parser.mly"
                                            ( Bin (Lt, _1, _3) )
# 616 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 65 "parser.mly"
                                            ( Bin (Le, _1, _3) )
# 624 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 66 "parser.mly"
                                            ( Bin (Gt, _1, _3) )
# 632 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 67 "parser.mly"
                                            ( Bin (Ge, _1, _3) )
# 640 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 68 "parser.mly"
                                            ( Bin (Eq, _1, _3) )
# 648 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 69 "parser.mly"
                                            ( Bin (Diff, _1, _3) )
# 656 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 71 "parser.mly"
                                            ( FunctionCall (_1, _3) )
# 664 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 73 "parser.mly"
                                            ( GetArr (_1, _3) )
# 672 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'varType) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 75 "parser.mly"
                                            ( NewArr(_4, _6))
# 680 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 76 "parser.mly"
                                            ( Readln (_3) )
# 687 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                                           ( Integer )
# 693 "parser.ml"
               : 'varType))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                                           ( Boolean )
# 699 "parser.ml"
               : 'varType))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'varType) in
    Obj.repr(
# 82 "parser.mly"
                                           ( Array (_3) )
# 706 "parser.ml"
               : 'varType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 86 "parser.mly"
                                            ( Expr _1 )
# 713 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'condition) in
    Obj.repr(
# 87 "parser.mly"
                                            ( _2 )
# 720 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 88 "parser.mly"
                                            ( Not _2 )
# 727 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 89 "parser.mly"
                                            ( Or(_1, _3) )
# 735 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 90 "parser.mly"
                                            ( And(_1, _3) )
# 743 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 94 "parser.mly"
                                            ( [_1] )
# 750 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arguments) in
    Obj.repr(
# 95 "parser.mly"
                                            ( _1 :: _3)
# 758 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                                            ( [] )
# 764 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'varList) in
    Obj.repr(
# 100 "parser.mly"
                                  ( _2 )
# 771 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
                                            ( [] )
# 777 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'varList) in
    Obj.repr(
# 104 "parser.mly"
                                             ( _1::_3 )
# 785 "parser.ml"
               : 'varList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var) in
    Obj.repr(
# 105 "parser.mly"
                                                     ( [_1] )
# 792 "parser.ml"
               : 'varList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'idList) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'varType) in
    Obj.repr(
# 107 "parser.mly"
                                            ( _1, _3 )
# 800 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "parser.mly"
                                            ( [_1] )
# 807 "parser.ml"
               : 'idList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'idList) in
    Obj.repr(
# 112 "parser.mly"
                                            ( _1::_3 )
# 815 "parser.ml"
               : 'idList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fun_proc) in
    Obj.repr(
# 116 "parser.mly"
                                            ( _1 :: _3 )
# 823 "parser.ml"
               : 'fun_proc))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
                                            ( [] )
# 829 "parser.ml"
               : 'fun_proc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'varType) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'vars) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'instruction) in
    Obj.repr(
# 122 "parser.mly"
                                            ( _2, { arguments = _4; result = Some _7; locals = _9; body = _10 } )
# 840 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'vars) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'instruction) in
    Obj.repr(
# 124 "parser.mly"
                                            ( _2, { arguments = _4; result = None; locals = _7; body = _8 } )
# 850 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'argsCont) in
    Obj.repr(
# 127 "parser.mly"
                                            ( _1 :: _2 )
# 858 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
                                            ( [] )
# 864 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argsCont) in
    Obj.repr(
# 131 "parser.mly"
                                            ( _2 :: _3)
# 872 "parser.ml"
               : 'argsCont))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                                            ( [] )
# 878 "parser.ml"
               : 'argsCont))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instruction) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'blocCont) in
    Obj.repr(
# 135 "parser.mly"
                                            ( _1 :: _2 )
# 886 "parser.ml"
               : 'bloc))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "parser.mly"
                                            ( [] )
# 892 "parser.ml"
               : 'bloc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bloc) in
    Obj.repr(
# 139 "parser.mly"
                                            ( _2 )
# 899 "parser.ml"
               : 'blocCont))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                                            ( [] )
# 905 "parser.ml"
               : 'blocCont))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)