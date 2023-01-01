
type varType =
    | Integer
    | Boolean
    | Array of varType


type variables = (string list * varType) list
type program = {
    globalVars : variables;
    fun_proc : (string * definition) list;
    main : instruction;
  }
and definition = {
    arguments : variables;
    result : varType option;
    locals : variables;
    body : instruction;
  }

and expression =
  | Int of int * Lexing.position
  | Bool of bool * Lexing.position
  | Bin of binaryOp * expression * expression * Lexing.position
  | Var of string * Lexing.position
  | FunctionCall of string * expression list * Lexing.position
  | GetArr of expression * expression * Lexing.position
  | NewArr of varType * expression * Lexing.position
  | UMinus of expression * Lexing.position
  | Readln

and binaryOp =
  | Plus
  | Minus
  | Times
  | Div
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Diff

and condition =
    | Expr of expression  * Lexing.position
    | Not of condition * Lexing.position
    | And of condition * condition * Lexing.position
    | Or of condition * condition * Lexing.position

and instruction =
  | SetVar of string * expression * Lexing.position
  | Sequence of instruction list * Lexing.position
  | If of condition * instruction * instruction * Lexing.position
  | While of condition * instruction * Lexing.position
  | ProcCall of string * expression list * Lexing.position
  | Write of expression list * Lexing.position
  | Writeln of expression list * Lexing.position
  | SetArr of expression * expression * expression * Lexing.position


val print: program -> unit

val check_scope: program -> bool

val check_types: program -> bool
