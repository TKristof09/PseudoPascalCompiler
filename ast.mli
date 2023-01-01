
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
  | Int of int
  | Bool of bool
  | Bin of binaryOp * expression * expression
  | Var of string
  | FunctionCall of string * expression list
  | GetArr of expression * expression
  | NewArr of varType * expression
  | UMinus of expression
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
    | Expr of expression
    | Not of condition
    | And of condition * condition
    | Or of condition * condition

and instruction =
  | SetVar of string * expression
  | Sequence of instruction list
  | If of condition * instruction * instruction
  | While of condition * instruction
  | ProcCall of string * expression list
  | Write of expression list
  | Writeln of expression list
  | SetArr of expression * expression * expression


val print: program -> unit

val check_scope: program -> bool

val check_types: program -> bool
