
type varType =
    | Integer
    | Boolean
    | Array of varType


type variables = (string list * varType) list (*(variable name, variable type) *)
type program = {
    globalVars : variables; (*global variables*)
    fun_proc : (string * definition) list; (*functions and procedures: (name, definition)*)
    main : instruction; (*the "main function" of the program*)
  }
and definition = { (*function or procedure definition*)
    arguments : variables; (*arguments of the function*)
    result : varType option; (*return value of function: None if procedure, Some v if function*)
    locals : variables; (*local variables*)
    body : instruction; (*function body*)
  }

and expression =
  | Int of int * Lexing.position
  | Bool of bool * Lexing.position
  | Bop of op * expression * expression * Lexing.position
  | Var of string * Lexing.position
  | FunctionCall of string * expression list * Lexing.position
  | GetArr of expression * expression * Lexing.position
  | NewArr of varType * expression * Lexing.position
  | UMinus of expression * Lexing.position
  | Readln

and op =
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


(*Print the AST of the program*)
val print: program -> unit

(*Check that each variable, function are used in the correct scope*)
val check_scope: program -> bool
(*Check for type correctness*)
val check_types: program -> bool
