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
  | Readln of expression list

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



let rec print_type t = match t with
    | Integer -> Printf.printf "Int: "
    | Boolean -> Printf.printf "Bool: "
    | Array(t') -> print_type t'; Printf.printf " Array: "

let rec print_names n = match n with
    | [] -> Printf.printf " : "
    | h::t -> Printf.printf "%s, " h; print_names t
let rec print_vars v prefix = match v with
    | [] -> Printf.printf "%s" prefix;Printf.printf "\n"
    | (names, t)::v' -> Printf.printf "%s" prefix; print_type t; print_names names; Printf.printf "\n"; print_vars v' prefix

let print_res_type r = match r with
    | None -> Printf.printf "void"
    | Some t -> print_type t

let print_op op = match op with
    | Plus -> Printf.printf " + "
    | Minus -> Printf.printf " - "
    | Times -> Printf.printf " * "
    | Div -> Printf.printf " / "
    | Lt -> Printf.printf " < "
    | Le -> Printf.printf " <= "
    | Gt -> Printf.printf " > "
    | Ge -> Printf.printf " >= "
    | Eq -> Printf.printf " == "
    | Diff -> Printf.printf " <> "

let rec print_expr e = match e with
    | Int i -> Printf.printf "Int %i" i
    | Bool b -> Printf.printf "Bool %b" b
    | Bin (op, e1, e2) -> print_expr e1; print_op op; print_expr e2
    | Var s -> Printf.printf "Var %s" s
    | FunctionCall (s, exprl) -> Printf.printf "FCall %s (" s; List.iter (fun x -> print_expr x; Printf.printf ",") exprl; Printf.printf ")"
    | GetArr (e1, e2) -> Printf.printf "ArrVar "; print_expr e1; Printf.printf "["; print_expr e2; Printf.printf "]"
    | NewArr (t, e') -> Printf.printf "new Array of "; print_type t; Printf.printf "["; print_expr e'; Printf.printf "]"
    | UMinus e' -> Printf.printf "-"; print_expr e'
    | Readln (exprl) -> Printf.printf "Read ("; List.iter (fun x -> print_expr x; Printf.printf ",") exprl; Printf.printf ")"; Printf.printf "\n"

let rec print_cond c = match c with
    | Expr e -> print_expr e;
    | Not c' -> Printf.printf "Not "; print_cond c'
    | And (c1,c2) -> print_cond c1; Printf.printf " AND "; print_cond c2
    | Or (c1,c2) -> print_cond c1; Printf.printf " OR "; print_cond c2

let rec print_instr i prefix = match i with
  | SetVar (name, expr) -> Printf.printf "%s" prefix; Printf.printf "Set %s = " name; print_expr expr; Printf.printf "\n"
  | Sequence l ->  Printf.printf "%s" prefix; Printf.printf "Sequence:\n"; List.iter (fun x -> print_instr x ("\t"^prefix)) l; Printf.printf "\n"
  | If (c,i1,i2) ->  Printf.printf "%s" prefix; Printf.printf "If "; print_cond c; Printf.printf " then \n"; print_instr i1 ("\t"^prefix); Printf.printf "\nElse\n"; print_instr i2 ("\t"^prefix); Printf.printf "\n"
  | While (c, i') ->  Printf.printf "%s" prefix;  Printf.printf "While "; print_cond c; Printf.printf " do \n"; print_instr i' ("\t"^prefix); Printf.printf "\n"
  | ProcCall (name, exprl) ->  Printf.printf "%s" prefix; Printf.printf "Call %s(" name; List.iter (fun x -> print_expr x; Printf.printf ",") exprl; Printf.printf ")"; Printf.printf "\n"
  | Write (exprl) ->  Printf.printf "%s" prefix; Printf.printf "Write ("; List.iter (fun x -> print_expr x; Printf.printf ",") exprl; Printf.printf ")"; Printf.printf "\n"
;
  | Writeln (exprl) ->  Printf.printf "%s" prefix; Printf.printf "Writeln ( ";List.iter (fun x -> print_expr x; Printf.printf ",") exprl; Printf.printf ")"; Printf.printf "\n"
;
  | SetArr (e1, e2, e3) ->  Printf.printf "%s" prefix; Printf.printf "SetArr: ";print_expr e1; Printf.printf "["; print_expr e2; Printf.printf "] = "; print_expr e3; Printf.printf "\n"


let rec print_fun_proc l = match l with
    | [] -> Printf.printf "\n////////\n"
    | (name, def)::l' ->
            Printf.printf "Function/procedure: %s\n" name;
            Printf.printf "\targs:\n";
            print_vars def.arguments "\t\t";
            Printf.printf "\tresult: "; print_res_type def.result;
            Printf.printf "\n\tbody:\n";
            print_instr def.body "\t";
            Printf.printf "/////////\n";
            print_fun_proc l'

let print p =
    Printf.printf "AST:\n";
    print_vars p.globalVars "";
    Printf.printf "////////////////////\n";
    print_fun_proc p.fun_proc;
    Printf.printf "////////////////////\n";
    Printf.printf "MAIN CODE:\n";
    print_instr p.main ""


let print_list l = List.iter (Printf.printf "%s ") l

let rec check_scope_expr e current_symbols = match e with
    | Int _ -> true
    | Bool _ -> true
    | Bin (_, e1, e2) -> (check_scope_expr e1 current_symbols) && (check_scope_expr e2 current_symbols)
    | Var s -> List.mem s current_symbols
    | FunctionCall (s, exprl) -> (List.mem s current_symbols) && (List.fold_left (fun acc e -> acc && (check_scope_expr e current_symbols)) true exprl)
    | GetArr (e1, e2) -> (check_scope_expr e1 current_symbols) && (check_scope_expr e2 current_symbols)
    | NewArr (_, e') -> check_scope_expr e' current_symbols
    | UMinus e' -> check_scope_expr e' current_symbols
    | Readln (exprl) ->(List.fold_left (fun acc e -> acc && (check_scope_expr e current_symbols)) true exprl)
let rec check_scope_cond c current_symbols = match c with
    | Expr e -> check_scope_expr e current_symbols
    | Not c' -> check_scope_cond c' current_symbols
    | And (c1, c2) -> (check_scope_cond c1 current_symbols) && (check_scope_cond c2 current_symbols)
    | Or (c1, c2) -> (check_scope_cond c1 current_symbols) && (check_scope_cond c2 current_symbols)

let rec check_scope_instr i current_symbols = match i with
    | SetVar (name, expr) -> (List.mem name current_symbols) && (check_scope_expr expr current_symbols)
    | Sequence l ->  List.fold_left (fun acc i' -> acc && (check_scope_instr i' current_symbols)) true l
    | If (c,i1,i2) ->  (check_scope_cond c current_symbols) && (check_scope_instr i1 current_symbols) && (check_scope_instr i2 current_symbols)
    | While (c, i') ->  (check_scope_cond c current_symbols) && (check_scope_instr i' current_symbols)
    | ProcCall (name, exprl) -> (List.mem name current_symbols) && (List.fold_left (fun acc e -> acc && (check_scope_expr e current_symbols)) true exprl)
    | Write (exprl) ->(List.fold_left (fun acc e -> acc && (check_scope_expr e current_symbols)) true exprl)
    | Writeln (exprl) -> (List.fold_left (fun acc e -> acc && (check_scope_expr e current_symbols)) true exprl)
    | SetArr (e1, e2, e3) -> (check_scope_expr e1 current_symbols) && (check_scope_expr e2 current_symbols) && (check_scope_expr e3 current_symbols)


let rec build_symbol_list vars = match vars with
    | [] -> []
    | (names,_)::t -> names @ (build_symbol_list t) (* probably not many variables so complexity is fine for now *)
let rec check_scope_fun l globals = match l with
    | [] -> true
    | (name, def)::t ->
            let symbols = name::(build_symbol_list def.locals) @ (build_symbol_list def.arguments) @ globals in
            print_list symbols;
            (check_scope_instr def.body symbols) && (check_scope_fun t (name::globals))

let check_scope p =
    let builtins = ["readln"; "writeln"; "write"] in
    let global_vars = builtins @ (build_symbol_list p.globalVars) in
    let globals = List.fold_left (fun acc (n,_) -> n::acc) global_vars p.fun_proc in
    (check_scope_fun p.fun_proc globals) && (check_scope_instr p.main globals)

