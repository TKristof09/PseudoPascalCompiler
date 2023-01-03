exception ScopeError of Lexing.position
exception TypeError of Lexing.position



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
    | Int (i,_) -> Printf.printf "Int %i" i
    | Bool (b,_) -> Printf.printf "Bool %b" b
    | Bop (op, e1, e2, _) -> print_expr e1; print_op op; print_expr e2
    | Var (s,_) -> Printf.printf "Var %s" s
    | FunctionCall (s, exprl, _) -> Printf.printf "FCall %s (" s; List.iter (fun x -> print_expr x; Printf.printf ",") exprl; Printf.printf ")"
    | GetArr (e1, e2, _) -> Printf.printf "ArrVar "; print_expr e1; Printf.printf "["; print_expr e2; Printf.printf "]"
    | NewArr (t, e', _) -> Printf.printf "new Array of "; print_type t; Printf.printf "["; print_expr e'; Printf.printf "]"
    | UMinus (e',_) -> Printf.printf "-"; print_expr e'
    | Readln -> Printf.printf "Readln ()\n"

let rec print_cond c = match c with
    | Expr (e,_) -> print_expr e;
    | Not (c',_) -> Printf.printf "Not "; print_cond c'
    | And (c1,c2, _) -> print_cond c1; Printf.printf " AND "; print_cond c2
    | Or (c1,c2, _) -> print_cond c1; Printf.printf " OR "; print_cond c2

let rec print_instr i prefix = match i with
  | SetVar (name, expr, _) -> Printf.printf "%s" prefix; Printf.printf "Set %s = " name; print_expr expr; Printf.printf "\n"
  | Sequence (l,_) ->  Printf.printf "%s" prefix; Printf.printf "Sequence:\n"; List.iter (fun x -> print_instr x ("\t"^prefix)) l; Printf.printf "\n"
  | If (c,i1,i2, _) ->  Printf.printf "%s" prefix; Printf.printf "If "; print_cond c; Printf.printf " then \n"; print_instr i1 ("\t"^prefix); Printf.printf "\nElse\n"; print_instr i2 ("\t"^prefix); Printf.printf "\n"
  | While (c, i', _) ->  Printf.printf "%s" prefix;  Printf.printf "While "; print_cond c; Printf.printf " do \n"; print_instr i' ("\t"^prefix); Printf.printf "\n"
  | ProcCall (name, exprl, _) ->  Printf.printf "%s" prefix; Printf.printf "Call %s(" name; List.iter (fun x -> print_expr x; Printf.printf ",") exprl; Printf.printf ")"; Printf.printf "\n"
  | Write (exprl, _) ->  Printf.printf "%s" prefix; Printf.printf "Write ("; List.iter (fun x -> print_expr x; Printf.printf ")") exprl; Printf.printf ")"; Printf.printf "\n"
;
  | Writeln (exprl, _) ->  Printf.printf "%s" prefix; Printf.printf "Writeln ( ";List.iter (fun x -> print_expr x; Printf.printf ")") exprl; Printf.printf ")"; Printf.printf "\n"
;
  | SetArr (e1, e2, e3, _) ->  Printf.printf "%s" prefix; Printf.printf "SetArr: ";print_expr e1; Printf.printf "["; print_expr e2; Printf.printf "] = "; print_expr e3; Printf.printf "\n"


let rec print_fun_proc l = match l with
    | [] -> Printf.printf "\n////////\n"
    | (name, def)::l' ->
            Printf.printf "Function/procedure: %s\n" name;
            Printf.printf "\tlocals:\n";
            print_vars def.locals "\t\t";
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




let rec check_scope_expr e current_symbols = match e with
    | Int (_,pos) -> if true then true else raise (ScopeError (pos))
    | Bool (_,pos) -> if true then true else raise (ScopeError (pos))
    | Bop (_, e1, e2, pos) -> if (check_scope_expr e1 current_symbols) && (check_scope_expr e2 current_symbols) then true else raise (ScopeError (pos))
    | Var (s,pos) -> if List.mem s current_symbols then true else raise (ScopeError (pos))
    | FunctionCall (s, exprl, pos) -> if (List.mem s current_symbols) && (List.fold_left (fun acc e -> acc && (check_scope_expr e current_symbols)) true exprl) then true else raise (ScopeError (pos))
    | GetArr (e1, e2, pos) -> if (check_scope_expr e1 current_symbols) && (check_scope_expr e2 current_symbols) then true else raise (ScopeError (pos))
    | NewArr (_, e', pos) -> if check_scope_expr e' current_symbols then true else raise (ScopeError (pos))
    | UMinus (e',pos) -> if check_scope_expr e' current_symbols then true else raise (ScopeError (pos))
    | Readln -> true
let rec check_scope_cond c current_symbols = match c with
    | Expr (e,pos) -> if check_scope_expr e current_symbols then true else raise (ScopeError (pos))
    | Not (c',pos) -> if check_scope_cond c' current_symbols then true else raise (ScopeError (pos))
    | And (c1, c2, pos) -> if (check_scope_cond c1 current_symbols) && (check_scope_cond c2 current_symbols) then true else raise (ScopeError (pos))
    | Or (c1, c2, pos) -> if (check_scope_cond c1 current_symbols) && (check_scope_cond c2 current_symbols) then true else raise (ScopeError (pos))

let rec check_scope_instr i current_symbols = match i with
    | SetVar (name, expr, pos) -> if (List.mem name current_symbols) && (check_scope_expr expr current_symbols) then true else raise (ScopeError (pos))
    | Sequence (l,pos) -> if  List.fold_left (fun acc i' -> acc && (check_scope_instr i' current_symbols)) true l then true else raise (ScopeError (pos))
    | If (c,i1,i2, pos) -> if  (check_scope_cond c current_symbols) && (check_scope_instr i1 current_symbols) && (check_scope_instr i2 current_symbols) then true else raise (ScopeError (pos))
    | While (c, i', pos) -> if  (check_scope_cond c current_symbols) && (check_scope_instr i' current_symbols) then true else raise (ScopeError (pos))
    | ProcCall (name, exprl, pos) -> if (List.mem name current_symbols) && (List.fold_left (fun acc e -> acc && (check_scope_expr e current_symbols)) true exprl) then true else raise (ScopeError (pos))
    | Write (exprl, _) ->(List.fold_left (fun acc e -> acc && (check_scope_expr e current_symbols)) true exprl)
    | Writeln (exprl, pos) -> if (List.fold_left (fun acc e -> acc && (check_scope_expr e current_symbols)) true exprl) then true else raise (ScopeError (pos))
    | SetArr (e1, e2, e3, pos) -> if (check_scope_expr e1 current_symbols) && (check_scope_expr e2 current_symbols) && (check_scope_expr e3 current_symbols) then true else raise (ScopeError (pos))


(*Create a list of symbol names from the variables list which is (names, type) list*)
let rec build_symbol_list vars = match vars with
    | [] -> []
    | (names,_)::t -> names @ (build_symbol_list t) (* probably not many variables so complexity of @ is fine for now *)
let rec check_scope_fun l globals = match l with
    | [] -> true
    | (name, def)::t ->
            let symbols = name::(build_symbol_list def.locals) @ (build_symbol_list def.arguments) @ globals in
            (check_scope_instr def.body symbols) && (check_scope_fun t (name::globals))

let check_scope p =
    let builtins = ["readln"; "writeln"; "write"] in
    let global_vars = builtins @ (build_symbol_list p.globalVars) in
    let globals = List.fold_left (fun acc (n,_) -> n::acc) global_vars p.fun_proc in
    try
        (check_scope_fun p.fun_proc globals) && (check_scope_instr p.main globals)
    with ScopeError pos -> Printf.printf "Scope error in %s at line %i: %i\n" pos.Lexing.pos_fname pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1); false


let rec add_to_list v n tail =
    if n = 0 then tail
    else v::(add_to_list v (n-1) tail)

(*Create a list corresponding to the types of arguments of a function ()*)
let rec build_vars_type_list args = match args with
    | [] -> []
    | (names, t)::l -> let n = List.length names in
                        add_to_list t n (build_vars_type_list l)

let rec eval_expr_type e globals locals functions = match e with
    | Int (_,_) -> Integer
    | Bool (_,_) -> Boolean
    | Bop (Plus, e1, e2, pos)
        | Bop (Minus, e1, e2, pos)
        | Bop (Times, e1, e2, pos)
        | Bop (Div, e1, e2, pos) -> let t1 = eval_expr_type e1 globals locals functions and
                                    t2 = eval_expr_type e2 globals locals functions in
                                if (t1 = Integer) && (t2 = Integer) then Integer
                                else raise (TypeError (pos))
    | Bop (Lt, e1, e2, pos)
        | Bop (Le, e1, e2, pos)
        | Bop (Gt, e1, e2, pos)
        | Bop (Ge, e1, e2, pos)
        | Bop (Eq, e1, e2, pos)
        | Bop (Diff, e1, e2, pos) ->let t1 = eval_expr_type e1 globals locals functions and
                                    t2 = eval_expr_type e2 globals locals functions in
                                if (t1 = Integer) && (t2 = Integer) then Boolean
                                else raise (TypeError (pos))
    | Var (s,_) -> (try Hashtbl.find locals s with Not_found -> Hashtbl.find globals s)
    | FunctionCall (name,exprl, pos) -> let arg_types = Hashtbl.find functions name in
                                    let expr_types = List.map (fun e' -> eval_expr_type e' globals locals functions) exprl in
                                    if arg_types = expr_types then (try Hashtbl.find globals name with Not_found -> raise (TypeError (pos)))
                                    else raise (TypeError (pos))

    | GetArr (e1, e2, pos) -> (match eval_expr_type e1 globals locals functions with
                            | Array(t) -> if (eval_expr_type e2 globals locals functions) = Integer then t
                                            else raise (TypeError (pos))

                            | _ -> raise (TypeError (pos)))

    | NewArr (t, e', pos) -> if (eval_expr_type e' globals locals functions) = Integer then Array(t)
                        else raise (TypeError (pos))

    | UMinus (e', pos) -> if (eval_expr_type e' globals locals functions) = Integer then Integer
                        else raise (TypeError (pos))
    | Readln -> Integer (*TODO maybe can be bool too?*)


let rec check_type_cond c globals locals functions = match c with
    | Expr (e,pos) -> if (eval_expr_type e globals locals functions) = Boolean then true else raise (TypeError (pos))
    | Not (c',_) -> check_type_cond c' globals locals functions
    | And (c1, c2, _) -> (check_type_cond c1 globals locals functions) && (check_type_cond c2 globals locals functions)
    | Or (c1, c2, _) -> (check_type_cond c1 globals locals functions) && (check_type_cond c2 globals locals functions)

let rec check_type_instr i globals locals functions = match i with
    | SetVar (name, expr, pos) -> let t = (try Hashtbl.find locals name with Not_found -> Hashtbl.find globals name) in (*Check locals first because if there is a variable with same name we want to use the local variable*)
                                    if (eval_expr_type expr globals locals functions) = t then true
                                    else raise (TypeError (pos))
    | Sequence (l,_) ->  List.fold_left (fun acc i' -> acc && (check_type_instr i' globals locals functions)) true l
    | If (c,i1,i2, _) ->  (check_type_cond c globals locals functions) && (check_type_instr i1 globals locals functions) && (check_type_instr i2 globals locals functions)
    | While (c, i', _) ->  (check_type_cond c globals locals functions) && (check_type_instr i' globals locals functions)
    | ProcCall (name, exprl, pos) -> if Hashtbl.mem globals name then raise (TypeError (pos))
(*if its in the map then its a fct not a procedure*)
                                else (let arg_types = Hashtbl.find functions name in
                                    let expr_types = List.map (fun e' -> eval_expr_type e' globals locals functions) exprl in
                                    arg_types = expr_types)
    | Write (_, _) -> true
    | Writeln (_, _) -> true
    | SetArr (e1, e2, e3, pos) -> (match (eval_expr_type e1 globals locals functions) with
                                | Array(t) -> ((eval_expr_type e2 globals locals functions) = Integer) && ((eval_expr_type e3 globals locals functions) = t)
                                | _ -> raise (TypeError (pos))

                                )
(*Create a map storing name -> type for each variable*)
let rec build_symbol_map vars map = match vars with
    | [] -> ()
    | (names,t)::l ->
            List.iter (function name -> Hashtbl.add map name t) names; build_symbol_map l map

let rec check_type_fun l globals functions = match l with
    | [] -> true
    | (_, def)::l' ->
            let tmp = Hashtbl.create 20 in
            build_symbol_map def.locals tmp;
            build_symbol_map def.arguments tmp;
            (check_type_instr def.body globals tmp functions) && (check_type_fun l' globals functions)

(*Create a map storing name -> arguments types list for each function*)
let rec build_functions_map l map = match l with
    | [] -> ()
    | (name, def)::l' -> Hashtbl.add map name (build_vars_type_list def.arguments); build_functions_map l' map

(*Create a map storing name -> return value type for each function (procedures arent added to this map)*)
let rec build_functions_return_map l map = match l with
    | [] -> ()
    | (name, def)::l' -> (match def.result with
                            | None -> ()
                            | Some t -> Hashtbl.add map name t
                            );
                            build_functions_return_map l' map

let check_types p =
    let functions = Hashtbl.create 20 in
    build_functions_map p.fun_proc functions;
    let globals = Hashtbl.create 20 in
    build_symbol_map p.globalVars globals;
    build_functions_return_map p.fun_proc globals;
    try
        (check_type_fun p.fun_proc globals functions) && (check_type_instr p.main globals globals functions)
    with TypeError pos -> Printf.printf "Type error in %s at line %i: %i\n" pos.Lexing.pos_fname pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1); false
