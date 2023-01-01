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
    | Readln -> Printf.printf "Readln ()\n"

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
    | Readln -> true
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
            (check_scope_instr def.body symbols) && (check_scope_fun t (name::globals))

let check_scope p =
    let builtins = ["readln"; "writeln"; "write"] in
    let global_vars = builtins @ (build_symbol_list p.globalVars) in
    let globals = List.fold_left (fun acc (n,_) -> n::acc) global_vars p.fun_proc in
    (check_scope_fun p.fun_proc globals) && (check_scope_instr p.main globals)


let rec add_to_list v n tail =
    if n = 0 then tail
    else v::(add_to_list v (n-1) tail)

let rec build_vars_type_list args = match args with
    | [] -> []
    | (names, t)::l -> let n = List.length names in
                        add_to_list t n (build_vars_type_list l)

exception TypeError of string
let print_map m = Hashtbl.iter (fun x y -> Printf.printf "%s -> " x; print_type y;Printf.printf "\n" ) m;;
let rec eval_expr_type e globals locals functions = match e with
    | Int _ -> Integer
    | Bool _ -> Boolean
    | Bin (Plus, e1, e2)
        | Bin (Minus, e1, e2)
        | Bin (Times, e1, e2)
        | Bin (Div, e1, e2) -> let t1 = eval_expr_type e1 globals locals functions and
                                    t2 = eval_expr_type e2 globals locals functions in
                                if (t1 = Integer) && (t2 = Integer) then Integer
                                else raise (TypeError "arith")
    | Bin(Lt, e1, e2)
        | Bin(Le, e1, e2)
        | Bin(Gt, e1, e2)
        | Bin(Ge, e1, e2)
        | Bin(Eq, e1, e2)
        | Bin(Diff, e1, e2) ->let t1 = eval_expr_type e1 globals locals functions and
                                    t2 = eval_expr_type e2 globals locals functions in
                                if (t1 = Integer) && (t2 = Integer) then Boolean
                                else raise (TypeError "comp")
    | Var s -> (try Hashtbl.find locals s with Not_found -> Hashtbl.find globals s)
    | FunctionCall (name,exprl) -> name; let arg_types = Hashtbl.find functions name in
                                    let expr_types = List.map (fun e' -> eval_expr_type e' globals locals functions) exprl in
                                    if arg_types = expr_types then (try Hashtbl.find globals name with Not_found -> raise (TypeError "proc instead of func"))
                                    else raise (TypeError (name ^ " args"))
    | GetArr (e1, e2) -> (match eval_expr_type e1 globals locals functions with
                            | Array(t) -> if (eval_expr_type e2 globals locals functions) = Integer then t
                                            else raise (TypeError "getarr index")
                            | _ -> raise (TypeError "getarr"))
    | NewArr (t, e') -> if (eval_expr_type e' globals locals functions) = Integer then Array(t)
                        else raise (TypeError "newarr ind")
    | UMinus (e') -> if (eval_expr_type e' globals locals functions) = Integer then Integer
                        else raise (TypeError "uminus")
    | Readln -> Integer (*TODO maybe can be bool too?*)


let rec check_type_cond c globals locals functions = match c with
    | Expr e -> (eval_expr_type e globals locals functions) = Boolean
    | Not c' -> check_type_cond c' globals locals functions
    | And (c1, c2) -> (check_type_cond c1 globals locals functions) && (check_type_cond c2 globals locals functions)
    | Or (c1, c2) -> (check_type_cond c1 globals locals functions) && (check_type_cond c2 globals locals functions)

let rec check_type_instr i globals locals functions = match i with
    | SetVar (name, expr) -> let t = (try Hashtbl.find locals name with Not_found -> Hashtbl.find globals name) in
                                (eval_expr_type expr globals locals functions) = t
    | Sequence l ->  List.fold_left (fun acc i' -> acc && (check_type_instr i' globals locals functions)) true l
    | If (c,i1,i2) ->  (check_type_cond c globals locals functions) && (check_type_instr i1 globals locals functions) && (check_type_instr i2 globals locals functions)
    | While (c, i') ->  (check_type_cond c globals locals functions) && (check_type_instr i' globals locals functions)
    | ProcCall (name, exprl) -> if Hashtbl.mem globals name then false (*if its in the map then its a fct not a procedure*)
                                else (let arg_types = Hashtbl.find functions name in
                                    let expr_types = List.map (fun e' -> eval_expr_type e' globals locals functions) exprl in
                                    arg_types = expr_types)
    | Write (_) -> true
    | Writeln (_) -> true
    | SetArr (e1, e2, e3) -> (match (eval_expr_type e1 globals locals functions) with
                                | Array(t) -> ((eval_expr_type e2 globals locals functions) = Integer) && ((eval_expr_type e3 globals locals functions) = t)
                                | _ -> false
                                )

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


let rec build_functions_map l map = match l with
    | [] -> ()
    | (name, def)::l' -> Hashtbl.add map name (build_vars_type_list def.arguments); build_functions_map l' map

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
    with TypeError s -> false
