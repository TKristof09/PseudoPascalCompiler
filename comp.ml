let list_files dir = List.map (Filename.concat dir) (Array.to_list (Sys.readdir dir))



exception ParseError of string;;

let parse_program pars lexer lexbuf =
    try
        pars lexer lexbuf
    with
        | Parsing.Parse_error ->
            let ln = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
            let cpos = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol in
                if Lexing.lexeme lexbuf <> "." then
                    while lexer lexbuf <> Parser.PERIOD do () done;
                    raise (ParseError ("Syntax error on line " ^ (string_of_int ln) ^ ": " ^ (string_of_int cpos) ^ "\n"))
        | Lexer.LexError ->
            let ln = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
            let cpos = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol in
            raise (ParseError ("Lex error on line " ^ (string_of_int ln) ^ ": " ^ (Lexing.lexeme lexbuf) ^ (string_of_int cpos) ^ "\n"))

let set_file_name lexbuf f =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
        Lexing.pos_fname = f;
    }

let test f =
    let ic = open_in f in
    let lexbuf = Lexing.from_channel ic in
    set_file_name lexbuf f;
    let p = parse_program Parser.program Lexer.token lexbuf in if ((Ast.check_scope p) && (Ast.check_types p)) then Printf.printf "--------- OK"
    else assert false

let test_false f =
    let ic = open_in f in
    let lexbuf = Lexing.from_channel ic in
    set_file_name lexbuf f;
    try
        let p = parse_program Parser.program Lexer.token lexbuf in assert (not ((Ast.check_scope p) && (Ast.check_types p)))
    with ParseError s -> Printf.printf "%s" s

(*
let _ =
    let f = "tests/OK/bigtri.p" in
    let ic = open_in f in
    let lexbuf = Lexing.from_channel ic in
    let p = parse_program (f^" : ") Parser.program Lexer.token lexbuf in Ast.print p
*)
let _ = List.iter (fun f -> Printf.printf "%s: " f; test f; Printf.printf "\n") (list_files "tests/OK")
let _ = Printf.printf "\n\n"
let _ = List.iter (fun f -> Printf.printf "%s: " f; test_false f; Printf.printf "\n") (list_files "tests/KO")
