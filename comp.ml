(** [dir_is_empty dir] is true, if [dir] contains no files except
 * "." and ".."
 *)
let dir_is_empty dir =
  Array.length (Sys.readdir dir) = 0

(** [dir_contents] returns the paths of all regular files that are
 * contained in [dir]. Each file is a path starting with [dir].
  *)
let dir_contents dir =
  let rec loop result = function
    | f::fs when Sys.is_directory f ->
          Sys.readdir f
          |> Array.to_list
          |> List.map (Filename.concat f)
          |> List.append fs
          |> loop result
    | f::fs -> loop (f::result) fs
    | []    -> result
  in
    loop [] [dir]


exception Error of string;;
let error s = raise (Error s);;

let parse_program pars lexer lexbuf =
  try
    let program = pars lexer lexbuf in Parsing.clear_parser(); program
  with Parsing.Parse_error ->
         let ln = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
         let cpos = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol in
           if Lexing.lexeme lexbuf <> "." then
           while lexer lexbuf <> Parser.PERIOD do () done;
           error
           (String.concat ""
              ["Syntax error on line "; string_of_int ln; ": "; string_of_int cpos; "\n"])
    | Lexer.LexError ->
         let ln = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
         let cpos = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol in
           error
           (String.concat ""
              ["Lexical error on line "; string_of_int ln; " : "; Lexing.lexeme lexbuf; ": "; string_of_int cpos; "\n"])

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
    with Error s -> Printf.printf "%s" s

(*
let _ =
    let f = "tests/OK/bigtri.p" in
    let ic = open_in f in
    let lexbuf = Lexing.from_channel ic in
    let p = parse_program (f^" : ") Parser.program Lexer.token lexbuf in Ast.print p
*)
let _ = List.iter (fun f -> Printf.printf "%s: " f; test f; Printf.printf "\n") (dir_contents "tests/OK")
let _ = Printf.printf "\n\n"
let _ = List.iter (fun f -> Printf.printf "%s: " f; test_false f; Printf.printf "\n") (dir_contents "tests/KO")
