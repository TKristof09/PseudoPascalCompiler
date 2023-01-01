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

let parse_program error_prefix parseur lexer lexbuf =
  let orig = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos in
  let orig = if orig > 0 then orig+1 else orig in
  try
    let program = parseur lexer lexbuf in Parsing.clear_parser(); program
  with Parsing.Parse_error ->
         let pos1 = Lexing.lexeme_start lexbuf - orig in
         let pos2 = Lexing.lexeme_end lexbuf - orig in
           if Lexing.lexeme lexbuf <> "." then
           while lexer lexbuf <> Parser.PERIOD do () done;
           error
           (String.concat ""
              [ error_prefix; string_of_int pos1; "-"; string_of_int pos2;
                ":\nSyntax error";])
    | Lexer.ParsingError ->
         let pos1 = Lexing.lexeme_start lexbuf - orig in
         let pos2 = Lexing.lexeme_end lexbuf - orig in
           error
           (String.concat ""
              [ error_prefix; string_of_int pos1; "-"; string_of_int pos2;
                ":\nLexical error "; Lexing.lexeme lexbuf])


let test f =
    let ic = open_in f in
    let lexbuf = Lexing.from_channel ic in
    let p = parse_program (f^" : ") Parser.program Lexer.token lexbuf in assert (Ast.check_scope p)

let test_false f =
    let ic = open_in f in
    let lexbuf = Lexing.from_channel ic in
    try
        let p = parse_program (f^" : ") Parser.program Lexer.token lexbuf in assert (not (Ast.check_scope p))
    with Error s -> Printf.printf "%s" s

let _ =
    let f = "tests/KO/typeif.p" in
    let ic = open_in f in
    let lexbuf = Lexing.from_channel ic in
    let p = parse_program (f^" : ") Parser.program Lexer.token lexbuf in Ast.print p
let _ = List.iter (fun f -> Printf.printf "%s\n" f; test f) (dir_contents "tests/OK")
let _ = List.iter (fun f -> Printf.printf "%s\n" f; test_false f) (dir_contents "tests/KO")
