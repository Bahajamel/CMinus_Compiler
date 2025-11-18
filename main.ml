open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.file Lexer.token lexbuf with
  | Lexer.Lexing_error msg ->
      Printf.fprintf stderr "%a: lexical error: %s\n"
        print_position lexbuf msg;
      exit 1
  | Parser.Error ->
      Printf.fprintf stderr "%a: syntax error\n"
        print_position lexbuf;
      exit 1

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0);
    exit 1
  end;
  
  let filename = Sys.argv.(1) in
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  
  let ast = parse_with_error lexbuf in
  close_in inx;
  
  Printf.printf "=== Abstract Syntax Tree ===\n\n";
  List.iter (Ast.print_decl 0) ast;
  Printf.printf "\n=== Parsing successful! ===\n"