{
open Parser
open Lexing

(* Exception personnalisée pour signaler une erreur lexicale *)
exception Lexing_error of string

(* Buffer pour les chaînes *)
let string_buffer = Buffer.create 256
}

(* Définitions lexicales *)
let digit = ['0'-'9']
let octal = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']

let sign = ['+' '-']

(* Formes des nombres entiers en C *)
let decimal = ['1'-'9'] digit*
let octal_num = '0' octal*
let hex_num = ('0' ['x' 'X']) hex+
(* Expression d'un entier optionnellement signé *)
let integer = sign? (decimal | octal_num | hex_num)

(* ==== Définition des littéraux flottants ==== *)
let exponent = ['e' 'E'] sign? digit+
let float_1 = sign? digit+ '.' digit* exponent?
let float_2 = sign? digit* '.' digit+ exponent?
let float_3 = sign? digit+ exponent
let float_const = float_1 | float_2 | float_3

rule token = parse
  | [' ' '\t' '\r' '\n']+     { token lexbuf }

  | "//" [^'\n']* ('\n'|eof)  { token lexbuf }
  | '#' [^'\n']* '\n'         { token lexbuf }
  | "/*"                      { comment lexbuf }

  (* opérateurs *)
  | "++" { INC }
  | "--" { DEC }

  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LE }
  | ">=" { GE }
  | "<"  { LT }
  | ">"  { GT }

  | "&&" { AND }
  | "||" { OR }
  | "!"  { BANG }

  | "+=" { PLUSEQ }
  | "-=" { MINUSEQ }
  | "*=" { STAREQ }
  | "/=" { SLASHEQ }
  | "%=" { PERCENTEQ }
  | "="  { ASSIGN }

  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { STAR }
  | "/"  { SLASH }
  | "%"  { PERCENT }
  | "&"  { AMPERSAND }

  (* délimiteurs *)
  | "("  { LPAR }
  | ")"  { RPAR }
  | "["  { LBRACKET }
  | "]"  { RBRACKET }
  | "{"  { LBRACE }
  | "}"  { RBRACE }
  | ";"  { SEMI }
  | ","  { COMMA }
  

  (* mots-clés *)
  | "return" { RETURN }
  | "if"     { IF }
  | "else"   { ELSE }
  | "while"  { WHILE }
  | "do"     { DO }
  | "for"    { FOR }
  | "sizeof" { SIZEOF }

  | "void"   { VOID }
  | "char"   { CHAR }
  | "int"    { INT }
  | "float"  { FLOAT }
  | "double" { DOUBLE }

  | "signed"   { SIGNED }
  | "unsigned" { UNSIGNED }
  | "short"    { SHORT }
  | "long"     { LONG }

  (* nombres flottants *)
  | float_const as f { FLOATCONST(float_of_string f) }

  (* entiers *)
  | integer as i {
      let value =
      (* On détecte le format de l'entier : hex, octal ou décimal *)
        if String.length i >= 2 && i.[0] = '0' &&
           (i.[1] = 'x' || i.[1] = 'X') then
          int_of_string i  (* hexadécimal *)
        else if String.length i >= 1 && i.[0] = '0' && String.length i > 1 then
          int_of_string ("0o" ^ String.sub i 1 (String.length i - 1))
          (* format octal → converti en notation OCaml 0oXXX *)
        else  
          int_of_string i             (* décimal *)
      in
      INTCONST(value)
    }

  (* chaînes *)
  | '"' {
      Buffer.clear string_buffer;
      string_literal lexbuf;
      STRINGCONST(Buffer.contents string_buffer)
    }

  (* identifiants *)
  | alpha alphanum* as id { ID(id) }

  | eof { EOF }

  | _ as c {
      raise (Lexing_error
        (Printf.sprintf "Unknown character '%c' line %d"
           c lexbuf.lex_curr_p.pos_lnum))
    }
(*  Gestion des commentaires C multi-lignes  *)
and comment = parse
  | "*/" { token lexbuf }
  | eof  { raise (Lexing_error "Unterminated comment") }
  | _    { comment lexbuf }


(* Gestion des chaînes de caractères *)
and string_literal = parse
  | '"' { () }  (* fin de la chaîne *)

(* Séquences d'échappement *)
  | "\\\"" { Buffer.add_char string_buffer '"'; string_literal lexbuf }
  | "\\\\" { Buffer.add_char string_buffer '\\'; string_literal lexbuf }
  | "\\n"  { Buffer.add_char string_buffer '\n'; string_literal lexbuf }
  | "\\t"  { Buffer.add_char string_buffer '\t'; string_literal lexbuf }
  | "\\r"  { Buffer.add_char string_buffer '\r'; string_literal lexbuf }

(* Les chaînes ne doivent pas contenir de retour à la ligne sinon erreur lexical *)
  | '\n'  { raise (Lexing_error "Newline in string literal") }
  | eof   { raise (Lexing_error "Unterminated string literal") }

  | _ as c {
      Buffer.add_char string_buffer c;
      string_literal lexbuf
    }
