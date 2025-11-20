{
open Parser
open Lexing

exception Lexing_error of string

(* Buffer pour les strings *)
let string_buffer = Buffer.create 256

}

(* Définitions des caractères *)
let digit = ['0'-'9']
let octal = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let sign = ['+' '-']

(* Constantes entières *)
let decimal = ['1'-'9'] digit*
let octal_num = '0' octal*
let hex_num = ('0' ['x' 'X']) hex+
let integer = sign? (decimal | octal_num | hex_num)

(* Constantes flottantes *)
let exponent = ['e' 'E'] sign? digit+
let float_1 = sign? digit+ '.' digit* exponent?
let float_2 = sign? digit* '.' digit+ exponent?
let float_3 = sign? digit+ exponent
let float_const = float_1 | float_2 | float_3

rule token = parse
  | [' ' '\t' '\r' '\n']+     { token lexbuf }
  | '#' [^'\n']* '\n'         { token lexbuf }  (* Ignorer les lignes # *)
  | "//" [^'\n']* ('\n'|eof)  { token lexbuf }  (* Commentaires C++ *)
  | "/*"                      { comment lexbuf }
  
  (* Opérateurs arithmétiques *)
  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { STAR }
  | '/'  { SLASH }
  | '%'  { PERCENT }
  
  (* Opérateurs de comparaison *)
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LE }
  | ">=" { GE }
  | '<'  { LT }
  | '>'  { GT }
  
  (* Opérateurs logiques *)
  | "&&" { AND }
  | "||" { OR }
  | '!'  { BANG }
  
  (* Opérateurs d'affectation *)
  | '='  { ASSIGN }
  | "+=" { PLUSEQ }
  | "-=" { MINUSEQ }
  | "*=" { STAREQ }
  | "/=" { SLASHEQ }
  | "%=" { PERCENTEQ }
  
  (* Opérateurs d'adresse et déréférencement *)
  | '&'  { AMPERSAND }
  
  (* Délimiteurs *)
  | '('  { LPAR }
  | ')'  { RPAR }
  | '['  { LBRACKET }
  | ']'  { RBRACKET }
  | '{'  { LBRACE }
  | '}'  { RBRACE }
  | ';'  { SEMI }
  | ','  { COMMA }
  
  (* Mots-clés de contrôle *)
  | "return" { RETURN }
  | "if"     { IF }
  | "else"   { ELSE }
  | "while"  { WHILE }
  | "do"     { DO }
  | "for"    { FOR }
  | "sizeof" { SIZEOF }
  
  (* Types de base *)
  | "void"   { VOID }
  | "char"   { CHAR }
  | "int"    { INT }
  | "float"  { FLOAT }
  | "double" { DOUBLE }
  
  (* Attributs de types *)
  | "signed"   { SIGNED }
  | "unsigned" { UNSIGNED }
  | "short"    { SHORT }
  | "long"     { LONG }
  
  (* Constantes flottantes (AVANT les entières pour priorité) *)
  | float_const as f { 
      FLOATCONST(float_of_string f) 
    }
  
  (* Constantes entières *)
  | integer as i { 
      (* Gestion des différentes bases *)
      let value = 
        if String.length i >= 2 && (i.[0] = '0' && (i.[1] = 'x' || i.[1] = 'X')) then
          (* Hexadécimal *)
          int_of_string i
        else if String.length i >= 1 && i.[0] = '0' && String.length i > 1 then
          (* Octal *)
          int_of_string ("0o" ^ String.sub i 1 (String.length i - 1))
        else
          (* Décimal *)
          int_of_string i
      in
      INTCONST(value)
    }
  
  (* Constantes de chaînes *)
  | '"' { 
      Buffer.clear string_buffer;
      string_literal lexbuf;
      STRINGCONST(Buffer.contents string_buffer)
    }
  
  (* Identifiants (APRÈS les mots-clés) *)
  | alpha alphanum* as id { ID(id) }
  
  | eof { EOF }
  | _ as c { 
      raise (Lexing_error (Printf.sprintf "Unknown character: '%c' at line %d" 
        c lexbuf.lex_curr_p.pos_lnum)) 
    }

and comment = parse
  | "*/" { token lexbuf }
  | eof  { raise (Lexing_error "Unterminated comment") }
  | _    { comment lexbuf }

and string_literal = parse
  | '"' { 
      (* Concaténation de chaînes consécutives *)
      match token lexbuf with
      | STRINGCONST s -> 
          Buffer.add_string string_buffer s;
          string_literal lexbuf
      | tok -> 
          (* Remettre le token dans le flux (impossible directement, 
             donc on termine ici) *)
          ()
    }
  | "\\\"" { Buffer.add_char string_buffer '"'; string_literal lexbuf }
  | "\\\\" { Buffer.add_char string_buffer '\\'; string_literal lexbuf }
  | "\\n"  { Buffer.add_char string_buffer '\n'; string_literal lexbuf }
  | "\\t"  { Buffer.add_char string_buffer '\t'; string_literal lexbuf }
  | "\\r"  { Buffer.add_char string_buffer '\r'; string_literal lexbuf }
  | '\n'   { raise (Lexing_error "Newline in string literal") }
  | eof    { raise (Lexing_error "Unterminated string literal") }
  | _ as c { Buffer.add_char string_buffer c; string_literal lexbuf }