open Syntax ;;

let parse str = 
  Parser.main Lexer.token 
    (Lexing.from_string str)
(*
let rec read () =
    flush stdout;

  let line = Parser.main Lexer.token (Lexing.from_channel stdin )
  in
  parse(line);
  read()
    ;;
  read()

 *)
