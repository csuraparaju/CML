open Cwrap

let rec repl () =
  print_string ">> ";
  flush stdout;
  let line = read_line () in
  let lexbuf = Lexer.init line in
    let rec traverse lex = 
        let (lex', tok) = Lexer.next_token lex in 
        match tok with 
          | None -> ()
          | Some t -> print_endline (Token.string_of_token t); traverse lex'
    in traverse lexbuf; repl ()

let _ = repl () ;;
