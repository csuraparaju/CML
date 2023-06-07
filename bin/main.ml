open Cwrap

let usage_msg = "Run file directly: ./cml [filename]\n Run repl: ./cml"

let rec repl () =
  print_string ">> ";
  flush stdout;
  let line = read_line () in
  let lexbuf = Lexer.init line in
    let rec traverse lex = 
        let (lex', tok) = Lexer.next_token lex in 
        match tok with 
          | None -> ()
          | Some t -> print_endline (Token.string_of_token_type t); traverse lex'
    in traverse lexbuf; repl ()

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let run args = 
  match args with 
    | [] -> print_endline "Welcome to the CML repl! Refer to https://github.com/csuraparaju/CML for docs!"; repl ()
    | [filename] -> 
        let src = read_whole_file filename in
        let lexbuf = Lexer.init src in
        let rec traverse lex = 
          let (lex', tok) = Lexer.next_token lex in 
          match tok with 
            | None -> ()
            | Some t -> print_endline (Token.string_of_token_type t); traverse lex'
        in traverse lexbuf
    | _ -> print_endline usage_msg

let _ = run (List.tl (Array.to_list Sys.argv))
