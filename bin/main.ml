open Cwrap

let usage_msg = "Run file directly: ./cml [filename]\n Run repl: ./cml"

(* let rec traverse lex = 
    let (lex', tok) = Lexer.next_token lex in 
      match tok with 
        | None -> ()
        | Some t -> print_endline (Token.string_of_token_type t); traverse lex' *)


let print_prog parser = 
  let prog = Parser.parse parser [] in
  Ast.str_of_program prog |> print_endline
  

let rec repl () =
  print_string ">> ";
  flush stdout;
  let line = read_line () in
  let par = Parser.init (Lexer.init line) in
  print_prog par; repl ()


let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let run args = 
  match args with 
    | [] -> print_endline "\nWelcome to the CML repl! Refer to https://github.com/csuraparaju/CML for docs!"; repl ()
    | [filename] -> 
        let src = read_whole_file filename in
        let par = Parser.init (Lexer.init src) in
          print_prog par
    | _ -> print_endline usage_msg

let _ = run (List.tl (Array.to_list Sys.argv))
