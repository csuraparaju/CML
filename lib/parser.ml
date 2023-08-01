open Ast

type t = {
  lex : Lexer.t;
  current : Token.t option;
}

let init src = 
  let newLex = Lexer.init src in 
  let (lex', currTok) = Lexer.next_token newLex in
  { lex = lex'; current = currTok } 

let advance_parser parser = 
  let (lex', currTok) = Lexer.next_token parser.lex in
  { lex = lex'; current = currTok }

let peek_next parser = 
  match parser.current with
    None -> None
  | Some _ -> 
      let parser' = advance_parser parser in
      parser'.current

let rec parse_expr parser =
  match parser.current with
    (* Parsed ast node and the updated parser *)
    Some (LET) -> (parse_let (parser), advance_parser((advance_parser(advance_parser(advance_parser(parser))))))
  | Some (LITERAL(lit)) -> (Exp(Literal(lit)), (advance_parser parser))
  | Some (IDENT(str)) -> (Exp(Identifier(str)), (advance_parser parser))
  | _ -> raise (Failure "Parse error: invalid expression")
  

and parse_let parser =
  let par = advance_parser parser in
  match par.current with
    Some(IDENT(str)) -> 
      (let par' = advance_parser par in
      match par'.current with
        Some (ASSIGN) -> 
          let (ast_node, _) = parse_expr (advance_parser par') in
          Stmt(Let(str, ast_node))
        | _ -> raise (Failure "Parse error: invalid let statement, \"=\" expected after identifier"))
   | _ -> raise (Failure "Parse error: invalid let statement, identifier expected after \"let\"")

(* Entry point *)
let rec parse parser =  
  match parser.current with 
      None -> []
    | Some _ -> 
        let (ast, parser') = parse_expr parser in
        ast :: (parse parser')


  


