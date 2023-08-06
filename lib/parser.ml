include Lexer
include Ast
include Token 

type t = {
  lex : Lexer.t;
  currTok : Token.t option;
  nextTok : Token.t option;
}

let prefix_prec = 7
let lowest = 1 

let precedence tok = 
  match tok with
    EQ | NOT_EQ -> 2
  | LT | GT -> 3
  | PLUS | MINUS -> 4
  | SLASH | ASTERISK -> 5
  | LPAREN | LBRACE -> 6
  | _ -> lowest

let init lex = 
  let (lex', tok) = Lexer.next_token lex in
  let (_, ntok) = Lexer.next_token lex' in
  {
    lex = lex';
    currTok = tok;
    nextTok = ntok;
  }


let report parser msg = 
  let pos = parser.lex.position in
  let line = parser.lex.line  in
  let ch = match parser.lex.ch with
  | Some c -> c
  | None -> ' ' in
  let err = Printf.sprintf "Parse Error: %s at line %d, position %d, char %c" msg line pos ch in
  raise (Failure err)


let advance parser = 
  let (lex', tok) = Lexer.next_token parser.lex in
  let (_, ntok) = Lexer.next_token lex' in
  {
    lex = lex';
    currTok = tok;
    nextTok = ntok;
}


let rec parse_stmt parser = 
  match parser.currTok with 
    Some LET -> parse_let_stmt (advance parser)
  | Some RETURN -> parse_return_stmt (advance parser)
  | _ -> parse_expr_stmt parser


and parse_let_stmt parser = 
  (match parser.currTok with 
    Some IDENT ident -> 
    (match parser.nextTok with
      Some ASSIGN ->
        let (expr, parser') = parse_expr (advance (advance parser)) in
        (match parser'.currTok with
          Some SEMICOLON -> 
            let node = Ast.LetStmt { name = Ast.Identifier(ident); value = expr } in
            (node, advance parser')
        | _ -> report parser' "expected semicolon (;)")
    | _ -> report parser "expected assignment (=)")
  | _ -> report parser "expected identifier")

and parse_return_stmt parser = 
  let (expr, parser') = parse_expr parser in
  (match parser'.currTok with
    Some SEMICOLON -> 
      let node = Ast.ReturnStmt { value = expr } in
      (node, advance parser')
  | _ -> report parser' "expected semicolon (;)")

and parse_expr_stmt parser =
  let (expr, parser') = parse_expr parser in
  (match parser'.currTok with
    Some SEMICOLON -> 
      let node = Ast.ExprStmt { exp = expr } in
      (node, advance parser')
  | _ -> report parser' "expected semicolon (;)")

and parse_expr parser = 
    (match parser.currTok with
      Some LITERAL lit -> (Ast.Literal lit, advance parser)
    | _ -> report parser "Not yet implemented") 

let rec parse parser acc =
  match parser.currTok with
   None -> { statements = (List.rev acc) }
  | _ -> 
    let (stmt, parser') = parse_stmt parser in
    parse parser' (stmt :: acc) 
  
