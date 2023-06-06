open Token

type t = {
  input : string;
  position : int;
  ch : char option;
}

let init input = 
  if String.length (input) = 0 then 
    {input; position = 0; ch = None}
  else
    {input; position = 0; ch = Some (String.get input 0)}

let rec next_token lexer = 
    let lex = skip_whitespace lexer in
    match lex.ch with
    | None -> (lex, None)
    | Some ch -> 
      match ch with
      | '+' -> (advance lex, Some PLUS)
      | '-' -> (advance lex, Some MINUS)
      | '(' -> (advance lex, Some LPAREN)
      | ')' -> (advance lex, Some RPAREN)
      | '{' -> (advance lex, Some LBRACE)
      | '}' -> (advance lex, Some RBRACE)
      | ',' -> (advance lex, Some COMMA)
      | ';' -> (advance lex, Some SEMICOLON)
      | '*' -> (advance lex, Some ASTERISK)
      | '/' -> (advance lex, Some SLASH)
      | '<' -> (advance lex, Some LT)
      | '>' -> (advance lex, Some GT)
      | '=' -> if peek_char (advance lex) '=' then (advance (advance lex), Some EQ) else (advance lex, Some ASSIGN)
      | '!' -> if peek_char (advance lex) '=' then (advance (advance lex), Some NOT_EQ) else (advance lex, Some BANG)
      | ch when is_identifier ch -> read_identifier lex
      | ch when is_digit ch -> read_number lex
      | _ -> raise (Failure "Lexing Error: Invalid character")
    
and skip_whitespace lexer = 
  let rec skip_whitespace' lex = 
    match lex.ch with
    | Some ch when ch = ' ' || ch = '\n' || ch = '\t' -> 
      skip_whitespace' (advance lex)
    | _ -> lex
  in 
  skip_whitespace' lexer

and advance lexer =
  let pos = lexer.position + 1 in
  let ch = 
    if pos >= String.length lexer.input then
      None
    else
      Some (String.get lexer.input pos)
  in
  {input = lexer.input; position = pos; ch}

and peek_char lexer ch =
  match lexer.ch with
  | Some c when c = ch -> true
  | _ -> false

and is_identifier ch =
  match ch with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

and is_digit ch =
  match ch with
  | '0' .. '9' -> true
  | _ -> false
 
and read_while lexer condtFN = 
  let start_pos = lexer.position in
  let rec read_while' lexer =
    match lexer.ch with
    | Some ch when condtFN ch -> read_while' (advance lexer)
    | _ -> lexer
  in
  let seekedLex = read_while' lexer in
  let endPos = seekedLex.position in
  let ident = String.sub lexer.input start_pos (endPos - start_pos) in
  (seekedLex, ident)

and read_identifier lexer =
  let (lex, ident) = read_while lexer is_identifier in
    match ident with
     "fun" -> let (lex', ident') = read_while (skip_whitespace lex) is_identifier in
              (lex', Some (FUNCTION ident'))
    | _ -> (lex, Some (lookup_ident ident))

and lookup_ident str = 
  match str with
    "let" -> LET
  | "true" -> TRUE
  | "false" -> FALSE
  | "if" -> IF
  | "else" -> ELSE
  | "return" -> RETURN
  | _ -> IDENT str

and read_number lexer =
  let (lex, number) = read_while lexer is_digit in
  (lex, Some (INT (int_of_string number)))
  
let pp fmt lexer =
  Format.fprintf fmt "Lexer: %s" lexer.input 

let show lexer =
  Format.asprintf "%a" pp lexer 

let rec token_list inLex acc = 
  let (lex, token) = next_token inLex in
  match token with
  | None -> List.rev acc
  | Some t -> token_list lex (t :: acc)