open Token

type t = {
  source : string;
  position : int;
  ch : char option;
  line : int;
  has_error : bool;
}

let report lex msg =
  let pos = lex.position in
  let line = lex.line in
  let ch = match lex.ch with
    | Some c -> c
    | None -> ' ' in
  let err = Printf.sprintf "Lexing Error: %s at line %d, position %d, char %c" msg line pos ch in
  raise (Failure err)

let init src =
  if String.length src = 0 then
    {source = src; position = 0; ch = None; line = 1; has_error = false}
  else
    {source = src; position = 0; ch = Some (String.get src 0); line = 1; has_error = false}

let rec next_token lex = 
  let lex' = skip_whitespace lex in
  match lex'.ch with
  | None -> (lex', None)
  | Some c -> 
    match c with 
    | '+' -> (advance lex', Some PLUS)
    | '-' -> (advance lex', Some MINUS)
    | '(' -> (advance lex', Some LPAREN)
    | ')' -> (advance lex', Some RPAREN)
    | '}' -> (advance lex', Some RBRACE)
    | ',' -> (advance lex', Some COMMA)
    | ';' -> (advance lex', Some SEMICOLON)
    | '*' -> (advance lex', Some ASTERISK)
    | '/' -> (advance lex', Some SLASH)
    | ':' -> (advance lex', Some COLON)
    (* Figure out how to implement hash structs later *)
    (* | '{' -> if peek_char lex' '(' then read_hash (advance (advance lex')) else (advance lex', Some LBRACE) *)
    | '{' -> (advance lex', Some LBRACE)
    | '<' -> if peek_char lex' '=' then (advance (advance lex'), Some LTE) else (advance lex', Some LT)
    | '>' -> if peek_char lex' '=' then (advance (advance lex'), Some GTE) else (advance lex', Some GT)
    | '=' -> if peek_char lex' '=' then (advance (advance lex'), Some EQ) else (advance lex', Some ASSIGN)
    | '!' -> if peek_char lex' '=' then (advance (advance lex'), Some NOT_EQ) else (advance lex', Some BANG)
    | '"' -> read_string (advance lex')
    | '[' -> read_array (advance lex')
    | ch when is_digit ch -> read_number lex'
    | ch when is_identifier ch -> read_identifier lex'
    | _ -> report lex' "Invalid character"

and advance lex =
  let pos = lex.position + 1 in
  let ch =
    if pos >= String.length lex.source then
      None
    else
      Some (String.get lex.source pos)
  in
  let line = if ch = Some '\n' then lex.line + 1 else lex.line in
  {source = lex.source; position = pos; ch; line; has_error = false} 

and peek_char lex ch =
  let peek = advance lex in
  match peek.ch with
  | Some c when c = ch -> true
  | _ -> false

and skip_whitespace lex =
  let rec skip_whitespace' lex =
    match lex.ch with
    | Some ch when ch = ' ' || ch = '\n' || ch = '\t' -> skip_whitespace' (advance lex)
    | _ -> lex
  in
  skip_whitespace' lex

and is_digit ch =
  match ch with
  | '0'..'9' -> true
  | _ -> false

and is_identifier ch = 
  match ch with
  | 'a'..'z' | 'A'..'Z' | '_' -> true
  | _ -> false

and read_while lexer condtFN = 
  let start_pos = lexer.position in
  let rec read_while' lexer =
    match lexer.ch with
    | Some ch when condtFN ch -> read_while' (advance lexer)
    | _ -> lexer
  in
  let lexer' = read_while' lexer in
  let len = lexer'.position - start_pos in
  let str = String.sub lexer'.source start_pos len in
  (lexer', str)

and read_number lex =
  let (lex', int_str) = read_while lex is_digit in
  (lex', Some (LITERAL (INT (int_of_string int_str))))

and read_string lex =
  let lex' = skip_whitespace lex in
  let (lex'', str) = read_while lex' (fun ch -> ch <> '"') in
  match lex''.ch with
  | Some '"' -> (advance lex'', Some (LITERAL (STRING str)))
  | _ -> report lex'' "Unterminated string"  

and read_array lex = 
  let rec read_array' lex acc = 
    let lex' = skip_whitespace lex in
    match lex'.ch with
    | Some ']' -> (advance lex', Some (LITERAL (ARRAY (List.rev acc))))
    | _ -> 
      let (lex'', token) = next_token lex' in
      match token with
      | Some LITERAL l -> read_array' lex'' (l::acc)
      | Some COMMA -> read_array' lex'' acc
      | Some _ -> report lex'' "Invalid array: expected literal or comma"
      | None -> report lex'' "Unterminated array"
  in
  read_array' (skip_whitespace lex) []


and read_identifier lex = 
  let (lex', str) = read_while lex is_identifier in
  match str with 
    "fun" -> let (lex'', rtype) = read_while (skip_whitespace lex') is_identifier in
              (lex'', Some (FUNCTION (parse_lit lex rtype)))
  | _ -> (lex', Some (lookup_ident str))

and parse_lit lex str = 
  match str with
    "int" -> FINT 
  | "bool" -> FBOOL  
  | "string" -> FSTRING 
  | "array" -> FARRAY 
  | _ -> report lex "Invalid function return type"

and lookup_ident str = 
  match str with
    "let" -> LET
  | "true" -> LITERAL (BOOL (true))
  | "false" -> LITERAL (BOOL (false))
  | "if" -> IF
  | "else" -> ELSE
  | "return" -> RETURN
  | _ -> IDENT str