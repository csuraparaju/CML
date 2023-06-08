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
    | '.' -> (advance lex', Some DOT)
    | '{' -> if peek_char lex' '(' then read_struct (advance (advance lex')) else (advance lex', Some LBRACE)
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

and read_struct lex = 
  let rec read_struct' lex acc = 
      let lex1 = skip_whitespace lex in
      match lex1.ch with
      | Some ')' -> 
          if peek_char lex1 '}' 
            then (advance (advance lex1), Some (LITERAL (STRUCT (List.rev acc)))) 
          else report lex1 "Invalid struct: expected closing brace"
      | _ ->
        let (lex2, token) = next_token lex1 in
        match token with
        | Some LET ->
          let (lex3, token1) = next_token (skip_whitespace lex2) in
          begin
            match token1 with
            | Some IDENT id -> 
              let (lex4, token2) = next_token (skip_whitespace lex3) in
              begin
                match token2 with
                | Some COLON -> 
                  let (lex5, token3) = next_token (skip_whitespace lex4) in
                  begin
                    match token3 with
                    | Some LITERAL l -> read_struct' lex5 ((id, l)::acc)
                    | _ -> report lex5 "Invalid struct: expected literal"
                  end
                | _ -> report lex4 "Invalid struct: expected colon"
              end
            | _ -> report lex3 "Invalid struct: expected identifier"
          end
        | Some COMMA -> read_struct' lex2 acc
        | Some _ -> report lex2 "Invalid struct: expected let or comma"
        | None -> report lex2 "Unterminated struct"
  in
  read_struct' (skip_whitespace lex) [] 


and read_identifier lex = 
  let (lex', str) = read_while lex is_identifier in
  match str with 
    "fun" -> let (lex'', rtype) = read_while (skip_whitespace lex') is_identifier in
              (lex'', Some (FUNCTION (parse_frtype lex rtype)))
  | _ -> (lex', Some (lookup_ident str))

and parse_frtype lex str = 
  match str with
    "int" -> FINT 
  | "bool" -> FBOOL  
  | "str" -> FSTRING 
  | "arr" -> FARRAY 
  | "void" -> VOID
  | "struct" -> FSTRUCT
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