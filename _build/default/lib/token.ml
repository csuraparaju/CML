type t = 
    ILLEGAL
  | IDENT of string
  | INT of int
  | ASSIGN
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH
  | LT
  | GT
  | EQ
  | NOT_EQ
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | FUNCTION of string
  | LET
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN 

let string_of_token tok =
  match tok with
    ILLEGAL -> "ILLEGAL"
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | INT i -> "INT(" ^ string_of_int i ^ ")"
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | BANG -> "BANG"
  | ASTERISK -> "ASTERISK"
  | SLASH -> "SLASH"
  | LT -> "LT"
  | GT -> "GT"
  | EQ -> "EQ"
  | NOT_EQ -> "NOT_EQ"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | FUNCTION (x) -> "FUNCTION(" ^ x ^ ")"
  | LET -> "LET"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"
