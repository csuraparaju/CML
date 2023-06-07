type literal =
    INT of int
  | BOOL of bool
  | STRING of string
  | ARRAY of literal list
  (* Figure out how to make this work *)
  (* | HASH of (literal * literal) list *)

type freturn = 
    FINT 
  | FBOOL
  | FSTRING
  | FARRAY
  (* | FHASH *)

type token_type = 
    ILLEGAL
  | IDENT of string
  | LITERAL of literal
  | ASSIGN
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH
  | LT
  | GT
  | EQ
  | LTE
  | GTE
  | NOT_EQ
  | COMMA
  | COLON
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | FUNCTION of freturn 
  | LET
  | IF
  | ELSE
  | RETURN

type t = token_type 

let rec string_of_literal lit = 
  match lit with
  | INT i -> "INT: " ^ string_of_int i
  | BOOL b -> "BOOL: " ^ string_of_bool b
  | STRING s -> "STRING: " ^ s
  | ARRAY a -> "ARRAY: [" ^ List.fold_left (fun acc x -> acc ^ " " ^ (string_of_literal x)) "" a ^ " ]"
  (* | HASH h -> "HASH: {" ^ List.fold_left (fun acc (k, v) -> acc ^ " " ^ "(" ^ (string_of_literal k) ^ "," ^ (string_of_literal v) ^ ")") "" h ^ " }" *)

let string_of_ftype t = 
  match t with
  | FINT -> "INT"
  | FBOOL -> "BOOL"
  | FSTRING -> "STRING"
  | FARRAY -> "ARRAY"
  (* | HASH -> "HASH" *)

let string_of_token_type tok = 
  match tok with
  | ILLEGAL -> "ILLEGAL"
  | IDENT s -> "IDENT (" ^ s ^ ")"
  | LITERAL l -> "LITERAL (" ^ (string_of_literal l) ^ ")"
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | BANG -> "BANG"
  | ASTERISK -> "ASTERISK"
  | SLASH -> "SLASH"
  | LT -> "LT"
  | GT -> "GT"
  | EQ -> "EQ"
  | LTE -> "LTE"
  | GTE -> "GTE"
  | NOT_EQ -> "NOT_EQ"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | COLON -> "COLON"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | FUNCTION s -> "FUNCTION(" ^ (string_of_ftype s) ^ ")"
  | LET -> "LET"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"      
