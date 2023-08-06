type freturn = 
    FINT 
  | FBOOL
  | FSTRING
  | FARRAY
  | VOID
  | FSTRUCT
  | FFUNCTION of freturn

type literal =
    INT of int
  | BOOL of bool
  | STRING of string
  | ARRAY of literal list
  | STRUCT of (string * literal) list
  | FUNCTION of (freturn * token_type list * token_type list)

and token_type = 
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
  | AND
  | OR
  | COMMA
  | COLON
  | DOT
  | MOD
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LET
  | IF
  | ELSE
  | RETURN

type t = token_type 

let rec string_of_literal lit = 
  match lit with
  | INT i -> "INT(" ^ string_of_int i ^ ")"
  | BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | STRING s -> "STRING(" ^ s ^ ")"
  | ARRAY a -> "ARRAY: [" ^ List.fold_left (fun acc x -> acc ^ " " ^ (string_of_literal x)) "" a ^ " ]"
  | STRUCT s -> "STRUCT: {" ^ List.fold_left (fun acc (k, v) -> acc ^ " " ^ k ^ ": " ^ (string_of_literal v)) "" s ^ " }" 
  | FUNCTION (ret, args, body) -> 
    "FUNCTION (" ^ (string_of_ftype ret) ^ " (" ^ 
    List.fold_left (fun acc x -> acc ^ " " ^ (string_of_token_type x)) "" args ^ ") {" ^
    List.fold_left (fun acc x -> acc ^ " " ^ (string_of_token_type x)) "" body ^ " }"

and string_of_ftype t = 
  match t with
  | FINT -> "INT"
  | FBOOL -> "BOOL"
  | FSTRING -> "STRING"
  | FARRAY -> "ARRAY"
  | VOID -> "VOID"
  | FSTRUCT -> "STRUCT"
  | FFUNCTION f -> "FUNCTION(" ^ (string_of_ftype f) ^ ")"

and string_of_token_type tok = 
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
  | AND -> "AND"
  | OR -> "OR"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | COLON -> "COLON"
  | MOD -> "MOD"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LET -> "LET"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"      
  | DOT -> "DOT"
