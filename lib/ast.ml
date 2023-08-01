
include Token 

type node =
  Exp of expression
| Stmt of statement

and expression =
    Literal of Token.literal
  | Identifier of string
  | Binary of expression * Token.t * expression
  | Unary of Token.t * expression
  | FunCall of expression * expression list (*Function name and arguments*)

and statement =
    Let of string * node
  | Block of node list
  | If of expression * statement * statement
  | Return of expression 

type t = node list

let rec string_of_ast ast = 
  match ast with 
    [] -> ""
  | x::xs -> (string_of_node x) ^ "\n" ^ (string_of_ast xs)

and string_of_node node =
  match node with
    Exp e -> string_of_expression e
  | Stmt s -> string_of_statement s

and string_of_expression e =
  match e with
    Literal t -> Token.string_of_literal t
  | Identifier s -> s
  | Binary (e1, t, e2) -> (string_of_expression e1) ^ " " ^ (Token.string_of_token_type t) ^ " " ^ (string_of_expression e2)
  | Unary (t, e) -> (Token.string_of_token_type t) ^ " " ^ (string_of_expression e)
  | FunCall (e, el) -> (string_of_expression e) ^ "(" ^ (List.fold_left (fun acc e -> acc ^ ", " ^ (string_of_expression e)) "" el) ^ ")"

and string_of_statement s =
  match s with
    Let (s, e) -> "LET " ^ s ^ " = " ^ (string_of_node e)
  | Block sl -> "{\n" ^ (string_of_ast sl) ^ "}"
  | If (e, s1, s2) -> "IF " ^ (string_of_expression e) ^ " THEN " ^ (string_of_statement s1) ^ " ELSE " ^ (string_of_statement s2)
  | Return e -> "RETURN " ^ (string_of_expression e)

let print_ast ast =
  print_string (string_of_ast ast)


  
