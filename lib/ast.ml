include Token

type node = 
    Expression of expression
  | Statement of statement

and expression = 
    Literal of Token.literal
  | Identifier of string
  | PrefixExp of { op: Token.t; right: expression }
  | InfixExp of { left: expression; op: Token.t; right: expression }
  | IfExp of { cond: expression; conseq: statement; alt: statement option }
  | CallExp of { func: Token.literal; arguments: expression list } 

and statement = 
  LetStmt of { name: expression; value: expression }
| ReturnStmt of { value : expression }
| ExprStmt of { exp : expression }
| BlockStmt of { stms: statement list }

type program = {
  statements: statement list
}

let rec str_of_exp exp = 
  match exp with
    Literal lit -> Token.string_of_literal lit 
  | Identifier id -> id
  | PrefixExp { op; right } -> "(" ^ (Token.string_of_token_type op) ^ (str_of_exp right) ^ ")"
  | InfixExp { left; op; right } -> "(" ^ (str_of_exp left) ^ (Token.string_of_token_type op) ^ (str_of_exp right) ^ ")"
  | IfExp { cond; conseq; alt } -> "if " ^ (str_of_exp cond) ^ " " ^ (str_of_stm conseq) ^ (match alt with None -> "" | Some stm -> " else " ^ (str_of_stm stm))
  | CallExp { func; arguments } -> (Token.string_of_literal func) ^ "(" ^ (String.concat ", " (List.map str_of_exp arguments)) ^ ")"

and str_of_stm stm =
  match stm with
    LetStmt { name; value } -> "let " ^ (str_of_exp name) ^ " = " ^ (str_of_exp value) ^ ";"
  | ReturnStmt { value } -> "return " ^ (str_of_exp value) ^ ";"
  | ExprStmt { exp } -> (str_of_exp exp) ^ ";"
  | BlockStmt { stms } -> "{ " ^ (String.concat " " (List.map str_of_stm stms)) ^ " }"

let str_of_program prog =
  String.concat "\n" (List.map str_of_stm prog.statements)



