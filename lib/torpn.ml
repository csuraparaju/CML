open Lexer 
open Token 

let precedence = function
  | PLUS | MINUS -> 1
  | ASTERISK | SLASH | MOD -> 2
  | LT | GT | LTE | GTE -> 3
  | EQ | NOT_EQ -> 4
  | _ -> 0

let rec to_rpn lex = 
  let rec to_rpn' lex stack output = 
    match lex with 
    | [] -> output @ stack
    | (t, _)::xs -> 
      match t with 
      | LITERAL _ -> to_rpn' xs stack (output @ [t])
      | LPAREN -> to_rpn' xs (LPAREN::stack) output
      | RPAREN -> 
        let rec pop_until_paren stack output = 
          match stack with 
          | [] -> raise (Failure "Mismatched parentheses")
          | LPAREN::xs -> output, xs
          | x::xs -> pop_until_paren xs (output @ [x])
        in
        let output', stack' = pop_until_paren stack [] in
        to_rpn' xs stack' (output @ output')
      | _ -> 
        let rec pop_until_lower stack output = 
          match stack with 
          | [] -> output, stack
          | x::xs -> 
            if precedence x >= precedence t then 
              pop_until_lower xs (output @ [x])
            else 
              output, stack
        in
        let output', stack' = pop_until_lower stack [] in
        to_rpn' xs (t::stack') (output @ output')
  in
  to_rpn' lex [] []

