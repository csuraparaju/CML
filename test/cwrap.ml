open Cwrap

(* Token Tests: *)
(* let token_tests = [
    (Token.ILLEGAL, "ILLEGAL");
    (Token.IDENT "foo", "IDENT foo");
    (Token.LITERAL (Token.INT 5), "LITERAL INT: 5");
    (Token.LITERAL (Token.BOOL true), "LITERAL BOOL: true");
    (Token.LITERAL (Token.STRING "hello"), "LITERAL STRING: hello");
    (Token.LITERAL (Token.ARRAY [Token.INT 1; Token.INT 2; Token.INT 3]), "LITERAL ARRAY: [ 1 2 3 ]");
    (Token.LITERAL (Token.HASH [(Token.INT 1, Token.INT 2); (Token.INT 3, Token.INT 4)]), "LITERAL HASH: { (1,2) (3,4) }");
    (Token.ASSIGN, "ASSIGN");
    (Token.PLUS, "PLUS");
    (Token.MINUS, "MINUS");
    (Token.BANG, "BANG");
    (Token.ASTERISK, "ASTERISK");
    (Token.SLASH, "SLASH");
    (Token.LT, "LT");
    (Token.GT, "GT");
    (Token.EQ, "EQ");
    (Token.LTE, "LTE");
    (Token.GTE, "GTE");
    (Token.NOT_EQ, "NOT_EQ");
    (Token.COMMA, "COMMA");
    (Token.SEMICOLON, "SEMICOLON");
    (Token.LPAREN, "LPAREN");
    (Token.RPAREN, "RPAREN");
    (Token.LBRACE, "LBRACE");
    (Token.RBRACE, "RBRACE");
    (Token.FUNCTION "foo", "FUNCTION(foo)");
    (Token.LET, "LET");
    (Token.IF, "IF");
    (Token.ELSE, "ELSE");
    (Token.RETURN, "RETURN");
  ]

  let test () = 
    List.iter (fun (tok, expected) -> 
      let actual = Token.string_of_token_type tok in
      assert(actual = expected)
      (* if actual <> expected then
        Printf.printf "FAIL: expected %s, got %s\n" expected actual
      else
        Printf.printf "PASS: %s\n" expected *)
    ) token_tests ;;
    test () ;; *)


(* Lexer Tests: Basics *)

let rec test_lexer (inLex) (acc) =
  let (lex', tok) = Lexer.next_token inLex in
  match tok with 
    None -> List.rev acc
  | Some t -> test_lexer (lex') (t::acc)

let tok1 = test_lexer (Lexer.init "\"hello\"") ([]) ;;
assert (tok1 = [Token.LITERAL (Token.STRING "hello")])

let tok2 = test_lexer (Lexer.init "[1,2,3]") ([]) ;;
assert (tok2 = [Token.LITERAL (Token.ARRAY [Token.INT 1; Token.INT 2; Token.INT 3])])

let tok3 = test_lexer (Lexer.init "[\"he\", \"llo\"]") ([]) ;;
assert (tok3 = [Token.LITERAL (Token.ARRAY [Token.STRING "he"; Token.STRING "llo"])])

let tok4 = test_lexer (Lexer.init "[[1],[2]]") ([]) ;;
List.iter (fun x -> print_string (Token.string_of_token_type x)) tok4 ;;







(* test_lexer: lexer -> token list -> token list *)
(* repeatedly calls next_token on inLex, adding the result to acc, until next_token returns None, at which point it returns the reversed acc *)
(* let rec test_lexer (inLex) (acc) =
  let (lex', tok) = Lexer.next_token inLex in
  match tok with 
    None -> List.rev acc
  | Some t -> test_lexer (lex') (t::acc)

(* Lexer Tests BASIC *)
let lst1 = test_lexer (Lexer.init "{ } } ) ( ; ,)") ([]) ;;
assert (lst1 = [Token.LBRACE; Token.RBRACE; Token.RBRACE; Token.RPAREN; Token.LPAREN; Token.SEMICOLON; Token.COMMA; Token.RPAREN])

let lst2 = test_lexer (Lexer.init "< > ; - + / * ") ([]) ;;
assert (lst2 = [Token.LT; Token.GT; Token.SEMICOLON; Token.MINUS; Token.PLUS; Token.SLASH; Token.ASTERISK])

(* Lexer Tests: read number *)
let lst3 = test_lexer (Lexer.init "123;456;789") ([]) ;;
assert (lst3 = [Token.INT 123; Token.SEMICOLON; Token.INT 456; Token.SEMICOLON; Token.INT 789])

(* Lexer Tests: read identifier *)
let lst4 = test_lexer (Lexer.init "abc;def;ghi") ([]) ;;
assert (lst4 = [Token.IDENT "abc"; Token.SEMICOLON; Token.IDENT "def"; Token.SEMICOLON; Token.IDENT "ghi"])

let lst5 = test_lexer (Lexer.init "let fun int true false if else return") ([]) ;;
assert (lst5 = [Token.LET; FUNCTION "int"; Token.TRUE; Token.FALSE; Token.IF; Token.ELSE; Token.RETURN])

(* Lexer Tests: assign/eq + bang/neq operators *)
let lst6 = test_lexer (Lexer.init "let five = 5;") ([]) ;;
assert (lst6 = [Token.LET; Token.IDENT "five"; Token.ASSIGN; Token.INT 5; Token.SEMICOLON])

let lst7 = test_lexer (Lexer.init "let ten = 10;") ([]) ;;
assert (lst7 = [Token.LET; Token.IDENT "ten"; Token.ASSIGN; Token.INT 10; Token.SEMICOLON])

let lst8 = test_lexer (Lexer.init "4 != 5") ([]) ;;
assert (lst8 = [Token.INT 4; Token.NOT_EQ; Token.INT 5])

let lst9 = test_lexer (Lexer.init "4 == 5") ([]) ;;
assert (lst9 = [Token.INT 4; Token.EQ; Token.INT 5])

let lst10 = test_lexer (Lexer.init "!x") ([]) ;;
assert (lst10 = [Token.BANG; Token.IDENT "x"])

(* Lexer Test: putting everything together *)
let lst11 = test_lexer (Lexer.init "let add = fun int (x,y) { return x + y; };") ([]) ;;
assert (lst11 = [Token.LET; Token.IDENT "add"; Token.ASSIGN; Token.FUNCTION ("int"); Token.LPAREN; Token.IDENT "x"; Token.COMMA; Token.IDENT "y"; Token.RPAREN; Token.LBRACE; Token.RETURN; Token.IDENT "x"; Token.PLUS; Token.IDENT "y"; Token.SEMICOLON; Token.RBRACE; Token.SEMICOLON;])
 *)

