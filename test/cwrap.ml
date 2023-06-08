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

let toks = test_lexer (Lexer.init "\" hello word\"") ([]) ;;
assert (toks = [Token.LITERAL (Token.STRING "hello word")])

let tok2 = test_lexer (Lexer.init "[1,2,3]") ([]) ;;
assert (tok2 = [Token.LITERAL (Token.ARRAY [Token.INT 1; Token.INT 2; Token.INT 3])])

let tok3 = test_lexer (Lexer.init "[\"he\", \"llo\"]") ([]) ;;
assert (tok3 = [Token.LITERAL (Token.ARRAY [Token.STRING "he"; Token.STRING "llo"])])
(* 
let tok4 = test_lexer (Lexer.init "[[1],[2]]") ([]) ;;
assert (tok4 = [Token.LITERAL (Token.Array ([Token.LITERAL Token.Array ([Token.INT 1]); Token.LITERAL Token.Array ([Token.INT 2])]))]) *)

let tok5 = test_lexer (Lexer.init "[ 1 , 2 , 4 ]") ([]) ;;
assert (tok5 = [Token.LITERAL (Token.ARRAY [Token.INT 1; Token.INT 2; Token.INT 4])])



(* test_lexer: lexer -> token list -> token list *)
(* repeatedly calls next_token on inLex, adding the result to acc, until next_token returns None, at which point it returns the reversed acc *)
let rec test_lexer (inLex) (acc) =
  let (lex', tok) = Lexer.next_token inLex in
  match tok with 
    None -> List.rev acc
  | Some t -> test_lexer (lex') (t::acc)

(* Lexer Tests BASIC *)
let tok1 = test_lexer (Lexer.init "let x = 5;") ([]) ;;
assert (tok1 = [Token.LET; Token.IDENT "x"; Token.ASSIGN; Token.LITERAL (Token.INT 5); Token.SEMICOLON])

(* let sample = 
  "
    let five = 5;
    let ten = 10;

    let add = fun int (x, y) {
      x + y;
    };

    let result = add(five, ten);

    let foo = if (5 < 10) {
      true;
    } else {
      false;
    };
  "

let tokSamp = test_lexer (Lexer.init sample) ([]) ;;
List.iter (fun t -> print_endline (Token.string_of_token_type t)) tokSamp ;; *)

(* let struct_test = "
let person = {(
    let name: "John",
    let age: 30,
    let address: {(
        let street: "Main St",
        let city: "New York",
        let state: "NY",
        let zip: 10001,
    )}
)};

let address = person.address;
let city = address.city;
" *)
(* 
let tokStruct = test_lexer (Lexer.init struct_test) ([]) ;;
List.iter (fun t -> print_endline (Token.string_of_token_type t)) tokStruct ;;  *)



