# CML, a small interpreted, functional programming language with C-like syntax. 

## Getting started

#### Running the provided binary:
    
    git clone https://github.com/csuraparaju/CML.git
    cd CML
    ./cml <path-to-CML-file>

#### Building from source:
    Prerequisites:
        - OCaml (>= 4.08.0)
        - Dune (>= 2.0.0)

            git clone https://github.com/csuraparaju/CML.git
            cd CML
            dune build
            dune exec


## Example Programs

#### Literal declaration and assignment: 
    let num = 10;

    let str = "Hello World!";

    let arr = [1, 2, 3];

    let point = {(
        let x = 10,
        let y = 20
    )};

    let <identifier> = <expression>

#### Function declaration and call:
##### Can be explicit in return value: 

    let add = fun int (a, b) {
        return a + b;
    };

##### Or implicit:
    
    let add = fun int (a, b) {
        a + b;
    };  

#### Functions are values:
Use the '->' operator to declare a higher order function. "fn" is a reserved keyword
indicating that the function is a hof that returns a function of the specified type. 

    let hof = fun fn->int (x){
        return (fun int (y) {
            return x + y;
        });
    };

##### Calling a function:
    let result = add(5, 10);



#### If-else statement:

    let result = if (10 > 5) {
        10;
    } else {
        5;
    };

#### Looping:
##### Since it is a purely functional language, there are no for or while loops. Instead use recursion. 
    let factorial = fun int (n) {
        if (n == 0) {
            return 1;
        } else {
            return n * factorial(n - 1);
        }
    }; 

## Grammar (BNF-notation)

    program ::= {statement}
    statement ::= let id = expression;
                | if (expression) {statement} else {statement}
                | return expression;
                | expression;
    expression ::= id
                | literal
                | fun type (id) {statement}
                | expression(expression)
                | expression operator expression
                | (expression)
    operator ::= + | - | * | / | < | >  | == | != | ! 
    type ::= int | float | string | bool | void | array | struct 
    literal ::= '0' .. '9' | '"' {char} '"' | true | false | [literal] | {( let id = expression,)}
    char ::= 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | ' ' | '\n' | '\t'

   