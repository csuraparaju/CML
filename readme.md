# CML, a small interpreted, functional programming language with C-like syntax. 

## Grammar (BNF-notation)
    ```
    program ::= {statement}
    statement ::= let id = expression;
                | if expression {statement} else {statement}
                | return expression;
                | expression;
    expression ::= id
                | literal
                | fun type (id) {statement}
                | expression(expression)
                | expression operator expression
                | (expression)
    operator ::= + | - | * | / | < | >  | == | != | ! 
    type ::= int | float | string | bool | void
    literal ::= '0' .. '9' | '"' {char} '"' | true | false
    char ::= 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | ' ' | '\n' | '\t'
    ```
   

## Example
    ```
    let fib = fun int (n) {
        if (n < 2) {
            return n;
        } else {
            return fib(n - 1) + fib(n - 2);
        }
    };

    let main = fun void () {
        let n = 10;
        let fib_n = fib(n);
    }
    ```