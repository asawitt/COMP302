module hw5

(* Assignment 5 *)
(* Student name: Asa Witt, Id Number: 260631573 *)
let intToFloat (x: int):float = 2.
let inToInt (x:int):int = 2

let floatToInt (x:float) = 2
let intToint (x:int):int = 2



(* This is the type definition for the expressions that we will produce as a result
of parsing the input strings. *)
let S = fun x -> (fun y -> (fun z -> (x z) (y z)))
let U = fun z -> 8
type exptree = Var of char | Expr of char * exptree * exptree

(* We only allow lower-case one-character variable names*)
let charSet = ['a' .. 'z']

(* Here is an example input string.  Blank spaces are not allowed. *)
let example = "(a+(b+(c*d)+e)*f)*g"
let example2 = "a*b"
(* This just tests if the character is one of the allowed variable names.*)
let isin (x: char) L = List.exists (fun y -> x = y) L
//
(* This is the top-level function.  It reads an input string and outputs the parse tree.
It is not recursive at top level but the main guts of it consists of three
mutually-recursive functions called expr, term and primary.  There is also a function
called getsym which returns the next character from the input string.  getsym is imperative
and uses the mutable local variables sym and cursor.  Please do not change the definition of
getsym or the declarations of sym and cursor.  No doubt all this can be done more slickly,
but I am trying to be as simple-minded as possible. *)

let parse (inputexp: string): exptree =
  let sym = ref inputexp.[0]
  let cursor = ref 0
  let getsym () =
    cursor := !cursor + 1
    sym := inputexp.[!cursor]
//(a+(b+(c*d)+e)*f)*g
  let rec expr (): exptree =
    let result = term()
    if not (!cursor = inputexp.Length - 1) then
      if !sym = '+' then
        getsym()
        Expr ('+', result, expr())
      elif !sym = ')' then
        result
      else
        printfn "sym is : %c. index: %d " !sym !cursor
        failwith "In expression"
    else result
  and term (): exptree =
    let result = primary()
    if (not (!cursor = inputexp.Length - 1 || !sym = '+')) then
      if !sym = '*' then
        getsym()
        Expr ('*', result, term())
      elif !sym = ')' then
        result
      else
        printfn "sym is : %c." !sym
        failwith "In term"
    else result
  and primary (): exptree =  //I did this for you.
    if !sym = '(' then
      getsym()
      let result = expr ()
      if not (!sym = ')') then
        printf("Mismatch at %c") !sym
        failwith "Mismatched parens"
      else
        if (!cursor = inputexp.Length - 1)
        then
          result
        else
          getsym()
          result
    elif (isin !sym charSet) then
      if (!cursor = inputexp.Length - 1)
      then
        (Var !sym)
      else
        let result = Var !sym in (getsym(); result)
    else
      printfn "sym is : %c." !sym
      failwith "In primary"
  expr() //This is part of the top-level function parse.

let p = parse example
(* Now for Question 2 *)

(*  Do not change this.  tempstore will be a global variable.  Remember to reset this between
tests of the program. *)

let mutable tempstore = 0

let codegen (e: exptree) =
  let rec helper (e: exptree, tag: char) =
    if tag = '=' then
      match e with
      |Var x -> printf "LOAD %c\n" x
      |Expr (char,left,right) ->
        helper (left, '=')
        helper (right, char)
    elif tag = '+' || tag = '*' then
      match e with
      |Var x ->
        if tag = '+' then
          printf "ADD %c\n" x
        elif tag = '*' then
          printf "MUL %c\n" x
      |Expr (char,left,right) ->
        tempstore <- tempstore + 1
        printf "STORE %d\n" tempstore
        helper (left, '=')
        helper (right, char)
        if (tag = '+') then
          printf "ADD %d\n" tempstore
        elif (tag = '*') then
          printf "MUL %d\n" tempstore
        else
          printf "Wrong tag: %c\n" tag
          failwith "wrong tag"
        tempstore <- tempstore - 1
    else
      printf "Wrong tag: %c\n" tag
      failwith "wrong tag"
    (* Code for helper goes here. *)
  helper(e,'=') //This is part of the-level function codegen.  Do not change it.

codegen p, '='
