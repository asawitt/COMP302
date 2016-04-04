open System

(* Assignment 4 *) (* Do not edit this line. *)
(* Asa Witt: Id Number: 260631573*) (* Edit this line. *)



let rec apply_list l =
  match l with
  | [] -> (fun x -> x)
  | f::fs -> (fun x -> (apply_list(fs))(f x))



type typExp =
  | TypInt
  | TypVar of char
  | Arrow of typExp * typExp
  | Lst of typExp

type substitution = (char * typExp) list

(* check if a variable occurs in a term *)
let rec occurCheck (v: char) (tau: typExp) : bool =
  match tau with
  |Arrow (x, y) -> occurCheck v x || occurCheck v y
  |Lst x -> if (occurCheck v x) then true else false
  |TypVar x -> if (v=x) then true else false
  |TypInt -> false
(* Testing purposes
let t1:typExp = Lst (Lst (Lst (Lst (Arrow (Lst (Arrow (TypInt, Lst(TypVar 'c'))), TypVar 'c')))));
let char1:char = 'b'
let char2:char = 'a'

let shouldBeFalse = occurCheck char1 t1
let shouldBeTrue = occurCheck char2 t1
*)
(* substitute typExp tau1 for all occurrences of type variable v in typExp tau2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
  if (occurCheck v tau2) then
    match tau2 with
    |Arrow (x,y) -> Arrow (substitute tau1 v x, substitute tau1 v y)
    |Lst x -> Lst(substitute tau1 v x)
    |TypVar x -> if (x=v) then tau1 else TypVar x
    |TypInt -> TypInt
  else tau2

let applySubst (sigma: substitution) (tau: typExp) : typExp =
    let rec helper (sigma2: substitution) (tau2: typExp): typExp =
      match sigma2 with
      |x::xs -> substitute (snd x) (fst x) (helper xs tau2)
      |[] -> tau2
    helper sigma tau
    //List.fold (fun acc x -> substitute (snd x) (fst x) acc) tau sigma would work,
    //but would fail on cases such as [(a'->int');(c'->a')] (or vice versa)
    //If this was supposed to be a 1-line thing, please consider the Fold
    //example (above) instead!

(* This is a one-line program *)

(*
let te3 = Arrow(TypInt, Arrow(TypVar 'a', Lst (TypVar 'b')));;
let te4 = Arrow(TypVar 'ay', Arrow (TypInt, Lst (TypInt)));;
let te5 = TypVar 'a';
let sub = ('a', TypInt)
let sub2:substitution = [('b', TypInt)]
let subCheck:typExp = substitute (snd sub) (fst sub) te4
let subs:substitution = sub::sub2
let subbed1 = applySubst subs te3
let subbed2 = applySubst subs te4
let te6 = Arrow(TypInt, TypInt)
let occurs:bool = occurCheck 'v' (Arrow(TypInt, TypInt))
*)
let rec unify (tau1: typExp) (tau2:typExp) : substitution =
  match tau1 with
  |Arrow (x,y) ->
    match tau2 with
    |Arrow(x2,y2) ->
        let subs1:substitution = (unify x x2);
        (unify (applySubst subs1 y) (applySubst subs1 y2)) @ subs1
    |TypVar x2 ->
      if (occurCheck x2 tau1) then failwith "Failed occurs check" else [x2, Arrow (x,y)]
    |Lst x -> failwith "Clash in principal type constructor"
    |TypInt -> failwith "Not unifiable"
  |Lst x ->
    match tau2 with
    |Lst x2 -> unify x x2
    |TypVar x2 ->
      if (occurCheck x2 tau1) then failwith "Failed occurs check" else [x2, tau1]
    |Arrow (x2,y2) -> failwith "Clash in principal type constructor"
    |TypInt -> failwith "Not unifiable"
  |TypVar x ->
    match tau2 with
    |Arrow(x2,y2) ->
      if (occurCheck x tau2) then failwith "Failed occurs check" else [x, Arrow (x2,y2)]
    |Lst x2 ->
        if (occurCheck x tau2) then failwith "Failed occurs check" else [x, tau2]
    |TypInt -> [x, TypInt]
    |TypVar x2 -> if (x=x2) then [] else [x2, TypVar x]
  |TypInt ->
    match tau2 with
    |TypInt -> []
    |TypVar x -> [x, TypInt]
    |_ -> failwith "Not unifiable"

(* Use the following signals if unification is not possible:

 failwith "Clash in principal type constructor"
 failwith "Failed occurs check"
 failwith "Not unifiable"

*)

(*
let tau2 = Arrow(TypVar 'a', TypVar 'a')
let tau1 = Arrow(TypVar 'b', TypVar 'c')
let testUnify = unify tau1 tau2

let te1 = Arrow(TypInt, Arrow(TypVar 'c', TypVar 'a'));;

//val te1 : typExp = Arrow (TypInt,Arrow (TypVar 'c',TypVar 'a'))

let te2 = Arrow(TypVar 'a',Arrow (TypVar 'b',TypVar 'c'));;

//val te2 : typExp = Arrow (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'))


//val it : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]
let result = unify te1 te2;;

//val result : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]

applySubst result te1;;
//val it : typExp = Arrow (TypInt,Arrow (TypInt,TypInt))
applySubst result te2;;
//val it : typExp = Arrow (TypInt,Arrow (TypInt,TypInt))









let te4 = Arrow(TypInt, Arrow(TypVar 'c', TypVar 'a'));;

//val te4 : typExp = Arrow(TypInt,Arrow (TypVar 'c',TypVar 'a'))

let te3 = Arrow(TypVar 'a',Arrow (TypVar 'b',TypVar 'c'));;

//val te3 : typExp = Arrow (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'))


//val it : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]
let result2 = unify te3 te4;;

//val result : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]

applySubst result2 te3;;
//val it : typExp = Prod (TypInt,Arrow (TypInt,TypInt))
applySubst result2 te4;;
//val it : typExp = Prod (TypInt,Arrow (TypInt,TypInt))
*)
let te1 = Arrow(TypVar 'a', TypVar 'a');;
let te2 = Arrow(TypVar 'b', TypVar 'c');;
let test1 = unify te1 te2
let te3 = Arrow(TypVar 'a', TypVar 'b');;
let te4 = Arrow(TypVar 'c', TypVar 'c');;
let test2 = unify te3 te4
let te5 = Arrow(TypVar 'a', TypVar 'b');;
let te6 = Arrow(TypVar 'c', TypVar 'd');;
let test3 = unify te5 te6
