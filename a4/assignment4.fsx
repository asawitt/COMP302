open System

(* Assignment 4 *)
(* Asa Witt: Id Number: 260631573*)

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
