(* Assignment 1 *)
(* Question 1 *)
(* val sumlist : l:float list -> float *)
let rec sumlist l =
  match l with
  | [] -> 0.0
  | x::xs -> float x + float (sumlist xs)

(* val squarelist : l:float list -> float list *)
let rec squarelist l =
  let square x = x*x
  match l with
  | [] -> l
  | x::xs -> float (square x) :: squarelist xs

let rec length l =
  match l with
  | [] -> 0.0
  | x::xs -> 1. + float (length xs)

(* val mean : l:float list -> float *)
let mean l =
  match l with
  | [] -> 0.0
  | x::xs -> sumlist l / length l

(* val mean_diffs : l:float list -> float list *)
let mean_diffs l =
  let avg = mean l
  let rec change list =
    match list with
    | [] -> list
    | x::xs -> (x-avg)::(change xs)
  change l

(* val variance : l:float list -> float *)
let variance l =
  match l with
  |x::xs -> float (sumlist (squarelist (mean_diffs l))) / double (length l)
  | [] -> 0.

(* Question 2 *)

(* val memberof : 'a * 'a list -> bool when 'a : equality *)
let rec memberof l =
  match l with
  | (a, []) -> false
  | (a,x::xs)-> if a <> x then memberof (a,xs) else true

(* val remove : 'a * 'a list -> 'a list when 'a : equality *)
let rec remove l  =
  match l with
  |(a, []) -> []
  |(a, x::xs) -> if a=x then xs else x::remove (a,xs)

(* Question 3 *)

(* val isolate : l:'a list -> 'a list when 'a : equality *)
let rec isolate l =
  match l with
  | [] -> l
  | x::xs -> if memberof (x, xs) then isolate xs else x :: isolate xs
(* Question 4 *)
(* val common : 'a list * 'a list -> 'a list when 'a : equality *)
let rec common l =
  match l with
  |(x::xs, []) -> []
  |([],x::xs) -> []
  |([],[]) -> []
  | (x::xs, z::zs) ->
    if memberof (x,z::zs)
      then isolate (x::common (xs, z::zs))
    else if memberof(z, x::xs)
      then isolate (z::common (x::xs, zs))
    else isolate (common (xs,zs))
(* Question 5 *)

(* val split : l:'a list -> 'a list * 'a list *)
let rec split l =
  let rec splitl len tuple =
    match tuple with
    |(x::xs, b) -> if length b >= len then (x::xs, b) else splitl len (xs,x::b)
    |([],a) -> tuple
  splitl ((length l)/2.) (l,[])

(* val merge : 'a list * 'a list -> 'a list when 'a : comparison *)
let rec merge l =
  match l with
  |([],a) -> a
  |(a,[]) -> a
  |(x::xs, y::ys) ->
  if x<y then x::merge ((xs), ((y::ys)))
  else y::merge ((x::xs), ((ys)))
(* val mergesort : l:'a list -> 'a list when 'a : comparison *)
let rec mergesort l =
  match l with
  |[] -> []
  |x::xs ->
    match xs with
    | [] -> [x]
    | x::xs ->
      match split l with
      | (a,b) -> merge(mergesort a, mergesort b)
