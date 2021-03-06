open System
(* Assignment 2 *)
(* Student name: Asa Witt, Id Number: 260631573 *)
(* Question 1 *)
let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)
(* val deriv : f:(float -> float) * dx:float -> x:float -> float *)
let rec newton(f,guess:float,tol:float,dx:float) =
  let next_term prev_guess = prev_guess - ((f(prev_guess))/(deriv (f,dx) prev_guess))
  let abs x:float = if (x<0.) then -x else x
  if abs (f guess) < tol then guess else newton (f,next_term guess,tol,dx)

(* val newton : f:(float -> float) * guess:float * tol:float * dx:float -> float *)

(* Question 2 *)

type term = float * int
type poly = term list
exception EmptyList
(* Multiply a term by a polynomial. *)
let rec mtp(t:term,p:poly):poly =
  match p with
  |x::xs -> match x with |(a,b) -> match t with | (c,d) -> (a*c, b+d)::mtp (t,xs)
  |[] -> []

let term1:term = (0., 2)
let poly1:poly = [(4., 2);(3., 1);(1., 0)]
let multiplied = mtp (term1, poly1)
(* val mtp : t:term * p:poly -> poly *)
(* Add a term to a polynomial. *)
let rec atp(t:term,p:poly):poly =
  match p with
  |x::xs -> match x with |(a,b) -> match t with |(c,d) -> if (b=d) then (a+c,b)::xs else if (b<d) then (c,d)::p else x::atp(t,xs)
  |[] -> [t]

let rec addpolys(p1:poly,p2:poly):poly =
  match p1 with
  | x::xs -> addpolys (xs,atp (x,p2))
  |[]-> p2

(* Multiply two polynomials.  All the remarks above apply here too. Raise an
exception if one of the polynomials is the empty list. *)
let multpolys(p1:poly,p2:poly) =
  if List.isEmpty p1 || List.isEmpty p2 then raise EmptyList
  let san_poly: poly = []
  let rec sanitize p:poly =
    match p with
    |x::xs ->  match x with |(a,b) -> if (a=0.) then sanitize xs else atp (x,sanitize xs)
    |[] -> []
  let rec recmultpolys (p3:poly,p4:poly) =
    match p3 with
    | x::xs -> mtp(x,p4)::recmultpolys (xs,p4)
    |[]-> []
  sanitize (List.concat (recmultpolys(p1,p2)))
(* val multpolys : p1:poly * p2:poly -> poly *)
let exp(b:float, e:int) =
  let rec helper(b:float, e:int, a: float) =
    if (b = 0.0) then 0.0
    elif (e = 0) then a
    elif (e % 2 = 1) then helper(b,e-1, b*a)
    else helper(b*b,e/2,a)
  helper(b,e,1.0)
(* Here is how you evaluate a term. *)
let evalterm (v:float) ((c,e):term) = if (e=0) then c else c * exp(v,e)

let rec evalpoly(p:poly,v:float):float =
  match p with
  |x::xs -> evalterm v x + evalpoly (xs,v)
  |[] -> 0.
(* val evalpoly : p:poly * v:float -> float *)
let rec diff (p:poly):poly =
    match p with
      | [] -> []
      | x::xs -> match x with | (a,b) -> if (b<=0) then (0.,0)::diff xs else (a*float b,b-1)::diff xs
(*  val diff : p:poly -> poly *)
(* Question 3 *)
type Country = string
type Chart = Set<Country*Country>
type Colour = Set<Country>
type Colouring = Set<Colour>
(* This is how you tell that two countries are neghbours.  It requires a chart.*)
let areNeighbours ct1 ct2 chart =
  Set.contains (ct1,ct2) chart || Set.contains (ct2,ct1) chart
(* val areNeighbours :
  ct1:'a -> ct2:'a -> chart:Set<'a * 'a> -> bool when 'a : comparison
  *)
(* The colour col can be extended by the country ct when they are no neighbours
according to chart.*)
let canBeExtBy col ct chart =
  not (Set.exists (fun x-> areNeighbours x ct chart) col)
(*
   val canBeExtBy :
  col:Set<'a> -> ct:'a -> chart:Set<'a * 'a> -> bool when 'a : comparison
*)
let rec extColouring (chart: Chart) (colours : Colouring) (country : Country) =
  if not (Set.isEmpty colours) then
    if (canBeExtBy (Set.minElement colours) country chart) then
      ((Set.remove (Set.minElement colours) colours).Add((Set.minElement colours).Add(country)))
    else Set.union (extColouring chart (Set.remove (Set.minElement colours) colours) country) (Set.empty.Add(Set.minElement colours))
  else colours.Add ((Set.empty:Colour).Add country)
(*
val extColouring :
  chart:Chart -> colours:Colouring -> country:Country -> Set<Set<Country>>
*)
let countriesInChart (chart: Chart) =
  Set.fold (fun (acc:Set<Country>) ((a,b): (Country*Country)) ->
  if (not (Set.contains a acc) && not (Set.contains b acc)) then
    acc.Add(a).Add(b)
  else if not (Set.contains a acc) then
    acc.Add a
  else if not (Set.contains b acc) then
    acc.Add b
  else acc) Set.empty chart
(* val countriesInChart : chart:Chart -> Set<Country> *)
(* Here is the final function.  It is also most conveniently done with Set.fold *)
let colourTheCountries (chart: Chart)  =
  Set.fold (fun (acc:Set<Set<Country>>) (x:Country) -> (extColouring chart acc x)) Set.empty (countriesInChart chart)
(* val colourTheCountries : chart:Chart -> Colouring *)
(* Question 4 *)
type Exptree =
  | Const of int
  | Var of string
  | Add of Exptree * Exptree
  | Mul of Exptree * Exptree
type Bindings = (string * int) list
(* The bindings are stored in a list rather than a BST for simplicity.  The
list is sorted by name, which is a string. *)
let rec lookup(name:string, env: Bindings) =
  match env with
  |x::xs -> match x with | (a,b) -> if (name=a) then Some(b) else lookup (name, xs)
  |[] -> None
(* val lookup : name:string * env:Bindings -> int option *)
(* Insert a new binding.  If the name is already there then the new binding should
be put in front of it so that the lookup finds the latest binding.  *)
let rec insert(name:string, value: int, b: Bindings) =
  match b with
  |x::xs -> if (name <= fst x) then (name,value)::b else insert(name,value,xs)
  |[] -> [(name,value)]
(* val insert : name:string * value:int * b:Bindings -> (string * int) list*)
let rec eval(exp : Exptree, env:Bindings) =
  match exp with
  |Add (a,b) ->
    match eval(a,env) with
    |Some(a) ->
      match eval(b,env) with
      |Some(b) -> Some(a + b)
      |None -> None
    |None -> None
  |Mul (a,b) ->
    match eval(a,env) with
    |Some(a) ->
      match eval(b,env) with
      |Some(b) -> Some(a * b)
      |None -> None
    |None -> None
  |Var a ->
    match (lookup (a,env)) with
    |Some(b) -> Some(b)
    |None -> None
  |Const a -> Some(a)
(* val eval : exp:Exptree * env:Bindings -> int option  *)
