open System

(* Assignment 3 *) (* Do not edit this line. *)
(* Student name: Asa Witt, Id Number: 260631573 *) (* Edit this line. *)
(* Question 1 *)
type Cell = { data : int; next : RList}
and RList = Cell option ref
(* For testing.  Uncomment if you want to use them.
let c1 = {data = 1; next = ref None}
let c2 = {data = 2; next = ref (Some c1)}
let c3 = {data = 3; next = ref (Some c2)}
let c5 = {data = 5; next = ref (Some c3)}
*)
(* This converts an RList to an ordinary list. *)
let rec displayList (c : RList) =
  match !c with
    | None -> []
    | Some { data = d; next = l } -> d :: (displayList l)

(* This may be useful.  You don't have to use it.*)
let cellToRList (c:Cell):RList = ref (Some c)

(* Example for testing. *)
let bigger(x:int, y:int) = (x > y)

let rec insert comp (item: int) (list: RList) =
  match !list with
  |Some {data = d; next = l} -> if (comp (item, d)) then list := Some ({data = item; next = ref !list}) else insert comp item l
  |None -> list := Some ({data = item; next = ref None})

(* Question 2*)

type transaction = Withdraw of int | Deposit of int | CheckBalance

let make_protected_account(opening_balance: int,password: string) =
  let balance = ref opening_balance
  fun (x:(string*transaction)) ->
    match x with
    |(pass,Withdraw amt) ->
      if (pass=password) then
        if amt <= !balance then balance := !balance - amt; printf "The new balance is %d" !balance
        else printf "You're broke, can't withdraw %d" amt
      else printf "Wrong Password, sorry bud."
    |(pass,Deposit amt) -> if (pass=password) then balance := !balance + amt; printf "New balance %d" !balance else printf "Wong password, sorry bud"
    |(pass, CheckBalance) -> if (pass=password) then printf "Balance: %d" !balance else printf "Wrong password, sorry bud"

(* Question 3 *)
open System.Collections.Generic;;

type ListTree<'a> = Node of 'a * (ListTree<'a> list)
let rec bfIter f ltr =
    let queue = Queue<ListTree<'a>> ()
    match ltr with
    |Node (var1,list1) ->
      f var1
      for vars in list1 do queue.Enqueue(vars)
    while queue.Count > 0 do
      match queue.Dequeue() with
      |Node(var2,list2) -> f var2; for vars in list2 do queue.Enqueue(vars)
