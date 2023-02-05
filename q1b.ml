(* 

Name1: Ben Lirio

*) 

(* Sample Tree *)

let ex = [(12, 7); (12, 43); (7, 4); (43, 33); (43,77)]


(*
      12
      /\ 
     /  \  
    7   43
   /    /\ 
  /    /  \  
 4    33  77
*)
         
(** [outgoing_nodes t n] returns the list of nodes outgoing from node
 ** [n] in the tree [t]. You may assume the node [n] exists in the
 ** tree [t] .
 ** Eg. outgoing_nodes ex 12 => [7; 43]
*)

let rec outgoing_nodes g (n:int) =
  match g with
  | (a,b)::t when a=n -> b :: outgoing_nodes t n
  | _::t -> outgoing_nodes t n
  | _ -> []


(* 
   The list of nodes of the tree without duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [12; 7; 4; 33; 43; 77]

*)

let rec nodes g =
  match g with


(* 
   Returns the leaves of a tree
   Eg. leaves ex =>  [4; 33; 77]
*)
    
let rec leaves t = failwith "adsfs"

  (*
let () =
  let ex = [(12, 7); (12, 43); (7, 4); (43, 33); (43,77)] in
  List.iter (Printf.printf "%d\n") (outgoing_nodes ex 12)
  *)
let () =
  let ex = [(12, 7); (12, 43); (7, 4); (43, 33); (43,77)] in
  List.iter (Printf.printf "%d\n") (nodes ex)
