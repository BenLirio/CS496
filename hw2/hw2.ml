(*
Define dTree, an algebraic type in OCaml which encodes binary decision trees as depicted in Fig 1. It should have two constructors whose names should be Leaf and Node. Leaves should hold values of type int whereas nodes should hold values of type char. Finally, note that values of type dtree cannot be empty, they are either leaves or
internal nodes.
*)
type dTree =
  | Leaf of int
  | Node of char * dTree * dTree

let tLeft = Node('w',Node('x',Leaf 2, Leaf 5), Leaf 8)
let tRight = Node('w',Node('x',Leaf 2, Leaf 5), Node('y', Leaf 7, Leaf 5))

let max : int -> int -> int =
  fun a b -> if a > b then a else b

let rec dTree_height : dTree -> int =
  fun dt -> match dt with
  | Leaf(_) -> 0
  | Node(_, left, right) ->
      let left_height = dTree_height left in
      let right_height = dTree_height right in
      let max_subtree = max left_height right_height in
      max_subtree + 1

let rec dTree_size : dTree -> int =
  fun dt -> match dt with
  | Leaf(_) -> 1
  | Node(_, left, right) ->
      let left_size = dTree_size left in
      let right_size = dTree_size right in
      left_size + right_size + 1


let rec dTree_is_perfect : dTree -> bool =
  fun dt -> match dt with
  | Leaf _ -> true
  | Node(_, left, right) ->
      let left_is_perfect = dTree_is_perfect left in
      let right_is_perfect = dTree_is_perfect right in

      let left_height = dTree_height left in
      let right_height = dTree_height right in

      (left_height = right_height)
      && left_is_perfect 
      && right_is_perfect

let rec dTree_map : (char -> char) -> (int -> int) -> dTree -> dTree =
  fun f g dt -> match dt with
  | Leaf(v) -> Leaf(g v)
  | Node(c,left,right) ->
      let left' = dTree_map f g left in
      let right' = dTree_map f g right in
      Node(f c, left', right')

let rec list_to_tree : char list -> dTree =
  fun vs ->
    match vs with
    | v::t -> Node(v,list_to_tree t, list_to_tree t)
    | [] -> Leaf 0


let rec dTree_to_string : dTree -> string =
  fun dt -> match dt with
  | Leaf(x) -> Printf.sprintf "%d" x
  | Node(c,left,right) -> Printf.sprintf "(%s, %c, %s)"
      (dTree_to_string left) c (dTree_to_string right)

type pair_encoding = int list * int
let pair_encodings : pair_encoding list =
  [ [0;0;0], 0
  ; [0;0;1], 1
  ; [0;1;0], 1
  ; [0;1;1], 0
  ; [1;0;0], 1
  ; [1;0;1], 0
  ; [1;1;0], 0
  ; [1;1;1], 1
  ]

let rec replace_leaf_at : dTree -> pair_encoding list -> dTree =
  fun dt pes -> match dt with
  | Leaf _ -> (match pes with [_,v] -> Leaf v | _ -> failwith "Bad encoding")
  | Node(c,left,right) ->
      let left_pes = List.filter
      (fun pe -> match pe with
      | h::t,_ when h=0 -> true
      | _ -> false)
      pes
      in
      let left_pes = List.map (fun x -> match x with _::t,v -> t,v | _ -> failwith "Bad encoding") left_pes in
      let right_pes = List.filter
      (fun pe -> match pe with
      | h::t,_ when h=1 -> true
      | _ -> false)
      pes
      in
      let right_pes = List.map (fun x -> match x with _::t,v -> t,v | _ -> failwith "Bad encoding") right_pes in
      Node(c,replace_leaf_at left left_pes, replace_leaf_at right right_pes)

let rec bf_to_dTree : pair_encoding list -> dTree =
  fun pes ->
    if List.length pes = 0 then failwith "Cannot create a tree with no encodings";
    let n = List.length (fst (List.hd pes)) in
    let vars = List.init n (fun i -> Char.chr ((Char.code 'a') + i)) in
    replace_leaf_at (list_to_tree vars) pes


let () =
  Printf.printf "%s\n" (dTree_to_string (bf_to_dTree pair_encodings))
