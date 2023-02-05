
(* ******************************************** *)
(* Name: Ben Lirio *)
(* Date: Feb 9 *)
(* Project Name: Basic Functions on Finite Automata *)
(* ******************************************** *)

(* ******************************************** *)
(* Types *)
(* ******************************************** *)
type symbol = char
type input = char list
type state = string
type transition = (state * symbol * state)
type tf = transition list
type fa = 
  { states: state list
  ; start:  state
  ; tf:     tf
  ; final:  state list
  }

(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)
let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rm_doubles : 'a list -> 'a list = fun lst ->
  let open List in
  fold_left (fun acc x -> if mem x acc then acc else x::acc) [] lst

(* Get the alphabet of a FA *)
let sigma : fa -> symbol list = fun fa ->
  rm_doubles @@ List.fold_left (fun acc (_,sym,_) -> sym :: acc) [] fa.tf

(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

let rec apply_transition_function : tf -> state -> symbol -> state option =
  fun tf st sym ->
    match tf with
    | (src,sym',dst)::t when sym'=sym && st=src-> Some dst
    | _::t -> apply_transition_function t st sym
    | _ -> None
    
let accept : fa -> input -> bool =
  fun fa input -> 
    let open List in
    let next_state cur sym =
      match cur with
      | Some st -> apply_transition_function fa.tf st sym
      | None -> None in
    let final_state = fold_left next_state (Some fa.start) input in
    match final_state with
    | Some st -> mem st fa.final
    | None -> false

let rec next : tf -> state -> symbol -> state list =
  fun tf st sym ->
    match tf with
    | (src, sym', dst)::t when src=st && sym'=sym -> dst :: next t st sym
    | _::t -> next t st sym
    | [] -> []

let deterministic : fa -> bool = fun fa ->
  let open List in
  let check_state : bool -> state -> bool = fun conj st ->
    let check_sym : bool -> symbol -> bool = fun conj' sym ->
      conj' && length @@ next fa.tf st sym < 2 in
    conj && fold_left check_sym true @@ sigma fa in
  fold_left check_state true fa.states

let valid : fa -> bool = fun fa ->
  let open List in
  let check_valid_state : bool -> state -> bool = fun acc st ->
    acc && mem st fa.states in
  deterministic fa && fold_left check_valid_state true (fa.start::fa.final)

let mark_next : fa -> state list -> state list = fun fa sts ->
  let open List in
  let sgm = sigma fa in
  let over_states : state list -> state -> state list = fun sts' st ->
    let over_symbols : state list -> symbol -> state list = fun sts'' sym ->
      sts'' @ next fa.tf st sym in
    sts'@ fold_left over_symbols [] sgm in
  rm_doubles @@ sts@fold_left over_states [] sts

let reachable : fa -> state list = fun fa ->
  let open List in
  let rec until_fixed : state list -> state list = fun sts ->
    let sts' = mark_next fa sts in
    if length sts = length sts' then sts' else until_fixed sts' in
  until_fixed [fa.start]

let remove_dead_states : fa -> fa = fun fa ->
  let open List in
  let sts' = reachable fa in
  let filter_tf : transition -> bool = fun t ->
    match t with
    | (src,_,dst) when (mem src sts') && (mem dst sts') -> true
    | _ -> false in
  let tf' = filter filter_tf fa.tf in
  let filter_state : state -> bool = fun st -> mem st sts' in
  let final' = filter filter_state fa.final in
  {fa with states=sts';tf=tf';final=final'}

