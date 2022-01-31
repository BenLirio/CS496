
type symbol = char
type input = char list

type state = string

type tf = (state * symbol * state) list

type fa = { states: state list; start:state; tf: tf; final: state list}

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let apply_transition_function (t : tf) (s : state) (a : symbol) : state option = None
let accept (m : fa) (s : input) : bool = false
let next (t : tf) (s : state) (a : symbol) : state list = []
let deterministic (m : fa) : bool = false
let valid (m : fa) : bool = false
let reachable (m : fa) : state list = []
let remove_dead_states (m : fa) : fa = m

let m1 = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [
           ("q0",'a',"q1");
           ("q1",'b',"q1");
           ("q1",'c',"q2")];
         final = ["q2"]}

let m2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [
            ("q0",'a',"q1");
            ("q1",'b',"q1");
            ("q1",'c',"q2");
            ("q3",'a',"q4")];
          final= ["q2"]
}
let tf_of_m2 = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]

let () =
  Printf.printf "%s\n" (draw m1)

let draw (m : fa) : string =
  let rec add_states acc = function
    | [] -> acc
    | h :: t -> add_states (acc^"\t\\node[state] ("^h^") {"^h^"};\n") t
  in
  let rec add_edges acc = function
    | (r, a, r') :: t -> add_edges (acc^"\t\t("^r^") edge node{"^(Char.escaped a)^"} ("^r'^")\n") t
    | [] -> acc
  in "\\begin{tikzpicture}\n"^(add_edges ((add_states "" m.states)^"\t\\draw\n") m.tf)^"\\end{tikzpicture}\n"

