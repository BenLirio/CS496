open Util.Assert
open Fa

let tf_of_a = 
  [ "q0",'a',"q1"
  ; "q1",'b',"q1"
  ; "q1",'c',"q2"
  ]

let a_0 =
  { states =
    [ "q0"
    ; "q1"
    ; "q2"
    ]
  ; start = "q0"
  ; tf = 
    [ "q0",'a',"q1"
    ; "q1",'b',"q1"
    ; "q1",'c',"q2"
    ]
  ; final = 
    [ "q2"
    ]
  }

let a_1 = 
  { states =
    ["q0"
    ; "q1"
    ; "q2"
    ; "q3"
    ; "q4"
    ]
  ; start = "q0"
  ; tf = 
    [ "q0",'a',"q1"
    ; "q1",'b',"q1"
    ; "q1",'c',"q2"
    ; "q3",'a',"q4"
    ]
  ; final = 
    [ "q2"
    ]
  }

let nd_0 =
  { states =
    [ "q0"
    ; "q1"
    ; "q2"
    ; "q3"
    ]
  ; start = "q0"
  ; tf =
    [ "q0", 'a', "q1"
    ; "q0", 'a', "q3"
    ; "q1", 'b', "q1"
    ; "q1", 'c', "q2"
    ]
  ; final =
    [ "q2"
    ]
  }

let nd_1 =
  { states =
    [ "q0"
    ; "q1"
    ; "q2"
    ; "q3"
    ; "q4"
    ]
  ; start = "q0"
  ; tf =
    [ "q0", 'a', "q1"
    ; "q0", 'a', "q2"
    ; "q1", 'a', "q1"
    ; "q1", 'b', "q3"
    ; "q2", 'c', "q4"
    ; "q4", 'c', "q4"
    ]
  ; final =
    [ "q3"
    ; "q4"
    ]
  }

let nd_2 =
  { states =
    [ "q0"
    ; "q1"
    ; "q2"
    ; "q3"
    ; "q4"
    ]
  ; start = "q0"
  ; tf =
    [ "q0", 'a', "q1"
    ; "q0", 'a', "q2"
    ; "q1", 'a', "q1"
    ; "q1", 'b', "q3"
    ; "q1", 'c', "q4"
    ; "q2", 'd', "q4"
    ; "q4", 'c', "q4"
    ]
  ; final =
    [ "q3"
    ; "q4"
    ]
  }

let transition_tests = [
  ("transition_function_0", assert_eqf 
    (fun () -> apply_transition_function tf_of_a "q0" 'a')
    (Some "q1")
  );
  ("transition_function_1", assert_eqf 
    (fun () -> apply_transition_function tf_of_a "q0" 'b')
    (None)
  );
  ("transition_function_1", assert_eqf 
    (fun () -> apply_transition_function tf_of_a "q1" 'c')
    (Some "q2")
  );
  ("transition_function_2", assert_eqf
    (fun () -> apply_transition_function tf_of_a "not_a_state" 'c')
    (None)
  );
]

let accept_tests = [
  ("accept_0", assert_eqf
    (fun () -> accept a_0 @@ input_of_string "abbc")
    (true)
  );
  ("accept_1", assert_eqf
    (fun () -> accept a_0 @@ input_of_string "ac")
    (true)
  );
  ("accept_2", assert_eqf
    (fun () -> accept a_0 @@ input_of_string "a")
    (false)
  );
  ("accept_3", assert_eqf
    (fun () -> accept a_0 @@ input_of_string "bb")
    (false)
  );
]

let next_tf = [("q1",'a',"q4"); ("q0",'b',"q1"); ("q1", 'a',"q5"); ("q0",'b',"q2"); ("q0",'b',"q3")]
let next_tests = [
  ("next_0", assert_eqf
    (fun () -> next a_0.tf "q1" 'c')
    (["q2"])
  );
  ("next_1", assert_eqf
    (fun () -> next a_0.tf "q1" 'b')
    (["q1"])
  );
  ("next_2", assert_eqf
    (fun () -> next a_0.tf "q1" 'a')
    ([])
  );
  ("next_3", assert_eqf
    (fun () -> next next_tf "q1" 'a')
    (["q4";"q5"])
  );
  ("next_4", assert_eqf
    (fun () -> next next_tf "q0" 'b')
    (["q1";"q2";"q3"])
  );
  ("next_5", assert_eqf
    (fun () -> next next_tf "q2" 'a')
    ([])
  );
  ("next_6", assert_eqf
    (fun () -> next nd_2.tf "q0"  'a')
    (["q1"; "q2"])
  );
  ("next_7", assert_eqf
    (fun () -> next nd_2.tf "q4"  'c')
    (["q4"])
  );
  ("next_8", assert_eqf
    (fun () -> next nd_2.tf "q2"  'a')
    ([])
  );
]

let deterministic_tests = [
  ("deterministic_0", assert_eqf
    (fun () -> deterministic a_0)
    (true)
  );
  ("deterministic_1", assert_eqf
    (fun () -> deterministic a_1)
    (true)
  );
  ("deterministic_2", assert_eqf
    (fun () -> deterministic nd_0)
    (false)
  );
  ("deterministic_2", assert_eqf
    (fun () -> deterministic nd_1)
    (false)
  );
]


let inval_0 =
  { states =
    [ "q0"
    ; "q1"
    ; "q2"
    ]
  ; start = "not_a_state"
  ; tf = []
  ; final = []
  }

let inval_1 =
  { states =
    [ "q0"
    ; "q1"
    ; "q2"
    ]
  ; start = "q0"
  ; tf = []
  ; final = ["not_a_state"]
  }

let valid_tests = [
  ("valid_0", assert_eqf
    (fun () -> valid inval_0)
    (false)
  );
  ("valid_1", assert_eqf
    (fun () -> valid inval_1)
    (false)
  );
  ("valid_3", assert_eqf
    (fun () -> valid a_0)
    (true)
  );
  ("valid_4", assert_eqf
    (fun () -> valid a_1)
    (true)
  );
]

let dead_0 =
  { states =
    [ "q0"
    ; "q1"
    ; "q2"
    ]
  ; start = "q0"
  ; tf = 
    [ "q0",'a',"q1"
    ; "q1",'b',"q1"
    ]
  ; final = 
    [ "q1"
    ; "q2"
    ]
  }

let dead_1 =
  { states =
    [ "q0"
    ; "q1"
    ; "q2"
    ]
  ; start = "q0"
  ; tf = 
    [ "q0",'a',"q1"
    ; "q1",'b',"q1"
    ; "q2",'a',"q1"
    ; "q2",'a',"q0"
    ]
  ; final = 
    [ "q1"
    ; "q2"
    ]
  }



(* Coped from https://riptutorial.com/ocaml/example/9123/generic-algorithms *)
let str_sort lst =
  let case_insensitive_compare a b =
    String.compare (String.lowercase a) (String.lowercase b)
  in
    List.sort case_insensitive_compare lst
  
let mark_next_tests = [
  ("next_of_list_0", assert_eqf
    (fun () -> str_sort (mark_next dead_0 ["q0"; "q1"]))
    (str_sort ["q0";"q1"])
  );
  ("next_of_list_1", assert_eqf
    (fun () -> str_sort (mark_next a_1 ["q4"; "q1"]))
    (str_sort ["q1";"q4";"q2"])
  );
  ("next_of_list_1", assert_eqf
    (fun () -> str_sort (mark_next a_1 ["q3"; "q0"]))
    (str_sort ["q0";"q3";"q1";"q4"])
  );
]

let reachable_tests = [
  ("reachable_0", assert_eqf
    (fun () -> str_sort (reachable a_0))
    (str_sort (["q0"; "q1"; "q2"]))
  );
  ("reachable_1", assert_eqf
    (fun () -> reachable dead_0)
    (["q0"; "q1"])
  );
  ("reachable_2", assert_eqf
    (fun () -> reachable dead_1)
    (["q0"; "q1"])
  );
]

let dead_0_fix =
  { states =
    [ "q0"
    ; "q1"
    ]
  ; start = "q0"
  ; tf = 
    [ "q0",'a',"q1"
    ; "q1",'b',"q1"
    ]
  ; final = 
    [ "q1"
    ]
  }

let dead_1_fix =
  { states =
    [ "q0"
    ; "q1"
    ]
  ; start = "q0"
  ; tf = 
    [ "q0",'a',"q1"
    ; "q1",'b',"q1"
    ]
  ; final = 
    [ "q1"
    ]
  }

let remove_dead_states_tests = [
  ("remove_dead_states_0", assert_eqf
    (fun () -> remove_dead_states dead_0)
    (dead_0_fix)
  );
  ("remove_dead_states_1", assert_eqf
    (fun () -> remove_dead_states dead_1)
    (dead_1_fix)
  );
]

let provided_tests : suite = [
  Test("Problem 1 (Transition function)", transition_tests);
  Test("Problem 2 (Accept)", accept_tests);
  Test("Problem 3 (Next)", next_tests);
  Test("Problem 4 (Deterministic)", deterministic_tests);
  Test("Problem 5 (Valid)", valid_tests);
  Test("Problem 6 helper (Next of List)", mark_next_tests);
  Test("Problem 6 (Reachable)", reachable_tests);
  Test("Problem 7 (remove Dead States)", remove_dead_states_tests);
] 
