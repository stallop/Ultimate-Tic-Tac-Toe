open OUnit2
open Board

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)


(********************************************************************
   End helper functions.
 ********************************************************************)



let miniblank: Micro.t = 
  {game = (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
   won = Blank;}

let machinewon : Micro.t = 
  {game = (Machine, Machine, Machine, Blank, Blank, Blank, Blank, Blank, Blank);
   won = Machine}

let humanwon : Micro.t = 
  {game = (Human, Human, Human, Blank, Blank, Blank, Blank, Blank, Blank);
   won = Human}

let miniboard1 : Micro.t = 
  {game = (Human, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
   won = Blank}

let macroblank = {Macro.game =
  ({Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank});
 next_pos = -1; won = Blank}

 let humwon = {Macro.game =
  ({Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Human},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Human},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Human},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank});
 next_pos = -1; won = Blank}

 let machwon = {Macro.game =
  ({Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Machine},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Machine},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Machine},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank});
 next_pos = -1; won = Blank}

let macroboard = {Macro.game =
  ({Micro.game =
     (Human, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank});
 next_pos = 1; won = Blank}

 let macroboard1 = {Macro.game =
  ({Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Human, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank});
 next_pos = 5; won = Blank}

 let macroboard2 = {Macro.game =
  ({Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Human},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Human, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank});
 next_pos = 5; won = Blank}

 let macroboard3 = {Macro.game =
  ({Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Machine, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Blank});
 next_pos = 5; won = Blank}

 let macroboard4 = {Macro.game =
  ({Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Machine},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Machine},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Machine},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Machine},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Machine, Blank, Blank, Blank, Blank);
    won = Machine},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Machine},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Machine},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Machine},
   {Micro.game =
     (Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank);
    won = Machine});
 next_pos = -1; won = Blank}

let board_tests =
  [
    "Blank microboard" >:: (fun _ -> assert_equal miniblank Micro.blank);

    "Checking whether Machine won" >:: 
            (fun _ -> assert_equal Human (Micro.checkwin humanwon.game Human));
    "Checking whether Human won" >:: 
      (fun _ -> assert_equal Machine (Micro.checkwin machinewon.game Machine));
    "Checking whether No one has won" >:: 
            (fun _ -> assert_equal Blank (Micro.checkwin miniblank.game Blank));

    "MiniUpdate" >:: 
            (fun _ -> assert_equal miniboard1 (Micro.update miniblank Human 1));

    "MiniUpdate Gone Wrong" >:: 
            (fun _ -> assert_raises InvalidLoc 
                      (fun () -> Micro.update miniboard1 Human 1));
    
    "Blank Macroboard" >:: (fun _ -> assert_equal macroblank Macro.blank);

    "Checking whether Machine won" >:: 
            (fun _ -> assert_equal Human (Macro.checkwin humwon.game Human));
    "Checking whether Human won" >:: 
          (fun _ -> assert_equal Machine (Macro.checkwin machwon.game Machine));
    "Checking whether No one has won" >:: 
            (fun _ -> assert_equal Blank (Macro.checkwin macroblank.game Blank));

    "MacroUpdate" >:: 
        (fun _ -> assert_equal macroboard (Macro.update macroblank Human 1 1));

    "MacroUpdate Gone Wrong" >:: 
            (fun _ -> assert_raises InvalidLoc 
                      (fun () -> Macro.update macroboard Human (-1) 1));

    "MiniUpdate Gone Wrong" >:: 
            (fun _ -> assert_raises InvalidLoc 
                      (fun () -> Micro.update miniboard1 Human 1));
    
    "Blank Macroboard" >:: (fun _ -> assert_equal macroblank Macro.blank);

    "Checking whether Machine won" >:: 
            (fun _ -> assert_equal Human (Macro.checkwin humwon.game Human));

    "MacroBoard play anywhere as Human" >:: 
        (fun _ -> assert_equal macroboard1 (Macro.update macroblank Human 5 5));

    "MacroBoard play anywhere as Machine" >:: 
        (fun _ -> assert_equal macroboard3 (Macro.update macroblank Machine 5 5));
    
    "MacroBoard illegal play to anywhere as Human" >:: 
        (fun _ -> assert_raises InvalidLoc 
                      (fun () -> Macro.update macroboard2 Human 1 1));
    
     "MacroBoard illegal play to anywhere as Machine" >:: 
        (fun _ -> assert_raises InvalidLoc 
                      (fun () -> Macro.update macroboard2 Machine 1 1));

<<<<<<< HEAD
      "Test for draw" >:: 
        (fun _ -> assert_equal macroboard4 (Macro.update macroboard4 Human 5 5));

=======
      "MacroBoard game commits to a draw" >:: 
        (fun _ -> assert_equal macroboard4 
                                        (Macro.update macroboard4 Machine 5 5));

                                        
>>>>>>> 717947cd1cea0d25da3389f24704448f2b4ec74e
]

let suite =
  "test suite for A7"  >::: List.flatten [
    board_tests;
  ]

let _ = run_test_tt_main suite
