open Board
open Machineagents

let checkquit (x:string) = if x = "q" then exit 0 else ()

module Bot (B: Player) = struct
  let rec play_game_machine (game: Macro.t) (diff : int) : unit = 
    match B.nextmove diff game with a,b -> 
      play_game_human (Macro.update game Machine a b) diff

  and play_game_human (game: Macro.t) (diff:int) = 
    if game.won = Blank then (
    print_endline "";
    Macro.pretty_print game;
    ANSITerminal.(print_string [blue] 
        ("Next Position to Play: "^
        (if game.next_pos = (-1) then "Anywhere " 
          else string_of_int game.next_pos)^"\n"));
    if game.next_pos = -1 then (
    ANSITerminal.(print_string [blue] "Enter Macro Position: ");
    try 
      let x = (read_line ()) in checkquit x;
        ANSITerminal.(print_string [blue] "Enter Micro Position: ");
        let y = (read_line ()) in checkquit y;
          play_game_machine 
          (Macro.update game Human (int_of_string x) (int_of_string y)) diff
    with InvalidLoc -> print_endline "Invalid Location"; play_game_human game diff
      | InvalidTuple -> print_endline "Invalid Tuple"; play_game_human game diff
      | e -> print_endline "You have made a mistake, please try again"; 
          play_game_human game diff
    )
    else 
    try 
      ANSITerminal.(print_string [blue] "Enter Micro Position: ");
      let y = (read_line ()) in checkquit y;
        play_game_machine (Macro.update game Human (-1) (int_of_string y)) diff
    with InvalidLoc -> print_endline "Invalid Location"; play_game_human game diff
        | InvalidTuple -> print_endline "Invalid Tuple"; play_game_human game diff
        | e -> print_endline "You have made a mistake, please try again"; 
          play_game_human game diff)
    else Macro.pretty_print game
end

module BotVsBot (B1 : Player) (B2 : Player) = struct 

  let rec play_1 (game:Macro.t) diff1 diff2 = 
    if game.won = Blank then 
      begin
        print_endline "";
        Macro.pretty_print game;
        let a,b = B1.nextmove diff1 game in
          play_2 (Macro.update game Machine a b) diff1 diff2
      end
    else Macro.pretty_print game
  and play_2 game diff1 diff2 = 
    if game.won = Blank then 
      begin
        print_endline "";
        Macro.pretty_print game;
        let a,b = B2.nextmove diff2 (Macro.flip game) in
          play_1 (Macro.update game Human a b) diff1 diff2
      end
    else Macro.pretty_print game
end

module Mini = Bot(Minimax)
module Monte = Bot(MonteCarlo)
module Random = Bot(Rand)

module MonteVsMini = BotVsBot(MonteCarlo)(Minimax)
module MonteVsRand = BotVsBot(MonteCarlo)(Rand)
module MiniVsRand = BotVsBot(Minimax)(Rand)

let rec play_bot () = 
 ANSITerminal.(print_string [red] 
              "\n\nWhich bot do you want?\n1) for Monte \n2) for Mini \n");
  let x = (read_line ()) in
    ANSITerminal.(print_string [red] "\n\nWhat difficulty do you want?\n");
      let y = int_of_string (read_line ()) in
    match x with 
    | "1" -> Monte.play_game_human Macro.blank y
    | "2" -> Mini.play_game_human Macro.blank y
    | "q" -> exit 0
    | _ -> play_bot ()

let opp (turn:value) : value = 
  match turn with 
  | Human -> Machine
  | Machine -> Human
  | Blank -> Blank

let rec play_game_humans (game: Macro.t) (turn:value)= 
if game.won = Blank then (
  print_endline "";
   (match turn with| Human -> print_endline "Player 1 turn:"; 
                  | Machine -> print_endline "Player 2 turn:";
                  | Blank -> (););
  Macro.pretty_print game;
  ANSITerminal.(print_string [blue] 
      ("Next Position to Play: "^
      (if game.next_pos = (-1) then "Anywhere " 
        else string_of_int game.next_pos)^"\n"));
  if game.next_pos = -1 then (
  ANSITerminal.(print_string [blue] "Enter Macro Position: ");
  try 
    let x = (read_line ()) in checkquit x;
      ANSITerminal.(print_string [blue] "Enter Micro Position: ");
      let y = (read_line ()) in checkquit y;
        play_game_humans 
        (Macro.update game turn (int_of_string x) (int_of_string y)) (opp turn)
with InvalidLoc -> print_endline "Invalid Location"; play_game_humans game turn
    | InvalidTuple -> print_endline "Invalid Tuple"; play_game_humans game turn
    | e -> print_endline "You have made a mistake, please try again"; 
        play_game_humans game turn
  )
  else 
  try 
    ANSITerminal.(print_string [blue] "Enter Micro Position: ");
    let y = (read_line ()) in checkquit y;
      play_game_humans (Macro.update game turn (-1) (int_of_string y)) (opp turn)
  with InvalidLoc -> print_endline "Invalid Location"; play_game_humans game turn
      | InvalidTuple -> print_endline "Invalid Tuple"; play_game_humans game turn
      | e -> print_endline "You have made a mistake, please try again"; 
        play_game_humans game turn)
 else Macro.pretty_print game

let play_human () = 
  ANSITerminal.(print_string [red] "\n\nHope you have a friend to play with\n");
  play_game_humans Macro.blank Human

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  ANSITerminal.(print_string [red] "\n\nWelcome to Ultimate Tic-Tac-Toe.\n");
  ANSITerminal.(print_string [red] "\n\n  Input q to quit at any time!\n");
  ANSITerminal.(print_string [blue] "\n\n  You v Bot    You v Someone else    Minimax 4 vs Random\n");
  ANSITerminal.(print_string [green] "\n\n  Option 1     Option 2              Option 3  \n\n");
  ANSITerminal.(print_string [blue] "\n\n  Monte Carlo 1000 vs Minimax 4      Monte Carlo 1000 vs Random\n");
  ANSITerminal.(print_string [green] "\n\n  Option 4                           Option 5 \n\n");

  let x = (read_line ()) in
  match x with 
  | "1" -> play_bot ()
  | "2" -> play_human ()
  | "3" -> MiniVsRand.play_1 Macro.blank 4 0
  | "4" -> MonteVsMini.play_1 Macro.blank 1000 4
  | "5" -> MonteVsRand.play_1 Macro.blank 1000 0
  | "q" -> exit 0
  | _ -> main ()

(* Execute the game engine. *)
let () = main ()