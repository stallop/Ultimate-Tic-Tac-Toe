
type value = Machine | Human | Blank
exception InvalidTuple
exception InvalidLoc
exception InvalidLine

module Micro = struct 
  (** The smaller tic tac toe game *)
  type microgame = value*value*value*value*value*value*value*value*value

  (** The values stored for small tic tac toe game *)
  type t = {
    game: microgame;
    won: value
    }

  (** Setting up a blank micro tic tac toe game *)
  let blank : t = {
    game =(Blank,Blank,Blank,Blank,Blank,Blank,Blank,Blank,Blank);
    won = Blank;
  }

  (** [checkwin game player] is a checker to see if an individual has won
    @param game is the game to check
    @param player is the value of the player who may have won
    @return player who has won or blank if no one has
   *)
  let checkwin (game:microgame) (player:value) : value = 
    match game with
    | (a,b,c,
       d,e,f,
       g,h,i) when (a <> Blank && a = b && b = c) || 
                   (a <> Blank && a = d && d = g) ||
                   (a <> Blank && a = e && e = i) || 
                   (b <> Blank && b = e && e = h) || 
                   (c <> Blank && c = f && f = i) || 
                   (d <> Blank && d = e && e = f) ||
                   (g <> Blank && g = h && h = i) ||
                   (g <> Blank && g = e && e = c) -> player
    | _ -> Blank

  (** [update initial player loc] is an updated game with a move in loc
    @param initial is the intial game
    @param player is the value of the player whose turn it is
    @param loc is the location the player is playing
    @return updated game
   *)
  let update (initial:t) (player:value) (loc:int) : t =
    match initial.game with
    (a,b,c,d,e,f,g,h,i) ->
      match loc with
      | 1 -> 	if a = Blank then { game = (player,b,c,d,e,f,g,h,i); 
                                   won = checkwin (player,b,c,d,e,f,g,h,i) player} 
        else raise InvalidLoc
      | 2 ->  if b = Blank then { game = (a,player,c,d,e,f,g,h,i); 
                                   won = checkwin (a,player,c,d,e,f,g,h,i) player} 
        else raise InvalidLoc
      | 3 ->  if c = Blank then { game = (a,b,player,d,e,f,g,h,i); 
                                   won = checkwin (a,b,player,d,e,f,g,h,i) player}
        else raise InvalidLoc
      | 4 ->  if d = Blank then { game = (a,b,c,player,e,f,g,h,i); 
                                   won = checkwin (a,b,c,player,e,f,g,h,i) player}
        else raise InvalidLoc
      | 5 ->  if e = Blank then { game = (a,b,c,d,player,f,g,h,i); 
                                   won = checkwin (a,b,c,d,player,f,g,h,i) player}
        else raise InvalidLoc
      | 6 ->  if f = Blank then { game = (a,b,c,d,e,player,g,h,i); 
                                   won = checkwin (a,b,c,d,e,player,g,h,i) player}
        else raise InvalidLoc
      | 7 ->  if g = Blank then { game = (a,b,c,d,e,f,player,h,i); 
                                   won = checkwin (a,b,c,d,e,f,player,h,i) player}
        else raise InvalidLoc
      | 8 ->  if h = Blank then { game = (a,b,c,d,e,f,g,player,i); 
                                   won = checkwin (a,b,c,d,e,f,g,player,i) player}
        else raise InvalidLoc
      | 9 ->  if i = Blank then { game = (a,b,c,d,e,f,g,h,player); 
                                   won = checkwin (a,b,c,d,e,f,g,h,player) player}
        else raise InvalidLoc
      | _ -> raise InvalidLoc

  let print_value (value:value) : string =  match value with 
    | Machine -> "O"
    | Human -> "X"
    | Blank -> " "

  let get_game micro =
    micro.game

  let get_won micro =
    micro.won

  let get_line (initial:t) (line:int) : string = 
    match line with 
    | 1 -> (
        match initial.game with 
        (a,b,c,_,_,_,_,_,_) -> 
          " "^(print_value a)^" | "^(print_value b)^" | "^(print_value c)^" " )
    | 2 ->( 
        match initial.game with 
        | (_,_,_,a,b,c,_,_,_) ->
          " "^(print_value a)^" | "^(print_value b)^" | "^(print_value c)^" " )
    | 3 ->( 
        match initial.game with 
        | (_,_,_,_,_,_,a,b,c) ->
          " "^(print_value a)^" | "^(print_value b)^" | "^(print_value c)^" " )
    | _ -> failwith "You shouldn't be here"

  let heuristic (asdf:t) : int = 
    match asdf.won with
    | Machine -> 24
    | Human -> -24
    | _ -> begin
        let (q,w,e,r,t,y,u,i,o) = asdf.game in 
        begin 
          match q with 
          | Machine -> 3
          | Human -> -3
          | Blank -> 0
        end + 
        begin 
          match w with 
          | Machine -> 2
          | Human -> -2
          | Blank -> 0
        end + 
        begin 
          match e with 
          | Machine -> 3
          | Human -> -3
          | Blank -> 0
        end + 
        begin 
          match r with 
          | Machine -> 2
          | Human -> -2
          | Blank -> 0
        end + 
        begin 
          match t with 
          | Machine -> 4
          | Human -> -4
          | Blank -> 0
        end + 
        begin 
          match y with 
          | Machine -> 2
          | Human -> -2
          | Blank -> 0
        end + 
        begin 
          match u with 
          | Machine -> 3
          | Human -> -3
          | Blank -> 0
        end + 
        begin 
          match i with 
          | Machine -> 2
          | Human -> -2
          | Blank -> 0
        end + 
        begin 
          match o with 
          | Machine -> 3
          | Human -> -3
          | Blank -> 0
        end
      end

  (** [nextmoves t] is an int list of the next possible moves for the 
      microgame
    @param t is the game to check
    @return int list of the next moves
   *)
  let nextmoves (t:t) : int list = 
    match t.won with 
    | Machine -> []
    | Human -> []
    | _ -> let (a,b,c,d,e,f,g,h,i) = t.game in 
      (if a = Blank then [1] else []) @
      (if b = Blank then [2] else []) @
      (if c = Blank then [3] else []) @
      (if d = Blank then [4] else []) @
      (if e = Blank then [5] else []) @
      (if f = Blank then [6] else []) @
      (if g = Blank then [7] else []) @
      (if h = Blank then [8] else []) @
      (if i = Blank then [9] else [])

  (** [flip asdf] is the reversed position of a small board  
      where Machine and Human are swapped
    @param asdf is the board to be flipped
    @return an identical board with Machine and Human reversed
   *)
  let flip (board : t) : t = 
    let (a,b,c,d,e,f,g,h,i) = board.game in
      {
        game = (
          begin
            match a with 
            | Machine -> Human
            | Human -> Machine
            | Blank -> Blank
          end, 
          begin
            match b with 
            | Machine -> Human
            | Human -> Machine
            | Blank -> Blank
          end, 
          begin
            match c with 
            | Machine -> Human
            | Human -> Machine
            | Blank -> Blank
          end, 
          begin
            match d with 
            | Machine -> Human
            | Human -> Machine
            | Blank -> Blank
          end, 
          begin
            match e with 
            | Machine -> Human
            | Human -> Machine
            | Blank -> Blank
          end, 
          begin
            match f with 
            | Machine -> Human
            | Human -> Machine
            | Blank -> Blank
          end, 
          begin
            match g with 
            | Machine -> Human
            | Human -> Machine
            | Blank -> Blank
          end, 
          begin
            match h with 
            | Machine -> Human
            | Human -> Machine
            | Blank -> Blank
          end, 
          begin
            match i with 
            | Machine -> Human
            | Human -> Machine
            | Blank -> Blank
          end
        );
      won = match board.won with 
        | Machine -> Human
        | Human -> Machine
        | Blank -> Blank;
    }

end

module Macro = struct
  (** Element of the macro game, which are microgames *)
  type e = Micro.t

  (** Macro game *)
  type macro = e*e*e*e*e*e*e*e*e

  (** Macro game baord with essential values for determining the game *)
  type t = { 
    game : macro;
    next_pos : int;
    won : value
  }

  (** Setting up a blank macro tic tac toe game, which is set to let 
      anyone play anywhere *)
  let blank : t = {
    game = (Micro.blank,Micro.blank,Micro.blank,Micro.blank,Micro.blank,
            Micro.blank,Micro.blank,Micro.blank,Micro.blank);
    next_pos = -1;
    won = Blank;
  }

  let getMicro (initial:t) (no:int) : Micro.t = 
    match no with 
    | 1 -> (match initial.game with (a,_,_,_,_,_,_,_,_) -> a)
    | 2 -> (match initial.game with (_,b,_,_,_,_,_,_,_) -> b)
    | 3 -> (match initial.game with (_,_,c,_,_,_,_,_,_) -> c)
    | 4 -> (match initial.game with (_,_,_,d,_,_,_,_,_) -> d)
    | 5 -> (match initial.game with (_,_,_,_,e,_,_,_,_) -> e)
    | 6 -> (match initial.game with (_,_,_,_,_,f,_,_,_) -> f)
    | 7 -> (match initial.game with (_,_,_,_,_,_,g,_,_) -> g)
    | 8 -> (match initial.game with (_,_,_,_,_,_,_,h,_) -> h)
    | 9 -> (match initial.game with (_,_,_,_,_,_,_,_,i) -> i)
    | _ -> raise InvalidLoc

     (** [nextmoves t] is an int*int list of the next possible moves for the 
      microgame
    @param t is the game to check
    @return int*int list of the next moves in the macro and micro board
   *)
  let rec nextmoves t	: (int * int) list =
    match t.next_pos with
    | -1 -> nextmoves {
        game = t.game;
        next_pos = 1;
        won = t.won;
      } @
            nextmoves {
              game = t.game;
              next_pos = 2;
              won = t.won;
            } @
            nextmoves {
              game = t.game;
              next_pos = 3;
              won = t.won;
            } @
            nextmoves {
              game = t.game;
              next_pos = 4;
              won = t.won;
            } @
            nextmoves {
              game = t.game;
              next_pos = 5;
              won = t.won;
            } @
            nextmoves {
              game = t.game;
              next_pos = 6;
              won = t.won;
            } @
            nextmoves {
              game = t.game;
              next_pos = 7;
              won = t.won;
            } @
            nextmoves {
              game = t.game;
              next_pos = 8;
              won = t.won;
            } @
            nextmoves {
              game = t.game;
              next_pos = 9;
              won = t.won;
            }
    | _ -> List.map 
             (fun x -> (t.next_pos, x)) 
             (Micro.nextmoves (getMicro t t.next_pos)) 

  let getNext (microgame:e) (pos:int): int = 
    match microgame.won with 
    | Machine | Human -> -1
    | Blank -> pos

  (** [checkwin game player] is a checker to see if an individual has won
    @param game is the game to check
    @param player is the value of the player who may have won
    @return player who has won or blank if no one has
   *)
  let checkwin (game:macro) (player: value) : value = 
    match game with
    | (a,b,c,
       d,e,f,
       g,h,i) when (a.won <> Blank && a.won = b.won && b.won = c.won) || 
                   (a.won <> Blank && a.won = d.won && d.won = g.won) ||
                   (a.won <> Blank && a.won = e.won && e.won = i.won) || 
                   (b.won <> Blank && b.won = e.won && e.won = h.won) || 
                   (c.won <> Blank && c.won = f.won && f.won = i.won) || 
                   (d.won <> Blank && d.won = e.won && e.won = f.won) ||
                   (g.won <> Blank && g.won = h.won && h.won = i.won) ||
                   (g.won <> Blank && g.won = e.won && e.won = c.won) -> player
    | _ -> Blank

  let new_line = "---+---+---+---+---+---+---+---+---\n"

  let print_horiz () = 
    print_string "---+---+---";
    ANSITerminal.(print_string [red] "+");
    print_string "---+---+---";
    ANSITerminal.(print_string [red] "+");
    print_string "---+---+---\n"

  let print_micro (micro:e) (line:int) : unit =
    match (Micro.get_won micro) with
    | Machine -> (
        match line with
        | 1 -> ANSITerminal.(print_string [blue] "  /-----\\  ")
        | 2 -> ANSITerminal.(print_string [blue] " |       | ")
        | 3 -> ANSITerminal.(print_string [blue] "  \\-----/  ")
        | _ -> raise InvalidLine )
    | Human -> (
        match line with
        | 1 -> ANSITerminal.(print_string [blue] "   \\   /   ")
        | 2 -> ANSITerminal.(print_string [blue] "     X     ")
        | 3 -> ANSITerminal.(print_string [blue] "   /   \\   ")
        | _ -> raise InvalidLine )
    | _ -> print_string (Micro.get_line micro line)

  let rec rec_pretty_print (micro_row:e*e*e) (line:int) : unit = 
    if line = 4 then () else
      match micro_row with
      | (a,b,c) -> (
          print_micro a line;
          ANSITerminal.(print_string [red] "|");
          print_micro b line;
          ANSITerminal.(print_string [red] "|");
          print_micro c line;
          print_string "\n";
          if line <> 3 then print_horiz ();
          rec_pretty_print micro_row (line + 1);
        )

  let pretty_print (initial:t) : unit =
  if List.length (nextmoves initial) = 0 then 
  (ANSITerminal.(print_string [red] "\nGood Game! Its a draw\n");) else
    if initial.won = Human then 
      (ANSITerminal.(print_string [red] "\nGood game! Player 1 wins!\n");)
    else if initial.won = Machine then
      (ANSITerminal.(print_string [red] "\nGood game! Player 2 wins!\n");)
    else 
      match initial.game with
      | (a,b,c,d,e,f,g,h,i) -> (
          rec_pretty_print (a,b,c) 1;
          ANSITerminal.(print_string [red] new_line);
          rec_pretty_print (d,e,f) 1;
          ANSITerminal.(print_string [red] new_line);
          rec_pretty_print (g,h,i) 1;
        )

  (** [heuristic asdf] is heuristics for specific game board
    @param asdf is the current game
    @return int value if the heuristics for the game board state
   *)
  let heuristic (asdf:t) : int = 
    match asdf.won with 
    | Machine -> 216
    | Human -> -216
    | _ -> begin
        let (q,w,e,r,t,y,u,i,o) = asdf.game in 
        3 * (Micro.heuristic q) + 2 * (Micro.heuristic w) + 3 * (Micro.heuristic e) +
        2 * (Micro.heuristic r) + 4 * (Micro.heuristic t) + 3 * (Micro.heuristic y) +
        3 * (Micro.heuristic u) + 2 * (Micro.heuristic i) + 3 * (Micro.heuristic o)
      end

    (** [update initial player loc] is an updated game with a move in loc on the 
    macro board and loc on the miniboard. Location on macro board is -1 if the 
    player has no choice on the next move.
    @param initial is the intial game
    @param player is the value of the player whose turn it is
    @param locmacro is the location the player is playing on the macro board
    @param locmicro is the location of the player on the micro board
    @return updated game
   *)
  let rec update (initial:t) (player:value) (locmacro:int) (locmicro:int) : t = 
  if List.length (nextmoves initial) = 0 then 
  {game = initial.game;next_pos = initial.next_pos;won = Blank} 
  else
    match initial.game with
    | (a,b,c,d,e,f,g,h,i) ->
      match locmacro with
      | (-1) -> update initial player initial.next_pos locmicro 
      | 1 -> if a.won = Blank then let mg = (Micro.update a player locmicro) in 
          { game = (mg,b,c,d,e,f,g,h,i);
            next_pos = if mg.won <> Blank  && locmicro = 1 
                       then - 1 
                       else getNext (getMicro initial locmicro) locmicro;
            won = checkwin (mg,b,c,d,e,f,g,h,i) player}
        else raise InvalidLoc
      | 2 -> if b.won = Blank then let mg = (Micro.update b player locmicro) in 
          { game = (a,mg,c,d,e,f,g,h,i);
            next_pos = if mg.won <> Blank  && locmicro = 2 
                       then (-1) 
                       else getNext (getMicro initial locmicro) locmicro;
            won = checkwin (a,mg,c,d,e,f,g,h,i) player}
        else raise InvalidLoc
      | 3 -> if c.won = Blank then let mg = (Micro.update c player locmicro) in 
          { game = (a,b,mg,d,e,f,g,h,i);
            next_pos = if mg.won <> Blank  && locmicro = 3 
                      then (-1) 
                       else getNext (getMicro initial locmicro) locmicro;
            won = checkwin (a,b,mg,d,e,f,g,h,i) player}
        else raise InvalidLoc
      | 4 -> if d.won = Blank then let mg = (Micro.update d player locmicro) in 
          { game = (a,b,c,mg,e,f,g,h,i);
            next_pos = if mg.won <> Blank  && locmicro = 4 
                      then (-1) 
                       else getNext (getMicro initial locmicro) locmicro;
            won = checkwin (a,b,c,mg,e,f,g,h,i) player}
        else raise InvalidLoc
      | 5 -> if e.won = Blank then let mg = (Micro.update e player locmicro) in 
          { game = (a,b,c,d,mg,f,g,h,i);
            next_pos = if mg.won <> Blank  && locmicro = 5 
                      then (-1) 
                       else getNext (getMicro initial locmicro) locmicro;
            won = checkwin (a,b,c,d,mg,f,g,h,i) player}
        else raise InvalidLoc
      | 6 -> if f.won = Blank then let mg = (Micro.update f player locmicro) in 
          { game = (a,b,c,d,e,mg,g,h,i);
            next_pos = if mg.won <> Blank  && locmicro = 6 
                      then (-1) 
                       else getNext (getMicro initial locmicro) locmicro;
            won = checkwin (a,b,c,d,e,mg,g,h,i) player}
        else raise InvalidLoc
      | 7 -> if g.won = Blank then let mg = (Micro.update g player locmicro) in 
          { game = (a,b,c,d,e,f,mg,h,i);
            next_pos = if mg.won <> Blank  && locmicro = 7 
                       then (-1) 
                       else getNext (getMicro initial locmicro) locmicro;
            won = checkwin (a,b,c,d,e,f,mg,h,i) player}
        else raise InvalidLoc
      | 8 -> if h.won = Blank then let mg = (Micro.update h player locmicro) in 
          { game = (a,b,c,d,e,f,g,mg,i);
            next_pos = if mg.won <> Blank  && locmicro = 8 
                       then (-1) 
                       else getNext (getMicro initial locmicro) locmicro;
            won = checkwin (a,b,c,d,e,f,g,mg,i) player}
        else raise InvalidLoc
      | 9 -> if i.won = Blank then let mg = (Micro.update i player locmicro) in 
          { game = (a,b,c,d,e,f,g,h,mg);
            next_pos = if mg.won <> Blank && locmicro = 9 
                       then (-1) 
                       else getNext (getMicro initial locmicro) locmicro;
            won = checkwin (a,b,c,d,e,f,g,h,mg) player}
        else raise InvalidLoc
      | _ -> raise InvalidLoc

  (** [flip asdf] is the reversed position of a game  
      where Machine and Human are swapped
    @param asdf is the game to be flipped
    @return an identical game with Machine and Human reversed
   *)
  let flip game : t = 
    {
      next_pos = game.next_pos;
      won = begin 
        match game.won with 
        | Machine -> Human
        | Human -> Machine 
        | Blank -> Blank 
      end;
      game = let (a,b,c,d,e,f,g,h,i) = game.game in 
        (
          Micro.flip a,
          Micro.flip b,
          Micro.flip c,
          Micro.flip d,
          Micro.flip e,
          Micro.flip f,
          Micro.flip g,
          Micro.flip h,
          Micro.flip i
        );
    }

end

