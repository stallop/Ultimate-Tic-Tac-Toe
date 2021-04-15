open Board

module type Player = sig
    val nextmove : int -> Macro.t -> int * int
end

module Rand : Player = struct 
    (** [nextmove diff board] returns a random move 
        possible from the currect board state 
        @param diff this doesn't matter, and exists to satisfy Player
        @param board current board state
        @return int * int of the move selected *)
    let rec nextmove (diff:int) (board: Macro.t)= 
        try
            let moves = Macro.nextmoves board in
                List.nth moves (Random.int (List.length moves)) 
        with | InvalidLoc -> nextmove diff board
end

module Minimax : Player = struct 
    let nextstates (board : Macro.t) (nextplayer : value) : Macro.t list =
        let moveslist = Macro.nextmoves board in 
        List.map 
            (fun move -> Macro.update board nextplayer (fst move) (snd move))
            moveslist

    let listavg (l : float list) : float =
        let (s,n) = List.fold_left (fun (a,b) c -> (a+.c,b+1)) (0.,0) l in 
            s/.(float_of_int n)
    let rec movehelper 
            (board : Macro.t) 
            (depthremaining : int)
            (nextplayer : value) : float =
        if (depthremaining = 0) || (board.won <> Blank) 
            || List.length (nextstates board nextplayer) = 0
        then (*
            let () = Macro.pretty_print board in
            let () = output_string stdout "\n\n\n\n" in *)
            float_of_int (Macro.heuristic board)
        else 
            match nextplayer with 
            | Blank -> failwith "movehelper called with no next player"
            | _ -> listavg (List.map 
                (fun (b : Macro.t) -> movehelper 
                    b 
                    (depthremaining-1) 
                    (if nextplayer = Machine then Human else Machine))
                (nextstates board nextplayer)
            )

    (** [nextmove diff board] returns the optimal move according to minimax.
        this uses [Macro.heuristic].
        @param diff the depth to be explored; 
            the time complexity is exponential wrt this
        @param board current board state
        @return int * int of the move selected *)
    let nextmove (diff:int) (board : Macro.t) : int * int = 
        let moveslist : (int * int) list = Macro.nextmoves board 
        (* list of moves *) in
        let stateslist : (Macro.t * (int * int)) list = 
            List.map (* list of (Macro.t, move) *)
                (fun m -> (Macro.update board Machine (fst m) (snd m), m))
                moveslist in
        let stateslistcalc : (float * (int * int)) list = 
            List.map (* list of (int, move) *)
                ( fun sm -> ((movehelper (fst sm) diff Human), snd sm) ) 
                stateslist in 
        snd (List.hd (List.sort (* move corresponding to highest movehelper value *)
            (fun a b -> match (fst b) -. (fst a) with
                | x when x < 0. -> -1
                | x when x = 0. -> 0
                | _ -> 1)
            stateslistcalc))

end

module MonteCarlo : Player = struct

    type board = Macro.t

    type state = {
        board : board;
        move: int*int;
        winEst : int;
    }

    let blankState = {board = Macro.blank; move = 0,0; winEst = -1000}

    let getNextPerm (v:int*int) : int*int = 
        if (fst v + 1)= 10 then (1, snd v + 1)
        else (fst v + 1, snd v)

    let rec getNextPosStates (board:board) (v:int*int) (acc:state list):
                                                                 state list = 
        if (board.next_pos = -1) then (
            if snd v = 10 then acc else
            try 
                let a = Macro.update board Machine (fst v) (snd v) in
                    getNextPosStates board (getNextPerm v)
                        ({board = a; move = v ; winEst = 0}::acc)

            with | InvalidLoc -> getNextPosStates board (getNextPerm v) acc
        )
        else
            if snd v = 10 then acc else
                try
                    let a = Macro.update board Machine board.next_pos (snd v) in
                            getNextPosStates board (0,snd v + 1)
                            ({board = a; move = (-1,snd v) ;winEst = 0}::acc)
                with | InvalidLoc -> getNextPosStates board (0,snd v + 1) acc

    let oppPlayer = function
        | Machine -> Human
        | Human -> Machine 
        | Blank -> Blank

    let rec makeRandomMoves (board:board) (player:value)= 
    let moves = Macro.nextmoves board in
    try
        if List.length moves = 0 then 0 else 
        match board.won with 
        | Machine -> 1
        | Human -> (-1)
        | Blank -> let rE = 
                List.nth moves (Random.int (List.length moves)) in 
                    makeRandomMoves (Macro.update board (player) 
                    (fst rE) (snd rE)) (oppPlayer player)
        with | InvalidLoc -> makeRandomMoves board player
        
    let rec getWinEst (board:board) (noGame:int): int = 
        let noGames = noGame in 
            let rec estimate (no:int) (acc:int)=
                if no = 0 then acc 
                else estimate (no-1) (makeRandomMoves board Machine + acc)
            in estimate noGames 0

    let rec winningState (initial:state) = function
        | h::t -> if (h.winEst > initial.winEst) then winningState h t 
                  else winningState initial t 
        | [] -> initial.move

    (** [nextmove diff board] returns the optimal move according to Monte Carlo.
        this uses [Macro.heuristic].
        @param diff the number of random games to bootstrap from
        @param board current board state
        @return int * int of the move selected *)
    let nextmove (noGame:int) (board:board) : int*int = 
        let statelist = getNextPosStates board (1,1) [] in 
            let rec setWinEst (acc:state list) = function
                | h :: t -> 
                    setWinEst ({board = h.board; 
                            move = h.move; 
                            winEst = getWinEst h.board noGame}::acc) t
                | [] -> winningState blankState acc in 
            setWinEst [] statelist
end