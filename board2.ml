
type value = Machine | Human | Blank
exception InvalidTuple
exception InvalidLoc
exception InvalidMicro

module Micro = struct 
  type microgame = value*value*value*value*value*value*value*value*value

  type t = Micro of microgame | Won of value

  let blank : t = Micro (Blank,Blank,Blank,Blank,Blank,Blank,Blank,Blank,Blank)

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

  let tuple_replace (tuple:microgame) (loc:int) (n:value) : microgame =
    match tuple with 
    | (a,b,c,d,e,f,g,h,i) -> 
      match loc with
      | 1 -> (n,b,c,d,e,f,g,h,i)
      | 2 -> (a,n,c,d,e,f,g,h,i)
      | 3 -> (a,b,n,d,e,f,g,h,i)
      | 4 -> (a,b,c,n,e,f,g,h,i)
      | 5 -> (a,b,c,d,n,f,g,h,i)
      | 6 -> (a,b,c,d,e,n,g,h,i)
      | 7 -> (a,b,c,d,e,f,n,h,i)
      | 8 -> (a,b,c,d,e,f,g,n,i)
      | 9 -> (a,b,c,d,e,f,g,h,n)
      | _ -> raise InvalidTuple

  let update (initial:t) (player:value) (loc:int) : t =
    match initial with
    | Micro (a,b,c,d,e,f,g,h,i)->
      let pos = 
        match loc with
        | 1 -> a
        | 2 -> b
        | 3 -> c
        | 4 -> d
        | 5 -> e
        | 6 -> f
        | 7 -> g
        | 8 -> h
        | 9 -> i
        | _ -> raise InvalidLoc 
      in if pos = Blank then raise InvalidLoc else
        let tuple_new = tuple_replace (a,b,c,d,e,f,g,h,i) loc player in
        let x = checkwin tuple_new player in 
        if x = Blank then Micro tuple_new else Won x
    | _ -> raise InvalidMicro

end

module Macro = struct
  type e = Micro.t

  type micro = e*e*e*e*e*e*e*e*e

  type t = { 
    game : micro;
    next_pos : int; 
    won : value;
  }

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

  let checkwin (game:micro) (player:value) : value = 
    match game with
    | (a,b,c,
       d,e,f,
       g,h,i) -> Blank
    | _ -> Blank

end