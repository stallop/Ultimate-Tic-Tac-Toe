type value = Machine | Human | Blank
exception InvalidTuple
exception InvalidLoc

module Micro : sig 
  type microgame = value*value*value*value*value*value*value*value*value
  type t = {game : microgame; won : value}
  val blank : t 

  val checkwin : microgame -> value -> value 

  val update : t -> value -> int -> t 

  val nextmoves : t -> int list 

  val flip : t -> t

end

module Macro : sig
    type e = Micro.t
    type macro = e*e*e*e*e*e*e*e*e
    type t = { 
        game : macro;
        next_pos : int;
        won : value;
    }
    val blank : t 

  val checkwin : macro -> value -> value 

  val update : t -> value -> int -> int -> t 

  val pretty_print : t -> unit

  val nextmoves : t -> (int * int) list 

  val heuristic : t -> int

  val flip : t -> t

end