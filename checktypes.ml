module type BoardSig = sig
  type value = Machine | Human | Blank
  exception InvalidTuple
  exception InvalidLoc

  module Micro : sig 
  type microgame 
  type t 
  val blank : t 

  val checkwin : microgame -> value -> value 

  val update : t -> value -> int -> t 

  val pretty_print : t -> int -> unit
end

module Macro : sig 
  type e 
  type micro 
  type t 
  val blank : t 

  val checkwin : micro -> value -> value 

  val update : t -> value -> int -> int -> t 

  val pretty_print : t -> int -> int -> unit
end
end

module BoardCheck : BoardSig = Board

module type AuthorsSig = sig
  val hours_worked : int list
end

module AuthorsCheck : AuthorsSig = Authors
