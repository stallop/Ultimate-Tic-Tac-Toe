open Board

module type Player = sig

    val nextmove : int -> Macro.t -> int * int

end

module Rand : sig

    val nextmove : int -> Macro.t -> int * int

end

module Minimax : sig 

    val nextmove : int -> Macro.t -> int * int

end

module MonteCarlo : sig 

    val nextmove : int -> Macro.t -> int * int

end