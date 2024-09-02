(** A representation of the contract that is played in each game *)

open Card

type contract = { strain : suit; level : int; position : int }
(** Type for a contract *)

val make_contract : suit -> int -> int -> contract
(** [make_contract s l p] creates a contract with strain [s], level [l], position 
    [p]*)

val get_strain : contract -> suit
(** [get_strain c] gets the strain of a contract [c]*)

val get_level : contract -> int
(** [get_level c] gets the level of a contract [c]*)

val print_contract : contract -> unit
(** [print_contract c] prints a contract [c]*)
