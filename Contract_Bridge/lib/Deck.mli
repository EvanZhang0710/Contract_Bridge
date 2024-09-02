(** A representation of a deck of 52 poker cards *)

open Card

type deck = card list
(** Type declaration for deck *)

val initialize_deck : unit -> deck
(** [initialize_deck ()] creates a deck of 52 cards *)

val shuffle : deck -> deck
(** [shuffle d] shuffles the deck [d] *)

val get_cards : deck -> card list
(** [get_cards d] is the getter that returns [d] *)

val get_thirteen : deck -> card list
(** [get_thirteen d] returns the first 13 cards of [d] *)

val print_deck : deck -> unit
(** [print_deck d] prints the deck [d] *)
