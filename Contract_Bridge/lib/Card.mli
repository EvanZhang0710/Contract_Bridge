(** A representation of a contract-bridge card *)

(** Type declaration for suit *)
type suit = Diamonds | Clubs | Hearts | Spades | NoTrump | Pass

type card = { suit : suit; value : int }
(** Type declaration for card *)

val make_card : suit -> int -> card
(** [make_card s v] creates a card with suit [s] and value [v]*)

val get_suit : card -> suit
(** [get_sult c] gets the suit of a card [c] *)

val get_value : card -> int
(** [get_value c] gets the value of a card [c]*)

val get_hcp : card -> int
(** [get_hcp c] gets the hcp value of a card [c]*)

val get_sorting_val : card -> int
(** [get_sorting_val c] gets the sorting value of a card [c]*)

val get_rank_val : card -> int
(** [get_rank_val c] gets the rank value of a card [c]*)

val get_char : card -> char
(** [get_char c] gets the character representation of a card [c]*)

val char_to_suit : char -> suit
(** [char_to_suit c] converts a character [c] to its corresponding suit *)

val string_of_value : int -> string
(** [string_of_value v] converts a value [v] to its string representation *)

val string_of_card : card -> string
(** [string_of_card c] converts a card [c] to its string representation *)

val suit_to_string : suit -> string
(** [suit_to_string s] converts a suit [s] to its string representation *)
