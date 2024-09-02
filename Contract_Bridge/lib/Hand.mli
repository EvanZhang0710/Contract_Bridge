(** A representation of a player's or AI's hand, which ranges from 1 to 13 cards *)

open Card

type hand = card list
(** Type declaration for hand *)

val make_hand : card list -> hand
(** [make_hand cards] checks whether cards contains 13 cards and returns hand if 
    the check passes*)

val get_hcp : hand -> int
(** [get_hcp h] returns the total hcp of a hand*)

val get_card_comp : hand -> int list
(** [get_card_comp h] returns the total card composition of a hand*)

val highest_in_suit : hand -> suit -> card option
(** [highest_in_suit h suit] returns the card option with the highest value of suit 
    [suit] in hand [h]. Return [None] if no such card exists *)

val lowest_in_suit : hand -> suit -> card option
(** [lowest_in_suit h suit] returns the card option with the lowest value of suit 
    [suit] in hand [h]. Return [None] if no such card exists *)

val has_suit : hand -> suit -> bool
(** [has_suit h suit] returns true if hand [h] has a card of suit [suit] *)

val sort_hand : hand -> card list
(** [sort_hand h] returns a sorted hand [h] *)

val remove_suit : hand -> suit -> card list
(** [remove_suit h suit] returns a hand [h] with all cards of suit [suit] removed *)

val hand_to_string : hand -> string
(** [hand_to_string h] returns a string representation of hand [h] *)

val print_hand : hand -> unit
(** [print_hand h] prints a string representation of hand [h] *)
