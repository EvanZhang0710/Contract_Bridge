(** The module to simulate a contract bridge game, which includes bidding and making
    tricks. *)

open Card
open Hand
open Contract

type game = {
  mutable deck : card list;
  suit_list : suit list;
  mutable declarer : int;
  mutable attacker : bool;
}
(** Type declaration for game *)

val create_game : card list -> game
(** Constructor of game *)

val distribute_cards : card list -> hand list
(** [distribute_cards deck] distribute [deck] into four groups of 13 cards *)

val deal : unit -> hand list
(** [deal ()] deals the cards to the players *)

val int_to_ai : int -> string
(** Function to convert an int to an AI name *)

val strain_is_valid : suit -> int -> contract -> bool
(** [strain_is_valid strain level current_contract] check whether [strain] is 
    valid with regard to [current_contract] *)

val level_is_valid : int -> contract -> bool
(** [level_is_valid level current_contract] check whether [level] is valid with 
    regard to [current_contract] *)

val char_of_string : string -> char
(** Function to convert a string to a char *)

val contract_player : hand -> contract -> int -> contract
(** [contract_player hand current_contract pos] reads in a contract from console 
    from [pos] player and update [current_contract]*)

val ai_bid_response :
  int list -> int -> contract option -> contract -> int -> contract
(** [ai_bid_response hand_comp hcp partner current_contract pos] lets AI decide 
    which contract to bid based on [partner]'s decision, [hand_comp], records 
    [current_contract] at position [pos]*)

val contract_ai :
  hand -> contract list -> contract -> contract option -> int -> contract
(** [contract_ai hand contracts cur_contract partner pos] lets AI decide which the 
    list of [contracts], records [current_contract] at position [pos]. It also 
    considers the [partner]'s contract*)

val bidding_phase : hand list -> contract
(** [bidding_phase hands] handles the bidding phase of the game *)

val get_declarer : contract list -> contract -> int
(** [get_declarer contracts current_contract] returns the position of the declarer *)

val highest_card : card list -> contract -> int option
(** [highest_card cards current_contract] returns the position of the player who 
    plays the highest card in [cards] *)

val display_hand : hand -> unit
(** Function to display the hand *)

val choose_card : hand -> contract -> card list -> card
(** Function to let player choose a card to play *)

val lowest_card : hand -> card option
(** [lowest_card hand] returns the card option with the lowest value in [hand]. 
    Return [None] if there are no cards in [hand] *)

val suit_to_index : suit -> int
(** Function to convert a suit to an index *)

val ai_make_trick : hand -> contract -> card list -> card
(** [ai_make_trick current_hand contract tricks] lets AI decides which card to play 
    in [current_hand] based on [contract] and previous [tricks]*)

val player_trick : hand -> contract -> card list -> card
(** [player_trick current_hand contract tricks] reads in a card from the console and
     update [tricks]*)

val compare_cards : contract -> card -> card -> int
(** Function to compare two cards *)

val determine_trick_winner : int -> bool -> bool
(** [determine_trick_winner win_pos attacker] determines whether [attacker] is true 
    and return the position [win_pos]*)

val new_hand : hand -> card -> hand
(** Function to remove a card from a hand *)

val determine_attacker_status : game -> bool
(** Function to determine whether the current player is an attacker *)

val play_round :
  hand list ->
  contract ->
  int ->
  int list ->
  card list ->
  bool ->
  hand list * int list * 'a list * int * int
(** Function to play each trick *)

val play_rounds :
  hand list ->
  contract ->
  int ->
  int ->
  int ->
  int list ->
  card list ->
  bool ->
  int
(** Recursive function to run each round of trick*)

val play_phase : game -> hand list -> contract -> int
(** Main function to handle the playing phase of the game *)
