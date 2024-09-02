open Contract_Bridge.Game
open Contract_Bridge.Hand
open Contract_Bridge.Card
open Contract_Bridge.Contract

(** Function to display a hand *)
let show_hand hand =
  let sorted_hand = sort_hand hand in
  let rec group_by_suit acc current_suit cards =
    match cards with
    | [] -> acc
    | c :: cs ->
        let suit_str = suit_to_string (get_suit c) in
        if current_suit = suit_str then
          group_by_suit (acc ^ " " ^ string_of_card c) current_suit cs
        else
          group_by_suit
            (acc ^ "\n" ^ suit_str ^ ": " ^ string_of_card c)
            suit_str cs
  in
  match sorted_hand with
  | [] -> ""
  | c :: cs ->
      group_by_suit
        (suit_to_string (get_suit c) ^ ": " ^ string_of_card c)
        (suit_to_string (get_suit c))
        cs

(** Function to display a contract *)

let contract_output contract =
  let strain_string =
    match contract.strain with
    | Spades -> "Spades"
    | Hearts -> "Hearts"
    | Clubs -> "Clubs"
    | Diamonds -> "Diamonds"
    | NoTrump -> "No Trump"
    | Pass -> "Pass"
  in
  let level_string = string_of_int contract.level in
  let position_string = string_of_int contract.position in
  "Contract: Level " ^ level_string ^ " " ^ strain_string ^ ", Position: "
  ^ position_string

(* Main game loop *)
let rec make_game () =
  print_endline "Starting a new game of Bridge.";

  (* Initialize deck and shuffle *)
  let deck =
    Contract_Bridge.Deck.initialize_deck ()
    |> Contract_Bridge.Deck.shuffle |> Contract_Bridge.Deck.shuffle
  in

  (* Create a new game instance *)
  let game = create_game deck in

  (* Deal cards *)
  let hands = List.map sort_hand (distribute_cards deck) in

  (* Show player's hand - assuming player's hand is the first one *)
  let player_hand = List.hd hands in
  print_endline "Your hand:";
  print_endline (show_hand player_hand);

  (* Bidding phase *)
  let contract = bidding_phase hands in

  print_endline "-----------------------------";
  print_endline "The bidding phase has ended.";
  print_endline (contract_output contract);

  if contract.strain <> Pass && contract.level <> 0 then (
    let score = play_phase game hands contract in

    (* Here you might want to integrate play_card function as part of playing phase *)
    print_endline "-----------------------------";
    print_endline ("The score for the attacker was: " ^ string_of_int score);
    let winning_score = contract.level + 6 in
    if score >= winning_score then
      print_endline "The attacker has won the game!"
    else print_endline "The defender has won the game!";
    print_endline "-----------------------------");

  (* Prompt for new game *)
  print_endline "Would you like to play another game? [Y/N]";
  let input = read_line () |> String.uppercase_ascii in
  match input with
  | "Y" -> make_game ()
  | "N" -> print_endline "Thanks for playing!"
  | _ -> print_endline "Invalid input."
(* Clean up or finalize any game state if necessary *)

(* Start the game loop *)
let () = make_game ()
