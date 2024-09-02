open Card
open Hand
open Contract
open Printf

type game = {
  mutable deck : card list;
  suit_list : suit list;
  mutable declarer : int;
  mutable attacker : bool;
}

(********************************* Preparing Phase *********************************)

let create_game deck =
  {
    deck;
    suit_list = [ Spades; Hearts; Diamonds; Clubs; Pass; NoTrump ];
    declarer = 0;
    attacker = false;
  }

let distribute_cards deck =
  let rec distribute_helper deck hands =
    match deck with
    | [] -> hands
    | _ ->
        let hand1, rest1 = get_thirteen deck [] in
        let hand2, rest2 = get_thirteen rest1 [] in
        let hand3, rest3 = get_thirteen rest2 [] in
        let hand4, _ = get_thirteen rest3 [] in
        [ hand1; hand2; hand3; hand4 ]
  and get_thirteen d acc =
    match d with
    | [] -> (List.rev acc, [])
    | h :: t ->
        if List.length acc = 12 then (List.rev (h :: acc), t)
        else get_thirteen t (h :: acc)
  in

  distribute_helper (Deck.shuffle deck) []

let deal () =
  let deck = Deck.initialize_deck () in
  let shuffled_deck = Deck.shuffle deck in
  print_string "Shuffling deck...\n";
  distribute_cards shuffled_deck

let int_to_ai i =
  if i = 0 then "North"
  else if i = 1 then "East"
  else if i = 2 then "South"
  else "West"

(********************************* Bidding Phase ***********************************)

let char_of_suit = function
  | Spades -> 'S'
  | Hearts -> 'H'
  | Diamonds -> 'D'
  | Clubs -> 'C'
  | Pass -> 'P'
  | NoTrump -> 'N'

let strain_is_valid strain level current_contract =
  let cur_strain_char = char_of_suit current_contract.strain in
  let new_strain_char = char_of_suit strain in
  let cur_level = current_contract.level in
  let val_st =
    List.mem strain [ Spades; Hearts; Diamonds; Clubs; Pass; NoTrump ]
  in
  if
    (new_strain_char > cur_strain_char && val_st)
    || (new_strain_char <= cur_strain_char && level > cur_level && val_st)
    || strain = Pass
  then true
  else false

let level_is_valid l current_contract =
  let cur_level = current_contract.level in
  let valid_level_range = l >= 1 && l <= 7 in
  if
    l = 0
    || (cur_level < l && valid_level_range)
    || (cur_level = l && current_contract.strain != NoTrump && valid_level_range)
  then true
  else false

let char_of_string s =
  match s with
  | "C" | "c" -> 'C'
  | "D" | "d" -> 'D'
  | "H" | "h" -> 'H'
  | "S" | "s" -> 'S'
  | "T" | "t" -> 'T'
  | "P" | "p" -> 'P'
  | _ -> failwith "Invalid input"

let contract_player hand current_contract pos =
  Printf.printf "Your hand: %s\n" (hand_to_string hand);

  let rec ask_for_level () =
    Printf.printf "Please enter your bid level (Integer from 1 to 13): ";
    try
      let level = read_int () in
      if level_is_valid level current_contract then level
      else (
        Printf.printf "Invalid level. Please enter a valid level.\n";
        ask_for_level ())
    with Failure _ | End_of_file | Scanf.Scan_failure _ ->
      Printf.printf "Invalid input. Please enter a number.\n";
      ask_for_level ()
  in

  let level = ask_for_level () in

  let rec ask_for_strain () =
    Printf.printf "Please enter your bid strain: ";
    try
      let strain_input = read_line () in
      let strain =
        strain_input |> char_of_string |> Char.uppercase_ascii |> char_to_suit
      in
      if strain_is_valid strain level current_contract then strain
      else (
        Printf.printf "Invalid strain. Please enter a valid strain.\n";
        ask_for_strain ())
    with Failure _ | End_of_file | Scanf.Scan_failure _ ->
      Printf.printf "Invalid input. Please enter a valid strain character.\n";
      ask_for_strain ()
  in

  let strain = ask_for_strain () in
  { strain; level; position = pos }

let ai_bid_response (hand_comp : int list) hcp partner current_contract pos =
  let clubs = List.nth hand_comp 0 in
  let diamonds = List.nth hand_comp 1 in
  let hearts = List.nth hand_comp 2 in
  let spades = List.nth hand_comp 3 in
  let balanced = clubs > 1 && diamonds > 1 && hearts > 1 && spades > 1 in

  match partner with
  | Some p when p.level = 1 && p.strain = Clubs ->
      if
        hcp >= 13 && hcp <= 15
        && level_is_valid 2 current_contract
        && strain_is_valid NoTrump 2 current_contract
      then { strain = NoTrump; level = 2; position = pos }
      else if
        hcp >= 16 && hcp <= 17
        && level_is_valid 3 current_contract
        && strain_is_valid NoTrump 3 current_contract
      then { strain = NoTrump; level = 3; position = pos }
      else { strain = Pass; level = 0; position = pos }
  | Some p when p.level = 1 && p.strain = Diamonds ->
      if
        hcp >= 13 && hcp <= 15
        && level_is_valid 2 current_contract
        && strain_is_valid NoTrump 2 current_contract
      then { strain = NoTrump; level = 2; position = pos }
      else if
        hcp >= 16 && hcp <= 17
        && level_is_valid 3 current_contract
        && strain_is_valid NoTrump 3 current_contract
      then { strain = NoTrump; level = 3; position = pos }
      else { strain = Pass; level = 0; position = pos }
  | Some p when p.level = 1 && (p.strain = Hearts || p.strain = Spades) ->
      if
        hcp >= 6 && spades >= 4 && hearts = 0
        && level_is_valid 1 current_contract
        && strain_is_valid Spades 1 current_contract
      then { strain = Spades; level = 1; position = pos }
      else if
        hcp >= 6 && hcp <= 9 && spades != 4 && hearts != 3
        && level_is_valid 1 current_contract
        && strain_is_valid NoTrump 1 current_contract
      then { strain = NoTrump; level = 1; position = pos }
      else if
        hcp >= 10 && diamonds >= 4
        && level_is_valid 1 current_contract
        && strain_is_valid Diamonds 1 current_contract
      then { strain = Diamonds; level = 1; position = pos }
      else if
        hcp >= 10 && clubs >= 4
        && level_is_valid 1 current_contract
        && strain_is_valid Clubs 1 current_contract
      then { strain = Clubs; level = 1; position = pos }
      else if
        hcp >= 13
        && level_is_valid 2 current_contract
        && strain_is_valid NoTrump 2 current_contract
      then { strain = NoTrump; level = 2; position = pos }
      else if
        hcp >= 10 && hcp <= 12 && hearts >= 3
        && level_is_valid 3 current_contract
        && strain_is_valid Hearts 3 current_contract
      then { strain = Hearts; level = 3; position = pos }
      else if
        hcp >= 15 && hcp <= 17 && balanced
        && level_is_valid 3 current_contract
        && strain_is_valid NoTrump 3 current_contract
      then { strain = NoTrump; level = 3; position = pos }
      else if
        hcp < 10 && hearts >= 5
        && level_is_valid 4 current_contract
        && strain_is_valid Hearts 4 current_contract
      then { strain = Hearts; level = 4; position = pos }
      else { strain = Pass; level = 0; position = pos }
  | Some p when p.level = 1 && p.strain = NoTrump ->
      if
        hcp >= 8
        && level_is_valid 2 current_contract
        && strain_is_valid Clubs 2 current_contract
      then { strain = Clubs; level = 2; position = pos }
      else { strain = Pass; level = 0; position = pos }
  | Some p when p.level = 2 && p.strain = Clubs ->
      if
        hcp >= 8 && spades >= 5
        && level_is_valid 2 current_contract
        && strain_is_valid Spades 2 current_contract
      then { strain = Spades; level = 2; position = pos }
      else if
        hcp >= 8 && hearts >= 5
        && level_is_valid 2 current_contract
        && strain_is_valid Hearts 2 current_contract
      then { strain = Hearts; level = 2; position = pos }
      else if
        hcp >= 8 && diamonds >= 5
        && level_is_valid 3 current_contract
        && strain_is_valid Diamonds 3 current_contract
      then { strain = Diamonds; level = 3; position = pos }
      else if
        hcp >= 8 && clubs >= 5
        && level_is_valid 3 current_contract
        && strain_is_valid Clubs 3 current_contract
      then { strain = Clubs; level = 3; position = pos }
      else if
        hcp = 8 && balanced
        && level_is_valid 2 current_contract
        && strain_is_valid NoTrump 2 current_contract
      then { strain = NoTrump; level = 2; position = pos }
      else { strain = Pass; level = 0; position = pos }
  | Some p
    when p.level = 2
         && (p.strain = Diamonds || p.strain = Hearts || p.strain = Spades) ->
      if
        level_is_valid 2 current_contract
        && strain_is_valid NoTrump 2 current_contract
      then { strain = NoTrump; level = 2; position = pos }
      else { strain = Pass; level = 0; position = pos }
  | Some p when p.level = 2 && p.strain = NoTrump ->
      if
        level_is_valid 3 current_contract
        && strain_is_valid Clubs 3 current_contract
      then { strain = Clubs; level = 3; position = pos }
      else { strain = Pass; level = 0; position = pos }
  | Some p when p.level = 3 && p.strain = NoTrump ->
      if
        level_is_valid 4 current_contract
        && strain_is_valid Clubs 4 current_contract
      then { strain = Clubs; level = 4; position = pos }
      else { strain = Pass; level = 0; position = pos }
  | None | Some _ -> { strain = Pass; level = 0; position = pos }

let contract_ai (hand : hand) (contracts : contract list)
    (cur_contract : contract) (partner : contract option) pos =
  let hcp = Hand.get_hcp hand in
  let hand_makeup = Hand.get_card_comp hand in
  let clubs = List.nth hand_makeup 0 in
  let diamonds = List.nth hand_makeup 1 in
  let hearts = List.nth hand_makeup 2 in
  let spades = List.nth hand_makeup 3 in
  let balanced = clubs > 2 && diamonds > 2 && hearts > 2 && spades > 2 in
  if hcp < 5 then { strain = Pass; level = 0; position = pos }
  else
    match partner with
    | None ->
        if hcp >= 10 then
          if
            clubs >= 3
            && level_is_valid 1 cur_contract
            && strain_is_valid Clubs 1 cur_contract
          then { strain = Clubs; level = 1; position = pos }
          else if
            diamonds >= 4
            && level_is_valid 1 cur_contract
            && strain_is_valid Diamonds 1 cur_contract
          then { strain = Diamonds; level = 1; position = pos }
          else if
            hearts >= 4
            && level_is_valid 1 cur_contract
            && strain_is_valid Hearts 1 cur_contract
          then { strain = Hearts; level = 1; position = pos }
          else if
            spades >= 4
            && level_is_valid 1 cur_contract
            && strain_is_valid Spades 1 cur_contract
          then { strain = Spades; level = 1; position = pos }
          else if
            balanced
            && level_is_valid 1 cur_contract
            && strain_is_valid NoTrump 1 cur_contract
          then { strain = NoTrump; level = 1; position = pos }
          else { strain = Pass; level = 0; position = pos }
        else { strain = Pass; level = 0; position = pos }
    | Some { level = 0; strain = Pass; position = _ }
      when List.length contracts < 4 ->
        if hcp >= 5 && hcp <= 11 then
          if
            clubs >= 3 && diamonds >= 3
            && level_is_valid 1 cur_contract
            && strain_is_valid Clubs 1 cur_contract
          then { strain = Clubs; level = 1; position = pos }
          else if
            clubs >= 4 && diamonds >= 4
            && level_is_valid 1 cur_contract
            && strain_is_valid Diamonds 1 cur_contract
          then { strain = Diamonds; level = 1; position = pos }
          else { strain = Pass; level = 0; position = pos }
        else if hcp >= 13 then
          if
            hearts >= 5 && hearts > spades
            && level_is_valid 1 cur_contract
            && strain_is_valid Hearts 1 cur_contract
          then { strain = Hearts; level = 1; position = pos }
          else if
            spades >= 5
            && level_is_valid 1 cur_contract
            && strain_is_valid Spades 1 cur_contract
          then { strain = Spades; level = 1; position = pos }
          else if
            hcp >= 22
            && level_is_valid 2 cur_contract
            && strain_is_valid Clubs 2 cur_contract
          then { strain = Clubs; level = 2; position = pos }
          else if
            hcp >= 20 && hcp <= 21 && balanced
            && level_is_valid 2 cur_contract
            && strain_is_valid NoTrump 2 cur_contract
          then { strain = NoTrump; level = 2; position = pos }
          else { strain = Pass; level = 0; position = pos }
        else { strain = Pass; level = 0; position = pos }
    | Some _ -> ai_bid_response hand_makeup hcp partner cur_contract pos

let strain_to_string strain =
  match strain with
  | Spades -> "Spades"
  | Hearts -> "Hearts"
  | Diamonds -> "Diamonds"
  | Clubs -> "Clubs"
  | NoTrump -> "No Trump"
  | Pass -> "Pass"

let bidding_phase hands =
  let rec bidding_loop contracts current_contract pass turns =
    if pass < 3 || (pass = 3 && turns = 3) then (
      let contract =
        if turns mod 4 < List.length hands then
          let hand = List.nth hands (turns mod 4) in
          if turns mod 4 = 0 then
            contract_player hand current_contract (turns mod 4)
          else
            let partner_contract =
              if turns >= 2 && List.length contracts > turns mod 4 then
                Some
                  (List.nth contracts
                     (List.length contracts - 1 - (turns mod 4)))
              else None
            in
            contract_ai hand contracts current_contract partner_contract
              (turns mod 4)
        else { strain = Pass; level = 0; position = -1 }
      in
      let new_pass =
        if
          contract.position = -1 || contract.level = 0 || contract.strain = Pass
        then pass + 1
        else 0
      in
      let new_contracts = contracts @ [ contract ] in
      Printf.printf "%s has bid: Level %d, Strain %s\n\n"
        (int_to_ai (turns mod 4))
        contract.level
        (strain_to_string contract.strain);
      bidding_loop new_contracts
        (if new_pass = 0 then contract else current_contract)
        new_pass (turns + 1))
    else current_contract
  in
  bidding_loop [] { strain = Pass; level = 0; position = -1 } 0 0

let get_declarer contracts current_contract =
  let rec helper contracts current_strain index =
    match contracts with
    | [] -> -1
    | { strain; _ } :: tail ->
        if strain = current_strain then index
        else helper tail current_strain (index + 1)
  in
  helper contracts current_contract.strain 0

(********************************* Playing Phase ***********************************)
let highest_card trick contract =
  match trick with
  | [] -> None
  | first_card :: _ ->
      let led_suit = get_suit first_card in
      let trump_suit = contract.strain in

      List.fold_left
        (fun (highest_index, highest_card) (i, current_card) ->
          match highest_card with
          | None -> (Some i, Some current_card)
          | Some hc ->
              if get_suit current_card = trump_suit && get_suit hc <> trump_suit
              then (Some i, Some current_card)
              else if
                get_suit current_card = trump_suit && get_suit hc = trump_suit
              then
                if get_value current_card > get_value hc then
                  (Some i, Some current_card)
                else (highest_index, highest_card)
              else if get_suit current_card = led_suit then
                if
                  get_suit hc <> trump_suit
                  && get_value current_card > get_value hc
                  || get_suit hc <> led_suit
                then (Some i, Some current_card)
                else (highest_index, highest_card)
              else (highest_index, highest_card))
        (None, None)
        (List.mapi (fun i card -> (i, card)) trick)
      |> fst

let display_hand hand =
  let sorted_hand = sort_hand hand in
  List.iteri
    (fun index card -> printf "%d: %s\n" index (string_of_card card))
    sorted_hand

let rec choose_card hand contract trick =
  printf "Your hand:\n";
  display_hand hand;
  printf "Choose a card to play (enter index): ";
  try
    let index = read_int () in
    if index < 0 || index >= List.length hand then (
      printf "Invalid index. Please choose a valid card.\n";
      choose_card hand contract trick)
    else List.nth hand index
  with Failure _ ->
    printf "Invalid input. Please enter a valid number.\n";
    choose_card hand contract trick

let lowest_card hand =
  List.fold_left
    (fun acc card ->
      match acc with
      | None -> Some card
      | Some lowest -> if card.value < lowest.value then Some card else acc)
    None hand

let suit_to_index suit =
  match suit with
  | Diamonds -> 0
  | Clubs -> 1
  | Hearts -> 2
  | Spades -> 3
  | NoTrump -> 4
  | Pass -> 5

let ai_make_trick current_hand contract tricks =
  let calculate_comp hand =
    let count_suit s =
      List.fold_left
        (fun acc card -> if get_suit card = s then acc + 1 else acc)
        0 hand
    in
    [
      count_suit Diamonds;
      count_suit Clubs;
      count_suit Hearts;
      count_suit Spades;
      count_suit NoTrump;
      count_suit Pass;
    ]
  in

  let cur_suit =
    match tricks with
    | [] -> None
    | first_card :: _ -> Some (get_suit first_card)
  in
  let winning_index_option = highest_card tricks contract in
  let winning_value =
    match winning_index_option with
    | Some index -> get_value (List.nth tricks index)
    | None -> 0
  in

  let people = List.length tricks in

  let comp = calculate_comp current_hand in

  let play_card_if_partner_lost () =
    let partner_lost =
      (people = 2 && winning_index_option = Some 1)
      || (people = 3 && winning_index_option = Some 2)
    in

    if partner_lost then
      match cur_suit with
      | Some suit ->
          let cur_suit_index = suit_to_index suit in
          if List.nth comp cur_suit_index > 0 then
            let your_max_card_option = highest_in_suit current_hand suit in
            let your_min_card_option = lowest_in_suit current_hand suit in
            match (your_max_card_option, your_min_card_option) with
            | Some max_card, Some min_card ->
                let your_max_value = get_value max_card in
                if winning_value < your_max_value then Some max_card
                else Some min_card
            | _, _ -> None
          else None
      | None -> None
    else None
  in

  let play_card_otherwise () =
    match cur_suit with
    | Some suit -> (
        let cur_suit_index = suit_to_index suit in
        if List.nth comp cur_suit_index > 0 then
          match lowest_in_suit current_hand suit with
          | Some card -> Some card
          | None -> None
        else
          match lowest_card current_hand with
          | Some card -> Some card
          | None -> None)
    | None -> lowest_card current_hand
  in

  match play_card_if_partner_lost () with
  | Some card -> card
  | None -> (
      match play_card_otherwise () with
      | Some card -> card
      | None -> failwith "AI cannot play any card")

let rec player_trick hand contract trick =
  let has_suit =
    match trick with
    | [] -> false
    | first_card :: _ ->
        List.exists (fun card -> get_suit card = get_suit first_card) hand
  in

  let rec get_card_choice () =
    let display_hand_with_indices hand =
      List.iteri
        (fun index card -> Printf.printf "%d: %s\n" index (string_of_card card))
        hand
    in
    print_endline "Your hand:";
    display_hand_with_indices hand;

    Printf.printf "Choose a card to play (enter index): ";
    try
      let index = read_int () in
      if index < 0 || index >= List.length hand then (
        print_endline "Invalid index. Please try again.";
        get_card_choice ())
      else
        let chosen_card = List.nth hand index in
        match trick with
        | [] -> chosen_card
        | first_card :: _ ->
            if has_suit && get_suit chosen_card <> get_suit first_card then (
              print_endline
                "Invalid card choice: must follow suit. Please try again.";
              Printf.printf "Suit of the first card in the trick: %s\n"
                (get_suit first_card |> suit_to_string);
              player_trick hand contract trick)
            else chosen_card
    with Failure _ ->
      print_endline "Invalid input. Please enter a valid number.";
      get_card_choice ()
  in

  get_card_choice ()

let compare_cards contract card1 card2 =
  let trump_suit = contract.strain in
  let card1_suit = get_suit card1 in
  let card2_suit = get_suit card2 in
  let card1_value = get_value card1 in
  let card2_value = get_value card2 in
  if card1_suit = trump_suit && card2_suit <> trump_suit then 1
  else if card1_suit <> trump_suit && card2_suit = trump_suit then -1
  else if card1_suit = trump_suit && card2_suit = trump_suit then
    if card1_value > card2_value then 1 else -1
  else if card1_suit = card2_suit then
    if card1_value > card2_value then 1 else -1
  else 0

let determine_trick_winner win_pos attacker =
  if attacker then win_pos = 0 || win_pos = 2 else win_pos = 1 || win_pos = 3

let new_hand hand played_card =
  List.filter (fun card -> card <> played_card) hand

let determine_attacker_status game =
  let player_position = 0 in
  game.declarer = player_position

let rec play_round hands contract turns positions tricks attacker =
  if List.length tricks < 4 then (
    let hand_index = turns mod 4 in
    let current_hand = List.nth hands hand_index in
    Printf.printf "%s's turn. Hand: %s\n" (int_to_ai hand_index)
      (hand_to_string current_hand);
    let played_card =
      if hand_index = 0 then player_trick current_hand contract tricks
      else ai_make_trick current_hand contract tricks
    in
    Printf.printf "*** %s plays: %s\n" (int_to_ai hand_index)
      (string_of_card played_card);
    let updated_hand = new_hand current_hand played_card in
    let updated_hands =
      List.mapi (fun i h -> if i = hand_index then updated_hand else h) hands
    in
    let updated_tricks = tricks @ [ played_card ] in
    let updated_positions = positions @ [ hand_index ] in
    play_round updated_hands contract (turns + 1) updated_positions
      updated_tricks attacker)
  else
    let best_card_index_option = highest_card tricks contract in
    match best_card_index_option with
    | Some best_card_index ->
        let winner_turn = List.nth positions best_card_index in
        let trick_was_made = determine_trick_winner winner_turn attacker in
        let round_score = if trick_was_made then 1 else 0 in
        Printf.printf "%s wins the trick!\n\n\n\n" (int_to_ai winner_turn);

        (hands, positions, [], round_score, winner_turn)
    | None -> failwith "No highest card found in the trick"

let rec play_rounds hands contract current_player rounds score positions tricks
    attacker =
  if rounds < 13 then
    let new_hands, new_positions, new_tricks, round_score, winner_turn =
      play_round hands contract current_player positions tricks attacker
    in
    play_rounds new_hands contract winner_turn (rounds + 1)
      (score + round_score) new_positions new_tricks attacker
  else score

let play_phase game hand_list contract =
  let attacker = determine_attacker_status game in
  play_rounds hand_list contract 1 0 0 [] [] attacker
