open Card

type hand = card list

let make_hand cards =
  if List.length cards <> 13 then failwith "Invalid hand" else cards

let get_hcp h = List.fold_left (fun acc c -> acc + get_hcp c) 0 h

let get_card_comp h =
  let count_suit s =
    List.fold_left (fun acc c -> if get_suit c = s then acc + 1 else acc) 0 h
  in
  [
    count_suit Clubs; count_suit Diamonds; count_suit Hearts; count_suit Spades;
  ]

let highest_in_suit h suit =
  List.fold_left
    (fun (max_val, max_card) c ->
      if get_suit c = suit && get_value c > max_val then (get_value c, Some c)
      else (max_val, max_card))
    (0, None) h
  |> snd

let lowest_in_suit h suit =
  List.fold_left
    (fun (min_val, min_card) c ->
      if get_suit c = suit && get_value c < min_val then (get_value c, Some c)
      else (min_val, min_card))
    (15, None) h
  |> snd

let has_suit h suit = List.exists (fun c -> get_suit c = suit) h

let sort_hand h =
  let compare_cards c1 c2 =
    match (get_suit c1, get_suit c2) with
    | Spades, Spades | Hearts, Hearts | Clubs, Clubs | Diamonds, Diamonds ->
        compare (get_value c2) (get_value c1)
    | s1, s2 -> compare s1 s2
  in
  List.rev (List.sort compare_cards h)

let remove_suit h suit = List.filter (fun c -> get_suit c <> suit) h

let hand_to_string h =
  let rec hand_to_string_helper h =
    let h = sort_hand h in
    match h with
    | [] -> ""
    | c :: t -> string_of_card c ^ " " ^ hand_to_string_helper t
  in
  hand_to_string_helper h

let print_hand h =
  let rec print_cards h =
    match h with
    | [] -> ()
    | c :: t ->
        print_string (string_of_card c);
        print_string " ";
        print_cards t
  in
  print_cards h;
  print_string "\n"
