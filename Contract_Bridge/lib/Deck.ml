open Card

type deck = card list

let initialize_deck () =
  let all_suits = [ Diamonds; Clubs; Hearts; Spades ] in
  let all_values = List.init 13 (fun i -> i + 2) in
  List.flatten
    (List.map (fun s -> List.map (fun v -> make_card s v) all_values) all_suits)

let () = Random.self_init ()

let shuffle (d : deck) =
  let rec shuffle_helper d acc =
    match d with
    | [] -> acc
    | h :: t ->
        let rand = Random.int (List.length acc + 1) in
        let rec insert_at lst n c =
          match lst with
          | [] -> [ c ]
          | h :: t -> if n = 0 then c :: h :: t else h :: insert_at t (n - 1) c
        in
        shuffle_helper t (insert_at acc rand h)
  in
  shuffle_helper d []

let get_cards d = d

let get_thirteen d =
  let rec get_thirteen_helper d acc =
    match d with
    | [] -> acc
    | h :: t ->
        if List.length acc = 13 then acc else get_thirteen_helper t (h :: acc)
  in
  get_thirteen_helper d []

let print_deck d =
  let print_card c =
    let s =
      match c.suit with
      | Diamonds -> "D"
      | Clubs -> "C"
      | Hearts -> "H"
      | Spades -> "S"
      | NoTrump -> "T"
      | Pass -> "P"
    in
    let v =
      match c.value with
      | 11 -> "J"
      | 12 -> "Q"
      | 13 -> "K"
      | 14 -> "A"
      | _ -> string_of_int c.value
    in
    print_string (s ^ v ^ " ")
  in
  let rec print_cards d =
    match d with
    | [] -> ()
    | h :: t ->
        print_card h;
        print_cards t
  in
  print_cards d
