type suit = Diamonds | Clubs | Hearts | Spades | NoTrump | Pass
type card = { suit : suit; value : int }

let make_card s v = { suit = s; value = v }
let get_suit c = c.suit
let get_value c = c.value

let get_hcp c =
  let hcp = c.value - 10 in
  if hcp > 0 then hcp else 0

let get_sorting_val c =
  match c.suit with
  | Spades -> 30
  | Hearts -> 20
  | Clubs -> 10
  | Diamonds -> 0
  | _ -> -1

let get_rank_val c =
  match c.suit with
  | Spades -> 3
  | Hearts -> 2
  | Clubs -> 0
  | Diamonds -> 1
  | _ -> -1

let get_char c =
  match c.suit with
  | Spades -> 'S'
  | Hearts -> 'H'
  | Clubs -> 'C'
  | Diamonds -> 'D'
  | NoTrump -> 'T'
  | Pass -> 'P'

let char_to_suit c =
  match c with
  | 'C' -> Clubs
  | 'D' -> Diamonds
  | 'H' -> Hearts
  | 'S' -> Spades
  | 'T' -> NoTrump
  | 'P' -> Pass
  | _ -> failwith "Invalid suit"

let string_of_value v =
  match v with
  | 11 -> "J"
  | 12 -> "Q"
  | 13 -> "K"
  | 14 -> "A"
  | _ -> string_of_int v

let string_of_char ch = String.make 1 ch

let string_of_card c =
  (c |> get_char |> string_of_char) ^ (c |> get_value |> string_of_value)

let suit_to_string s =
  match s with
  | Spades -> "Spades"
  | Hearts -> "Hearts"
  | Clubs -> "Clubs"
  | Diamonds -> "Diamonds"
  | NoTrump -> "No Trump"
  | Pass -> "Pass"
