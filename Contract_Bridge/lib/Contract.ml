open Card

type contract = { strain : suit; level : int; position : int }

let make_contract s l p = { strain = s; level = l; position = p }
let get_strain c = c.strain
let get_level c = c.level

let print_contract c =
  let s = get_strain c in
  let l = get_level c in
  let s_str = Card.suit_to_string s in
  let l_str = string_of_int l in
  print_string (l_str ^ s_str)
