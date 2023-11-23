open Poker
open Cards

let royal_flush =
  [
    { rank = 10; suit = Spades };
    { rank = 11; suit = Spades };
    { rank = 12; suit = Spades };
    { rank = 13; suit = Spades };
    { rank = 14; suit = Spades };
  ]

let () = print_cards royal_flush
let starter_deck = shuffle_deck full_deck
let () = print_cards starter_deck
let flop, post_flop_deck = draw_flop starter_deck
let () = print_cards flop
let turn_card, post_turn_deck = draw_turn_river post_flop_deck
let turn = flop @ [ turn_card ]
let () = print_cards turn
let river_card, post_river_deck = draw_turn_river post_turn_deck
let river = turn @ [ river_card ]
let () = print_cards river

let simple_one_pair =
  [
    { rank = 1; suit = Clubs };
    { rank = 2; suit = Diamonds };
    { rank = 3; suit = Diamonds };
    { rank = 11; suit = Spades };
    { rank = 5; suit = Clubs };
    { rank = 6; suit = Clubs };
    { rank = 1; suit = Diamonds };
  ]

let simple_two_pair =
  [
    { rank = 3; suit = Clubs };
    { rank = 2; suit = Diamonds };
    { rank = 3; suit = Diamonds };
    { rank = 11; suit = Spades };
    { rank = 5; suit = Clubs };
    { rank = 6; suit = Clubs };
    { rank = 6; suit = Diamonds };
  ]

let simple_three_of_a_kind =
  [
    { rank = 3; suit = Clubs };
    { rank = 6; suit = Spades };
    { rank = 3; suit = Diamonds };
    { rank = 11; suit = Spades };
    { rank = 5; suit = Clubs };
    { rank = 6; suit = Clubs };
    { rank = 6; suit = Diamonds };
  ]

let () =
  print_endline
    (string_of_int (Option.value (one_pair simple_one_pair) ~default:0))

let part1 = Option.value (two_pair simple_two_pair) ~default:(0, 0)

let () =
  print_endline (string_of_int (fst part1) ^ "," ^ string_of_int (snd part1))

let () =
  print_endline
    (string_of_int
       (Option.value (three_of_kind simple_three_of_a_kind) ~default:0))
