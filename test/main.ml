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
