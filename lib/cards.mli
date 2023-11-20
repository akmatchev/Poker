type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades

type card = {
  rank : int;
  suit : suit;
}

val empty : card list
val full_deck : card list
val shuffle_deck : card list -> card list
val top_card : card list -> card
val card_to_ascii : card -> string
val cards_to_ascii : card list -> string
val print_card : card -> unit
val print_cards : card list -> unit
val draw_card : card list -> card list
val draw_flop : card list -> card list * card list
val draw_turn_river : card list -> card * card list
