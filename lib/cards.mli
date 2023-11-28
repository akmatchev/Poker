(**This file is the representation type for cards and different actions one can
   take with a deck of cards*)

(**Type for representing the suit of a card*)
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
(**[empty] is the representation of an empty deck*)

val ranks : int list
(**[ranks] is a list of all possible ranks*)

val suits : suit list
(**[suits] is a list of all possible suits*)

val top_card : card list -> card
(**[top_card] returns the card at the top of a deck*)

val full_deck : card list
(**[full_deck] is the representation of a full deck of cards that is not
   shuffled*)

val shuffle_deck : card list -> card list
(**[shuffle_deck] shuffles a given deck of cards*)

val suit_to_string : suit -> string
(**[suit_to_string] converts the suit of a card to its string representation.
   Used in printing cards*)

val rank_to_string : int -> string
(**[rank_to_string] converts the rank of a card to a string representation.*)

val card_to_ascii : card -> string
(**[card_to_ascii] converts are card to an ascii art representation of the card*)

val cards_to_ascii : card list -> string
(**[cards_to_ascii] converts are list of cards to their ascii art
   representations side-by-side*)

val print_card : card -> unit
(**[print_card] prints the ascii art representation of a card*)

val print_cards : card list -> unit
(**[print_cards] prints the ascii art representations of multiple cards
   side-by-side*)

val draw_card : card list -> card list
(**[draw_card] draws a card from the top of the deck and returns the resulting
   deck*)

val draw_flop : card list -> card list * card list
(**[drop_flop] draws a flop from the given deck of cards. It returns the three
   cards of the flop as well as the resulting deck*)

val draw_turn_river : card list -> card * card list
(**[draw_turn_river] is a function that is used to draw both the turn and river
   cards for a particular round*)
