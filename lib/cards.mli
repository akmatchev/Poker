(**This module is the representation type for cards and different actions one
   can take with a deck of cards*)

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
(**Abstract type for representing a card*)

val suits : suit list

val empty : card list
(**[empty] is the representation of an empty deck*)

val full_deck : card list
(**[full_deck] is the representation of a full deck of cards that is not
   shuffled*)

val shuffle_deck : card list -> card list
(**[shuffle_deck] shuffles a given deck of cards*)

val top_card : card list -> card
(**[top_card] returns the card at the top of a deck*)

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

val burn_card : card list -> card list
(**[burn_card] burns the card from the top of the deck and returns the resulting
   deck*)
val suit_to_string : suit -> string

val one_pair : card list -> int option
(**[one_pair] if list of cards has pair returns option of rank of pair, else option of None*)
val two_pair : card list -> (int * int) option
(**[two_pair] if list of cards has two pair returns option of rank of both pairs pair, else option of None*)
val three_of_kind : card list -> int option
(**[three_of_kind] if list of cards has three of kind returns option of rank of three of a kind, else option of None*)
val four_of_kind : card list -> int option
(**[four_of_kind] if list of cards has three of kind returns option of rank of four of a kind, else option of None*)
val full_house : card list -> (int * int) option
(**[full_house] if list of cards has full house returns option of rank of pair and three of a kind, else option of None*)

val draw_flop : card list -> card list * card list
(**[drop_flop] draws a flop from the given deck of cards. It returns the three
   cards of the flop as well as the resulting deck*)

val draw_turn_river : card list -> card * card list
(**[draw_turn_river] is a function that is used to draw both the turn and river
   cards for a particular round*)
