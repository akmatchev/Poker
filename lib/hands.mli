(**This file is the representation type for different categories of 5-card hands
   and the logic for determining the best possible hand for players*)

open Cards
open Player

(**Type for representing different hand categories*)
type category =
  | High of {
      hcard1 : int;
      hcard2 : int;
      hcard3 : int;
      hcard4 : int;
      hcard5 : int;
    }
  | One_Pair of {
      pair : int;
      hcard1 : int;
      hcard2 : int;
      hcard3 : int;
    }
  | Two_Pair of {
      pair1 : int;
      pair2 : int;
      hcard : int;
    }
  | Three_Of_A_Kind of {
      three_kind : int;
      hcard1 : int;
      hcard2 : int;
    }
  | Straight of { hcard : int }
  | Flush of {
      hcard1 : int;
      hcard2 : int;
      hcard3 : int;
      hcard4 : int;
      hcard5 : int;
    }
  | Full_House of {
      three_kind : int;
      pair : int;
    }
  | Four_Of_A_Kind of {
      four_kind : int;
      hcard : int;
    }
  | Straight_Flush of { hcard : int }
  | Royal_Flush

val category_to_string : category -> string
(**[category_to_string] converts a category to its string representation*)

val compare_by_rank : card -> card -> int
(**[compare_by_rank] compares cards by their rank*)

val sort_by_rank : card list -> card list
(**[sort_by_rank] sorts card by their rank in descending order*)

val cards_to_ranks : card list -> int list
(**[cards_to_ranks] converts a list of cards to a list of their respective ranks*)

val card_combinations : card list -> card list -> card list list
(**[card_combinations] lists all possible 5-card hands a player can achieve*)

val flush_checker : card list -> bool
(**[flush_checker] checks if a hand is a flush*)

val straight_checker : card list -> int
(**[straight_checker] checks if a hand is a straight*)

val is_royal_flush : card list -> category option
(**[is_royal_flush] determines whether a hand is a royal flush*)

val is_straight_flush : card list -> category option
(**[is_straight_flush] determines whether a hand is a straight flush*)

val is_four_of_kind : card list -> category option
(**[is_four_of_kind] determines whether a hand is a four of a kind*)

val is_full_house : card list -> category option
(**[is_full_house] determines whether a hand is a full house*)

val is_flush : card list -> category option
(**[is_flush] determines if a hand is at best a flush*)

val is_straight : card list -> category option
(**[is_straight] determines if a hand is at best a straight*)

val is_three_of_kind : card list -> category option
(**[is_three_of_kind] determines if a hand is at best a three of a kind hand*)

val is_two_pair : card list -> category option
(**[is_two_pair] determines if a hand is at best a two pair hand*)

val is_pair : card list -> category option
(**[is_pair] determines if a hand is at best a pair hand*)

val is_high_card : card list -> category option
(**[is_high_card] determines if a hand is at bet a high card hand*)

val hand_category : card list -> category
(**[hand_category] determines what type of category a hand is*)

val compare_hands : category -> category -> int
(**[compare_hands] is a comparison function for hands*)

val possible_hands : card list -> card list -> category list
(**[possible_hands] is a list of all possible 5 card hands a player can achieve
   given the board and the player's hand*)

val best_player_hand : card list -> card list -> category
(**[best_player_hand] is the best possible hand category a player can achieve
   given the board and the player's hand*)

val print_best_player_hand : card list -> player -> unit
(**[print_best_player_hand] prints the best possible 5-card hand category a
   player has*)

val best_hand : card list -> player -> player -> player option
(**[best_hand] compares which player has the better hand in a given game of
   poker*)
