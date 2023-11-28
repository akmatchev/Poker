open Cards

open Player
(**This file is the representation type for cards and different actions one can
   take with a deck of cards*)

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
val compare_by_rank : card -> card -> int
val sort_by_rank : card list -> card list
val cards_to_ranks : card list -> int list
val card_combinations : card list -> card list -> card list list
val flush_checker : card list -> bool
val straight_checker : card list -> int
val is_royal_flush : card list -> category option
val is_straight_flush : card list -> category option
val is_four_of_kind : card list -> category option
val is_full_house : card list -> category option
val is_flush : card list -> category option
val is_straight : card list -> category option
val is_three_of_kind : card list -> category option
val is_two_pair : card list -> category option
val is_pair : card list -> category option
val is_high_card : card list -> category option
val hand_category : card list -> category
val compare_hands : category -> category -> int
val possible_hands : card list -> card list -> category list
val best_player_hand : card list -> card list -> category
val print_best_player_hand : card list -> player -> unit
val better_hand : card list -> player -> player -> int
