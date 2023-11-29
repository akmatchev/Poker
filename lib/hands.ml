open Cards
open Player

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

(**[category_to_string] converts a category to its string representation.
   Returns: the string of a category. Requires: input is a valid category*)
let category_to_string = function
  | High _ -> "High_Card"
  | One_Pair _ -> "One_Pair"
  | Two_Pair _ -> "Two_Pair"
  | Three_Of_A_Kind _ -> "Three_Of_A_Kind"
  | Straight _ -> "Straight"
  | Flush _ -> "Flush"
  | Full_House _ -> "Full_House"
  | Four_Of_A_Kind _ -> "Four_Of_A_Kind"
  | Straight_Flush _ -> "Straight_Flush"
  | Royal_Flush -> "Royal_Flush"

(**[compare_by_rank] compares cards by their rank. Returns a negative integer if
   [card1]'s rank is less than [card2]'s rank, 0 is [card1]'s rank is equal to
   [card2]'s rank, and a positive integer is [card1]'s rank is greater than
   [card2]'s rank. Requires: [card1] and [card2] are valid cards*)
let compare_by_rank (card1 : card) (card2 : card) : int =
  compare card1.rank card2.rank

(**[sort_by_rank] sorts card by their rank in descending order. Returns: a
   sorted list of cards by rank in descending order. Requires: cards have a
   valid rank*)
let sort_by_rank (hand : card list) : card list =
  List.rev (List.sort compare_by_rank hand)

(**[cards_to_ranks] converts a list of cards to a list of their respective
   ranks. Returns: a list of ranks of cards*)
let cards_to_ranks (hand : card list) : int list =
  List.map (fun card -> card.rank) hand

(**[card_combinations] lists all possible 5-card hands a player can achieve
   given the [board] and [player_hand]. Returns: a list of all possible 5-card
   combinations. Requires: [board] and [player_hand] do not contain any
   duplicated cards*)
let card_combinations (board : card list) (player_hand : card list) :
    card list list =
  let rec combinations k lst =
    if k = 0 then [ [] ]
    else
      match lst with
      | [] -> []
      | hd :: tl ->
          let with_hd =
            List.map (fun rest -> hd :: rest) (combinations (k - 1) tl)
          in
          let without_hd = combinations k tl in
          with_hd @ without_hd
  in
  combinations 5 (board @ player_hand)

(**[flush_checker] checks if a hand is a flush. Returns: true is a flush on the
   hand exists*)
let flush_checker (hand : card list) : bool =
  let rec flush_helper = function
    | [] | [ _ ] -> true
    | h :: (h2 :: t1 as t) -> if h.suit = h2.suit then flush_helper t else false
  in
  flush_helper hand

(**[straight_checker] checks if a hand is a straight. Requires: [hand] is a
   valid hand*)
let straight_checker (hand : card list) : int =
  let sorted_hand = sort_by_rank hand in
  let sorted_hand_ranks = cards_to_ranks sorted_hand in
  if sorted_hand_ranks = [ 14; 5; 4; 3; 2 ] then -100
  else
    let rec is_d_sequence = function
      | [] | [ _ ] -> true
      | h1 :: (h2 :: t1 as t) -> h1 = h2 + 1 && is_d_sequence t
    in
    if is_d_sequence sorted_hand_ranks then List.hd sorted_hand_ranks else 0

(**[is_royal_flush] determines whether a hand is a royal flush. Returns: Some
   Royal_Flush if a hand is a royal flush, None otherwise. Requires: [hand] is a
   valid hand*)
let is_royal_flush (hand : card list) : category option =
  if flush_checker hand then
    let lst = [ 14; 13; 12; 11; 10 ] in
    let sorted_hand_ranks =
      List.map (fun card -> card.rank) (sort_by_rank hand)
    in
    if lst = sorted_hand_ranks then Some Royal_Flush else None
  else None

(**[is_straight_flush] determines whether a hand is a straight flush. Returns:
   Some Straight_Flush (hcard1) where (hcard1) is the highest ranked card in the
   hand. Requires: [hand] is a valid hand*)
let is_straight_flush (hand : card list) : category option =
  if flush_checker hand && is_royal_flush hand = None then
    let checker = straight_checker hand in
    if checker = 0 then None
    else
      let sorted_hand = sort_by_rank hand in
      let sorted_hand_ranks = cards_to_ranks sorted_hand in
      if sorted_hand_ranks = [ 14; 5; 4; 3; 2 ] then
        Some (Straight_Flush { hcard = -100 })
      else Some (Straight_Flush { hcard = checker })
  else None

(**[is_four_of_kind] determines whether a hand is a four of a kind. Returns:
   Some Four_Of_A_Kind (four_kind; hcard) is the hand is a four of a kind, where
   four_kind is the rank of the four of a kind, and hcard is the rank of the
   card not included in the four of a kind, and None otherwise. Requires: [hand]
   is a valid hand*)
let is_four_of_kind (hand : card list) : category option =
  let sorted_hand = sort_by_rank hand in
  match sorted_hand with
  | [ a; b; c; d; e ] when a.rank = b.rank && b.rank = c.rank && c.rank = d.rank
    -> Some (Four_Of_A_Kind { four_kind = a.rank; hcard = e.rank })
  | [ a; b; c; d; e ] when b.rank = c.rank && c.rank = d.rank && d.rank = e.rank
    -> Some (Four_Of_A_Kind { four_kind = b.rank; hcard = a.rank })
  | _ -> None

(**[is_full_house] determines whether a hand is a full house. Returns: Some
   Full_House (three_kind; pair) is the hand is a full house, where three_kind
   is the rank of the three of a kind, and pair is the rank of the pair in the
   full house, and None otherwise. Requires: [hand] is a valid hand*)
let is_full_house (hand : card list) : category option =
  let sorted_hand = sort_by_rank hand in
  match sorted_hand with
  | [ a; b; c; d; e ] when a.rank = b.rank && b.rank = c.rank && d.rank = e.rank
    -> Some (Full_House { three_kind = a.rank; pair = d.rank })
  | [ a; b; c; d; e ] when a.rank = b.rank && c.rank = d.rank && d.rank = e.rank
    -> Some (Full_House { three_kind = c.rank; pair = a.rank })
  | _ -> None

(**[is_flush] determines if a hand is at best a flush. Returns: Some Flush
   (hcard1; hcard2; hcard3; hcard4; hcard5) is the hand is a flush, where hcard1
   -> hcard5 are the ranks of the cards in descending order, and None otherwise.
   Requires: [hand] is a valid hand*)
let is_flush (hand : card list) : category option =
  if flush_checker hand = true && is_straight_flush hand = None then
    let sorted_hand_ranks = cards_to_ranks (sort_by_rank hand) in
    match sorted_hand_ranks with
    | [ a; b; c; d; e ] ->
        Some
          (Flush { hcard1 = a; hcard2 = b; hcard3 = c; hcard4 = d; hcard5 = e })
    | _ -> None
  else None

(**[is_straight] determines if a hand is at best a straight. Returns: Some
   Straight (hcard) if the hand is a straight, where hcard is the rank of the
   highest card that is part of the straight, and None otherwise. Requires:
   [hand] is a valid hand*)
let is_straight (hand : card list) : category option =
  if straight_checker hand <> 0 && is_straight_flush hand = None then
    let sorted_hand = sort_by_rank hand in
    let sorted_hand_ranks = cards_to_ranks sorted_hand in
    if sorted_hand_ranks = [ 14; 5; 4; 3; 2 ] then
      Some (Straight { hcard = -100 })
    else
      let top = List.hd sorted_hand in
      Some (Straight { hcard = top.rank })
  else None

(**[is_three_of_kind] determines if a hand is at best a three of a kind hand.
   Returns: Some Three_Of_A_Kind (three_kind; hcard1; hcard2) if the hand is a
   three of a kind, where three_kind is the rank of the cards that form the
   three of a kind, and hcard1, hcard2 are the ranks of the remaining cards in
   descending order. Requires: [hand] is a valid hand*)
let is_three_of_kind (hand : card list) : category option =
  if is_four_of_kind hand = None && is_full_house hand = None then
    let sorted_hand = sort_by_rank hand in
    match sorted_hand with
    | [ a; b; c; d; e ] when a.rank = b.rank && b.rank = c.rank ->
        Some
          (Three_Of_A_Kind
             { three_kind = a.rank; hcard1 = d.rank; hcard2 = e.rank })
    | [ a; b; c; d; e ] when b.rank = c.rank && c.rank = d.rank ->
        Some
          (Three_Of_A_Kind
             { three_kind = b.rank; hcard1 = a.rank; hcard2 = e.rank })
    | [ a; b; c; d; e ] when c.rank = d.rank && d.rank = e.rank ->
        Some
          (Three_Of_A_Kind
             { three_kind = c.rank; hcard1 = a.rank; hcard2 = b.rank })
    | _ -> None
  else None

(**[is_two_pair] determines if a hand is at best a two pair hand. Returns Some
   Two_Pair (pair1; pair2; hcard) if the hand is a two pair, where pair1 is the
   rank of the pair with the higher rank, pair2 is the pair with the lower rank,
   and hcard is the rank of the remaining card, and None otherwise. Requires:
   [hand] is a valid hand*)
let is_two_pair (hand : card list) : category option =
  if is_full_house hand = None then
    let sorted_hand = sort_by_rank hand in
    match sorted_hand with
    | [ a; b; c; d; e ] when a.rank = b.rank && c.rank = d.rank ->
        Some (Two_Pair { pair1 = a.rank; pair2 = c.rank; hcard = e.rank })
    | [ a; b; c; d; e ] when a.rank = b.rank && d.rank = e.rank ->
        Some (Two_Pair { pair1 = a.rank; pair2 = d.rank; hcard = c.rank })
    | [ a; b; c; d; e ] when b.rank = c.rank && d.rank = e.rank ->
        Some (Two_Pair { pair1 = b.rank; pair2 = d.rank; hcard = a.rank })
    | _ -> None
  else None

(**[is_pair] determines if a hand is at best a pair hand. Returns: Some One_Pair
   (pair; hcard1; hcard2; hcard3) if the hand is a pair, where pair is the rank
   of the pair, hcard1 -> hcard3 are the ranks of the remaining cards in
   descending order, and returns None otherwise. Requires: [hand] is a valid
   hand*)
let is_pair (hand : card list) : category option =
  if
    is_four_of_kind hand = None
    && is_full_house hand = None
    && is_three_of_kind hand = None
    && is_two_pair hand = None
  then
    let sorted_hand = sort_by_rank hand in
    match sorted_hand with
    | [ a; b; c; d; e ] when a.rank = b.rank ->
        Some
          (One_Pair
             {
               pair = a.rank;
               hcard1 = c.rank;
               hcard2 = d.rank;
               hcard3 = e.rank;
             })
    | [ a; b; c; d; e ] when b.rank = c.rank ->
        Some
          (One_Pair
             {
               pair = b.rank;
               hcard1 = a.rank;
               hcard2 = d.rank;
               hcard3 = e.rank;
             })
    | [ a; b; c; d; e ] when c.rank = d.rank ->
        Some
          (One_Pair
             {
               pair = c.rank;
               hcard1 = a.rank;
               hcard2 = b.rank;
               hcard3 = e.rank;
             })
    | [ a; b; c; d; e ] when d.rank = e.rank ->
        Some
          (One_Pair
             {
               pair = d.rank;
               hcard1 = a.rank;
               hcard2 = b.rank;
               hcard3 = c.rank;
             })
    | _ -> None
  else None

(**[is_high_card] determines if a hand is at bet a high card hand. Returns: Some
   (hcard1; hcard2; hcard3; hcard4; hcard5), where hcard1 -> hcard5 are the
   ranks of the cards in descending order, and None otherwise. Requires: [hand]
   is a valid hand *)
let is_high_card (hand : card list) : category option =
  if
    is_royal_flush hand = None
    && is_straight_flush hand = None
    && is_four_of_kind hand = None
    && is_full_house hand = None
    && is_flush hand = None
    && is_straight hand = None
    && is_three_of_kind hand = None
    && is_two_pair hand = None
    && is_pair hand = None
  then
    let sorted_hand = sort_by_rank hand in
    match sorted_hand with
    | [ a; b; c; d; e ] ->
        Some
          (High
             {
               hcard1 = a.rank;
               hcard2 = b.rank;
               hcard3 = c.rank;
               hcard4 = d.rank;
               hcard5 = e.rank;
             })
    | _ -> None
  else None

(**[hand_category] determines what type of category a hand is. Returns: the
   category that a hand is. Requires: [hand] is a valid hand*)
let hand_category (hand : card list) : category =
  match is_royal_flush hand with
  | Some rf -> rf
  | None -> (
      match is_straight_flush hand with
      | Some sf -> sf
      | None -> (
          match is_four_of_kind hand with
          | Some fkind -> fkind
          | None -> (
              match is_full_house hand with
              | Some fh -> fh
              | None -> (
                  match is_flush hand with
                  | Some fl -> fl
                  | None -> (
                      match is_straight hand with
                      | Some s -> s
                      | None -> (
                          match is_three_of_kind hand with
                          | Some tkind -> tkind
                          | None -> (
                              match is_two_pair hand with
                              | Some tpair -> tpair
                              | None -> (
                                  match is_pair hand with
                                  | Some p -> p
                                  | None -> (
                                      match is_high_card hand with
                                      | Some high -> high
                                      | None -> failwith "Imposible")))))))))

(**[compare_hands] is a comparison function for hands. Returns: a negative
   integer if [h1] is worse than [h2], 0 is [h1] is equal to [h2] and a positive
   integer if [h1] is better than [h2]*)
let compare_hands (h1 : category) (h2 : category) : int =
  match (h1, h2) with
  | Royal_Flush, Royal_Flush -> 0
  | Royal_Flush, _ -> 1
  | _, Royal_Flush -> -1
  | Straight_Flush s1, Straight_Flush s2 -> compare s1.hcard s2.hcard
  | Straight_Flush _, _ -> 1
  | _, Straight_Flush _ -> -1
  | Four_Of_A_Kind f1, Four_Of_A_Kind f2 ->
      if compare f1.four_kind f2.four_kind = 0 then compare f1.hcard f2.hcard
      else compare f1.four_kind f2.four_kind
  | Four_Of_A_Kind _, _ -> 1
  | _, Four_Of_A_Kind _ -> -1
  | Full_House fh1, Full_House fh2 ->
      let threes = compare fh1.three_kind fh2.three_kind in
      if threes = 0 then compare fh1.pair fh2.pair else threes
  | Full_House _, _ -> 1
  | _, Full_House _ -> -1
  | Flush f1, Flush f2 ->
      let c1 = compare f1.hcard1 f2.hcard1 in
      if c1 = 0 then
        let c2 = compare f1.hcard2 f2.hcard2 in
        if c2 = 0 then
          let c3 = compare f1.hcard3 f2.hcard3 in
          if c3 = 0 then
            let c4 = compare f1.hcard4 f2.hcard4 in
            if c4 = 0 then compare f1.hcard5 f2.hcard5 else c4
          else c3
        else c2
      else c1
  | Flush _, _ -> 1
  | _, Flush _ -> -1
  | Straight s1, Straight s2 -> compare s1.hcard s2.hcard
  | Straight _, _ -> 1
  | _, Straight _ -> -1
  | Three_Of_A_Kind t1, Three_Of_A_Kind t2 ->
      let threes = compare t1.three_kind t2.three_kind in
      if threes = 0 then
        let hc = compare t1.hcard1 t2.hcard1 in
        if hc = 0 then compare t1.hcard2 t2.hcard2 else hc
      else threes
  | Three_Of_A_Kind _, _ -> 1
  | _, Three_Of_A_Kind _ -> -1
  | Two_Pair tp1, Two_Pair tp2 ->
      let p1 = compare tp1.pair1 tp2.pair1 in
      if p1 = 0 then
        let p2 = compare tp1.pair2 tp2.pair2 in
        if p2 = 0 then compare tp1.hcard tp2.hcard else p2
      else p1
  | Two_Pair _, _ -> 1
  | _, Two_Pair _ -> -1
  | One_Pair p1, One_Pair p2 ->
      let pair = compare p1.pair p2.pair in
      if pair = 0 then
        let hc1 = compare p1.hcard1 p2.hcard1 in
        if hc1 = 0 then
          let hc2 = compare p1.hcard2 p2.hcard2 in
          if hc2 = 0 then compare p1.hcard3 p2.hcard3 else hc2
        else hc1
      else pair
  | One_Pair _, _ -> 1
  | _, One_Pair _ -> -1
  | High h1, High h2 ->
      let hc1 = compare h1.hcard1 h2.hcard1 in
      if hc1 = 0 then
        let hc2 = compare h1.hcard2 h2.hcard2 in
        if hc2 = 0 then
          let hc3 = compare h1.hcard3 h2.hcard3 in
          if hc3 = 0 then
            let hc4 = compare h1.hcard4 h2.hcard4 in
            if hc4 = 0 then compare h1.hcard5 h2.hcard5 else hc4
          else hc3
        else hc2
      else hc1

(**[possible_hands] is a list of all possible 5 card hands a player can achieve
   given the board and the player's hand. Returns: a list of hand categories
   that correspond to all of the different types of categories of hands a player
   can make with the give [board] and their [player_hand]. Requires: [board] and
   [player_hand] are valid boards and two card hands (no overlap/duplicates) *)
let possible_hands (board : card list) (player_hand : card list) : category list
    =
  List.map hand_category (card_combinations board player_hand)

(**[best_player_hand] is the best possible hand category a player can achieve
   given the board and the player's hand. Returns: the best possible category a
   player can achieve. Requires: [board] and [player_hand] are valid boards and
   two card hands (no overlap/duplicates)*)
let best_player_hand (board : card list) (player_hand : card list) : category =
  let pos_hands = possible_hands board player_hand in
  match pos_hands with
  | [] -> failwith "Not possible"
  | c1 :: rest ->
      List.fold_left
        (fun hand1 hand2 ->
          if compare_hands hand1 hand2 < 0 then hand2 else hand1)
        c1 rest

(**[print_best_player_hand] prints the best possible 5-card hand category a
   player has. Requires: [board] and [player_hand] are valid boards and two card
   hands (no overlap/duplicates)*)
let print_best_player_hand (board : card list) (player : player) : unit =
  print_endline (category_to_string (best_player_hand board player.hand))

(**[best_hand] compares which player has the better hand in a given game of
   poker. Returns: a negative integer if [player1] has a worse hand than
   [player2], 0 if their hands are equal, and 1 if [player1] has a better hand
   than [player2]. Requires: [board] and [player_hand] are valid boards and two
   card hands (no overlap/duplicates)*)

let best_hand (board : card list) (player1 : player) (player2 : player) :
    player option =
  let player1_hand = best_player_hand board player1.hand in
  let player2_hand = best_player_hand board player2.hand in
  let c = compare_hands player1_hand player2_hand in
  if c < 0 then Some player2 else if c = 0 then None else Some player1
