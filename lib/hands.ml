open Cards

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

let compare_by_rank card1 card2 = compare card1.rank card2.rank
let sort_by_rank (hand : card list) = List.rev (List.sort compare_by_rank hand)
let cards_to_ranks (hand : card list) = List.map (fun card -> card.rank) hand

(* Function that gets all the possible hands on the board a player could have *)
(* let possible_hands (board : card list) (player_hand : card list) = let rec
   card_combos lst = let rec card_combos_aux acc len lst = if len = 0 then [ []
   ] else match lst with | [] -> acc | h :: t -> let rec combo_map acc f =
   function | [] -> acc | h :: t -> combo_map (f h :: acc) f t in let gen =
   combo_map acc (fun z -> h :: z) (card_combos_aux [] (len - 1) t) in
   card_combos_aux gen len t in card_combos_aux [] 5 lst in card_combos [ board
   @ player_hand ] *)

let flush_checker (hand : card list) =
  let rec flush_helper = function
    | [] | [ _ ] -> true
    | h :: (h2 :: t1 as t) -> if h.suit = h2.suit then flush_helper t else false
  in
  flush_helper hand

let straight_checker (hand : card list) : int =
  let sorted_hand = sort_by_rank hand in
  let sorted_hand_ranks = cards_to_ranks sorted_hand in
  if sorted_hand_ranks = [ 14; 5; 4; 3; 2 ] then 100
  else
    let rec is_d_sequence = function
      | [] | [ _ ] -> true
      | h1 :: (h2 :: t1 as t) -> h1 = h2 + 1 && is_d_sequence t
    in
    if is_d_sequence sorted_hand_ranks then List.hd sorted_hand_ranks else 0

let is_royal_flush (hand : card list) : category option =
  if flush_checker hand then
    let lst = [ 14; 13; 12; 11; 10 ] in
    let sorted_hand_ranks =
      List.map (fun card -> card.rank) (sort_by_rank hand)
    in
    if lst = sorted_hand_ranks then Some Royal_Flush else None
  else None

let is_straight_flush (hand : card list) : category option =
  if flush_checker hand && is_royal_flush hand = None then
    let checker = straight_checker hand in
    if checker = 0 then None else Some (Straight_Flush { hcard = checker })
  else None

let is_four_of_kind (hand : card list) : category option =
  let sorted_hand = sort_by_rank hand in
  match sorted_hand with
  | [ a; b; c; d; e ] when a.rank = b.rank && b.rank = c.rank && c.rank = d.rank
    -> Some (Four_Of_A_Kind { four_kind = a.rank; hcard = e.rank })
  | [ a; b; c; d; e ] when b.rank = c.rank && c.rank = d.rank && d.rank = e.rank
    -> Some (Four_Of_A_Kind { four_kind = b.rank; hcard = a.rank })
  | _ -> None

let is_full_house (hand : card list) : category option =
  let sorted_hand = sort_by_rank hand in
  match sorted_hand with
  | [ a; b; c; d; e ] when a.rank = b.rank && b.rank = c.rank && d.rank = e.rank
    -> Some (Full_House { three_kind = a.rank; pair = d.rank })
  | [ a; b; c; d; e ] when a.rank = b.rank && c.rank = d.rank && d.rank = e.rank
    -> Some (Full_House { three_kind = c.rank; pair = d.rank })
  | _ -> None

let is_flush (hand : card list) : category option =
  if flush_checker hand = true && is_straight_flush hand = None then
    let sorted_hand_ranks = cards_to_ranks (sort_by_rank hand) in
    match sorted_hand_ranks with
    | [ a; b; c; d; e ] ->
        Some
          (Flush { hcard1 = a; hcard2 = b; hcard3 = c; hcard4 = d; hcard5 = e })
    | _ -> None
  else None

let is_straight (hand : card list) : category option =
  if straight_checker hand <> 0 && is_straight_flush hand = None then
    let top = List.hd (sort_by_rank hand) in
    Some (Straight { hcard = top.rank })
  else None

let is_three_of_kind (hand : card list) : category option =
  if is_four_of_kind hand = None then
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

(* Function to check if a hand is a pair *)
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

let high_card (hand : card list) : category option =
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

(* (**[one_pair_draft] returns rank of a pair in list of cards if pair exists,
   otherwise returns none. Returns: option of rank of pair if pair exists
   otherwise None.*) let one_pair_draft h = let rec helper h1 = match h1 with |
   [] -> None | hd :: t -> ( match List.find_opt (fun x -> if x.rank = hd.rank
   then true else false) t with | None -> helper t | Some a -> Some a.rank) in
   helper h

   (**[two_pair_draft] returns rank of a two disinct pairs in list of cards if
   they exist, otherwise returns none. Returns: option tuple of rank of both
   pairs if pairs exists otherwise None.*) let two_pair_draft h = let rankin =
   one_pair_draft h in match rankin with | None -> None | Some a -> ( let rest =
   List.find_all (fun x -> if x.rank <> a then true else false) h in match
   one_pair_draft rest with | None -> None | Some b -> Some (a, b))

   (**[three_of_kind_draft] returns rank of a three of kind in list of cards if
   they exist, otherwise returns none. Returns: option tuple of rank of both
   pairs if pairs exists otherwise None.*) let three_of_kind_draft h = let rec
   helper h1 = match h1 with | [] -> None | hd :: t -> ( match one_pair_draft t
   with | None -> helper t | Some a -> if hd.rank = a then Some a else helper t)
   in helper h

   (**[hand_category] classifies 7-card hand as a category of poker hands and
   returns this category. Returns: category. Requires: the size of hand is 7*)
   let hand_category h = h

   (**[compare_hand] compares first hand with second hand and returns -1 if h1
   is worse than h2, 0 if h1 = h2, 1 if h1 is better than h2. Returns: int.
   Requires: the size of both hands are 7*) let compare_hand h1 h2 = h1 *)
