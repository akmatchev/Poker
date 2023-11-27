open Random

type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades

type card = {
  rank : int;
  suit : suit;
}

(**[empty] represents an empty deck of cards*)
let empty = []

(**[ranks] is a list of all possible ranks a card could have. It is used to
   create the full_deck of cards*)
let ranks = [ 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14 ]

(**[suits] is a list of all possible suits a card could have. It is used to
   create the full deck of cards*)
let suits = [ Hearts; Diamonds; Clubs; Spades ]

(**[top_card] returns the card at the top of the deck*)
let top_card d = List.hd d

(**[full_deck] is an unshuffled full deck of cards*)
let full_deck =
  let product l1 l2 =
    List.concat
      (List.map (fun e1 -> List.map (fun e2 -> { rank = e1; suit = e2 }) l2) l1)
  in
  product ranks suits

(**[shuffle_deck] shuffles the input deck of cards and returns a shuffled
   version of given deck*)
let shuffle_deck deck =
  Random.self_init ();
  let deck_arr = Array.of_list deck in
  let switch i j arr =
    let t = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- t
  in
  let n = Array.length deck_arr in
  for index = n - 1 downto 1 do
    let new_i = Random.int (index + 1) in
    switch new_i index deck_arr
  done;
  Array.to_list deck_arr

(**[suit_to_string] takes in the suit of a card. Returns: the string
   representation of the suit. Requires: the input is of type suit*)
let suit_to_string = function
  | Hearts -> "♥"
  | Diamonds -> "♦"
  | Clubs -> "♣"
  | Spades -> "♠"

(**[rank_to_string] converts the rank of a card to a string representation.
   Returns: the string representation of the card. Requires: the input r is 2 <=
   r <= 14*)
let rank_to_string = function
  | 11 -> "J"
  | 12 -> "Q"
  | 13 -> "K"
  | 14 -> "A"
  | n -> string_of_int n

let card_to_ascii card =
  Printf.sprintf
    "\n\
    \  +---------+\n\
    \  |%2s       |\n\
    \  |         |\n\
    \  |    %s    |\n\
    \  |         |\n\
    \  |       %2s|\n\
    \  +---------+" (rank_to_string card.rank) (suit_to_string card.suit)
    (rank_to_string card.rank)

let cards_to_ascii (deck : card list) =
  match deck with
  | [] -> "Empty"
  | a :: b :: c :: d :: e :: t ->
      Printf.sprintf
        "\n\
        \  +---------+ +---------+ +---------+ +---------+ +---------+\n\
        \  |%2s       | |%2s       | |%2s       | |%2s       | |%2s       |\n\
        \  |         | |         | |         | |         | |         |\n\
        \  |    %s    | |    %s    | |    %s    | |    %s    | |    %s    |\n\
        \  |         | |         | |         | |         | |         |\n\
        \  |       %2s| |       %2s| |       %2s| |       %2s| |       %2s|\n\
        \  +---------+ +---------+ +---------+ +---------+ +---------+"
        (rank_to_string a.rank) (rank_to_string b.rank) (rank_to_string c.rank)
        (rank_to_string d.rank) (rank_to_string e.rank) (suit_to_string a.suit)
        (suit_to_string b.suit) (suit_to_string c.suit) (suit_to_string d.suit)
        (suit_to_string e.suit) (rank_to_string a.rank) (rank_to_string b.rank)
        (rank_to_string c.rank) (rank_to_string d.rank) (rank_to_string e.rank)
  | a :: b :: c :: d :: t ->
      Printf.sprintf
        "\n\
        \  +---------+ +---------+ +---------+ +---------+\n\
        \  |%2s       | |%2s       | |%2s       | |%2s       |\n\
        \  |         | |         | |         | |         |\n\
        \  |    %s    | |    %s    | |    %s    | |    %s    |\n\
        \  |         | |         | |         | |         |\n\
        \  |       %2s| |       %2s| |       %2s| |       %2s|\n\
        \  +---------+ +---------+ +---------+ +---------+"
        (rank_to_string a.rank) (rank_to_string b.rank) (rank_to_string c.rank)
        (rank_to_string d.rank) (suit_to_string a.suit) (suit_to_string b.suit)
        (suit_to_string c.suit) (suit_to_string d.suit) (rank_to_string a.rank)
        (rank_to_string b.rank) (rank_to_string c.rank) (rank_to_string d.rank)
  | a :: b :: c :: t ->
      Printf.sprintf
        "\n\
        \  +---------+ +---------+ +---------+\n\
        \  |%2s       | |%2s       | |%2s       |\n\
        \  |         | |         | |         |\n\
        \  |    %s    | |    %s    | |    %s    |\n\
        \  |         | |         | |         |\n\
        \  |       %2s| |       %2s| |       %2s|\n\
        \  +---------+ +---------+ +---------+" (rank_to_string a.rank)
        (rank_to_string b.rank) (rank_to_string c.rank) (suit_to_string a.suit)
        (suit_to_string b.suit) (suit_to_string c.suit) (rank_to_string a.rank)
        (rank_to_string b.rank) (rank_to_string c.rank)
  | a :: b :: t ->
      Printf.sprintf
        "\n\
        \  +---------+ +---------+\n\
        \  |%2s       | |%2s       |\n\
        \  |         | |         |\n\
        \  |    %s    | |    %s    |\n\
        \  |         | |         |\n\
        \  |       %2s| |       %2s|\n\
        \  +---------+ +---------+" (rank_to_string a.rank)
        (rank_to_string b.rank) (suit_to_string a.suit) (suit_to_string b.suit)
        (rank_to_string a.rank) (rank_to_string b.rank)
  | a :: b -> card_to_ascii a

let print_card card = print_endline (card_to_ascii card)
let print_cards lst = print_endline (cards_to_ascii lst)

(**[draw_card] draws a card from a deck. Returns: the resulting deck. Requires:
   the deck is nonemtpy*)
let draw_card lst = List.tl lst

(**[draw_flop] draws the flop of the current deck. Returns: a tuple of card
   lists. The first element is the flop and the second element is the resulting
   deck. Requires: the size of the deck is >4*)
let draw_flop deck =
  let d1 = draw_card deck in
  let first = top_card d1 in
  let d2 = draw_card d1 in
  let second = top_card d2 in
  let d3 = draw_card d2 in
  let third = top_card d3 in
  ([ first; second; third ], draw_card d3)

(**[draw_turn_river] draws the turn or river of a given round. Returns: a tuple
   whose first element is the card drawn and the second element is the resulting
   deck. Requires: the size of the deck is >1*)
let draw_turn_river deck =
  let d1 = draw_card deck in
  let turn = top_card d1 in
  (turn, draw_card d1)

type category =
  | High
  | One_Pair
  | Two_Pair
  | Three_Of_A_Kind
  | Straight
  | Flush
  | Full_House
  | Four_Of_A_Kind
  | Straight_Flush
  | Royal_Flush

(**[one_pair] returns rank of a pair in list of cards if pair exists, otherwise
   returns none. Returns: option of rank of pair if pair exists otherwise None.*)
let one_pair h =
  let rec helper h1 =
    match h1 with
    | [] -> None
    | hd :: t -> (
        match
          List.find_opt (fun x -> if x.rank = hd.rank then true else false) t
        with
        | None -> helper t
        | Some a -> Some a.rank)
  in
  helper h

(* think of have 3 pair, which is the same as the best two pair in 7 cards *)

(**[two_pair] returns rank of a two disinct pairs in list of cards if they
   exist, otherwise returns none. Returns: option tuple of rank of both pairs if
   pairs exists otherwise None.*)
let two_pair h =
  let rankin = one_pair h in
  match rankin with
  | None -> None
  | Some a -> (
      let rest =
        List.find_all (fun x -> if x.rank <> a then true else false) h
      in
      match one_pair rest with
      | None -> None
      | Some b -> Some (a, b))

(**[three_of_kind] returns rank of a three of kind in list of cards if they
   exist, otherwise returns none. Returns: option of rank of three of a kind if
   it exists otherwise None.*)
let three_of_kind h =
  let rec helper h1 =
    match h1 with
    | [] -> None
    | hd :: t -> (
        match one_pair t with
        | None -> helper t
        | Some a -> if hd.rank = a then Some a else helper t)
  in
  helper h

(**[full_house] returns rank of a disinct pair and three of a kind in list of
   cards if they exist, otherwise returns none. Returns: option tuple of rank of
   pair and three of a kind if they both exist otherwise None.*)
let full_house h =
  let rankin = three_of_kind h in
  match rankin with
  | None -> None
  | Some a -> (
      let rest =
        List.find_all (fun x -> if x.rank <> a then true else false) h
      in
      match one_pair rest with
      | None -> None
      | Some b -> Some (b, a))

(**[four_of_kind] returns rank of a four of kind in list of cards if they exist,
   otherwise returns none. Returns: option of rank of four of a kind if it
   exists otherwise None.*)
let four_of_kind h =
  let rec helper h1 =
    match h1 with
    | [] -> None
    | hd :: t -> (
        match three_of_kind t with
        | None -> helper t
        | Some a -> if hd.rank = a then Some a else helper t)
  in
  helper h

(**[hand_category] classifies 7-card hand as a category of poker hands and
   returns this category. Returns: category. Requires: the size of hand is 7*)
let hand_category h = h

(**[compare_hand] compares first hand with second hand and returns -1 if h1 is
   worse than h2, 0 if h1 = h2, 1 if h1 is better than h2. Returns: int.
   Requires: the size of both hands are 7*)
let compare_hand h1 h2 = h1
