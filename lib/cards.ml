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

(**[ranks] is a list of all possible ranks*)
let ranks = [ 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14 ]

(**[suits] is a list of all possible suits*)
let suits = [ Hearts; Diamonds; Clubs; Spades ]

(**[top_card] Returns: the card at the top of the deck. Requires: the deck is
   nonempty *)
let top_card d = List.hd d

(**[full_deck] is an unshuffled full deck of cards*)
let full_deck =
  let product l1 l2 =
    List.concat
      (List.map (fun e1 -> List.map (fun e2 -> { rank = e1; suit = e2 }) l2) l1)
  in
  product ranks suits

(**[shuffle_deck] shuffles the input deck of cards. Returns: a shuffled version
   of given deck. Requires: the deck is nonempty*)
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
   Returns: the string representation of the card. Requires: the input r is of a
   valid rank*)
let rank_to_string = function
  | 11 -> "J"
  | 12 -> "Q"
  | 13 -> "K"
  | 14 -> "A"
  | n -> string_of_int n

(**[card_to_ascii] converts a card to its ascii representation. Returns: an
   ascii representation of a card. Requires: the input is a valid card *)
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

(**[cards_to_ascii] converts are list of cards to their ascii art
   representations side-by-side. Returns: a side-by-side ascii representation of
   cards.*)
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

(**[print_card] prints the ascii art representation of a card. Requires: a valid
   card input*)
let print_card card = print_endline (card_to_ascii card)

(**[print_cards] prints the ascii art representations of multiple cards
   side-by-side. Requires: all cards in the list are valid cards*)
let print_cards lst = print_endline (cards_to_ascii lst)

(**[draw_card] draws a card from a deck. Returns: the resulting deck. Requires:
   the deck is nonemtpy*)
let draw_card lst = List.tl lst

(**[draw_hands] draws the hands for two players. Returns: (h1, h2, r_deck),
   where h1 is the hand of player1, h2 is the hand of player2, and r_deck is the
   remaining deck *)
let draw_hands deck =
  let p1card1 = top_card deck in
  let d1 = draw_card deck in
  let p2card1 = top_card d1 in
  let d2 = draw_card d1 in
  let p1card2 = top_card d2 in
  let d3 = draw_card d2 in
  let p2card2 = top_card d3 in
  let r_deck = draw_card d3 in
  ([ p1card1; p1card2 ], [ p2card1; p2card2 ], r_deck)

(**[draw_flop] draws the flop of the current deck. Returns: a tuple of card
   lists. The first element is the flop and the second element is the resulting
   deck. Requires: the size of the deck is greater than 4*)
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
