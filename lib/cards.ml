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

let empty = []
let ranks = [ 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14 ]
let suits = [ Hearts; Diamonds; Clubs; Spades ]
let top_card d = List.hd d

let full_deck =
  let product l1 l2 =
    List.concat
      (List.map (fun e1 -> List.map (fun e2 -> { rank = e1; suit = e2 }) l2) l1)
  in
  product ranks suits

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

let suit_of_string str =
  match String.capitalize_ascii str with
  | "Hearts" -> Some Hearts
  | "Diamonds" -> Some Diamonds
  | "Clubs" -> Some Clubs
  | "Spades" -> Some Spades
  | _ -> None

let suit_to_string = function
  | Hearts -> "♥"
  | Diamonds -> "♦"
  | Clubs -> "♣"
  | Spades -> "♠"

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
let draw_card lst = List.tl lst

let draw_flop deck =
  let d1 = draw_card deck in
  let first = top_card d1 in
  let d2 = draw_card d1 in
  let second = top_card d2 in
  let d3 = draw_card d2 in
  let third = top_card d3 in
  ([ first; second; third ], draw_card d3)

let draw_turn_river deck =
  let d1 = draw_card deck in
  let turn = top_card d1 in
  (turn, draw_card d1)
