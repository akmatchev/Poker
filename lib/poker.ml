module Poker = struct
  type suit = Hearts | Diamonds | Clubs | Spades
  type card = { rank : int; suit : suit }

  let ranks = [ 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14 ]
  let suits = [ Hearts; Diamonds; Clubs; Spades ]

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
  
  (** single_card_to_ascii returns the asci text representation of the row indicated by index, with the values dictated by card *)
  let single_card_to_ascii index card =
  let buffer = if card.rank = 10 then "" else " " in
  let string_rank = rank_to_string card.rank in
  let string_suit = suit_to_string card.suit in
  match index with
  | 0 | 6 -> "+---------+ "
  | 1 -> "|"^string_rank^buffer^"       | "
  | 2 | 4 -> "|         | "
  | 3 -> "|    "^string_suit^"    | "
  | 5 -> "|       "^buffer^string_rank^"| "
  | _ -> ""

(** many_cards_to_ascii returns a string representation of the list cards with the ascci cards side-by-side *)
let many_cards_to_ascii cards =
let many_cards_helper cards i =
(List.fold_right (fun x y -> y^single_card_to_ascii i x) cards "") in
let rec helper_two i =
if i < 0 then "" else helper_two (i-1) ^ ("\n" ^ many_cards_helper cards i) in
helper_two 6


  let create_deck () =
    let product l1 l2 =
      List.concat
        (List.map
           (fun e1 -> List.map (fun e2 -> { rank = e1; suit = e2 }) l2)
           l1)
    in
    product ranks suits

  let create_cards () = 
    let product l1 l2 =
      List.concat
        (List.map
           (fun e1 -> List.map (fun e2 -> { rank = e1; suit = e2 }) l2)
           l1)
    in
    product [10; 3] [Hearts; Diamonds]
  
  let print_some_cards () = 
  print_endline (many_cards_to_ascii (create_cards ()))
  let print_card r s =
    let is_valid_rank r = r >= 2 && r <= 14 in
    let is_valid_suit s =
      match s with Hearts | Diamonds | Clubs | Spades -> true
    in

    if is_valid_rank r && is_valid_suit s then
      let card = { rank = r; suit = s } in
      print_endline (card_to_ascii card)
    else print_endline "Invalid card"

  let empty_deck = []
  let full_deck = create_deck ()

  let () = print_some_cards ()

  let rec print_deck = function
    | [] -> print_endline ""
    | { rank; suit } :: t ->
        print_card rank suit;
        print_deck t

  (* let print_full_deck = print_deck full_deck *)

  let draw_card (deck : card list) : card * card list =
    match deck with
    | [] -> failwith "Empty deck, cannot draw a card"
    | _ ->
        let drawn_index = Random.int (List.length deck) in
        let drawn_card = List.nth deck drawn_index in
        let rest = List.filter (fun c -> c <> drawn_card) deck in
        (drawn_card, rest)
end