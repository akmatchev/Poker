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

(** create_cards returns list of cards with every combination of rank from ranks and suit from suits *)
  let create_cards ranks suits = 
    let product l1 l2 =
      List.concat
        (List.map
           (fun e1 -> List.map (fun e2 -> { rank = e1; suit = e2 }) l2)
           l1)
    in
    product ranks suits
  
  (** print_cards prints list of cards *)
  let print_cards cards = 
    print_endline (many_cards_to_ascii cards)
  
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

  let rec print_deck = function
    | [] -> print_endline ""
    | { rank; suit } :: t ->
        print_card rank suit;
        print_deck t

  (* let print_full_deck = print_deck full_deck *)

  (** draw_card returns pair of card drawn from given deck, and the original deck without drawn card *)
  let draw_card (deck : card list) : card * card list =
    match deck with
    | [] -> failwith "Empty deck, cannot draw a card"
    | _ ->
        let drawn_index = Random.int (List.length deck) in
        let drawn_card = List.nth deck drawn_index in
        let rest = List.filter (fun c -> c <> drawn_card) deck in
        (drawn_card, rest)
  
  (** draw_cards returns pair of list of drawn cards from original deck, and the original deck without drawn cards *)
  let rec draw_cards (deck : card list) (num : int) : card list * card list = 
    if num > 0 then match 
    draw_card deck with 
    | (drawn_card, rest_deck) ->
      match draw_cards rest_deck (num -1) with
      | (drawn_cards, final_deck ) -> (drawn_card :: drawn_cards, final_deck)
    else ([], deck)

  (** draw_flop returns pair of 3 cards from given deck, and the original deck without a burn card and the 3 returned cards *)
  let draw_flop (deck : card list) : (card list * card list) =
    match draw_cards deck 4 with
    | (a, b) -> (List.tl a, b)

  let print_some_cards () =
  let pair = (draw_flop (create_cards [12; 13; 4; 9] [Hearts; Spades; Clubs])) in
  print_cards (fst pair);print_cards (snd pair)
  

  let () = print_some_cards ()
end
