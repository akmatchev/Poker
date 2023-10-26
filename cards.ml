module Cards1 = struct
  type suit = Hearts | Diamonds | Clubs | Spades
  type rank = int
  type card = { rank : rank; suit : suit }

  let ranks = [ 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14 ]
  let suits = [ Hearts; Diamonds; Clubs; Spades ]

  let string_of_suit = function
    | Hearts -> "♥"
    | Diamonds -> "♦"
    | Clubs -> "♣"
    | Spades -> "♠"

  let string_of_rank = function
    | 11 -> "J"
    | 12 -> "Q"
    | 13 -> "K"
    | 14 -> "A"
    | n -> string_of_int n

  let create_deck () =
    let product l1 l2 =
      List.concat (List.map (fun e1 -> List.map (fun e2 -> (e1, e2)) l2) l1)
    in
    List.map (fun (r, s) -> { rank = r; suit = s }) (product ranks suits)

  let print_card card =
    Printf.printf "%s%s " (string_of_rank card.rank) (string_of_suit card.suit)

  let print_deck deck = List.iter print_card deck

  let () =
    let deck = create_deck () in
    print_deck deck
end

module Cards2 = struct
  type suit = Hearts | Diamonds | Clubs | Spades
  type card = { rank : int; suit : suit }

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

  let () =
    let example_card = { rank = 10; suit = Diamonds } in
    print_endline (card_to_ascii example_card)
end
