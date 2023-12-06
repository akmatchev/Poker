open Poker
open Cards

let royal_flush =
  [
    { rank = 10; suit = Spades };
    { rank = 11; suit = Spades };
    { rank = 12; suit = Spades };
    { rank = 13; suit = Spades };
    { rank = 14; suit = Spades };
  ]

let () = print_cards royal_flush

let rec print_deck = function
  | [] -> print_endline ""
  | h :: t ->
      print_card h;
      print_deck t

let rec demo deck () =
  let r_deck = ref deck in
  print_endline "Do you want to draw a card? (yes/no)";
  let input = String.lowercase_ascii (read_line ()) in
  match input with
  | "yes" | "y" ->
      if List.length deck = 0 then print_endline "Sorry, the deck is empty"
      else
        let drawn_card = top_card deck in
        print_endline "You drew the following card:";
        print_card drawn_card;
        r_deck := draw_card deck;
        demo !r_deck ()
  | "no" | "n" -> (
      print_endline "Okay, maybe next time!";
      print_endline "Do you want to print the remaining deck?";
      let next_input = String.lowercase_ascii (read_line ()) in
      match next_input with
      | "yes" | "y" -> print_deck !r_deck
      | "no" | "n" -> print_endline "Bye"
      | _ -> print_endline "Invalid input")
  | _ -> print_endline "Invalid input"

let () =
  print_endline "Welcome to Poker!";
  demo (shuffle_deck full_deck) ()
