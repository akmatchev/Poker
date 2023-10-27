let row_b = "+---------+ +---------+ +---------+ +---------+ +---------+" in
let rown1 = "|10       | | J       | | Q       | | K       | | A       |" in 
let row_r = "|         | |         | |         | |         | |         |" in 
let row_s = "|    ♠    | |    ♠    | |    ♠    | |    ♠    | |    ♠    |" in 
let rown2 = "|       10| |       J | |       Q | |       K | |       A |" in
print_endline row_b;
print_endline rown1;
print_endline row_r;
print_endline row_s;
print_endline row_r;
print_endline rown2;
print_endline row_b;
print_newline ()

open Poker
let rec draw_cards deck () = let open Poker in 
  print_endline "Do you want to draw a card? (yes/no)";
  let input = String.lowercase_ascii (read_line ()) in
  match input with
  | "yes" | "y" ->
      if List.length deck = 0 then print_endline "Sorry, the deck is empty!" else 
      let drawn_card, r_deck = Poker.draw_card deck in
      print_endline "You drew the following card:";
      Poker.print_card drawn_card.rank drawn_card.suit;
      draw_cards r_deck ()
  | "no" -> (print_endline "Okay, maybe next time!";
    print_endline "Do you want to print the remaining deck?";
    let next_input = String.lowercase_ascii (read_line ()) in 
    match next_input with 
    | "yes" -> Poker.print_deck deck
    | "no" -> print_endline "Bye"
    | _ -> print_endline "Invalid input. Please enter 'yes' or 'no";)

  | _ ->
      print_endline "Invalid input. Please enter 'yes' or 'no'";
      draw_cards deck ()

let () =
  print_endline "Welcome to Poker!";
  draw_cards Poker.full_deck ()