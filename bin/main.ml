open Poker
open Cards
open Hands
open Player
open Game

let royal_flush =
  [
    { rank = 10; suit = Spades };
    { rank = 11; suit = Spades };
    { rank = 12; suit = Spades };
    { rank = 13; suit = Spades };
    { rank = 14; suit = Spades };
  ]

let fresh_deck = shuffle_deck full_deck
let p1hand, p2hand, deck = draw_hands fresh_deck
let player1 = { name = "Player 1"; hand = p1hand; chips = 100 }
let player1 = { name = "Player 2"; hand = p2hand; chips = 100 }
let flop, post_flop_deck = draw_flop deck
let turn, post_turn_deck = draw_turn_river post_flop_deck
let turn_board = List.rev (turn :: List.rev flop)
let river, post_river_deck = draw_turn_river post_turn_deck
let final_board = List.rev (river :: List.rev turn_board)
let pot = ref 0

let print_lines () =
  for i = 1 to 50 do
    print_endline ""
  done

let round () = ()
(* print_lines (); print_cards player1.hand; print_endline "Player 1: What would
   you like to do (check/raise/fold)?"; let rec get_preflop_input () = let input
   = String.lowercase_ascii (read_line ()) in match input with | "check" ->
   () *)

let main () =
  print_cards royal_flush;
  print_endline "Welcome to Poker!";
  print_endline "Are you ready to begin? (y/n)";
  let rec get_begin_input () =
    let input = String.lowercase_ascii (read_line ()) in
    match input with
    | "yes" | "y" -> true
    | "no" | "n" -> false
    | _ ->
        print_endline "Invalid input. Please enter 'yes/y' or 'no/n'.";
        get_begin_input ()
  in
  if get_begin_input () = false then print_endline "Ok, maybe next time. Bye"
  else print_endline "Let's begin.";
  print_endline "Player 1: Press Enter when ready";
  ignore (read_line ());
  round ()

let () = main ()
