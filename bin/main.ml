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
let player2 = { name = "Player 2"; hand = p2hand; chips = 100 }
let flop, post_flop_deck = draw_flop deck
let turn, post_turn_deck = draw_turn_river post_flop_deck
let turn_board = List.rev (turn :: List.rev flop)
let river, post_river_deck = draw_turn_river post_turn_deck
let final_board = List.rev (river :: List.rev turn_board)
let pot = ref 0

(* let print_lines () = for i = 1 to 50 do print_endline "" done *)

let preflop = init_game player1 player2

let welcome =
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
  if get_begin_input () = false then (
    print_endline "Ok, maybe next time. Bye";
    false)
  else (
    print_endline "Let's begin.";
    true)

let main () =
  let to_begin = welcome in
  if not to_begin then ()
  else
    let player1 = { name = "Player 1"; hand = p1hand; chips = 100 } in
    let player2 = { name = "Player 2"; hand = p2hand; chips = 100 } in
    let preflop = init_game player1 player2 in
    let post_preflop_state = round preflop in
    match post_preflop_state with
    | Flop f -> print_endline (state_to_string post_preflop_state)
    | End { winner; pot } -> print_endline (state_to_string post_preflop_state)
    | _ -> failwith "Impossible"

let () = main ()
