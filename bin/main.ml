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

let end_message (game : game_state) =
  match game with
  | End { winner; pot } -> (
      match winner with
      | None ->
          print_endline
            ("Player 1 and Player 2 split the pot of " ^ string_of_int pot
           ^ " chips!")
      | Some player ->
          print_endline (player.name ^ " wins " ^ string_of_int pot ^ " chips!")
      )
  | _ -> failwith "Impossible"

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
    let fresh_deck = shuffle_deck full_deck in
    let p1hand, p2hand, deck = draw_hands fresh_deck in
    let player1 = { name = "Player 1"; hand = p1hand; chips = 100 } in
    let player2 = { name = "Player 2"; hand = p2hand; chips = 100 } in
    let preflop = init_game player1 player2 in
    let post_preflop_state = round preflop in
    match post_preflop_state with
    | Flop _ -> (
        let post_flop_state = round post_preflop_state in
        match post_flop_state with
        | Turn _ -> (
            let post_turn_state = round post_flop_state in
            match post_turn_state with
            | River _ -> (
                let post_river_state = round post_turn_state in
                match post_river_state with
                | Showdown _ ->
                    post_river_state |> transition_to_end |> end_message
                | End { winner; pot } -> end_message post_river_state
                | _ -> failwith "impossible")
            | Showdown _ -> post_turn_state |> transition_to_end |> end_message
            | End { winner; pot } -> end_message post_turn_state
            | _ -> failwith "Impossible")
        | Showdown _ -> post_flop_state |> transition_to_end |> end_message
        | End { winner; pot } -> end_message post_flop_state
        | _ -> failwith "impossible")
    | Showdown _ ->
        print_endline
          (post_preflop_state |> transition_to_end |> state_to_string)
    | End { winner; pot } -> end_message post_preflop_state
    | _ -> failwith "Impossible"

let () = main ()
