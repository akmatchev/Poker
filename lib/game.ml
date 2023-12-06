open Cards
open Player
open Hands

type game_state =
  | PreFlop of {
      player1 : player;
      player2 : player;
      pot : int;
      minbet : int;
      deck : card list;
    }
  | Flop of {
      board : card list;
      player1 : player;
      player2 : player;
      pot : int;
      minbet : int;
      deck : card list;
    }
  | Turn of {
      board : card list;
      player1 : player;
      player2 : player;
      pot : int;
      minbet : int;
      deck : card list;
    }
  | River of {
      board : card list;
      player1 : player;
      player2 : player;
      pot : int;
      minbet : int;
      deck : card list;
    }
  | Showdown of {
      board : card list;
      player1 : player;
      player2 : player;
      pot : int;
    }
  | End of {
      winner : player option;
      pot : int;
    }

type action =
  | Check
  | Bet of int
  | Fold

let print_lines () =
  for i = 1 to 50 do
    print_endline ""
  done

(** [init_game players small_blind big_blind] initializes a new poker game with
    the given [players], [small_blind], and [big_blind]. *)
let init_game player1 player2 =
  PreFlop
    { player1; player2; pot = 0; minbet = 0; deck = shuffle_deck full_deck }

(** [get_pot_state game] returns the current pot in the current game state. *)
let get_pot = function
  | PreFlop pf -> pf.pot
  | Flop f -> f.pot
  | Turn t -> t.pot
  | River r -> r.pot
  | Showdown s -> s.pot
  | End e -> e.pot

(** [get_board_state game] returns the board cards in the current game state. *)
let get_board_state = function
  | PreFlop _ -> []
  | Flop f -> f.board
  | Turn t -> t.board
  | River r -> r.board
  | Showdown s -> s.board
  | _ -> []

(** [transition_to_flop game flop_cards] transitions the game state to the Flop
    state with the given [flop_cards]. *)
let transition_to_flop game =
  match game with
  | PreFlop { player1; player2; pot; minbet; deck } ->
      let flop, post_flop_deck = draw_flop deck in
      Flop
        {
          board = flop;
          player1;
          player2;
          pot = 0;
          minbet = 0;
          deck = post_flop_deck;
        }
  | _ -> failwith "Invalid transition to Flop state"

(** [transition_to_turn game turn_card] transitions the game state to the Turn
    state with the given [turn_card]. *)
let transition_to_turn game =
  match game with
  | Flop { board; player1; player2; pot; deck } ->
      let turn_card, post_turn_deck = draw_turn_river deck in
      Turn
        {
          board = List.rev (turn_card :: List.rev board);
          player1;
          player2;
          pot;
          minbet = 0;
          deck = post_turn_deck;
        }
  | _ -> failwith "Invalid transition to Turn state"

(** [transition_to_river game river_card] transitions the game state to the
    River state with the given [river_card]. *)
let transition_to_river game river_card =
  match game with
  | Turn { board; player1; player2; pot; deck } ->
      let river_card, post_river_deck = draw_turn_river deck in
      River
        {
          board = List.rev (river_card :: List.rev board);
          player1;
          player2;
          pot;
          minbet = 0;
          deck = post_river_deck;
        }
  | _ -> failwith "Invalid transition to River state"

(** [transition_to_showdown game winners] transitions the game state to the
    Showdown state with the given [winners]. *)
let transition_to_showdown game winners =
  match game with
  | River { board; player1; player2; pot } ->
      Showdown { board; player1; player2; pot }
  | _ -> failwith "Invalid transition to Showdown state"

(** [transition_to_end game winners] transitions the game state to the End state
    with the given [winners]. *)
let transition_to_end game winners =
  match game with
  | Showdown { board; player1; player2; pot } ->
      End { winner = best_hand board player1 player2; pot }
  | _ -> failwith "Invalid transition to End state"

(*Valid bet amount*)
let rec get_valid_first_bet_amount () =
  print_endline "Enter your bet amount:";
  try
    let bet_amount = read_int () in
    if bet_amount <= 0 then (
      print_endline "Invalid bet amount. Please enter a positive value.";
      get_valid_first_bet_amount ())
    else bet_amount
  with Failure _ ->
    print_endline "Invalid input. Please enter a valid number.";
    get_valid_first_bet_amount ()

let rec get_valid_call_raise minbet () =
  print_endline "Enter your bet amount!";
  try
    let bet_amount = read_int () in
    if bet_amount <= minbet then (
      print_endline
        ("Invalid bet amount. Bet must be at least " ^ string_of_int minbet
       ^ ".");
      get_valid_call_raise minbet ())
    else bet_amount
  with Failure _ ->
    print_endline "Invalid input. Please enter a valid number.";
    get_valid_call_raise minbet ()

let rec valid_player2_bet minbet () =
  print_endline "Player 2: What would you like to do (bet/fold)?";
  let rec get_player_bet_action () =
    let input = String.lowercase_ascii (read_line ()) in
    match input with
    | "check" ->
        print_endline "Have to bet or fold";
        get_player_bet_action ()
    | "bet" ->
        print_endline "Enter your bet amount:";
        let bet_amount = get_valid_call_raise minbet () in
        Bet bet_amount
    | "fold" -> Fold
    | _ ->
        print_endline "Invalid input. Please enter 'check', 'bet', or 'fold'";
        get_player_bet_action ()
  in
  get_player_bet_action ()

let rec get_player_action () =
  let input = String.lowercase_ascii (read_line ()) in
  match input with
  | "check" -> Check
  | "bet" ->
      print_endline "Enter your bet amount:";
      let bet_amount = get_valid_first_bet_amount () in
      Bet bet_amount
  | "fold" -> Fold
  | _ ->
      print_endline "Invalid input. Please enter 'check', 'bet', or 'fold'";
      get_player_action ()

let rec get_player1_bet_post_player2_bet minbet () =
  let rec get_player_bet_action () =
    let input = String.lowercase_ascii (read_line ()) in
    match input with
    | "check" ->
        print_endline "Have to bet or fold";
        get_player_bet_action ()
    | "bet" ->
        print_endline "Enter your bet amount:";
        let bet_amount = get_valid_call_raise minbet () in
        Bet bet_amount
    | "fold" -> Fold
    | _ ->
        print_endline "Invalid input. Please enter 'check', 'bet', or 'fold'";
        get_player_bet_action ()
  in
  get_player_bet_action ()

(*Preflop betting*)
let rec preflop_betting player1 player2 pot minbet deck : game_state =
  if minbet <> 0 then (
    print_endline "Player 1: What would you like to do (bet/raise)?";
    let action = get_player1_bet_post_player2_bet minbet () in
    match action with
    | Check -> failwith "Impossible"
    | Bet b -> failwith "TODO"
    | Fold -> failwith "TODO")
  else print_endline "Player 1: What would you like to do (check/bet/fold)?";
  let rec get_player_action () =
    let input = String.lowercase_ascii (read_line ()) in
    match input with
    | "check" -> Check
    | "bet" ->
        print_endline "Enter your bet amount:";
        let bet_amount = get_valid_first_bet_amount () in
        Bet bet_amount
    | "fold" -> Fold
    | _ ->
        print_endline "Invalid input. Please enter 'check', 'bet', or 'fold'";
        get_player_action ()
  in
  let player1_action = get_player_action () in
  match player1_action with
  | Check -> (
      print_endline "Player 2: What would you like to do (check/bet/fold)?";
      let player2_action = get_player_action () in
      match player2_action with
      | Check ->
          transition_to_flop
            (PreFlop { player1; player2; pot = 0; minbet = 0; deck })
      | Bet b ->
          preflop_betting player1 (remove_chips player2 b) (pot + b) b deck
      | Fold -> End { winner = Some player1; pot })
  | Bet b -> (
      if b = minbet then
        transition_to_flop
          (PreFlop
             {
               player1 = remove_chips player1 b;
               player2;
               pot = pot + b;
               minbet;
               deck;
             })
      else
        let player1 = remove_chips player1 (player1.chips - b) in
        let minbet = b - minbet in
        let pot = pot + b in
        print_endline "Player 2: What would you like to do (bet/fold)?";
        let player2_action = get_player_action () in
        match player2_action with
        | Bet b ->
            if b = minbet then
              transition_to_flop
                (PreFlop
                   {
                     player1;
                     player2 = remove_chips player2 b;
                     minbet;
                     pot = pot + b;
                     deck;
                   })
            else
              preflop_betting player1 (remove_chips player2 b) (pot + b)
                (b - minbet) deck
        | Check -> failwith "Impossible"
        | Fold -> End { winner = Some player1; pot = b })
  | Fold -> End { winner = Some player2; pot }
