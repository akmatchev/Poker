open Cards
open Player
open Hands

(** Abstract type for representing different states of a round of poker *)
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

(** Abstract representation type for representing different actions a player can
    take during a round of poker*)
type action =
  | Check
  | Bet of int
  | Fold

(** [init_game players] initializes a new poker game with the given [player1]
    and [player2]. *)
let init_game (player1 : player) (player2 : player) : game_state =
  PreFlop
    { player1; player2; pot = 0; minbet = 0; deck = shuffle_deck full_deck }

(** [transition_to_flop game] transitions the game state to the Flop state with
    the given preflop state [game]. *)
let transition_to_flop (game : game_state) : game_state =
  match game with
  | PreFlop { player1; player2; pot; minbet; deck } ->
      let flop, post_flop_deck = draw_flop deck in
      Flop
        {
          board = flop;
          player1;
          player2;
          pot;
          minbet = 0;
          deck = post_flop_deck;
        }
  | _ -> failwith "Invalid transition to Flop state"

(** [transition_to_turn game] transitions the game state to the Turn state with
    the given frop state [game]. *)
let transition_to_turn (game : game_state) : game_state =
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

(** [transition_to_river game] transitions the game state to the River state
    with the given turn state [game]. *)
let transition_to_river (game : game_state) : game_state =
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

(** [transition_to_showdown game] transitions the game state to the Showdown
    state with the given river state [game]. *)
let transition_to_showdown (game : game_state) : game_state =
  match game with
  | PreFlop { player1; player2; pot; minbet; deck } ->
      let flop, post_flop_deck = draw_flop deck in
      let turn_card, post_turn_deck = draw_turn_river post_flop_deck in
      let river_card, post_river_deck = draw_turn_river post_turn_deck in
      Showdown
        {
          board = List.rev (river_card :: turn_card :: List.rev flop);
          player1;
          player2;
          pot;
        }
  | Flop { board; player1; player2; pot; minbet; deck } ->
      let turn_card, post_turn_deck = draw_turn_river deck in
      let river_card, post_river_deck = draw_turn_river post_turn_deck in
      Showdown
        {
          board = List.rev (river_card :: turn_card :: board);
          player1;
          player2;
          pot;
        }
  | Turn { board; player1; player2; pot; minbet; deck } ->
      let river_card, post_river_deck = draw_turn_river deck in
      Showdown { board = List.rev (river_card :: board); player1; player2; pot }
  | River { board; player1; player2; pot } ->
      Showdown { board; player1; player2; pot }
  | _ -> failwith "Invalid transition to Showdown state"

(** [transition_to_end game] transitions the game state to the End state with
    the given showdown state [game]. *)
let transition_to_end game =
  match game with
  | Showdown { board; player1; player2; pot } ->
      End { winner = best_hand board player1 player2; pot }
  | _ -> failwith "Invalid transition to End state"

(**[state_to_string] converts a game state to its string representation. Used
   for testing*)
let state_to_string (state : game_state) : string =
  let card_to_string (card : card) : string =
    Printf.sprintf "(Rank: %s, Suit: %s)" (rank_to_string card.rank)
      (suit_to_string card.suit)
  in
  let cards_to_string (cards : card list) : string =
    String.concat ", " (List.map card_to_string cards)
  in
  let player_to_string (player : player) : string =
    Printf.sprintf "Chips: %d, Hand: %s" player.chips
      (cards_to_string player.hand)
  in
  let winner_to_string (winner : player option) : string =
    match winner with
    | Some player ->
        Printf.sprintf "%s\n %s" player.name (player_to_string player)
    | None -> "No Winner"
  in
  match state with
  | PreFlop { player1; player2; pot; minbet; deck } ->
      Printf.sprintf
        "PreFlop:\nPlayer 1: %s\nPlayer 2: %s\nPot: %d\nMin Bet: %d\nDeck: %s"
        (player_to_string player1) (player_to_string player2) pot minbet
        (card_to_string (top_card deck))
  | Flop { board; player1; player2; pot; minbet; deck } ->
      Printf.sprintf
        "Flop:\n\
         Board: %s\n\
         Player 1: %s\n\
         Player 2: %s\n\
         Pot: %d\n\
         Min Bet: %d\n\
         Deck: %s" (cards_to_string board) (player_to_string player1)
        (player_to_string player2) pot minbet
        (card_to_string (top_card deck))
  | Turn { board; player1; player2; pot; minbet; deck } ->
      Printf.sprintf
        "Turn:\n\
         Board: %s\n\
         Player 1: %s\n\
         Player 2: %s\n\
         Pot: %d\n\
         Min Bet: %d\n\
         Deck: %s" (cards_to_string board) (player_to_string player1)
        (player_to_string player2) pot minbet
        (card_to_string (top_card deck))
  | River { board; player1; player2; pot; minbet; deck } ->
      Printf.sprintf
        "River:\n\
         Board: %s\n\
         Player 1: %s\n\
         Player 2: %s\n\
         Pot: %d\n\
         Min Bet: %d\n\
         Deck: %s" (cards_to_string board) (player_to_string player1)
        (player_to_string player2) pot minbet
        (card_to_string (top_card deck))
  | Showdown { board; player1; player2; pot } ->
      Printf.sprintf "Showdown:\nBoard: %s\nPlayer 1: %s\nPlayer 2: %s\nPot: %d"
        (cards_to_string board) (player_to_string player1)
        (player_to_string player2) pot
  | End { winner; pot } ->
      Printf.sprintf "End:\nWinner: %s\nPot: %d" (winner_to_string winner) pot

(**[print_lines ()] prints 100 lines in the terminal. Used in the user interface
   so that players cannot see other players' cards*)
let print_lines () =
  for i = 1 to 100 do
    print_endline ""
  done

(**[ready_input player ()] is a sequence of print statements that is used when
   transitioning from [player1] to [player2] and vice versa. Used when a player
   is handing over the computer to another player *)
let ready_input (player : int) () =
  if player = 1 then print_endline "Player 1: Press Enter when ready"
  else print_endline "Player 2: Press Enter when ready";
  ignore (read_line ())

(**[print_board board] prints the cards that are on the [board] currently. Used
   as part of the user interface*)
let print_board board () =
  print_endline "Board:";
  print_cards board

(**[get_bet_action] gets a valid bet action from a player with chips [chips] and
   whose bet to match is [minbet]*)
let rec get_bet_action (chips : int) (minbet : int) () : int =
  print_endline "Enter your bet amount!";
  if minbet = 0 then (
    try
      let bet_amount = read_int () in
      if bet_amount <= minbet then (
        print_endline "Invalid bet amount. Bet must be positive";
        get_bet_action chips minbet ())
      else if bet_amount > chips then (
        print_endline "Invalid bet. You do not have enough chips!";
        get_bet_action chips minbet ())
      else bet_amount
    with Failure _ ->
      print_endline "Invalid input. Please enter a valid betting number.";
      get_bet_action chips minbet ())
  else
    try
      let bet_amount = read_int () in
      if bet_amount <> minbet then (
        print_endline
          ("Invalid bet amount. Bet must be " ^ string_of_int minbet ^ ".");
        get_bet_action chips minbet ())
      else if bet_amount > chips then (
        print_endline "Invalid bet. You do not have enough chips!";
        get_bet_action chips minbet ())
      else bet_amount
    with Failure _ ->
      print_endline "Invalid input. Please enter a valid betting number.";
      get_bet_action chips minbet ()

(**[get_player_action] gets a valid action from [player] depending on what the
   minimum bet is *)
let rec get_player_action (player : player) (minbet : int) () : action =
  let input = String.lowercase_ascii (read_line ()) in
  match input with
  | "check" ->
      if minbet = 0 then Check
      else (
        print_endline ("Have to bet " ^ string_of_int minbet ^ " or fold!");
        get_player_action player minbet ())
  | "bet" ->
      let bet_amount = get_bet_action player.chips minbet () in
      Bet bet_amount
  | "fold" -> Fold
  | _ ->
      print_endline "Invalid input. Please enter 'check', 'bet', or 'fold'";
      get_player_action player minbet ()

(**[round] performs a round of poker that is at a given game_state [game]*)
let round (game : game_state) : game_state =
  match game with
  | PreFlop { player1; player2; pot; minbet; deck } as p -> (
      print_lines ();
      ready_input 1 ();
      print_endline "Player 1 Hand:";
      print_cards player1.hand;
      print_endline ("Player 1 chips: " ^ string_of_int player1.chips);
      print_endline "Player 1: What would you like to do (check/bet/fold)?";
      let player1_action = get_player_action player1 0 () in
      match player1_action with
      | Check -> (
          print_lines ();
          ready_input 2 ();
          print_endline "Player 2 Hand:";
          print_cards player2.hand;
          print_endline ("Player 2 chips: " ^ string_of_int player2.chips);
          print_endline "Player 2: What would you like to do (check/bet/fold)?";
          let player2_action = get_player_action player2 0 () in
          match player2_action with
          | Check -> transition_to_flop p
          | Bet b -> (
              print_lines ();
              ready_input 1 ();
              print_endline "Player 1 Hand:";
              print_cards player1.hand;
              print_endline ("Player 2 bet " ^ string_of_int b ^ " chips");
              print_endline ("Player 1 chips: " ^ string_of_int player1.chips);
              print_endline "Player 1: What would you like to do (bet/fold)?";
              let pl2 = remove_chips player2 b in
              let minbet = b in
              let pot2 = pot + b in
              let player1bet_action = get_player_action player1 b () in
              match player1bet_action with
              | Bet b2 ->
                  let pl1 = remove_chips player1 b2 in
                  let finalpot = pot2 + b2 in
                  if finalpot = 200 then
                    transition_to_showdown
                      (PreFlop
                         {
                           player1 = pl1;
                           player2 = pl2;
                           pot = pot2 + b2;
                           minbet;
                           deck;
                         })
                  else
                    transition_to_flop
                      (PreFlop
                         {
                           player1 = pl1;
                           player2 = pl2;
                           pot = pot2 + b2;
                           minbet;
                           deck;
                         })
              | Fold -> End { winner = Some pl2; pot = pot2 }
              | Check -> failwith "Impossible")
          | Fold -> End { winner = Some player1; pot })
      | Bet b -> (
          print_lines ();
          ready_input 2 ();
          print_endline "Player 2 Hand:";
          print_cards player2.hand;
          print_endline ("Player 1 bet " ^ string_of_int b ^ " chips");
          print_endline "Player 2: What would you like to do (bet/fold)?";
          let pl1 = remove_chips player1 b in
          let minbet = b in
          let pot2 = pot + b in
          let player2bet_action = get_player_action player2 b () in
          match player2bet_action with
          | Bet b2 ->
              let pl2 = remove_chips player2 b2 in
              let finalpot = pot2 + b2 in
              if finalpot = 200 then
                transition_to_showdown
                  (PreFlop
                     {
                       player1 = pl1;
                       player2 = pl2;
                       pot = pot2 + b2;
                       minbet;
                       deck;
                     })
              else
                transition_to_flop
                  (PreFlop
                     {
                       player1 = pl1;
                       player2 = pl2;
                       pot = pot2 + b2;
                       minbet;
                       deck;
                     })
          | Fold -> End { winner = Some pl1; pot = pot2 }
          | Check -> failwith "Impossible")
      | Fold -> End { winner = Some player2; pot })
  | Flop { board; player1; player2; pot; minbet; deck } as p -> (
      print_lines ();
      ready_input 1 ();
      print_board board ();
      print_endline "Player 1 Hand:";
      print_cards player1.hand;
      print_endline ("Player 1 chips: " ^ string_of_int player1.chips);
      print_endline "Player 1: What would you like to do (check/bet/fold)?";
      let player1_action = get_player_action player1 0 () in
      match player1_action with
      | Check -> (
          print_lines ();
          ready_input 2 ();
          print_board board ();
          print_endline "Player 2 Hand:";
          print_cards player2.hand;
          print_endline ("Player 2 chips: " ^ string_of_int player2.chips);
          print_endline "Player 2: What would you like to do (check/bet/fold)?";
          let player2_action = get_player_action player2 0 () in
          match player2_action with
          | Check -> transition_to_turn p
          | Bet b -> (
              print_lines ();
              ready_input 1 ();
              print_board board ();
              print_endline "Player 1 Hand:";
              print_cards player1.hand;
              print_endline ("Player 2 bet " ^ string_of_int b ^ " chips");
              print_endline ("Player 1 chips: " ^ string_of_int player1.chips);
              print_endline "Player 1: What would you like to do (bet/fold)?";
              let pl2 = remove_chips player2 b in
              let minbet = b in
              let pot2 = pot + b in
              let player1bet_action = get_player_action player1 b () in
              match player1bet_action with
              | Bet b2 ->
                  let pl1 = remove_chips player1 b2 in
                  let finalpot = pot2 + b2 in
                  if finalpot = 200 then
                    transition_to_showdown
                      (Flop
                         {
                           board;
                           player1 = pl1;
                           player2 = pl2;
                           pot = pot2 + b2;
                           minbet;
                           deck;
                         })
                  else
                    transition_to_turn
                      (Flop
                         {
                           board;
                           player1 = pl1;
                           player2 = pl2;
                           pot = pot2 + b2;
                           minbet;
                           deck;
                         })
              | Fold -> End { winner = Some pl2; pot = pot2 }
              | Check -> failwith "Impossible")
          | Fold -> End { winner = Some player1; pot })
      | Bet b -> (
          print_lines ();
          ready_input 2 ();
          print_board board ();
          print_endline "Player 2 Hand:";
          print_cards player2.hand;
          print_endline ("Player 1 bet " ^ string_of_int b ^ " chips");
          print_endline "Player 2: What would you like to do (bet/fold)?";
          let pl1 = remove_chips player1 b in
          let minbet = b in
          let pot2 = pot + b in
          let player2bet_action = get_player_action player2 b () in
          match player2bet_action with
          | Bet b2 ->
              let pl2 = remove_chips player2 b2 in
              let finalpot = pot2 + b2 in
              if finalpot = 200 then
                transition_to_showdown
                  (Flop
                     {
                       board;
                       player1 = pl1;
                       player2 = pl2;
                       pot = pot2 + b2;
                       minbet;
                       deck;
                     })
              else
                transition_to_turn
                  (Flop
                     {
                       board;
                       player1 = pl1;
                       player2 = pl2;
                       pot = pot2 + b2;
                       minbet;
                       deck;
                     })
          | Fold -> End { winner = Some pl1; pot = pot2 }
          | Check -> failwith "Impossible")
      | Fold -> End { winner = Some player2; pot })
  | Turn { board; player1; player2; pot; minbet; deck } as p -> (
      print_lines ();
      ready_input 1 ();
      print_board board ();
      print_endline "Player 1 Hand:";
      print_cards player1.hand;
      print_endline ("Player 1 chips: " ^ string_of_int player1.chips);
      print_endline "Player 1: What would you like to do (check/bet/fold)?";
      let player1_action = get_player_action player1 0 () in
      match player1_action with
      | Check -> (
          print_lines ();
          ready_input 2 ();
          print_board board ();
          print_endline "Player 2 Hand:";
          print_cards player2.hand;
          print_endline ("Player 2 chips: " ^ string_of_int player2.chips);
          print_endline "Player 2: What would you like to do (check/bet/fold)?";
          let player2_action = get_player_action player2 0 () in
          match player2_action with
          | Check -> transition_to_river p
          | Bet b -> (
              print_lines ();
              ready_input 1 ();
              print_board board ();
              print_endline "Player 1 Hand:";
              print_cards player1.hand;
              print_endline ("Player 2 bet " ^ string_of_int b ^ " chips");
              print_endline ("Player 1 chips: " ^ string_of_int player1.chips);
              print_endline "Player 1: What would you like to do (bet/fold)?";
              let pl2 = remove_chips player2 b in
              let minbet = b in
              let pot2 = pot + b in
              let player1bet_action = get_player_action player1 b () in
              match player1bet_action with
              | Bet b2 ->
                  let pl1 = remove_chips player1 b2 in
                  let finalpot = pot2 + b2 in
                  if finalpot = 200 then
                    transition_to_showdown
                      (Turn
                         {
                           board;
                           player1 = pl1;
                           player2 = pl2;
                           pot = pot2 + b2;
                           minbet;
                           deck;
                         })
                  else
                    transition_to_river
                      (Turn
                         {
                           board;
                           player1 = pl1;
                           player2 = pl2;
                           pot = pot2 + b2;
                           minbet;
                           deck;
                         })
              | Fold -> End { winner = Some pl2; pot = pot2 }
              | Check -> failwith "Impossible")
          | Fold -> End { winner = Some player1; pot })
      | Bet b -> (
          print_lines ();
          ready_input 2 ();
          print_board board ();
          print_endline "Player 2 Hand:";
          print_cards player2.hand;
          print_endline ("Player 1 bet " ^ string_of_int b ^ " chips");
          print_endline "Player 2: What would you like to do (bet/fold)?";
          let pl1 = remove_chips player1 b in
          let minbet = b in
          let pot2 = pot + b in
          let player2bet_action = get_player_action player2 b () in
          match player2bet_action with
          | Bet b2 ->
              let pl2 = remove_chips player2 b2 in
              let finalpot = pot2 + b2 in
              if finalpot = 200 then
                transition_to_showdown
                  (Turn
                     {
                       board;
                       player1 = pl1;
                       player2 = pl2;
                       pot = pot2 + b2;
                       minbet;
                       deck;
                     })
              else
                transition_to_river
                  (Turn
                     {
                       board;
                       player1 = pl1;
                       player2 = pl2;
                       pot = pot2 + b2;
                       minbet;
                       deck;
                     })
          | Fold -> End { winner = Some pl1; pot = pot2 }
          | Check -> failwith "Impossible")
      | Fold -> End { winner = Some player2; pot })
  | River { board; player1; player2; pot; minbet; deck } as p -> (
      print_lines ();
      ready_input 1 ();
      print_board board ();
      print_endline "Player 1 Hand:";
      print_cards player1.hand;
      print_endline ("Player 1 chips: " ^ string_of_int player1.chips);
      print_endline "Player 1: What would you like to do (check/bet/fold)?";
      let player1_action = get_player_action player1 0 () in
      match player1_action with
      | Check -> (
          print_lines ();
          ready_input 2 ();
          print_board board ();
          print_endline "Player 2 Hand:";
          print_cards player2.hand;
          print_endline ("Player 2 chips: " ^ string_of_int player2.chips);
          print_endline "Player 2: What would you like to do (check/bet/fold)?";
          let player2_action = get_player_action player2 0 () in
          match player2_action with
          | Check -> transition_to_showdown p
          | Bet b -> (
              print_lines ();
              ready_input 1 ();
              print_board board ();
              print_endline "Player 1 Hand:";
              print_cards player1.hand;
              print_endline ("Player 2 bet " ^ string_of_int b ^ " chips");
              print_endline ("Player 1 chips: " ^ string_of_int player1.chips);
              print_endline "Player 1: What would you like to do (bet/fold)?";
              let pl2 = remove_chips player2 b in
              let minbet = b in
              let pot2 = pot + b in
              let player1bet_action = get_player_action player1 b () in
              match player1bet_action with
              | Bet b2 ->
                  let pl1 = remove_chips player1 b2 in
                  transition_to_showdown
                    (River
                       {
                         board;
                         player1 = pl1;
                         player2 = pl2;
                         pot = pot2 + b2;
                         minbet;
                         deck;
                       })
              | Fold -> End { winner = Some pl2; pot = pot2 }
              | Check -> failwith "Impossible")
          | Fold -> End { winner = Some player1; pot })
      | Bet b -> (
          print_lines ();
          ready_input 2 ();
          print_board board ();
          print_endline "Player 2 Hand:";
          print_cards player2.hand;
          print_endline ("Player 1 bet " ^ string_of_int b ^ " chips");
          print_endline "Player 2: What would you like to do (bet/fold)?";
          let pl1 = remove_chips player1 b in
          let minbet = b in
          let pot2 = pot + b in
          let player2bet_action = get_player_action player2 b () in
          match player2bet_action with
          | Bet b2 ->
              let pl2 = remove_chips player2 b2 in
              transition_to_showdown
                (River
                   {
                     board;
                     player1 = pl1;
                     player2 = pl2;
                     pot = pot2 + b2;
                     minbet;
                     deck;
                   })
          | Fold -> End { winner = Some pl1; pot = pot2 }
          | Check -> failwith "Impossible")
      | Fold -> End { winner = Some player2; pot })
  | Showdown s -> transition_to_end game
  | End _ -> failwith ""
