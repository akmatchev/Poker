open Cards
open Player
open Hands

type game_state =
  | PreFlop of {
      players : player list;
      small_blind : int;
      big_blind : int;
    }
  | Flop of {
      board : card list;
      players : player list;
      pot : int;
    }
  | Turn of {
      board : card list;
      players : player list;
      pot : int;
    }
  | River of {
      board : card list;
      players : player list;
      pot : int;
    }
  | Showdown of {
      board : card list;
      players : player list;
      pot : int;
      winners : player list;
    }
  | End of {
      players : player list;
      winners : player list;
    }

(** [init_game players small_blind big_blind] initializes a new poker game with
    the given [players], [small_blind], and [big_blind]. *)
let init_game players small_blind big_blind =
  PreFlop { players; small_blind; big_blind }

(** [get_players_state game] returns the list of players in the current game
    state. *)
let get_players_state = function
  | PreFlop { players; _ }
  | Flop { players; _ }
  | Turn { players; _ }
  | River { players; _ }
  | Showdown { players; _ }
  | End { players; _ } -> players

(** [get_pot_state game] returns the current pot in the current game state. *)
let get_pot_state = function
  | Flop { pot; _ } | Turn { pot; _ } | River { pot; _ } | Showdown { pot; _ }
    -> pot
  | PreFlop _ | End _ -> 0

(** [get_board_state game] returns the board cards in the current game state. *)
let get_board_state = function
  | Flop { board; _ }
  | Turn { board; _ }
  | River { board; _ }
  | Showdown { board; _ } -> board
  | PreFlop _ | End _ -> []

(** [get_winners_state game] returns the list of winners in the current game
    state. *)
let get_winners_state = function
  | Showdown { winners; _ } | End { winners; _ } -> winners
  | PreFlop _ | Flop _ | Turn _ | River _ -> []

(** [transition_to_flop game flop_cards] transitions the game state to the Flop
    state with the given [flop_cards]. *)
let transition_to_flop game flop_cards =
  match game with
  | PreFlop { players; small_blind; big_blind } ->
      Flop { board = flop_cards; players; pot = small_blind + big_blind }
  | _ -> failwith "Invalid transition to Flop state"

(** [transition_to_turn game turn_card] transitions the game state to the Turn
    state with the given [turn_card]. *)
let transition_to_turn game turn_card =
  match game with
  | Flop { board; players; pot } ->
      Turn { board = turn_card :: board; players; pot }
  | _ -> failwith "Invalid transition to Turn state"

(** [transition_to_river game river_card] transitions the game state to the
    River state with the given [river_card]. *)
let transition_to_river game river_card =
  match game with
  | Turn { board; players; pot } ->
      River { board = river_card :: board; players; pot }
  | _ -> failwith "Invalid transition to River state"

(** [transition_to_showdown game winners] transitions the game state to the
    Showdown state with the given [winners]. *)
let transition_to_showdown game winners =
  match game with
  | River { board; players; pot } -> Showdown { board; players; pot; winners }
  | _ -> failwith "Invalid transition to Showdown state"

(** [transition_to_end game winners] transitions the game state to the End state
    with the given [winners]. *)
let transition_to_end game winners =
  match game with
  | Showdown { players; _ } -> End { players; winners }
  | _ -> failwith "Invalid transition to End state"
