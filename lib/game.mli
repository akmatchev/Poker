open Cards
open Player

type game_state =
  | PreFlop of {
      player1 : player;
      player2 : player;
      pot : int;
      minbet : int;
    }
  | Flop of {
      board : card list;
      player1 : player;
      player2 : player;
      pot : int;
      minbet : int;
    }
  | Turn of {
      board : card list;
      player1 : player;
      player2 : player;
      pot : int;
      minbet : int;
    }
  | River of {
      board : card list;
      player1 : player;
      player2 : player;
      pot : int;
      minbet : int;
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

val init_game : player -> player -> game_state
(** [init_game player1 player2] initializes a new poker game with the given
    [player1] and [player2]. *)

val get_pot : game_state -> int
(** [get_pot game] returns the current pot in the current game state. *)

val get_board_state : game_state -> card list
(** [get_board_state game] returns the board cards in the current game state. *)

val transition_to_flop : game_state -> card list -> game_state
(** [transition_to_flop game flop_cards] transitions the game state to the Flop
    state with the given [flop_cards]. *)

val transition_to_turn : game_state -> card -> game_state
(** [transition_to_turn game turn_card] transitions the game state to the Turn
    state with the given [turn_card]. *)

val transition_to_river : game_state -> card -> game_state
(** [transition_to_river game river_card] transitions the game state to the
    River state with the given [river_card]. *)

val transition_to_showdown : game_state -> player list -> game_state
(** [transition_to_showdown game winners] transitions the game state to the
    Showdown state with the given [winners]. *)

val transition_to_end : game_state -> player list -> game_state
(** [transition_to_end game winners] transitions the game state to the End state
    with the given [winners]. *)
