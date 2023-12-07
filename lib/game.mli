open Cards
open Player

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

val init_game : player -> player -> game_state
(** [init_game player1 player2] initializes a new poker game with the given
    [player1] and [player2]. *)

val transition_to_flop : game_state -> game_state
(** [transition_to_flop game] transitions the game state to the Flop state . *)

val transition_to_turn : game_state -> game_state
(** [transition_to_turn game] transitions the game state to the Turn state. *)

val transition_to_river : game_state -> game_state
(** [transition_to_river game] transitions the game state to the River state. *)

val transition_to_showdown : game_state -> game_state
(** [transition_to_showdown game] transitions the game state to the Showdown
    state. *)

val transition_to_end : game_state -> game_state
(** [transition_to_end game] transitions the game state to the End state . *)

val state_to_string : game_state -> string
(**[state_to_string] converts a game state to its string representation. Used
   for testing*)

val get_bet_action : int -> int -> unit -> int
(**[get_bet_action] gets a valid bet action from a player with chips [chips] and
   whose bet to match is [minbet]*)

val get_player_action : player -> int -> unit -> action
(**[get_player_action] gets a valid action from [player] depending on what the
   minimum bet is *)

val round : game_state -> game_state
(**[round] performs a round of poker that is at a given game_state [game]*)
