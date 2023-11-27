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
