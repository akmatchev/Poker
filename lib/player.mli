(* player.mli *)

open Cards

type player = {
  name : string;
  hand : card list;
  chips : int;
}

val deal_to_player : card -> player -> player
(** [deal_to_player card player] adds the [card] to the [player]'s hand. *)

val create_player : string -> card list -> int -> player
(** [create_player name hand chips] creates a new player with the given [name],
    initial [hand], and starting number of [chips]. *)

val get_player_id : player -> string
(** [get_player_id player] returns the name of the given [player]. *)

val get_player_hand : player -> card list
(** [get_player_hand player] returns the hand of the given [player]. *)

val get_player_chips : player -> int
(** [get_player_chips player] returns the number of chips the given [player]
    has. *)

val update_player_hand : player -> card list -> player
(** [update_player_hand player new_hand] updates the hand of the given [player]
    with the new hand [new_hand]. *)

val update_player_chips : player -> int -> player
(** [update_player_chips player new_chips] updates the number of chips of the
    given [player] with the new amount [new_chips]. *)

val add_chips : player -> int -> player
(** [add_chips player amount] adds the specified [amount] of chips to the given
    [player]. *)

val remove_chips : player -> int -> player
(** [remove_chips player amount] removes the specified [amount] of chips from
    the given [player]. *)
