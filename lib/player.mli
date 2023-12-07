(* player.mli *)

open Cards

type player = {
  name : string;
  hand : card list;
  chips : int;
}

val get_player_chips : player -> int
(** [get_player_chips player] returns the number of chips the given [player]
    has. *)

val update_player_chips : player -> int -> player
(** [update_player_chips player new_chips] updates the number of chips of the
    given [player] with the new amount [new_chips]. *)

val remove_chips : player -> int -> player
(** [remove_chips player amount] removes the specified [amount] of chips from
    the given [player]. *)
