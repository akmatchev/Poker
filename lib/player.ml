open Cards

type player = {
  name : string;
  hand : card list;
  chips : int;
}

(** [get_player_chips player] returns the number of chips the given [player]
    has. *)
let get_player_chips player = player.chips

(** [update_player_chips player new_chips] updates the number of chips of the
    given [player] with the new amount [new_chips]. *)
let update_player_chips player new_chips = { player with chips = new_chips }

(** [remove_chips player amount] removes the specified [amount] of chips from
    the given [player]. *)
let remove_chips player amount =
  let current_chips = get_player_chips player in
  if current_chips >= amount then
    update_player_chips player (current_chips - amount)
  else
    (* Optional: Handle the case where trying to remove more chips than the
       player has. *)
    failwith "Not enough chips to remove"
