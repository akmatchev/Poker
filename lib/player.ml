open Cards

type player = {
  name : string;
  hand : card list;
  chips : int;
}

let deal_to_player card player =
  { name = player.name; hand = card :: player.hand; chips = player.chips }

(** [create_player id hand chips] creates a new player with the given [id],
    initial [hand], and starting number of [chips]. *)
let create_player name hand chips = { name; hand; chips }

(** [get_player_id player] returns the name of the given [player]. *)
let get_player_id player = player.name

(** [get_player_hand player] returns the hand of the given [player]. *)
let get_player_hand player = player.hand

(** [get_player_chips player] returns the number of chips the given [player]
    has. *)
let get_player_chips player = player.chips

(** [update_player_hand player new_hand] updates the hand of the given [player]
    with the new hand [new_hand]. *)
let update_player_hand player new_hand = { player with hand = new_hand }

(** [update_player_chips player new_chips] updates the number of chips of the
    given [player] with the new amount [new_chips]. *)
let update_player_chips player new_chips = { player with chips = new_chips }

(** [add_chips player amount] adds the specified [amount] of chips to the given
    [player]. *)
let add_chips player amount =
  let current_chips = get_player_chips player in
  update_player_chips player (current_chips + amount)

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
