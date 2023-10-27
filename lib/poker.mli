module Poker : sig
  type suit = Hearts | Diamonds | Clubs | Spades
  type card = { rank : int; suit : suit }

  val ranks : int list
  val suits : suit list
  val suit_of_string : string -> suit option
  val suit_to_string : suit -> string
  val rank_to_string : int -> string
  val card_to_ascii : card -> string
  val create_deck : unit -> card list
  val print_card : int -> suit -> unit
  val empty_deck : 'a list
  val full_deck : card list
  val print_deck : card list -> unit
  (* val print_full_deck : unit *)
  val draw_card : card list -> card * card list
end
