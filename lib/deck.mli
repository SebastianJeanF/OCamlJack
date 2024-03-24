(*ssm238*)
(** Type representing a deck of cards. *)
type deck

(** [create_deck ()] creates a new deck of cards. *)
val create_deck : unit -> deck

(** [shuffle_deck deck] shuffles the given deck of cards. *)
val shuffle_deck : deck -> unit

(** [draw_card deck] draws a card from the given deck, returning the card and the updated deck. *)
val draw_card : deck -> Card.card * deck
