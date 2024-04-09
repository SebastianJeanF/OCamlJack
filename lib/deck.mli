(* @authors Eric Han (eh636), Sebastian Jean-Francois (sj728), 
   Sriram Murali (ssm238), Varun Gande (vg262) *)
   
(** Type representing a deck of cards. *)
type deck

(** [create_deck ()] creates a new deck of cards. *)
val create_deck : unit -> deck

(** [shuffle_deck deck] is a newly-shuffled [deck]. *)
val shuffle_deck : deck -> deck

(** [draw_card deck] draws a card from the given deck, returning the card and the updated deck. *)
val draw_card : deck -> Card.card * deck
