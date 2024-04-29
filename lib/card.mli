(* @authors Eric Han (eh636), Sebastian Jean-Francois (sj728), Sriram Murali
   (ssm238), Varun Gande (vg262) *)

type rank = int
(** Type representing parts of a playing card. *)

type suit =
  | Spades
  | Hearts
  | Diamonds
  | Clubs

type card
(** Type representing a playing card. *)

val create : rank -> suit -> card
(** [create rank suit] creates a new card with the given rank and suit. *)

val get_rank : card -> rank
(** [get_rank card] returns the rank of the given card. *)

val get_suit : card -> suit
(** [get_suit card] returns the suit of the given card. *)

val to_string : card -> string
(** [to_string card] returns a string representation of the given card. *)
