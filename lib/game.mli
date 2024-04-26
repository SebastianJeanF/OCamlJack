(* @authors Eric Han (eh636), Sebastian Jean-Francois (sj728), Sriram Murali
   (ssm238), Varun Gande (vg262) *)

(** [dealer_strategy] describes what the dealer does *)
type dealer_strategy = HitUntil of int

(** [state] describes what part of the game is currently running*)
type state =
  | NewPlayer
  | Bust
  | Continue
  | End
  | TryAgain

(** [t] represents information about the game *)
type t = {
  mutable players : Player.player array;
  mutable curr_player : int;
  mutable deck : Deck.deck;
  mutable state : state;
  mutable dealer_strategy : dealer_strategy;
}

(** [move] represents the availability of moves a user can do*)
type move =
  | Hit
  | Stand
  | DoubleDown

val add_player : string -> t -> t
(** [add_player player game] is an updated [game] session with [player] added *)

val update : move -> t -> t
(** [update move game] is an updated [game] where the current player made
    [move] *)

val get_dealer : t -> Player.player
(** [get_dealer game] is the dealer of the [game] *)

val get_curr_player : t -> Player.player
(** [get_curr_player game] is the current player of the [game] *)

val get_state : t -> state
(** [get_state game] is the current state of the [game]*)

val get_end_result : t -> (Player.player * bool) array
(** [get_end_result game] is a reporting of every player in the [game]
    associataed with whether they won their bet or not *)

val make_dealer : Deck.deck -> dealer_strategy -> t
(** [make_dealer deck strategy] creates a new game dealer with the given deck
    and strategy. *)

val new_game : dealer_strategy -> t
(** [new_game strategy] creates a new game with the given dealer strategy. *)

val update_dealer : t -> unit
(** [update_dealer game] updates the dealer's hand according to the game's
    dealer strategy. *)

val has_won : Player.player -> t -> bool
(** [has_won player game] checks if the given player wins the game. *)


(** [set_balances balance game] is an updated [game] where every
    player starts with [balance]. Requires: function is only be run before
    starting the [game]; [balance] is a non-negative integer *)
val set_balances : int -> t -> t