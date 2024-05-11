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

type t = {
  mutable players : Player.player array;
  mutable curr_player_idx : int;
  mutable deck : Deck.deck;
  mutable state : state;
  mutable dealer_strategy : dealer_strategy;
}
(** [t] represents information about the game *)

(** [move] represents the availability of moves a user can do*)
type move =
  | Hit
  | Stand
  | DoubleDown


val init_game : dealer_strategy -> t
(** [init_game strategy] creates a new game with the given dealer [strategy]. *)
  
val get_dealer : t -> Player.player
(** [get_dealer game] is the dealer of the [game] *)

val get_curr_player : t -> Player.player
(** [get_curr_player game] is the current player of the [game] *)

val get_state : t -> state
(** [get_state game] is the current state of the [game]*)

val update : move -> t -> t
  (** [update move game] is an updated [game] where the current player made [move]
      in the running [game] *)

val has_won : Player.player -> t -> bool
(** [has_won player game] checks if the given player has won the game. Requires:
    [game] is not currently running; [game] has ran at least once *)

val set_balances : int -> t -> t
(** [set_balances balance game] is an updated [game] where every non-dealer 
    player starts with [balance]. Requires: [game] has never ran before; 
    [balance] is a non-negative integer *)

val add_player : string -> t -> t
(** [add_player name game] is an updated [game] session with player [name]
    added. Requires: [game] has never been ran before; bets have not been placed
    yet *)

val place_bet : int -> t -> t
(** [place_bet bet game] is an updated [game] where the current non-dealer player places
    [bet], incrementing the current player. Requires: [game] is not
    currently running; all players that will be added to [game] have already
    been added; [bet] is a non-negative integer. Raises: [InsufficientBalance]
    if current player cannot afford balance *)

val start_game : t -> t
(** [start_game game] is an updated [game] where the game is started. Requires:
    [game] is not currently running; at least 1 non-human player is in [game]. *)
    
val get_end_result : t -> (Player.player * bool) array
(** [get_end_result game] is a reporting of every player in the [game]
    associataed with whether they won their bet or not. Requires: [game] is not
    currently running; [game] has ran at least once *)

(* [TODO]: get_players function to get list of all players in 
   game -> Used for testing suite *)
