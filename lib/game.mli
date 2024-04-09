(* @authors Eric Han (eh636), Sebastian Jean-Francois (sj728), 
   Sriram Murali (ssm238), Varun Gande (vg262) *)


(** [state] describes what part of the game is currently running*)
type state =
  | NewPlayer
  | Bust
  | Continue
  | End
  | TryAgain

(** [t] represents information about the game *)
type t


(** [new_game ()] is a newly-initialized game containing just the dealer *)
val new_game : unit -> t  

(** [add_player player game] is an updated [game] session with [player]
     added *)
val add_player : string -> t -> t

(** [update move game] is an updated [game] where the current
    player made [move]. Requires: [move] is either "stand" or "hit" *)
val update : string -> t -> t

(** [get_dealer game] is the dealer of the [game] *)
val get_dealer : t -> Player.player 

(** [get_curr_player game] is the current player of the [game] *)
val get_curr_player : t -> Player.player

(** [get_state game] is the current state of the [game]*)
val get_state : t -> state

(** [get_end_result game] is a reporting of every player in the [game]
    associataed with whether they won their bet or not *)
val get_end_result : t -> (Player.player * bool) array 

