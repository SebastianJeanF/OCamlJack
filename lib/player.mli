(* @authors Eric Han (eh636), Sebastian Jean-Francois (sj728), Sriram Murali
   (ssm238), Varun Gande (vg262) *)

open Card

type player


(** [get_balance player] is the token balance of [player] *)
val get_balance : player -> int

(** [get_hand] is the hand of the [player] *)
val get_hand : player -> card list

(** [get_name player] is the name of [player] *)
val get_name : player -> string

(** [place_bet x] is a player who placed a bet value [x] in a game. *)
val place_bet : int -> player -> player

(** [update_balance is_win] is a player who won twice their 
    bet if [is_win] is true, and lost their bet if [is_win] is false *)
val update_balance : bool -> player -> player

(** [add_card card player] is a [player] given an additional [card] *)
val add_card : card -> player -> player

(** [get_hand_value player] is the value of [player]'s hand *)
val get_hand_value : player -> int 


(** [is_bust player] is whether [player] has a bust or not*)
val is_bust : player -> bool

(** [create name] is a new player called [name]*)
val create : string -> player