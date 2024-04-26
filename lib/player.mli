(* @authors Eric Han (eh636), Sebastian Jean-Francois (sj728), Sriram Murali
   (ssm238), Varun Gande (vg262) *)

open Card

type player

exception InsufficientBalance

(** [get_balance player] is the token balance of [player] *)
val get_balance : player -> int

(** [get_hand] is the hand of the [player] *)
val get_hand : player -> card list

(** [get_name player] is the name of [player] *)
val get_name : player -> string

(** [place_bet bet player] is an updated [player] who placed a bet value [bet]
    in a game, resulting in [player]'s balance decreasing by [bet]. Raises: 
    [InsufficientBalance] if [bet] is less than [player]'s balance  *)
val place_bet : int -> player -> player

(** [update_bet factor player] is an updated [player] whose bet increased
    by [factor] (rounded up to nearest whole number).
    Raises: [InsufficientBalance] if [player] cannot afford
    the increased bet out of their balance. Requires: [factor] is 1 or greater
    *)
val update_bet : float -> player -> player 

(** [update_balance factor player] is an updated [player] who won a [factor] 
    times their bet (rounded up to nearest whole number), their balance 
    increasing by that amount; [player]'s bet resets to 0. Example:
    Player.balance = 4, Player.bet = 3  -> update_balance 2. player ->
    Player.balance = 4 + 2*3, Player.bet = 0 *)
val update_balance : float -> player -> player

(** [add_card card player] is a [player] given an additional [card] *)
val add_card : card -> player -> player

(** [get_hand_value player] is the value of [player]'s hand *)
val get_hand_value : player -> int 


(** [is_bust player] is whether [player] has a bust or not*)
val is_bust : player -> bool

(** [create name] is a new player called [name] *)
val create : string  ->  player

(** [init_balance balance] is an updated [player] with their balance
    initialized to [balance]. Requires: [balance] is a non-negative integer *)
val init_balance : int -> player -> player