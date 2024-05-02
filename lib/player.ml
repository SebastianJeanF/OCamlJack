open Card

type player = {
  name : string;
  hand : card list;
  balance : int;
  bet : int;
}

exception InsufficientBalance

let max_value = 21
let get_balance p = p.balance
let get_hand p = p.hand
let get_name p = p.name
let create name = { name; hand = []; balance = 0; bet = 0 }

let init_balance balance = function
  | { name; hand; bet; _ } -> { name; hand; balance; bet }

let add_card card p =
  match p with
  | { name; hand; balance; bet } -> { name; hand = card :: hand; balance; bet }

let get_hand_value p =
  let rec helper hand value aces_count =
    match hand with
    | [] -> value
    | h :: t ->
        let rank = Card.get_rank h in
        if rank = 1 then
          let new_value1 = helper t (value + 1) (aces_count + 1) in
          let new_value2 =
            if value + 11 <= max_value then helper t (value + 11) aces_count
            else new_value1
          in
          max new_value1 new_value2
        else helper t (value + min rank 10) aces_count
  in
  helper p.hand 0 0

(** [multiply bet factor] is (integer [bet]) times (float [factor]), rounded up
    to nearest integer *)
let mulitply bet factor =
  let round_up num = int_of_float (num +. 1.) in
  round_up (float_of_int bet *. factor)

let win_bet factor = function
  | { name; hand; balance; bet } ->
      (* [TODO] Fix win_bet, appears to be a off-by-one error with balance *)
      let new_balance = balance + mulitply bet factor in
      { name; hand; balance = new_balance; bet = 0 }

let place_bet bet = function
  | { name; hand; balance; _ } ->
      let new_balance = balance - bet in
      if new_balance < 0 then raise InsufficientBalance
      else { name; hand; balance = new_balance; bet }

let multiply_bet factor = function
  | { name; hand; balance; bet } ->
      let new_bet = mulitply bet factor in
      let new_balance = balance - (new_bet - bet) in
      if new_balance < 0 then raise InsufficientBalance
      else { name; hand; balance = new_balance; bet = new_bet }

let is_bust p = get_hand_value p > max_value
let clear_hand p = { p with hand = [] }
