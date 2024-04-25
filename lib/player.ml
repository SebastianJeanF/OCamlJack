open Card

type player = {
  name : string;
  hand : card list;
  balance : int;
  bet : int;
}

let max_value = 21
let get_balance p = p.balance
let get_hand p = p.hand
let get_name p = p.name
let create name = { name; hand = []; balance = 0; bet = 0 }

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

let update_balance is_win = function
  | { name; hand; balance; bet } ->
      let new_balance = if is_win then balance + (bet * 2) else balance in
      { name; hand; balance = new_balance; bet = 0 }

let place_bet amount = function
  | { name; hand; balance; _ } ->
      let new_balance = balance - amount in
      { name; hand; balance = new_balance; bet = amount }

let is_bust p = get_hand_value p > max_value
