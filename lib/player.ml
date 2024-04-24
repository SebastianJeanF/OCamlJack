open Card

type player = {
  name : string;
  hand : card list;
  balance : int;
}

let max_value = 21
let get_balance p = p.balance
let get_hand p = p.hand
let get_name p = p.name
let create name = { name; hand = []; balance = 0 }

let add_card card p =
  match p with
  | { name; hand; balance } -> { name; hand = card :: hand; balance }

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

let is_bust p = get_hand_value p > max_value
