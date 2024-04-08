open Card

type player = {
  name : string;
  hand : card list;
  balance : int;
}

let get_balance p = p.balance
let get_hand p = p.hand
let get_name p = p.name
let create name = { name; hand = []; balance = 0 }

let add_card card p =
  match p with
  | { name; hand; balance } -> { name; hand = card :: hand; balance }

let get_hand_value p =
  let rec helper hand =
    match hand with
    | [] -> 0
    | h :: t -> Card.get_rank h + helper t
  in
  helper p.hand
