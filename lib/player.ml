open Card

type player = {
  name : string;
  hand : card array;
  balance : int;
}

let get_balance p = p.balance
let get_hand p = p.hand
let get_name p = p.name
