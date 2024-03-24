(*ssm238*)
type rank = int
type suit = Spades | Hearts | Diamonds | Clubs

type card = { rank : rank; suit : suit }

let create rank suit = { rank; suit }

let get_rank card = card.rank

let get_suit card = card.suit

let rank_to_string = function
  | 1 -> "Ace"
  | 2 -> "2"
  | 3 -> "3"
  | 4 -> "4"
  | 5 -> "5"
  | 6 -> "6"
  | 7 -> "7"
  | 8 -> "8"
  | 9 -> "9"
  | 10 -> "10"
  | 11 -> "Jack"
  | 12 -> "Queen"
  | 13 -> "King"
  | _ -> failwith "Invalid rank"

let suit_to_string = function
  | Spades -> "Spades"
  | Hearts -> "Hearts"
  | Diamonds -> "Diamonds"
  | Clubs -> "Clubs"

let to_string card =
  let rank_str = rank_to_string card.rank in
  let suit_str = suit_to_string card.suit in
  rank_str ^ " of " ^ suit_str