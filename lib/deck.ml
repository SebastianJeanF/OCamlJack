(*ssm238*)

open Card

type deck = card list

let create_deck () =
  let ranks = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13] in
  let suits = [Spades; Hearts; Diamonds; Clubs] in
  let make_card rank suit = create rank suit in
  let cards = List.concat (List.map (fun s -> List.map (make_card s) suits) ranks) in
  cards

let shuffle_deck deck =
  Random.self_init ();
  let rec shuffle = function
    | [] -> []
    | [x] -> [x]
    | lst ->
        let len = List.length lst in
        let i = Random.int len in
        let el = List.nth lst i in
        let rest = List.filter (fun x -> x <> el) lst in
        el :: shuffle rest
  in
  let shuffled = shuffle deck in
  List.iter (fun x -> ignore (Random.int (get_rank x))) shuffled 

let draw_card = function
  | [] -> failwith "Empty deck"
  | card :: rest -> (card, rest)
