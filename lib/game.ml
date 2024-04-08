(* @authors Eric Han (eh636), Sebastian Jean-Francois (sj728), Sriram Murali
   (ssm238), Varun Gande (vg262) *)

type state =
  | NewPlayer
  | Bust
  | Continue
  | End
  | TryAgain

type t = {
  mutable players : Player.player array;
  mutable curr_player : int;
  mutable deck : Deck.deck;
  mutable state : state;
}

let make_dealer deck =
  let card, deck = Deck.draw_card deck in
  let dealer = Player.(create "Dealer" |> add_card card) in
  { players = Array.make 1 dealer; curr_player = 0; deck; state = NewPlayer }

let add_player_helper game player =
  let temp = Array.make 1 player in
  let updated_array = Array.append temp game.players in
  game.players <- updated_array

let add_player name game =
  let card, updated_deck = Deck.draw_card game.deck in
  let player = Player.(create name |> add_card card) in
  game.deck <- updated_deck;
  add_player_helper game player;
  game

let new_game () =
  let deck = Deck.create_deck () in
  make_dealer deck

let get_curr_player game =
  let idx = game.curr_player in
  game.players.(idx)

let get_dealer g =
  let idx = Array.length g.players - 1 in
  g.players.(idx)

let get_state g = g.state
let max_value = 21

let update move game =
  if game.state = End then game
  else
    let curr_player = get_curr_player game in
    match move with
    | "Hit" ->
        let drawn_card, updated_deck = Deck.draw_card game.deck in
        let updated_player = Player.add_card drawn_card curr_player in
        let hand_value = Player.get_hand_value updated_player in
        let new_state = if hand_value > max_value then Bust else Continue in
        let () = game.state <- new_state in
        let () = game.deck <- updated_deck in
        game
    | "Stand" ->
        game.curr_player <- game.curr_player + 1;
        game.state <- End;
        game
    | _ ->
        game.state <- TryAgain;
        game

let get_end_result g =
  Array.map (fun a -> (a, Player.get_hand_value a <= max_value)) g.players
