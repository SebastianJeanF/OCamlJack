type dealer_strategy = HitUntil of int

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
  mutable dealer_strategy : dealer_strategy;
}

let make_dealer deck strategy =
  let card, deck = Deck.draw_card deck in
  let dealer = Player.create "Dealer" |> Player.add_card card in
  {
    players = Array.make 1 dealer;
    curr_player = 0;
    deck;
    state = NewPlayer;
    dealer_strategy = strategy;
  }

let add_player_helper game player =
  let temp = Array.make 1 player in
  let updated_array = Array.append temp game.players in
  game.players <- updated_array

let add_player name game =
  let card, updated_deck = Deck.draw_card game.deck in
  let player = Player.create name |> Player.add_card card in
  game.deck <- updated_deck;
  add_player_helper game player;
  game

let new_game strategy =
  let deck = Deck.create_deck () |> Deck.shuffle_deck in
  make_dealer deck strategy

let get_curr_player game = game.players.(game.curr_player)

let get_dealer g =
  let idx = Array.length g.players - 1 in
  g.players.(idx)

let get_state g = g.state

let update_dealer g =
  let rec hit_until strategy dealer deck =
    match strategy with
    | HitUntil limit ->
        if Player.get_hand_value dealer < limit then
          let drawn_card, updated_deck = Deck.draw_card deck in
          let updated_dealer = Player.add_card drawn_card dealer in
          hit_until strategy updated_dealer updated_deck
        else dealer
  in
  let updated_dealer = hit_until g.dealer_strategy (get_dealer g) g.deck in
  let idx = Array.length g.players - 1 in
  g.players.(idx) <- updated_dealer

let update move game =
  if game.state = End then game
  else
    let curr_player = get_curr_player game in
    match move with
    | "hit" ->
        let drawn_card, updated_deck = Deck.draw_card game.deck in
        let updated_player = Player.add_card drawn_card curr_player in
        let new_state =
          if Player.is_bust updated_player then
            let () = update_dealer game in
            Bust
          else Continue
        in
        game.state <- new_state;
        game.deck <- updated_deck;
        game.players.(game.curr_player) <- updated_player;
        game
    | "stand" ->
        game.curr_player <- game.curr_player + 1;
        game.state <- End;
        update_dealer game;
        game
    | _ ->
        game.state <- TryAgain;
        game

let is_victory p g =
  (not (Player.is_bust p))
  && Player.get_hand_value p > Player.get_hand_value (get_dealer g)

let get_end_result g = Array.map (fun p -> (p, is_victory p g)) g.players
