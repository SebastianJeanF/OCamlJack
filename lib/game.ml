type dealer_strategy = HitUntil of int

type state =
  | NewPlayer
  | Bust
  | Continue
  | End
  | TryAgain

type move =
  | Hit
  | Stand
  | DoubleDown

type t = {
  (* The right-most player in the players array is the dealer *)
  mutable players : Player.player array;
  mutable curr_player : int;
  mutable deck : Deck.deck;
  mutable state : state;
  mutable dealer_strategy : dealer_strategy;
}

let get_dealer g =
  let dealer_idx = Array.length g.players - 1 in
  g.players.(dealer_idx)

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
  (* We want to insert this new player at the SECOND to last spot in the players
     array. Example: [A, B, Dealer] -> add C -> [A, B, C, Dealer] *)
  let len = Array.length game.players in
  let updated_players = Array.make (len + 1) player in
  if len > 1 then Array.blit game.players 0 updated_players 0 (len - 1);

  (* We ensure that dealer is still last *)
  updated_players.(len) <- get_dealer game;
  game.players <- updated_players;
  game

let add_player name game =
  let card, updated_deck = Deck.draw_card game.deck in
  let player = Player.create name |> Player.add_card card in
  game.deck <- updated_deck;
  add_player_helper game player

let new_game strategy =
  let deck = Deck.create_deck () |> Deck.shuffle_deck in
  make_dealer deck strategy

let get_curr_player game = game.players.(game.curr_player)

let set_balances balance g =
  (*All non-dealer players will have their balances initialized*)
  for i = 0 to Array.length g.players - 2 do
    let player = g.players.(i) in
    g.players.(i) <- Player.init_balance balance player
  done;
  g

let get_state g = g.state

(** At the end of the game, the dealer needs to be updated with the appropriate
    amount of cards based on the dealer_strategy *)
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
  let dealer_idx = Array.length g.players - 1 in
  g.players.(dealer_idx) <- updated_dealer

let update move game =
  if game.state = End then game
  else
    let curr_player = get_curr_player game in
    match move with
    | Hit ->
        let drawn_card, updated_deck = Deck.draw_card game.deck in
        let updated_player = Player.add_card drawn_card curr_player in

        let new_state =
          (* If the current player busts, the Game State becomes Bust if there
             is at least one more player who needs to play; otherwise it becomes
             End *)
          if Player.is_bust updated_player then
            let is_last_player =
              Array.length game.players - 2 == game.curr_player
            in
            if is_last_player then End else Bust
          else Continue
        in
        game.state <- new_state;
        game.deck <- updated_deck;
        game.players.(game.curr_player) <- updated_player;
        if new_state == Bust then game.curr_player <- game.curr_player + 1;
        game
    | Stand ->
        let is_last_player =
          (* Last non-dealer player at index (len - 2) *)
          Array.length game.players - 2 == game.curr_player
        in
        if is_last_player then game.state <- End else game.state <- NewPlayer;
        game.curr_player <- game.curr_player + 1;
        game
    | _ ->
        game.state <- TryAgain;
        game

let has_won p g =
  (not (Player.is_bust p))
  && Player.get_hand_value p > Player.get_hand_value (get_dealer g)

let get_end_result g =
  update_dealer g;
  Array.map (fun p -> (p, has_won p g)) g.players
