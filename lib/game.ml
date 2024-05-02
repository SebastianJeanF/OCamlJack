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
  mutable curr_player_idx : int;
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
    curr_player_idx = 0;
    deck;
    state = End;
    dealer_strategy = strategy;
  }

let init_game strategy =
  let deck = Deck.create_deck () |> Deck.shuffle_deck in
  make_dealer deck strategy

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

let get_curr_player game = game.players.(game.curr_player_idx)

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

        (* If the current player busts, the Game State becomes Bust if there is
           at least one more player who needs to play; otherwise it becomes End
           and game is over *)
        let new_state =
          if Player.is_bust updated_player then
            let is_last_player =
              Array.length game.players - 2 == game.curr_player_idx
            in
            if is_last_player then End else Bust
          else Continue
        in
        game.state <- new_state;
        game.deck <- updated_deck;
        game.players.(game.curr_player_idx) <- updated_player;
        if new_state == Bust then
          game.curr_player_idx <- game.curr_player_idx + 1;
        game
    | Stand ->
        let is_last_player =
          (* Last non-dealer player at index (len - 2) *)
          Array.length game.players - 2 == game.curr_player_idx
        in
        if is_last_player then game.state <- End else game.state <- NewPlayer;
        game.curr_player_idx <- game.curr_player_idx + 1;
        game
    | _ ->
        game.state <- TryAgain;
        game

let has_won p g =
  (not (Player.is_bust p))
  && (Player.is_bust (get_dealer g)
     || Player.get_hand_value p > Player.get_hand_value (get_dealer g))

(* At the end of the game, each player needs to win back their bet if they won
   the game*)
let update_players g =
  let updated_players =
    Array.map
      (fun p -> if has_won p g then Player.win_bet 2. p else p)
      g.players
  in
  g.players <- updated_players

let compute_end_result g =
  update_dealer g;
  update_players g;
  g.curr_player_idx <- 0;
  Array.map (fun p -> (p, has_won p g)) g.players

let place_bet amount g =
  let curr_player = get_curr_player g in
  if curr_player = get_dealer g then g
  else
    let curr_player = get_curr_player g in
    let updated_player = Player.place_bet amount curr_player in
    g.players.(g.curr_player_idx) <- updated_player;
    g.curr_player_idx <- g.curr_player_idx + 1;
    g

(* [TODO] Make start_game function that changes game state from End to
   NewPlayer, and sets curr_player to equal 0; Check if there is at least one
   non-dealer player in the game *)

let clear_hands g =
  let updated_players = Array.map (fun p -> Player.clear_hand p) g.players in
  g.players <- updated_players;
  g

let deal_hands g =
  (* All non-dealer players are handed two cards*)
  for i = 0 to Array.length g.players - 2 do
    let card1, deck = Deck.draw_card g.deck in
    let card2, deck = Deck.draw_card deck in
    g.players.(i) <- Player.add_card card1 (Player.add_card card2 g.players.(i));
    g.deck <- deck
  done;

  (* Give dealer one card *)
  let card1, deck = Deck.draw_card g.deck in
  let dealer_idx = Array.length g.players - 1 in
  g.players.(dealer_idx) <- get_dealer g |> Player.add_card card1;
  g.deck <- deck;
  g

let start_game g =
  g.deck <- Deck.(create_deck () |> shuffle_deck);
  g.state <- NewPlayer;
  g.curr_player_idx <- 0;
  clear_hands g |> deal_hands
