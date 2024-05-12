(* @authors Eric Han (eh636), Sebastian Jean-Francois (sj728), Sriram Murali
   (ssm238), Varun Gande (vg262) *)

open OUnit2
open Cs3110finalproject
open Cs3110finalproject.Deck
open Cs3110finalproject.Card
open Cs3110finalproject.Game
open Cs3110finalproject.Player
(*---------DECK---------*)

let compare_decks expected actual =
  let sorted_expected =
    List.sort (fun c1 c2 -> compare (to_string c1) (to_string c2)) expected
  in
  let sorted_actual =
    List.sort (fun c1 c2 -> compare (to_string c1) (to_string c2)) actual
  in
  assert_equal sorted_expected sorted_actual

let test_create_deck _ =
  let deck = create_deck () in
  let all_ranks = List.init 13 (fun i -> i + 1) in
  let all_suits = [ Spades; Hearts; Diamonds; Clubs ] in
  let all_cards =
    List.fold_left
      (fun acc rank ->
        List.fold_left
          (fun acc' suit -> Card.create rank suit :: acc')
          acc all_suits)
      [] all_ranks
  in
  compare_decks all_cards deck

let test_shuffle_empty_deck _ =
  let empty_deck = [] in
  let shuffled_empty_deck = shuffle_deck [] in
  assert_equal (List.length empty_deck) (List.length shuffled_empty_deck);
  compare_decks empty_deck shuffled_empty_deck

let test_shuffle_deck _ =
  let deck = create_deck () in
  let shuffled_deck = shuffle_deck deck in
  assert_equal (List.length deck) (List.length shuffled_deck);
  compare_decks deck shuffled_deck

let test_draw_card _ =
  let deck = create_deck () in
  let card, remaining_deck = draw_card deck in
  assert_bool "Drawn card is from the deck" (List.mem card deck);
  assert_bool "Remaining deck does not contain drawn card"
    (not (List.mem card remaining_deck));
  assert_equal (List.length deck - 1) (List.length remaining_deck)

let test_draw_card_empty_deck _ =
  let empty_deck = [] in
  assert_raises (Failure "Empty deck") (fun () -> draw_card empty_deck)

(*---------CARD---------*)

let test_create_card _ =
  let c = Card.create 1 Spades in
  assert_equal 1 (get_rank c);
  assert_equal Spades (get_suit c)

let test_to_string_card _ =
  let c = Card.create 7 Clubs in
  assert_equal "7 of Clubs" (to_string c)

(** Helper functions to make
    - make_random_move : ? -> Game.move
    - make_random_bet : ? -> int
    - create_ordered_deck : ? -> Deck.t
    - create_reverse_orderd_decl : ? -> Deck.t *)

(** Test that in any dealer strategy game mode
    - The player always win if they don't have over 21 AND the dealer busted *)

(** Test that whenever the dealer is risky
    - Dealer's hand is 20 or greater
    - Each game has random number of people playing? *)

(** Test that whenever the dealer is normal
    - Dealer's hand is 17 or greater *)

(** Test that whenever the dealer is safe
    - Dealer's hand is 14 or greater*)

(** Test for bets
    - Make sure that crashes happen appropriately
    - Betting for 1 person
    - Betting for random number of players at beginning of game is the expected
      values *)

(*---------GAME---------*)
let default_balance = 100

(* Helper function to create a game with a single player, balance default to
   100 *)
let create_test_game strategy num_players =
  let game = ref (init_game strategy) in
  for i = 1 to num_players do
    game := add_player ("Player " ^ string_of_int i) !game
  done;
  set_balances default_balance !game

let dealer_strategy = HitUntil 17

(* let easy_dealer_strategy = HitUntil 14  *)
(* let hard_dealer_strategy = HitUntil 20 *)

(* Helper function to make a random move *)
let make_random_move () =
  let random = Random.int 3 in
  match random with
  | 0 -> Stand
  | 1 -> Hit
  | _ -> DoubleDown

let make_random_simple_move () =
  let random = Random.int 2 in
  match random with
  | 0 -> Stand
  | _ -> Hit

(* Helper function to create a random bet *)

let run_helper f g =
  let rec run_game g =
    match get_state g with
    | End -> g
    | _ -> run_game (update (f ()) g)
  in
  run_game (start_game g)

let run_random_game g = run_helper make_random_move g
let run_random_simple_game g = run_helper make_random_simple_move g
let max_valid_hand = 21

let test_valid_player_win =
  QCheck2.Test.make ~count:1000 ~name:"player wins correctly in 1-player game"
    QCheck2.Gen.unit (fun () ->
      let players =
        create_test_game dealer_strategy 1 |> run_random_game |> get_end_result
      in
      let player_won, player_val, dealer_val =
        ( snd players.(0),
          get_hand_value (fst players.(0)),
          get_hand_value (fst players.(1)) )
      in
      let valid_player_win =
        player_val > dealer_val || dealer_val > max_valid_hand
      in
      (not player_won) || valid_player_win)

let test_valid_player_lost =
  QCheck2.Test.make ~count:1000 ~name:"player loses correctly in 1-player game"
    QCheck2.Gen.unit (fun () ->
      let players =
        create_test_game dealer_strategy 1 |> run_random_game |> get_end_result
      in
      let player_won, player_val, dealer_val =
        ( snd players.(0),
          get_hand_value (fst players.(0)),
          get_hand_value (fst players.(1)) )
      in
      let valid_player_lost =
        (player_val <= dealer_val && dealer_val <= max_valid_hand)
        || player_val > max_valid_hand
      in
      player_won || valid_player_lost)

let test_valid_players_win =
  QCheck2.Test.make ~count:1000
    ~name:"players win correctly in multi-player game" QCheck2.Gen.unit
    (fun () ->
      let num_players = Random.int 10 + 1 in
      let players =
        create_test_game dealer_strategy num_players
        |> run_random_game |> get_end_result
      in
      let valid_player_win player_val dealer_val =
        player_val > dealer_val || dealer_val > max_valid_hand
      in
      let passed = ref true in
      for i = 0 to num_players - 1 do
        let player, has_won = players.(i) in
        let player_val = get_hand_value player in
        let dealer_val = get_hand_value (fst players.(num_players)) in
        if not ((not has_won) || valid_player_win player_val dealer_val) then
          passed := false
      done;
      !passed)

let test_valid_players_lost =
  QCheck2.Test.make ~count:1000
    ~name:"players lose correctly in multi-player game" QCheck2.Gen.unit
    (fun () ->
      let num_players = Random.int 10 + 1 in
      let players =
        create_test_game dealer_strategy num_players
        |> run_random_game |> get_end_result
      in
      let valid_player_lost player_val dealer_val =
        (player_val <= dealer_val && dealer_val <= max_valid_hand)
        || player_val > max_valid_hand
      in
      let passed = ref true in
      for i = 0 to num_players - 1 do
        let player, has_won = players.(i) in
        let player_val = get_hand_value player in
        let dealer_val = get_hand_value (fst players.(num_players)) in
        if not (has_won || valid_player_lost player_val dealer_val) then
          passed := false
      done;
      !passed)

let test_simple_bets_lost =
  QCheck2.Test.make ~count:1000
    ~name:"player lose correct bet amount when they can only stand or hit"
    QCheck2.Gen.unit (fun () ->
      let bet = Random.int 101 in
      let players =
        create_test_game dealer_strategy 1
        |> Game.place_bet bet |> run_random_simple_game |> get_end_result
      in
      let player, has_won = players.(0) in
      let balance = Player.get_balance player in
      has_won || balance = default_balance - bet)

let test_simple_bets_win =
  QCheck2.Test.make ~count:1000
    ~name:"player win correct bet amount when they can only stand or hit"
    QCheck2.Gen.unit (fun () ->
      let bet = Random.int 101 in
      let players =
        create_test_game dealer_strategy 1
        |> Game.place_bet bet |> run_random_simple_game |> get_end_result
      in
      let player, has_won = players.(0) in
      let balance = Player.get_balance player in
      (not has_won) || balance = default_balance + bet)

let test_players_simple_bet_outcomes =
  QCheck2.Test.make ~count:1000
    ~name:"all players get correct bet amount when they can only stand or hit"
    QCheck2.Gen.unit (fun () ->
      let num_players = Random.int 10 + 1 in
      let bets = Array.init num_players (fun _ -> Random.int 101) in

      let game = create_test_game dealer_strategy num_players in
      let players =
        Array.fold_left (fun g bet -> Game.place_bet bet g) game bets
        |> run_random_simple_game |> get_end_result
      in
      let valid_balance_outcome (player, has_won) bet =
        let balance = Player.get_balance player in
        (has_won && balance = default_balance + bet)
        || ((not has_won) && balance = default_balance - bet)
      in
      let passed = ref true in
      for i = 0 to num_players - 1 do
        if not (valid_balance_outcome players.(i) bets.(i)) then passed := false
      done;
      !passed)

let test_multi_game_valid_player_outcome =
  QCheck2.Test.make ~count:100
    ~name:
      "player wins correctly in multi-player session, multiple games in a row"
    QCheck2.Gen.unit (fun () ->
      let passed = ref true in
      for _ = 0 to 10 do
        let num_players = Random.int 10 + 1 in
        let players =
          create_test_game dealer_strategy num_players
          |> run_random_game |> get_end_result
        in
        let valid_player_lost player_val dealer_val =
          (player_val <= dealer_val && dealer_val <= max_valid_hand)
          || player_val > max_valid_hand
        in
        let valid_player_win player_val dealer_val =
          player_val > dealer_val || dealer_val > max_valid_hand
        in
        let passed = ref true in
        for i = 0 to num_players - 1 do
          let player = fst players.(i) in
          let player_val = get_hand_value player in
          let dealer_val = get_hand_value (fst players.(num_players)) in
          if
            not
              (valid_player_win player_val dealer_val
              || valid_player_lost player_val dealer_val)
          then passed := false
        done
      done;
      !passed)

let test_init_game _ =
  let game = init_game (HitUntil 17) in
  assert_equal 1 (Array.length game.players);
  assert_equal End game.state;
  assert_equal (HitUntil 17) game.dealer_strategy

let test_add_player _ =
  let game = init_game (HitUntil 17) in
  let game_with_player = add_player "Player1" game in
  assert_equal 2 (Array.length game_with_player.players);
  assert_equal "Player1" (Player.get_name game_with_player.players.(0))

let test_get_curr_player _ =
  let game = init_game (HitUntil 17) in
  let game_with_player = add_player "Player1" game in
  assert_equal
    (Player.get_name game_with_player.players.(0))
    (Player.get_name (get_curr_player game_with_player))

let test_has_won _ =
  let game = init_game (HitUntil 17) in
  let game_with_player = add_player "Player1" game in
  let player =
    Player.add_card (Card.create 2 Clubs) (Player.create "Player1")
  in
  let dealer =
    Player.add_card (Card.create 11 Hearts) (Player.create "Dealer")
  in
  assert_equal false
    (has_won player { game_with_player with players = [| player; dealer |] })

(* Test case for a tie between player and dealer *)
let test_game_tie _ =
  let game = create_test_game dealer_strategy 1 in
  let player = add_card (Card.create 10 Hearts) (get_curr_player game) in
  let dealer = add_card (Card.create 10 Hearts) (get_dealer game) in
  let game = { game with players = [| player; dealer |] } in
  assert_equal (has_won player game) false

(* Test for a game with multiple players *)
let test_game_with_multiple_players _ =
  let game = create_test_game dealer_strategy 5 in
  assert_equal 6 (Array.length game.players)

(* Test for a game with the maximum number of players *)
let test_game_with_max_players _ =
  let game = create_test_game dealer_strategy 10 in
  assert_equal 11 (Array.length game.players)

(* Test for a game with no players *)
let test_game_with_no_players _ =
  let game = create_test_game dealer_strategy 0 in
  assert_equal 1 (Array.length game.players)

(* Test for a game with a different dealer strategy *)
let test_game_with_different_dealer_strategy _ =
  let game = create_test_game (HitUntil 14) 1 in
  assert_equal (HitUntil 14) game.dealer_strategy

(* Test for a tie between player and dealer in a multi-player game *)
let test_multi_player_game_tie _ =
  let game = create_test_game dealer_strategy 5 in
  let players = game.players in
  let dealer = get_dealer game in
  Array.iter
    (fun player ->
      let player = add_card (Card.create 10 Hearts) player in
      let dealer = add_card (Card.create 10 Hearts) dealer in
      let game = { game with players = [| player; dealer |] } in
      assert_equal (has_won player game) false)
    players

(* Test case for hitting when the player's hand is not bust *)
(* Helper function to add cards to a player's hand *)
let add_cards cards player =
  List.fold_left (fun p c -> Player.add_card c p) player cards

(* Test case for hitting when the player's hand already contains 21 points *)
let test_update_hit_at_21 _ =
  let game = create_test_game dealer_strategy 1 in
  let player_cards = [ Card.create 10 Hearts; Card.create 11 Spades ] in
  (* Player's hand already has 21 *)
  let player = add_cards player_cards (get_curr_player game) in
  let game = { game with players = Array.make 2 player } in
  let updated_game = update Hit game in
  assert_equal (get_state updated_game) End

(* Test case for standing *)
let test_update_stand _ =
  let game = create_test_game dealer_strategy 1 in
  let player_cards = [ Card.create 2 Clubs; Card.create 10 Hearts ] in
  let player = add_cards player_cards (get_curr_player game) in
  let game = { game with players = Array.make 2 player } in
  let updated_game = update Stand game in
  assert_equal (get_state updated_game) End

(* Test case for double down when the player has sufficient balance *)
let test_update_double_down_sufficient_balance _ =
  let game = create_test_game dealer_strategy 1 in
  let player_cards = [ Card.create 2 Clubs; Card.create 10 Hearts ] in
  let player = add_cards player_cards (get_curr_player game) in
  let game = { game with players = Array.make 2 player } in
  let updated_game = update DoubleDown game in
  assert_equal (get_state updated_game) End

(* Test that the get_dealer function returns the dealer player *)
let test_get_dealer _ =
  let game = init_game (HitUntil 17) in
  let dealer = get_dealer game in
  assert_equal (Player.get_name dealer) "Dealer"

(* Test that set_balances sets the balances of all non-dealer players *)
let test_set_balances _ =
  let game = init_game (HitUntil 17) in
  let game = add_player "Player 1" game in
  let game = add_player "Player 2" game in
  let game = set_balances 100 game in
  let player1 = game.players.(0) in
  let player2 = game.players.(1) in
  assert_equal (Player.get_balance player1) 100;
  assert_equal (Player.get_balance player2) 100

(* Test that get_state returns the correct game state *)
let test_get_state _ =
  let game = init_game (HitUntil 17) in
  assert_equal (get_state game) End

(*---------PLAYER---------*)

(* Test cases for get_balance *)
let test_get_balance _ =
  let player = Player.create "Alice" in
  let player_with_balance = Player.init_balance 100 player in
  assert_equal 100 (Player.get_balance player_with_balance)

(* Test cases for get_hand *)
let test_get_hand _ =
  let player = Player.create "Bob" in
  let card1 = Card.create 1 Card.Spades in
  let card2 = Card.create 13 Card.Hearts in
  let player_with_cards =
    Player.add_card card2 (Player.add_card card1 player)
  in
  assert_equal [ card2; card1 ] (Player.get_hand player_with_cards)

(* Test cases for get_name *)
let test_get_name _ =
  let player = Player.create "Charlie" in
  assert_equal "Charlie" (Player.get_name player)

(* Test cases for init_balance *)
let test_init_balance _ =
  let player = Player.create "Eve" in
  let player_with_balance = Player.init_balance 1000 player in
  assert_equal 1000 (Player.get_balance player_with_balance)

(* Test cases for add_card *)
let test_add_card _ =
  let player = Player.create "Frank" in
  let card = Card.create 5 Card.Diamonds in
  let player_with_card = Player.add_card card player in
  assert_equal [ card ] (Player.get_hand player_with_card)

(* Test cases for get_hand_value *)
let test_get_hand_value _ =
  let card1 = Card.create 2 Card.Spades in
  let card2 = Card.create 3 Card.Hearts in
  let card3 = Card.create 5 Card.Clubs in
  let player = Player.create "Grace" in
  let player_with_cards =
    Player.add_card card3 (Player.add_card card2 (Player.add_card card1 player))
  in
  assert_equal 10 (Player.get_hand_value player_with_cards)

(* Test cases for place_bet *)
let test_place_bet _ =
  let player = Player.create "Ivy" in
  let player_with_balance = Player.init_balance 100 player in
  assert_raises Player.InsufficientBalance (fun () ->
      Player.place_bet 101 player_with_balance);
  let player_with_bet = Player.place_bet 50 player_with_balance in
  assert_equal 50 (Player.get_balance player_with_bet)

(* Test cases for is_bust *)
let test_is_bust _ =
  let card1 = Card.create 1 Card.Spades in
  let card2 = Card.create 13 Card.Hearts in
  let card3 = Card.create 10 Card.Clubs in
  let player = Player.create "Kate" in
  let player_with_cards =
    Player.add_card card3 (Player.add_card card2 (Player.add_card card1 player))
  in
  assert_equal false (Player.is_bust player_with_cards);
  let player_with_cards2 =
    Player.add_card (Card.create 5 Card.Diamonds) player_with_cards
  in
  assert_equal true (Player.is_bust player_with_cards2)

(* Test cases for clear_hand *)
let test_clear_hand _ =
  let player = Player.create "Liam" in
  let card1 = Card.create 1 Card.Spades in
  let card2 = Card.create 13 Card.Hearts in
  let player_with_cards =
    Player.add_card card2 (Player.add_card card1 player)
  in
  let player_with_clear_hand = Player.clear_hand player_with_cards in
  assert_equal [] (Player.get_hand player_with_clear_hand)

(* Test for a player with insufficient balance placing a bet *)
let test_insufficient_balance_placing_bet _ =
  let player = Player.create "Jack" in
  let player_with_balance = Player.init_balance 50 player in
  assert_raises Player.InsufficientBalance (fun () ->
      Player.place_bet 100 player_with_balance)

(* Test for a player getting bust after hitting *)
let test_player_getting_bust_after_hit _ =
  let game = create_test_game dealer_strategy 1 in
  let player = add_card (Card.create 10 Hearts) (get_curr_player game) in
  let player_with_hit =
    update Hit { game with players = [| player; get_dealer game |] }
  in
  assert_equal (get_state player_with_hit) End

(* Test for adding multiple cards to a player's hand *)
let test_add_multiple_cards_to_hand _ =
  let player = Player.create "Jason" in
  let cards =
    [ Card.create 5 Hearts; Card.create 10 Diamonds; Card.create 2 Clubs ]
  in
  let player_with_cards = add_cards cards player in
  assert_equal (List.length cards)
    (List.length (Player.get_hand player_with_cards))

(* Test for a player getting blackjack *)
let test_player_getting_blackjack _ =
  let game = create_test_game dealer_strategy 1 in
  let player = add_card (Card.create 1 Hearts) (get_curr_player game) in
  let player_with_blackjack = add_card (Card.create 11 Hearts) player in
  let game_with_blackjack =
    { game with players = [| player_with_blackjack; get_dealer game |] }
  in
  assert_equal true (has_won player_with_blackjack game_with_blackjack)

(*---------TEST SUITE---------*)
let valid_player_win_check = QCheck_runner.to_ounit2_test test_valid_player_win

let valid_player_lost_check =
  QCheck_runner.to_ounit2_test test_valid_player_lost

let valid_players_win_check =
  QCheck_runner.to_ounit2_test test_valid_players_win

let valid_players_lost_check =
  QCheck_runner.to_ounit2_test test_valid_players_lost

let simple_bets_lost_check = QCheck_runner.to_ounit2_test test_simple_bets_lost
let simple_bets_win_check = QCheck_runner.to_ounit2_test test_simple_bets_win

let players_simple_bet_outcomes_check =
  QCheck_runner.to_ounit2_test test_players_simple_bet_outcomes

let multi_game_valid_player_outcome_check =
  QCheck_runner.to_ounit2_test test_multi_game_valid_player_outcome

let suite =
  "test_suite"
  >::: [
         "rank and suit of created card" >:: test_create_card;
         "string representation of card" >:: test_to_string_card;
         "created deck contains all cards" >:: test_create_deck;
         "shuffled deck contains all cards" >:: test_shuffle_deck;
         "shuffled empty deck is empty" >:: test_shuffle_empty_deck;
         "drawn card" >:: test_draw_card;
         "drawing from empty deck" >:: test_draw_card_empty_deck;
         "test_init_game" >:: test_init_game;
         "test_add_player" >:: test_add_player;
         "test_get_current_player" >:: test_get_curr_player;
         "test_has_won" >:: test_has_won;
         "test_is_tie" >:: test_game_tie;
         "test_game_with_multiple_players" >:: test_game_with_multiple_players;
         "test_game_with_max_players" >:: test_game_with_max_players;
         "test_game_with_no_players" >:: test_game_with_no_players;
         "test_game_with_different_dealer_strategy"
         >:: test_game_with_different_dealer_strategy;
         "test_multi_player_game_tie" >:: test_multi_player_game_tie;
         "test_update_stand" >:: test_update_stand;
         "test_update_double_down_sufficient_balance"
         >:: test_update_double_down_sufficient_balance;
         "test_get_dealer" >:: test_get_dealer;
         "test_set_balances" >:: test_set_balances;
         "test_get_state" >:: test_get_state;
         "test_get_balance" >:: test_get_balance;
         "test_get_hand" >:: test_get_hand;
         "test_get_name" >:: test_get_name;
         "test_init_balance" >:: test_init_balance;
         "test_add_card" >:: test_add_card;
         "test_get_hand_value" >:: test_get_hand_value;
         "test_place_bet" >:: test_place_bet;
         "test_is_bust" >:: test_is_bust;
         "test_update_hit_at_21" >:: test_update_hit_at_21;
         "test_clear_hand" >:: test_clear_hand;
         "test_insufficient_balance_placing_bet"
         >:: test_insufficient_balance_placing_bet;
         "test_player_getting_bust_after_hit"
         >:: test_player_getting_bust_after_hit;
         "test_add_multiple_cards_to_hand" >:: test_add_multiple_cards_to_hand;
         "test_player_getting_blackjack" >:: test_player_getting_blackjack;
         (* QCheck2 Tests*)
         valid_player_win_check;
         valid_player_lost_check;
         valid_players_win_check;
         valid_players_lost_check;
         simple_bets_lost_check;
         simple_bets_win_check;
         players_simple_bet_outcomes_check;
         multi_game_valid_player_outcome_check;
       ]

let _ = run_test_tt_main suite
