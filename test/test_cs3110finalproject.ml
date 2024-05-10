(* @authors Eric Han (eh636), Sebastian Jean-Francois (sj728), Sriram Murali
   (ssm238), Varun Gande (vg262) *)

open OUnit2
(* open Cs3110finalproject.Game open Cs3110finalproject.Player *)

(*---------DECK---------*)
open Cs3110finalproject
open Cs3110finalproject.Deck
open Cs3110finalproject.Card
open Cs3110finalproject.Game

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

(*---------CARD---------*)

let test_create_card _ =
  let c = Card.create 1 Spades in
  assert_equal 1 (get_rank c);
  assert_equal Spades (get_suit c)

let test_to_string_card _ =
  let c = Card.create 7 Clubs in
  assert_equal "7 of Clubs" (to_string c)

(*---------GAME---------*)
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

(* Helper function to create a game with a single player *)
let create_test_game strategy =
  let game = init_game strategy in
  let game = add_player "Player 1" game in
  set_balances 100 game

let dealer_strategy = HitUntil 17

(* Test case for hitting when the player's hand is not bust *)
(* Helper function to add cards to a player's hand *)
let add_cards cards player =
  List.fold_left (fun p c -> Player.add_card c p) player cards

(* Test case for standing *)
let test_update_stand _ =
  let game = create_test_game dealer_strategy in
  let player_cards = [ Card.create 2 Clubs; Card.create 10 Hearts ] in
  let player = add_cards player_cards (get_curr_player game) in
  let game = { game with players = Array.make 2 player } in
  let updated_game = update Stand game in
  assert_equal (get_state updated_game) End

(* Test case for double down when the player has sufficient balance *)
let test_update_double_down_sufficient_balance _ =
  let game = create_test_game dealer_strategy in
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

let suite =
  "test_suite"
  >::: [
         "rank and suit of created card" >:: test_create_card;
         "string representation of card" >:: test_to_string_card;
         "created deck contains all cards" >:: test_create_deck;
         "shuffled deck contains all cards" >:: test_shuffle_deck;
         "drawn card" >:: test_draw_card;
         "test_init_game" >:: test_init_game;
         "test_add_player" >:: test_add_player;
         "test_get_current_player" >:: test_get_curr_player;
         "test_has_won" >:: test_has_won;
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
         "test_clear_hand" >:: test_clear_hand;
       ]

let _ = run_test_tt_main suite
