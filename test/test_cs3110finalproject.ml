(* @authors Eric Han (eh636), Sebastian Jean-Francois (sj728), Sriram Murali
   (ssm238), Varun Gande (vg262) *)

open OUnit2
(* open Cs3110finalproject.Game open Cs3110finalproject.Player *)

(*---------DECK---------*)

open Cs3110finalproject.Deck
open Cs3110finalproject.Card

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
        List.fold_left (fun acc' suit -> create rank suit :: acc') acc all_suits)
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
  let c = create 1 Spades in
  assert_equal 1 (get_rank c);
  assert_equal Spades (get_suit c)

let test_to_string_card _ =
  let c = create 7 Clubs in
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

(*--------PLAYER--------*)

(*---UNIT TEST SUITE----*)

let suite =
  "Test suite"
  >::: [
         "rank and suit of created card" >:: test_create_card;
         "string representation of card" >:: test_to_string_card;
         "created deck contains all cards" >:: test_create_deck;
         "shuffled deck contains all cards" >:: test_shuffle_deck;
         "drawn card" >:: test_draw_card;
       ]

let () = run_test_tt_main suite
