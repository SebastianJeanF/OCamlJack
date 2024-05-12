(* @authors Eric Han (eh636), Sebastian Jean-Francois (sj728), Sriram Murali
   (ssm238), Varun Gande (vg262) *)
module O = Cs3110finalproject

let title =
  "   ___   ____                _       _            _    _ \n\
  \  / _ \\ / ___|__ _ _ __ ___ | |     | | __ _  ___| | _| |\n\
  \ | | | | |   / _` | '_ ` _ \\| |  _  | |/ _` |/ __| |/ / |\n\
  \ | |_| | |__| (_| | | | | | | | | |_| | (_| | (__|   <|_|\n\
  \  \\___/ \\____\\__,_|_| |_| |_|_|  \\___/ \\__,_|\\___|_|\\_(_)"

(* let print_card c = let rank, suit = O.Card.(get_rank c, get_suit c) in match
   suit with *)

let turn_message =
  "What move do you want to make next?\n\
  \ (Type 'stand', 'hit', or 'double down'): "

let try_again_message =
  "Invalid move. Valid moves are 'stand', 'hit', or 'double down'."

let no_double_down_message =
  "You can't double down! You don't have enough tokens!"

let player_bust_message =
  "You lost! Your hand totaled higher than 21 and you busted!"

let player_lost_message = "You lost! You didn't total higher than the dealer!"

let dealer_bust_message =
  "You won! The dealer totaled higher than 21 and busted!"

let player_win_message = "You won! You totaled higher than the dealer!"

let print_newlines n =
  for _ = 1 to n do
    print_endline ""
  done

let print_card card =
  let rank, suit = O.Card.(get_rank card, get_suit card) in
  let rank_str =
    match rank with
    | 1 -> " A "
    | 11 -> " J "
    | 12 -> " Q "
    | 13 -> " K "
    | _ -> " " ^ string_of_int rank ^ " "
  in
  let suit_str, suit_color =
    match suit with
    | Hearts | Diamonds ->
        let suit_char =
          match suit with
          | Hearts -> "♥"
          | Diamonds -> "♦"
          | _ -> ""
        in
        (suit_char, "\027[31m")
    | Clubs | Spades ->
        let suit_char =
          match suit with
          | Clubs -> "♣"
          | Spades -> "♠"
          | _ -> ""
        in
        (suit_char, "\027[30m")
  in
  print_string "┌───┐";
  print_endline "";
  print_string ("│" ^ rank_str ^ "│");
  print_endline "";
  print_string ("│ " ^ suit_color ^ suit_str ^ "\027[0m │");
  print_endline "";
  print_string "└───┘";
  print_newline ()

let print_mystery_hand dealer =
  let () = print_endline (O.Player.get_name dealer) in
  let cards = O.Player.get_hand dealer in
  print_card (List.hd cards);
  print_string "┌───┐";
  print_endline "";
  print_string "│ ? │";
  print_endline "";
  print_string "└───┘";
  print_endline ("Value: " ^ string_of_int (O.Player.get_hand_value dealer))

let print_reg_hand player =
  let () = print_endline (O.Player.get_name player) in
  let cards = O.Player.get_hand player in
  List.iter print_card cards;
  print_endline ("Value: " ^ string_of_int (O.Player.get_hand_value player))

let print_round_end g =
  let open O.Game in
  let players = get_end_result g in
  print_reg_hand (get_dealer g);

  (* Print result for each players*)
  for i = Array.length players - 2 downto 0 do
    print_newlines 2;
    let player, isWin = players.(i) in
    let message =
      if not isWin then
        if O.Player.is_bust player then player_bust_message
        else player_lost_message
      else if O.Player.is_bust (get_dealer g) then dealer_bust_message
      else player_win_message
    in
    print_reg_hand player;
    print_endline message;
    print_endline ("Balance: " ^ string_of_int (O.Player.get_balance player))
  done

let print_move_result message g =
  let open O.Game in
  print_newlines 1;
  print_mystery_hand (get_dealer g);
  print_newlines 1;
  print_reg_hand (get_curr_player g);
  print_newlines 1;
  print_endline message;
  ()

let print_game_state g =
  let open O.Game in
  match get_state g with
  | NewPlayer | Continue -> print_move_result turn_message g
  | TryAgain ->
      (* TryAgain only happens when a player attemps to DoubleDown, but don't
         have enough tokens *)
      print_move_result (no_double_down_message ^ "\n" ^ turn_message) g
  | Bust ->
      print_endline player_bust_message;
      print_move_result turn_message g
  | End -> print_round_end g

let rec get_user_move g =
  let move = read_line () in
  let try_again () =
    print_move_result try_again_message g;
    get_user_move g
  in
  let open O.Game in
  match move with
  | "hit" -> Hit
  | "stand" -> Stand
  | "double down" -> DoubleDown
  | _ ->
      print_newlines 10;
      try_again ()

let is_game_over g =
  let open O.Game in
  match get_state g with
  | O.Game.End -> true
  | _ -> false

let rec turn_loop g =
  let () = print_game_state g in
  if is_game_over g then g
  else
    (* [TODO]: When computer player module is implemented, get_computer_move
       should be added *)
    let move = get_user_move g in
    let g = O.Game.update move g in
    let () = print_newlines 10 in
    turn_loop g

let global_num_players = ref 0

let rec init_players game =
  let is_player = ref false in
  let is_valid_input = ref false in

  (* Get Player Type *)
  while not !is_valid_input do
    print_endline "Is this player a human or computer?";
    print_string "Please type 'h' for human or 'c' for computer: ";
    let input = read_line () in
    match input with
    | "h" | "c" ->
        if input = "h" then is_player := true;
        is_valid_input := true
    | _ ->
        print_newlines 2;
        print_endline "**Invalid Input**"
  done;

  (* Get Player Name *)
  let updated_game =
    if !is_player then
      let _ = print_string "What name will you go by? " in
      let name = read_line () in
      O.Game.add_player name game
    else
      (* For now, a computer player is just a human with name "Computer" lol *)
      O.Game.add_player "* Computer *" game
  in
  global_num_players := !global_num_players + 1;

  (* Repeat if Adding New Player *)
  let is_init_done = ref false in
  let all_players_added_game = ref updated_game in
  while not !is_init_done do
    print_endline "Add another player? ";
    print_string "Please type 'yes' or 'no': ";
    let input = read_line () in
    match input with
    | "yes" ->
        print_newlines 2;
        all_players_added_game := init_players updated_game;
        is_init_done := true
    | "no" -> is_init_done := true
    | _ ->
        print_newlines 2;
        print_endline "**Invalid Input**"
  done;
  !all_players_added_game

let introduction () =
  print_endline title;
  print_endline "";
  print_string "Welcome to OCamlJack! Press ENTER to begin: ";
  let _ = read_line () in
  print_string
    "Please type in the difficulty of the game, which determines\n\
    \    when the dealer will stop drawing cards ('risky', 'safe', or \
     'normal'): ";
  let input = read_line () in
  let dealer_strategy =
    match input with
    | "risky" -> O.Game.HitUntil 20
    | "safe" -> O.Game.HitUntil 14
    | _ -> O.Game.HitUntil 17
  in
  let game = O.Game.(init_game dealer_strategy) in
  let players_added_game = init_players game in

  let rec init_balances () =
    let error_message = "** Please enter a valid positive integer! **" in
    try
      print_newlines 1;
      print_string "Please type in the starting balance for all players: ";
      let num = int_of_string (read_line ()) in
      if num < 1 then
        let () =
          print_newlines 1;
          print_endline error_message
        in
        init_balances ()
      else num
    with Failure _ ->
      print_newlines 1;
      print_endline error_message;
      init_balances ()
  in
  let balance = init_balances () in
  let fully_init_game = O.Game.set_balances balance players_added_game in
  fully_init_game

let place_bets game =
  (* One player places bet *)
  let place_bet game =
    let curr_player = O.Game.get_curr_player game in
    let name = O.Player.get_name curr_player in
    print_newlines 1;
    print_endline (name ^ ", it's your turn to bet!");
    print_endline
      ("Your current balance is: "
      ^ string_of_int (O.Player.get_balance curr_player));
    print_endline "Please type in the amount you would like to bet: ";
    let bet = int_of_string (read_line ()) in
    O.Game.place_bet bet game
  in

  (* Ask again for player to place bet if error occurs *)
  let rec place_bet_loop game =
    try place_bet game with
    | Failure _ ->
        (* Likely, they incorrectly bet a non-numeric value *)
        let () =
          print_newlines 1;
          print_endline "** Please enter a valid integer! **"
        in
        place_bet_loop game
    | O.Player.InsufficientBalance ->
        let () =
          print_newlines 1;
          print_endline "** You don't have enough money to make that bet! **"
        in
        place_bet_loop game
  in

  (* Get bets from all players *)
  let rec place_all_bets_helper num game =
    match num with
    | 0 -> game
    | _ ->
        let updated_game = place_bet_loop game in
        place_all_bets_helper (num - 1) updated_game
  in
  place_all_bets_helper !global_num_players game

let program () =
  let init_game = introduction () in

  let rec game_loop new_game =
    let running_game = place_bets new_game |> O.Game.start_game in
    let finished_game = turn_loop running_game in
    let _ = read_line () in
    let rec ask_repeat_game () =
      print_newlines 2;
      print_endline "Would you like to play again? ";
      print_endline "Please type 'yes' or 'no': ";
      let input = read_line () in
      match input with
      | "yes" -> game_loop finished_game
      | "no" -> exit 0
      | _ ->
          print_endline "**Invalid Input**";
          ask_repeat_game ()
    in
    ask_repeat_game ()
  in
  game_loop init_game

let () = program ()
