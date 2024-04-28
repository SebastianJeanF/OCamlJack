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
  "What move do you want to make next?\n (Type 'stand' or 'hit'): "

let try_again_message = "Invalid move. Valid moves are 'stand' or 'hit'"

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

let print_mystery_hand dealer =
  let () = print_endline (O.Player.get_name dealer) in
  let () = print_string "[" in
  let cards = O.Player.get_hand dealer in
  let () = print_string (O.Card.to_string (List.hd cards) ^ ", ") in
  let () = print_endline "?]" in
  print_endline ("Value: " ^ string_of_int (O.Player.get_hand_value dealer))

let print_reg_hand player =
  let () = print_endline (O.Player.get_name player) in
  let () = print_string "[" in
  let cards = O.Player.get_hand player in
  let () =
    for x = List.length cards - 1 downto 1 do
      print_string (O.Card.to_string (List.nth cards x) ^ ", ")
    done
  in
  let () = print_endline (O.Card.to_string (List.hd cards) ^ "]") in
  print_endline ("Value: " ^ string_of_int (O.Player.get_hand_value player))

let print_round_end g =
  let open O.Game in
  print_reg_hand (get_dealer g);
  let players = get_end_result g in
  for i = Array.length players - 2 downto 0 do
    print_newlines 2;
    let player, isWin = (get_end_result g).(i) in
    let message =
      if not isWin then
        if O.Player.is_bust player then player_bust_message
        else player_lost_message
      else if O.Player.is_bust (get_dealer g) then dealer_bust_message
      else player_win_message
    in
    print_reg_hand player;
    print_endline message
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
  | TryAgain -> print_move_result try_again_message g
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

(** TODO: turn_loop returns the game, ask user if they want to play again *)
let rec turn_loop g =
  let () = print_game_state g in
  if is_game_over g then ()
  else
    (* [TODO]: When computer player module is implemented, get_computer_move
       should be added *)
    let move = get_user_move g in
    let g = O.Game.update move g in
    let () = print_newlines 10 in
    turn_loop g

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
    "Please type in the type of dealer AI you want to play(risky, safe, \
     normal): ";
  let input = read_line () in
  let dealer_strategy =
    match input with
    | "risky" -> O.Game.HitUntil 20
    | "safe" -> O.Game.HitUntil 14
    | _ -> O.Game.HitUntil 17
  in
  let game = O.Game.(new_game dealer_strategy) in
  let players_added_game = init_players game in
  let () =
    (* [TODO]: make while loop to check that person enters a valid
       integer/natural number *)
    print_string "Please type in the starting balance for all players: "
  in
  let balance = int_of_string (read_line ()) in
  let fully_init_game = O.Game.set_balances balance players_added_game in
  fully_init_game

(* [TODO] Get bets from each player *)
let program () =
  let new_game = introduction () in
  turn_loop new_game

let () = program ()
