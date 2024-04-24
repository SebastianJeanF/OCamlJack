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

let print_mystery_hand player =
  let () = print_endline (O.Player.get_name player) in
  let () = print_string "[" in
  let cards = O.Player.get_hand player in
  let () = print_string (O.Card.to_string (List.hd cards) ^ ", ") in
  let () = print_endline "?]" in
  print_endline ("Value: " ^ string_of_int (O.Player.get_hand_value player))

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

let print_round_end message g =
  let open O.Game in
  print_newlines 1;
  print_reg_hand (get_dealer g);
  print_newlines 1;
  print_reg_hand (get_curr_player g);
  print_newlines 1;
  print_endline message;
  ()

let print_move_result message g =
  let open O.Game in
  print_newlines 1;
  print_mystery_hand (get_dealer g);
  print_newlines 1;
  print_reg_hand (get_curr_player g);
  print_newlines 1;
  print_endline message;
  ()

let iterate g =
  let open O.Game in
  match get_state g with
  | NewPlayer | Continue -> print_move_result turn_message g
  | TryAgain -> print_move_result try_again_message g
  | Bust -> print_round_end player_bust_message g
  | End ->
      let isWin = snd (get_end_result g).(0) in
      let message =
        if not isWin then
          if O.Player.is_bust (get_curr_player g) then player_bust_message
          else player_lost_message
        else if O.Player.is_bust (get_dealer g) then dealer_bust_message
        else player_win_message
      in
      print_round_end message g

let get_user_move g =
  let move = read_line () in
  O.Game.update move g

let rec loop g is_last =
  let open O.Game in
  let () = iterate g in
  let g = get_user_move g in
  let () = print_newlines 10 in
  if is_last then ()
  else
    match get_state g with
    | End | Bust -> loop g true
    | _ -> loop g false

let program () =
  let () = print_endline title in
  let () = print_endline "" in
  let () = print_string "Welcome to OCamlJack! Press ENTER to begin: " in
  let name = read_line () in
  let () =
    print_string
      "Please type in the type of AI you want to play(risky, safe, normal): "
  in
  let computer = read_line () in
  let x =
    match computer with
    | "risky" -> O.Game.HitUntil 20
    | "safe" -> O.Game.HitUntil 14
    | _ -> O.Game.HitUntil 17
  in
  let game = O.Game.(new_game x |> add_player name) in
  loop game false

let () = program ()
