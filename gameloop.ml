open Gameboard
open Display
open Command
open Ai_hard
open Ai_easy
open Ai_medium

let rules = " 
  Here is how you play, Good Luck!

  This game functions on keyboard inputs and phases. There are 3 phases:
    - Welcome Phase
    - Placement Phase
    - Play Phase

  The first phase is the Welcome phase. Press 'P' to Play.

  The Second phase is the Placement phase. The highlighted cursor 
  indicates where the camls will be placed. You have 5 camls, one with
  5 humps, one with 4 humps, two with 3 humps, and one with 2 humps. 
  It is important to note that the coordinates of your caml placements 
  cannot overlap. Here are the main keys you will use for the Place 

  After placing your ships, the AI will also place its ships, and the play phase
  begins. Here are the main keys you will use for the Play phase:
    - 'W' moves your cursor up
    - 'A' moves your cursor left
    - 'S' moves your cursor down
    - 'D' moves your cursor right
    - 'F' fires where your cursor currently is
    - 
"

type phase = Placement | Play | Menu | Win | Lose
let in_phase = ref Menu

(* let modes = [Gameboard.Easy; Gameboard.Medium; Gameboard.Hard]
let mode_set = ref false

let curr_mode = ref Medium

let get_rnd_elt bound = Random.int bound

let set_mode () = 
  if !mode_set = false then
    begin
      mode_set := true;
      curr_mode := List.nth modes (get_rnd_elt 3);
      !curr_mode
    end
  else
    !curr_mode *)

(* Reference to the counter for the number of ships placed in placement phase *)
let ship_i = ref 0
let surrender = ref false

(* ship_coordinates is an array of tuples. Each tuple holds the x,y of the 
   upper-left coordinate of the ship, the length, and orientation *)
let ship_coordinates = Array.make 5 (0, 0, 0, Horizontal)
let opp_coordinates = Array.make 5 (0, 0, 0, Horizontal)

let bullets = (Array.make_matrix 1 1 1)::[]

let starttime = Sys.time ()
let turn_count = ref 0
let score = ref 0
let err_msg = ref "Welcome to BattleCamL! Place the ships:"
let debug = ref true

let incr_turn () = turn_count := !turn_count + 1
let incr_score () = score := !score + 1

let int_of_phase = function 
  | Placement -> 0
  | Play -> 1
  | Menu -> 2
  | Win -> 3
  | Lose -> 4

let update_err = function 
  | No_contact _ -> err_msg := "You missed... Try again!" 
  | Already_hit _ -> err_msg := "You already hit there!"
  | Already_miss _ -> err_msg := "Yoy already missed there!"
  | Contact _ -> err_msg := "You hit a ship, nice!"
  | _ -> err_msg := "BaTtLeCaMl"

(* Change later to display responsive results *)
let handle_fire win b = 
  if (!ship_i != 5) then b 
  else
    let (x, y) = (!crosshair_y - 1, !crosshair_x - 1) in
    let res = Gameboard.fire (x, y) b in
    update_err res;
    match res with 
    | No_contact m -> incr_turn (); m
    | Already_hit m -> m 
    | Already_miss m -> m 
    | Contact m -> incr_turn ();incr_score (); m
    | _ -> failwith "Unimplemented"

(* let place_ship matrix ship_i x y = 
   let ship_len = Array.length (List.nth ships ship_i) in 
   for i = 0 to (ship_len - 1) do 
    matrix.(y - 1).(x + i - 1) <- (List.nth ships ship_i).(i)
   done *)

(* Invert the orientation of the ship at index [ship] in the ships matrix *)
let handle_rotate ships ship =
  if snd ships.(ship) = Horizontal
  then ships.(ship) <- (fst (ships.(ship)), Vertical)
  else ships.(ship) <- (fst (ships.(ship)), Horizontal)

(*[check_win b] returns whether there are no remaining [Unhit] elements in [b]*)
let check_win b = 
  let win = ref true in 
  for i = 0 to (Array.length b - 1) do 
    for j = 0 to (Array.length b.(0) - 1) do 
      if (b.(i).(j) = Unhit) then 
        win := false 
      else ()
    done 
  done;
  !win 
      
let check_placement coords ships ship x y orientation =
  let ship_len = Array.length (fst (ships.(ship))) in
  for i = 0 to ship - 1 do
    match coords.(i) with
    | (x', y', len', ori') -> 
      if orientation = Horizontal && ori' = Horizontal
      then
        begin
          if y = y' && 
            (x + ship_len > x' && x + ship_len < x' + len'
            || x' + len' > x && x' < x)
          then raise (Invalid_argument "ship cannot be placed here")
        end
      else if orientation = Horizontal && ori' = Vertical then
        begin
          if (y >= y' && y <= y' + len')
          && (x < x' && x + ship_len > x' || x = x')
          then raise (Invalid_argument "ship cannot be placed here")
        end
      else if orientation = Vertical && ori' = Horizontal then
        begin
          if (x >= x' && x <= x' + len')
          && (y < y' && y + ship_len > y' || y = y')
          then raise (Invalid_argument "ship cannot be placed here")
        end
      else if orientation = Vertical && ori' = Vertical then
        begin
          if x = x' && (y + ship_len > y' && y + ship_len < y' + len'
                        || y' + len' > y && y' < y)
          then raise (Invalid_argument "ship cannot be placed here")
        end
  done

let place_ship matrix ships ship x y rot = 
  if rot then handle_rotate ships ship
  else
    let ship_len = Array.length (fst (ships.(ship))) in 
    let orientation = snd (ships.(ship)) in
    if (orientation = Horizontal && x + ship_len > 11)
    || (orientation = Vertical && y + ship_len > 11)
    then raise (Invalid_argument "ship is out of bounds")
    else begin
      for i = 0 to (ship_len - 1) do
        if orientation = Horizontal then
          begin
            if ships = opp_ships
            then
              begin 
                check_placement opp_coordinates opp_ships ship x y orientation;
                matrix.(y - 1).(x + i - 1) <- (fst (opp_ships.(ship))).(i);
                opp_coordinates.(ship) <- (x, y, ship_len, orientation);
              end
            else
              begin
                check_placement ship_coordinates ships ship x y orientation;
                matrix.(y - 1).(x + i - 1) <- (fst (ships.(ship))).(i);
                ship_coordinates.(ship) <- (x, y, ship_len, orientation);
              end
          end
        else
          begin
            if ships = opp_ships
            then
              begin
                check_placement opp_coordinates opp_ships ship x y orientation;
                matrix.(y + i - 1).(x - 1) <- (fst (opp_ships.(ship))).(i);
                opp_coordinates.(ship) <- (x, y, ship_len, orientation)
              end
            else
              begin
                check_placement ship_coordinates ships ship x y orientation;
                matrix.(y + i - 1).(x - 1) <- (fst (ships.(ship))).(i);
                ship_coordinates.(ship) <- (x, y, ship_len, orientation)
              end
          end
      done
    end

(* Returns a crosshair matrix from a given ship matrix. 
   This is used for placement-phase highlighting *)
let cross_mat_of_ship ship orient = 
  let len = Array.length ship in 
  if (orient = Horizontal) then 
    Array.make_matrix len 1 1
  else 
    Array.make_matrix 1 len 1

(* Returns the orientation equivalent of a boolean *)
let orient_of_rot = function
  | false -> Horizontal 
  | true -> Vertical

(* Updates the cursor matrix to reflect the next ship that is to be placed *)
let update_cur_ship () = 
  if (!ship_i = 5) then 
    crosshair_mat := (List.nth bullets 0)
  else
    let s,orient = Array.get ships (!ship_i) in
    crosshair_mat := cross_mat_of_ship s (orient)

(* Resets the internal variables of the game. *)
let reset_game () = 
  ship_i := 0;
  turn_count := 0;
  score := 0;
  cur_x := 1;
  cur_y := 1;
  err_msg := "Welcome to BattleCamL! Place the ships:";
  surrender := false


(* Changes the internal phase of the game *)
let change_phase p =
  match p with 
    | Placement -> 
        update_cur_ship ();
        reset_game ();
        menu_end ();
        win_end ();
        lose_end ();
        let s,orient = Array.get ships (!ship_i) in 
        crosshair_mat := cross_mat_of_ship s orient;
        placement_init ();
        in_phase := Placement
    | Play -> 
        update_cur_ship ();
        play_init ();
        in_phase := Play
    | Menu -> 
        menu_init ();
        in_phase := Menu
    | Win -> 
        play_end ();
        win_init ();
        in_phase := Win
    | Lose -> 
        play_end ();
        lose_init ();
        in_phase := Lose
        
let handle_placement win b rot =
  try
    if (!ship_i < 5) then 
      begin
        update_cur_ship ();
        place_ship b ships !ship_i (!crosshair_x) !crosshair_y rot;
        if rot then ()
        else 
          (* update_cur_ship rot; *)
        if not rot then incr ship_i;
        if (!ship_i = 5) then change_phase Play else ()
      end
    else update_cur_ship ()
  (* TODO: include useful error message: "You have placed all the ships!" *)
  with 
  | Invalid_argument e -> ()
(*TODO: print error message [e]*)

let handle_input_menu win b = 
  match get_key win with 
  | Quit -> ignore(exit_display ()); b 
  | Place -> change_phase Placement; b
  | _ -> b

let handle_input win b = 
  match get_key win with
  | Down -> if !crosshair_y < 10 then incr crosshair_y 
    else crosshair_y := 1;
    cur_timer := 0.; b
  | Up -> if !crosshair_y > 1 then decr crosshair_y
    else crosshair_y := 10;
    cur_timer := 0.; b
  | Left -> if !crosshair_x > 1 then decr crosshair_x
    else crosshair_x := 10;
    cur_timer := 0.; b
  | Right -> if !crosshair_x < 10 then incr crosshair_x
    else crosshair_x := 1;
    cur_timer := 0.; b
  | Fire -> cur_timer := 0.;
    handle_fire win b
  | Place -> cur_timer := 0.;
    handle_placement win b false; b
  | Rotate -> cur_timer := 0.;
    handle_placement win b true; b
  | Nuke -> cur_timer := 0.;
    nuke_board
  | Surrender -> cur_timer := 0.;
    surrender := true; b
  | Quit -> ignore(exit_display ()); b
  | _ -> b

let ship_lst = ref []
let empty_lst = ref []
let empty_lst_made = ref false
let ship_lst_made = ref false

(* Blank for now *)
let ai_fire opp_b = 
  turn_count := !turn_count + 1; 
  Ai_medium.ai_fire opp_b

(* Generates a random board with placed ships for the ai *)
let ai_placement () =
  let count = ref 0 in
  let m = Array.make_matrix 10 10 Empty in
  while !count < 5 do
    try
      Random.self_init ();
      if Random.bool ()
      then place_ship m opp_ships !count (Random.int 11) (Random.int 11) true
      else
        begin
          place_ship m opp_ships !count (Random.int 11) (Random.int 11) false;
          incr count
        end
    with
    | Invalid_argument e -> ();
  done;
  m

let check_overlap matrix x y p =
  if matrix.(y - 1).(x - 1) = Empty
  then matrix.(y - 1).(x - 1) <- Uncollected p
  else raise (Invalid_argument "powerup cannot be placed here")

let powerup_placement powerups = 
  let m = ai_placement () in
  let rec place powerups = 
    begin
      match powerups with
      | [] -> ()
      | h :: t -> try
          Random.self_init ();
          check_overlap m (Random.int 11) (Random.int 11) h;
          place t
        with
        | Invalid_argument e -> place (h :: t)
    end
  in place powerups; m

(* The main recursive game loop *)
let rec play_game b opp_b t = 
  let dt = Sys.time () -. t in
  let ntime = render b opp_b (int_of_phase !in_phase) !turn_count !score  !err_msg dt in
  match !in_phase with 
  | Placement -> 
    begin
      update_cur_ship ();
      if (!turn_count mod 2 = 0) then
        let b' = handle_input !Display.b_win b in
        if (!surrender = true) then 
          change_phase Lose;
        play_game b' opp_b ntime
      else 
        let opp_b' = ai_fire opp_b in 
        play_game b opp_b' t
    end
  | Play ->
    begin
      if (!turn_count mod 2 = 0) then
        let opp_b' = handle_input !Display.b_win opp_b in
        if (check_win opp_b') then 
          change_phase Win;
        if (!surrender = true) then 
          change_phase Lose;
        play_game b opp_b' ntime
      else 
        let b' = ai_fire b in 
        if (check_win b') then 
          change_phase Lose;
        play_game b' opp_b t
    end
  | Menu | Win | Lose -> 
    begin 
      ignore(handle_input_menu !scr b);
      play_game (init_matrix ()) (powerup_placement easy_mode_powerups) t
    end


let main () = 
  let dt = Sys.time () -. starttime in
  print_string "Welcome!";
  change_phase Menu;
  play_game demo_board demo_board dt

let () = main ()


