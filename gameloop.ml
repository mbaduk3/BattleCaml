open Gameboard
open Display
open Command
open Ai_hard
open Ai_easy
open Ai_medium

type phase = Placement | Play | Menu
let in_phase = ref Menu

(* Reference to the counter for the number of ships placed in placement phase *)
let ship_i = ref 0

let ship_coordinates = Array.make 5 (0, 0, 0, Horizontal)
let opp_coordinates = Array.make 5 (0, 0, 0, Horizontal)
let starttime = Sys.time ()

let bullets = (Array.make_matrix 1 1 1)::[]


let turn_count = ref 0

let incr_turn () = turn_count := !turn_count + 1

let int_of_phase = function 
  | Placement -> 0
  | Play -> 1
  | Menu -> 2

(* Change later to display responsive results *)
let handle_fire win b = 
  if (!ship_i != 5) then b 
  else
  let (x, y) = (!crosshair_y - 1, !crosshair_x - 1) in
  let res = Gameboard.fire (x, y) b in
  match res with 
    | No_contact m -> incr_turn (); m
    | Already_hit m -> m 
    | Already_miss m -> m 
    | Contact m -> incr_turn (); m
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

let check_placement coords ships ship x y orientation =
  let ship_len = Array.length (fst (ships.(ship))) in
  for i = 0 to ship - 1 do
    match coords.(i) with
    | (x', y', len', ori') -> if orientation = Horizontal && ori' = Horizontal
      then
        begin
          if y = y' && (x + ship_len > x' && x + ship_len < x' + len'
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
    
(* Changes the internal phase of the game *)
let change_phase p =
  update_cur_ship ();
  match p with 
    | Placement -> 
        let s,orient = Array.get ships (!ship_i) in 
        crosshair_mat := cross_mat_of_ship s orient;
        placement_init ();
        in_phase := Placement
    | Play -> 
        play_init ();
        in_phase := Play
    | Menu -> 
        in_phase := Menu
        
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
  | Quit -> ignore(exit_display ()); b
  | _ -> b

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
      then place_ship m opp_ships !count (Random.int 10) (Random.int 10) true
      else
        begin
          place_ship m opp_ships !count (Random.int 10) (Random.int 10) false;
          incr count
        end
    with
    | Invalid_argument e -> ();
  done;
  m

let check_overlap matrix x y =
  if matrix.(y - 1).(x - 1) = Empty
  then matrix.(y - 1).(x - 1) <- Uncollected
  else raise (Invalid_argument "powerup cannot be placed here")

let powerup_placement powerups = 
  let m = ai_placement () in
  let rec place powerups = 
    begin
      match powerups with
      | [] -> ()
      | h :: t -> try
          Random.self_init ();
          check_overlap m (Random.int 10) (Random.int 10);
          place t
        with
        | Invalid_argument e -> place (h :: t)
    end
  in place powerups; m

(* The main recursive game loop *)
let rec play_game b opp_b t = 
  let dt = Sys.time () -. t in
  let ntime = render b opp_b (int_of_phase !in_phase) !turn_count dt in
  match !in_phase with 
  | Placement -> 
    begin
    update_cur_ship ();
    if (!turn_count mod 2 = 0) then
      let b' = handle_input !Display.b_win b in
      play_game b' opp_b ntime
    else 
      let opp_b' = ai_fire opp_b in 
      play_game b opp_b' t
      end
  | Play ->
    begin
      if (!turn_count mod 2 = 0) then
        let opp_b' = handle_input !Display.b_win opp_b in
        play_game b opp_b' ntime
      else 
        let b' = ai_fire b in 
        play_game b' opp_b t
    end
  | Menu -> 
    begin 
      ignore(handle_input_menu !scr b);
      play_game b opp_b t
    end

let main () = 
  let dt = Sys.time () -. starttime in
  print_string "Welcome!";
  change_phase Menu;
  play_game demo_board (powerup_placement easy_mode_powerups) dt

let () = main ()


