open Gameboard
open Display
open Command

type phase = Placement | Play
let in_phase = ref Placement

(* Reference to the counter for the number of ships placed in placement phase *)
let ship_i = ref 0

let ship_coordinates = Array.make 5 (0, 0, 0, Horizontal)
let starttime = Sys.time ()

let opp_board1 = demo_opp_board
let bullets = (Array.make_matrix 1 1 1)::[]


let turn_count = ref 0

let incr_turn () = turn_count := !turn_count + 1

let int_of_phase = function 
  | Placement -> 0
  | Play -> 1

(* Change later to display responsive results *)
let handle_fire win b = 
  if (!ship_i != 5) then b 
  else
    let (x, y) = (!crosshair_y - 1, !crosshair_x - 1) in
    turn_count := !turn_count + 1;
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
let handle_rotate ship =
  if snd ships.(ship) = Horizontal
  then ships.(ship) <- (fst (ships.(ship)), Vertical)
  else ships.(ship) <- (fst (ships.(ship)), Horizontal)

let check_placement ship x y orientation =
  let ship_len = Array.length (fst (ships.(ship))) in
  for i = 0 to ship - 1 do
    match ship_coordinates.(i) with
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

let place_ship matrix ship x y rot = 
  if rot then handle_rotate ship
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
            check_placement ship x y orientation;
            matrix.(y - 1).(x + i - 1) <- (fst (ships.(ship))).(i);
            ship_coordinates.(ship) <- (x, y, ship_len, orientation)
          end
        else
          begin
            check_placement ship x y orientation;
            matrix.(y + i - 1).(x - 1) <- (fst (ships.(ship))).(i);
            ship_coordinates.(ship) <- (x, y, ship_len, orientation)
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

let change_phase p =
  match p with 
    | Placement -> 
        let s,orient = Array.get ships (!ship_i) in 
        crosshair_mat := cross_mat_of_ship s orient;
        placement_init ()
    | Play -> 
        play_init ()
        
let handle_placement win b rot =
  try
    if (!ship_i < 5) then 
      begin
        place_ship b !ship_i (!crosshair_x) !crosshair_y rot;
        if rot then ()
        else 
          (* update_cur_ship rot; *)
          if not rot then incr ship_i;
          if (!ship_i = 5) then change_phase Play else ()
      end
    else ()
  (* TODO: include useful error message: "You have placed all the ships!" *)
  with 
  | Invalid_argument e -> ()
(*TODO: print error message [e]*)

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
  | Quit -> exit_display (); b
  | _ -> b

(* Blank for now *)
let ai_fire opp_b = turn_count := !turn_count + 1; opp_b

let ai_placement =
  let count = ref 0 in
  let m = init_matrix () in
  while !count < 5 do
    try
      let x = Random.int 10 in
      let y = Random.int 10 in
      let rot = Random.bool () in
      place_ship m !count x y rot;
      if rot then ()
      else incr count;
    with
    | Invalid_argument e -> ()
  done;
  m

let rec play_game b opp_b t = 
  let dt = Sys.time () -. t in
  let ntime = render b opp_b (int_of_phase !in_phase) dt in
  if (!in_phase = Placement) then update_cur_ship ();
  if (!turn_count mod 2 = 0) then
    let b' = handle_input !Display.b_win b in
    play_game b' opp_b ntime
  else 
    let opp_b' = ai_fire opp_b in 
    play_game b opp_b' t

let main () = 
  let dt = Sys.time () -. starttime in
  print_string "Welcome!";
  change_phase Placement;
  play_game demo_board ai_placement dt

let () = main ()


