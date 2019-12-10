open Gameboard
open Display
open Command

type phase = Placement | Play
let in_phase = ref Placement

(* Reference to the counter for the number of ships placed in placement phase *)
let ship_i = ref 0
let starttime = Sys.time ()

let opp_board = demo_opp_board
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

let place_ship matrix ship_i x y = 
  let ship_len = Array.length (List.nth ships ship_i) in 
  for i = 0 to (ship_len - 1) do 
    matrix.(y - 1).(x + i - 1) <- (List.nth ships ship_i).(i)
  done

let cross_mat_of_ship ship = 
  let len = Array.length ship in 
  Array.make_matrix len 1 1

let update_cur_ship () = 
  if (!ship_i = 4) then 
    crosshair_mat := (List.nth bullets 0)
  else
    crosshair_mat := cross_mat_of_ship (List.nth ships (!ship_i + 1))

let change_phase p =
  match p with 
    | Placement -> 
        crosshair_mat := cross_mat_of_ship (List.nth ships !ship_i);
        placement_init ()
    | Play -> 
        play_init ()

let rec handle_placement win b =
  try
    if (!ship_i < 5) then 
      begin
        update_cur_ship ();
        place_ship b !ship_i (!crosshair_x) !crosshair_y; 
        incr ship_i;
        if (!ship_i = 5) then change_phase Play else ()
      end
      else 
      (* TODO: include useful error message: "You have placed all the ships!" *)
      ()
  with 
    | Invalid_argument e -> 
        (*TODO: error message: "ship is out of bounds"*)
        incr ship_i;
        ()

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
               handle_placement win b; b
    | Quit -> exit_display (); b
    | _ -> b

(* Blank for now *)
let ai_fire opp_b = turn_count := !turn_count + 1; opp_b

let rec play_game b opp_b t = 
  let dt = Sys.time () -. t in
  let ntime = render b opp_b (int_of_phase !in_phase) dt in
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
  play_game demo_board opp_board dt

let () = main ()

