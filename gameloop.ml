open Gameboard
open Display
open Command

type phase = Placement | Play
let in_phase = ref Placement

(* Reference to the counter for the number of ships placed in placement phase *)
let ship_i = ref 0
let starttime = Sys.time ()

(* Change later to display responsive results *)
let handle_fire win b = 
  if (!ship_i != 5) then b 
  else
  let (x, y) = (!crosshair_y - 1, !crosshair_x - 1) in
  let res = Gameboard.fire (x, y) b in
  match res with 
    | No_contact m -> m
    | Already_hit m -> m 
    | Already_miss m -> m 
    | Contact m -> m
    | _ -> failwith "Unimplemented"

let place_ship matrix ship x y = 
  let ship_len = Array.length (List.nth ships ship) in 
  for i = 0 to (ship_len - 1) do 
    matrix.(y - 1).(x + i - 1) <- (List.nth ships ship).(i)
  done

let change_phase p =
  match p with 
    | Placement -> 
        placement_init ()
    | Play -> 
        play_init ()

let rec handle_placement win b =
  try
    if (!ship_i < 5) then 
      begin
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


let rec play_game b t = 
  let dt = Sys.time () -. t in
  let ntime = render b dt in
  let b' = handle_input !Display.b_win b in
  play_game b' ntime


let main () = 
  let dt = Sys.time () -. starttime in
  print_string "Welcome!";
  change_phase Placement;
  play_game demo_board dt

let () = main ()

