open Gameboard
open Display
open Command

exception All_placed

(* Reference to the counter for the number of ships placed in placement phase *)
let ship_i = ref 0

(* Change later to display responsive results *)
let handle_fire win b = 
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

let rec handle_placement win b =
  if (!ship_i < 5) then 
    begin
      place_ship b !ship_i !crosshair_x !crosshair_y; 
      incr ship_i
    end
  else 
    (* TODO: include useful error message: "You have placed all the ships!" *)
    ()

let handle_input win b = 
  match get_key win with
    | Down -> if !crosshair_y < 10 then incr crosshair_y 
              else crosshair_y := 1; b
    | Up -> if !crosshair_y > 0 then decr crosshair_y
            else crosshair_y := 9; b
    | Left -> if !crosshair_x > 0 then decr crosshair_x
              else crosshair_x := 9; b
    | Right -> if !crosshair_x < 10 then incr crosshair_x
                else crosshair_x := 1; b
    | Fire -> handle_fire win b
    | Place -> handle_placement win b; b
    | Quit -> exit_display (); b
    | _ -> b

let rec play_game b = 
  render b;
  let b' = handle_input !Display.b_win b in
  play_game b'

let main () = 
  print_string "Welcome!";
  play_game demo_board

let () = main ()

