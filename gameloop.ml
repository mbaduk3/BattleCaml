open Gameboard
open Display
open Command

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
    | Quit -> exit_display (); b
    | _ -> b

let place_ship matrix ship x y = 
  for i = 0 to ((Array.length (List.nth ships ship)) - 1) do 
    matrix.(y - 1).(x + i - 1) <- (List.nth ships ship).(i)
  done

let rec play_game b = 
  render b;
  let b' = handle_input !Display.b_win b in
  play_game b'

let rec handle_placement win b ship =
  match get_key win with
    | Down -> if !crosshair_y < 10 then incr crosshair_y 
              else crosshair_y := 1; (b, ship)
    | Up -> if !crosshair_y > 0 then decr crosshair_y
            else crosshair_y := 9; (b, ship)
    | Left -> if !crosshair_x > 0 then decr crosshair_x
              else crosshair_x := 9; (b, ship)
    | Right -> if !crosshair_x < 10 then incr crosshair_x
                else crosshair_x := 1; (b, ship)
    | Quit -> exit_display (); (b, ship)
    | Place -> 
          begin
          try 
            place_ship b ship !crosshair_x !crosshair_y
          with _ -> 
            play_game demo_board 
          end; (b, ship + 1)
    | _ -> (b, ship)

let rec place_ships (b, i) =
  render b;
  let (b', i') = handle_placement !Display.b_win b i in
  place_ships (b', i')

let main () = 
  print_string "Welcome!";
  place_ships (demo_board, 0)

let () = main ()

