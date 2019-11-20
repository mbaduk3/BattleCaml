open Gameboard
open Display
open Command

(* type command = Fire of coord | Retry | Quit (*| Rotate | Hint |*)

let execute_command cmd b = 
  let exec c b = 
    match c with 
    | Fire c -> fire c b
    | Retry -> Misc 
    | Quit -> exit 0; 
  in 
  match exec cmd b with
    | Contact b' -> print_string "You hit! Shoot again"; b' 
    | No_contact b' -> print_string "You missed...Try again"; b' 
    | Already_hit b' -> print_string "You already hit there!"; b' 
    | Already_miss b' -> print_string "You already missed there!"; b' 
    | Misc -> b


let head = function
  | [] -> ""
  | h::t -> h

(* Returns a command based on the contents of [str] *)
let parse str board = 
  let str' = String.split_on_char ' ' str in 
  match head str' with 
  | "fire" -> 
    if List.length str' < 3 then (raise Malformed)
    else 
      let xcoord = int_of_string (second_elt str') in 
      let ycoord = int_of_string (third_elt str') in
      let dim = Array.length board in 
      if xcoord > dim || ycoord > dim || 
         xcoord < 1 || ycoord < 1 then raise Out_of_bounds 
      else
        Fire (xcoord - 1, ycoord - 1)
  | "quit" -> Quit
  | _ -> raise Malformed

let clean_input str board = 
  try 
    parse str board 
  with 
    | Malformed -> print_string "Malformed command"; Retry 
    | Out_of_bounds -> print_string "Coor
    
    dinates are out of bounds!"; Retry
*)

(* Change later to display responsive results *)
let handle_fire win b = 
  let (x, y) = (!crosshair_y - 1, !crosshair_x - 1) in
  let res = Gameboard.fire (x, y) b in
  match res with 
    | No_contact m -> m
    | Already_hit m -> m 
    | Already_miss m -> m 
    | Contact m -> m

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
    (* | Rotate -> true
    | Save -> false
    | Other -> false *)


let rec play_game b = 
  try
    render b;
    let b' = handle_input !Display.b_win b in
    play_game b'
  with e -> exit 0
  
let main () = 
  print_string "Welcome!";
  play_game demo_board

let () = main ()

(*
open Curses

exception Invalid_input

type command = Fire | Rotate | Quit | Save | Up | Down | Left | Right | Other

let width = 10
let height = 10
let start_x win = ref (snd (getyx win))
let start_y win = ref (fst (getyx win))

(* let key_pad = keypad scr true

let no_delay = nodelay scr true *)

let key_inputs win = 
  match char_of_int (wgetch win) with
  | 'S' | 's' -> Down 
  | 'W' | 'w' -> Up
  | 'A' | 'a' -> Left
  | 'D' | 'd' -> Right
  | 'F' | 'f' -> Fire
  | 'R' | 'r' -> Rotate
  | 'Q' | 'q' -> Quit
  | _ -> Save

let moveto win x y = Curses.wmove win y x

let move key_input win = 
  let start_x = start_x win in
  let start_y = start_y win in
  match key_input with
  | Down -> 
            if !start_y < 9 then incr start_y else start_y := 0;
            let x = !start_x
            in let y = !start_y in 
            moveto win x y
  | Up -> 
           if !start_y > 0 then decr start_y else start_y := 9; 
           let x = !start_x
           in let y = !start_y in
           moveto win x y
  | Left -> 
            if !start_x > 0 then decr start_x else start_x := 9; 
            let x = !start_x
            in let y = !start_y in
            moveto win x y
  | Right -> 
             if !start_x < 9 then incr start_x else start_x := 0; 
             let x = !start_x
             in let y = !start_y in
             moveto win x y
  | Fire -> let (y1, x1) = getyx win in moveto win x1 y1
  | Rotate -> true
  | Quit -> endwin (); exit 0
  | Save -> false
  | Other -> false

let init () = initscr ()

let get_point win x y = ignore(moveto win x y); (x, y)

let rec run win =
	Curses.clear();
	ignore (Curses.noecho());
  ignore(Curses.box win 0 0);
	ignore(cbreak());
  ignore (refresh ());
	ignore (keypad win true);
  (* ignore (nodelay win true); *)
  ignore (wrefresh win);
  while move (key_inputs win) win <> false do
    ignore (move (key_inputs win) win);
    ignore (wrefresh win);
  done;
  endwin ();
  exit 0
  *)
