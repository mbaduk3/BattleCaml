open Gameboard
open Display

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
    | Out_of_bounds -> print_string "Coordinates are out of bounds!"; Retry
*)

let rec play_game b = 
    render b;
    play_game b
  
let main () = 
  print_string "Welcome!";
  play_game demo_board

let () = main ()