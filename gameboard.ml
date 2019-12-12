type coord = (int * int)
type powerup = Sea_mine | Bomb | Double_bullet | Points | Repair_kit
type entry = Hit | Miss | Unhit | Empty | Collected | Uncollected of powerup
type t = entry array array
type ship = t
type response = Contact of t | No_contact of t | 
                Already_hit of t | Already_miss of t | Misc
type orientation = Vertical | Horizontal

exception Malformed 
exception Out_of_bounds

let init_matrix () = Array.make_matrix 10 10 Empty

let rec index lst elem acc = 
  match lst with
  | [] -> acc
  | h::t -> if h = elem then acc else index t elem (succ acc)

let get_array_from i j arr = 
  let lst = Array.to_list arr in 
  let rec array_match i j lst acc num = 
    (match lst with
     | [] -> acc
     | h::t -> if num >= i && num < j 
       then array_match i j t (acc@[h]) (succ num) 
       else array_match i j t acc (succ num)) in
  Array.of_list (array_match i j lst [] 0)

let create_ship len = Array.make len Unhit

let ships = Array.make 5 (Array.make 0 Unhit, Horizontal)
let caml_5 = ships.(0) <- (create_ship 5, Horizontal)
let caml_4 = ships.(1) <- (create_ship 4, Horizontal)
let caml_3 = ships.(2) <- (create_ship 3, Horizontal)
let caml_3' = ships.(3) <- (create_ship 3, Horizontal)
let caml_2 = ships.(4) <- (create_ship 2, Horizontal)

let opp_ships = Array.make 5 (Array.make 0 Unhit, Horizontal)
let opp_5 = opp_ships.(0) <- (create_ship 5, Horizontal)
let opp_4 = opp_ships.(1) <- (create_ship 4, Horizontal)
let opp_3 = opp_ships.(2) <- (create_ship 3, Horizontal)
let opp_3' = opp_ships.(3) <- (create_ship 3, Horizontal)
let opp_2 = opp_ships.(4) <- (create_ship 2, Horizontal)

let hard_mode_powerups = Sea_mine :: Bomb :: Double_bullet
                         :: Points :: Repair_kit :: []
let easy_mode_powerups = hard_mode_powerups @ hard_mode_powerups

let string_of_entry e = 
  match e with 
  | Hit -> "H"
  | Collected -> "C"
  | Miss -> "M" 
  | Unhit -> "." 
  | Empty -> "." 
  | Uncollected p -> "."

let new_mod n m = (n + m) mod m

let get_row m num = m.(num)

let demo_board = 
  init_matrix ()

let nuke_board = 
  Array.make_matrix 10 10 Hit

let transpose m = 
  Array.init (Array.length m.(0)) (fun i -> 
      Array.init (Array.length m) (fun j -> m.(j).(i)))

(** [get_val_of_coord m c] is the value at coordinates x, y contained in [c], 
    of matrix [m] *)
let get_val_of_coord (m:t) (c:coord) = m.(fst c).(snd c)

let check_explosion x y m =
  if m.(x).(y) = Unhit then m.(x).(y) <- Hit
  else m.(x).(y) <- Miss

let handle_powerup c m p = 
  if p = Sea_mine 
  then if fst c = 0 && snd c = 0 then begin
      check_explosion (fst c) (snd c + 1) m;
      check_explosion (fst c + 1) (snd c) m;
      check_explosion (fst c + 1) (snd c + 1) m;
    end
    else if fst c = 0 && snd c = 9 then begin
      check_explosion (fst c) (snd c - 1) m;
      check_explosion (fst c + 1) (snd c - 1) m;
      check_explosion (fst c + 1) (snd c) m;
    end
    else if fst c = 9 && snd c = 0 then begin
      check_explosion (fst c - 1) (snd c) m;
      check_explosion (fst c - 1) (snd c + 1) m;
      check_explosion (fst c) (snd c + 1) m;
    end
    else if fst c = 9 && snd c = 9 then begin
      check_explosion (fst c - 1) (snd c - 1) m;
      check_explosion (fst c - 1) (snd c) m;
      check_explosion (fst c) (snd c + 1) m;
    end
    else if fst c = 0 then begin
      check_explosion (fst c) (snd c - 1) m;
      check_explosion (fst c) (snd c + 1) m;
      check_explosion (fst c + 1) (snd c - 1) m;
      check_explosion (fst c + 1) (snd c) m;
      check_explosion (fst c + 1) (snd c + 1) m;
    end
    else if snd c = 0 then begin
      check_explosion (fst c - 1) (snd c) m;
      check_explosion (fst c + 1) (snd c) m;
      check_explosion (fst c - 1) (snd c + 1) m;
      check_explosion (fst c) (snd c + 1) m;
      check_explosion (fst c + 1) (snd c + 1) m;
    end
    else if fst c = 9 then begin
      check_explosion (fst c - 1) (snd c - 1) m;
      check_explosion (fst c - 1) (snd c) m;
      check_explosion (fst c - 1) (snd c + 1) m;
      check_explosion (fst c) (snd c - 1) m;
      check_explosion (fst c) (snd c + 1) m;
    end
    else if snd c = 9 then begin
      check_explosion (fst c - 1) (snd c - 1) m;
      check_explosion (fst c - 1) (snd c) m;
      check_explosion (fst c) (snd c - 1) m;
      check_explosion (fst c + 1) (snd c - 1) m;
      check_explosion (fst c + 1) (snd c) m;
    end
    else begin
      check_explosion (fst c - 1) (snd c - 1) m;
      check_explosion (fst c - 1) (snd c) m;
      check_explosion (fst c - 1) (snd c + 1) m;
      check_explosion (fst c) (snd c - 1) m;
      check_explosion (fst c) (snd c + 1) m;
      check_explosion (fst c + 1) (snd c - 1) m;
      check_explosion (fst c + 1) (snd c) m;
      check_explosion (fst c + 1) (snd c + 1) m;
    end

let fire (c:coord) m = 
  match get_val_of_coord m c with
  | Empty -> m.(fst c).(snd c) <- Miss; No_contact m
  | Hit ->  Already_hit m
  | Collected -> Already_hit m
  | Miss -> Already_miss m
  | Unhit -> m.(fst c).(snd c) <- Hit; Contact m
  | Uncollected p -> m.(fst c).(snd c) <- Collected;
    handle_powerup c m p; Contact m

let string_of_response r = 
  match r with 
    | Contact m -> "contact"
    | No_contact m -> "no contact"
    | Already_hit m -> "already hit"
    | Already_miss m -> "already miss"
    | Misc -> "misc"

let second_elt lst = List.nth lst 1
let third_elt lst = List.nth lst 2

let string_of_tuple tup = 
  let x = fst tup in 
  let y = snd tup in
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

(* let give_hint matrix = 
  for i = 0 to Array.length matrix do
    if matrix.(i).(4) = Unhit then 
      print_string ("\nThere is an unhit caml at " ^ string_of_tuple (i, 4) ^ "\n")
    else
      print_string "\nCouldn't Find Anything For You. Just Keep Firing!\n"
  done *)

(** [format_row row] prints the elements of array [row] to the console *)
let format_row (row: entry array) = 
  Array.iter (fun elem -> print_string (string_of_entry elem)) row;
  print_string "\n"

(** [format board] prints the elements of Board [board] to the console *)
let format (board:t) = 
  print_string "\n";
  Array.iter format_row board