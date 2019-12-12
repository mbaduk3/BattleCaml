open Gameboard
open Random

let fire_lst = [true; false]

(* [determine_hard_fire ()] has a 50% chance or returning true and a 50% chance
of returning false. *)
let determine_hard_fire () = 
    let elt = Random.int 2 in
    List.nth fire_lst elt

(* [get_all_empty_coords miss_coords] returns a list of coordinate pairs representing
the coordinates of type Empty. *)
let empty_coords_calculated = ref false

let get_all_empty_coords m reflst = 
  if !empty_coords_calculated = false then
  begin
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length m.(i) - 1 do
      match m.(i).(j) with
        | Empty -> reflst := (i, j):: !reflst
        | _ -> ()
    done
  done;
  empty_coords_calculated := true;
  !reflst
  end
  else
    !reflst

(* [remove_empty coord reflst] *)
let remove_empty coord reflst = 
  reflst := List.filter (fun c -> if c = coord then false else true) !reflst

let print_tuple tup = 
  match tup with
    | (a, b) -> let aval = string_of_int a 
                      in let bval = string_of_int b in
      (print_string ("(" ^ aval ^ ", " ^ bval ^ ") "))

let rec index lst elt num = 
  match lst with
    | [] -> num
    | h::t -> if h = elt then num else index t elt (succ num)

(* [get_empty_coord miss_coords] returns a list of coordinate pairs representing
the coordinates of type Empty. *)
let get_empty_coord miss_coords = 
  let rnd_ind = Random.int (List.length !miss_coords) in
  print_string "\n";
  print_string "ERROR IS EMPTY COORD: ";
  print_string "LENGTH: ";
  print_int (List.length !miss_coords);
  print_string " INDEX: ";
  print_int (rnd_ind);
  print_string "\n";
  let (x, y) = List.nth !miss_coords rnd_ind in
  (* print_tuple (x, y); *)
  remove_empty (x, y) miss_coords;
  (x, y)

(* [current_ship_index] is a reference to a value between 1 and 4 inclusive*)
let current_ship_index = ref 0;;

let curr_elt = 0;;

(* [ships_at_index ind] returns the ship at  *)
let ship_at_index cv ind = 
  List.nth !cv ind
;;

let rec update_ship_index lst_of_lst num = 
  let first_non_empty_found = ref false in
  match lst_of_lst with
    | [] -> current_ship_index := num
    | h::t -> if h = [] && !first_non_empty_found = false then
               update_ship_index t (succ num)
               else
               begin
               first_non_empty_found := true;
               update_ship_index t num
               end

let get_coord_of_hit ship = 
  List.nth ship curr_elt

let len_of_curr_ship ship = 
  List.length (ship);;

let filter_coord (lst : (int * int) list) (coord : int * int) = 
  List.filter (fun c -> if c = coord then false else true) lst;;

let rec replace lst ind elt =
  match lst with
  | [] -> lst
  | h::t -> if ind = 0 then
              elt::(replace t (ind - 1) elt)
            else
              h::(replace t (ind - 1) elt)
;;

let filter_ships ship_lst ship_ind new_ship =
  replace ship_lst ship_ind new_ship
;;

(* lst will be current lst being fired at *)
let update_ship_lst cv = 
  update_ship_index !cv 0;
  let curr_ship = ship_at_index cv !current_ship_index in
  let coord = get_coord_of_hit curr_ship in
  let updated_ship = filter_coord curr_ship coord in
  let updated_ship_lst = filter_ships !cv !current_ship_index updated_ship in
  cv := updated_ship_lst

let rec print_lst lst = 
  match lst with
    | [] -> ()
    | h::t -> print_tuple h; print_lst t

let rec print_ship_lst lst_of_lst = 
  match lst_of_lst with
    | [] -> ()
    | h::t -> print_lst h; print_ship_lst t

(* [ai_fire m ship_lst] returns *)
let ai_fire m cv empty_lst = 
  match false with
    | true ->
              let curr_ship = ship_at_index cv !current_ship_index in
              let (x, y) = (get_coord_of_hit curr_ship) in
              print_string "\n";
              print_string "Updated Ship List: ";
              print_ship_lst !cv;
              print_string "\n";
              print_string "Fired at: ";
              print_tuple (x, y);
              print_string "\n";
              m.(x).(y) <- Hit;
              m
    | false ->
              let (x, y) = get_empty_coord empty_lst in
              (* print_string "\n";
              print_string "Empty List: ";
              print_lst !empty_lst;
              print_string "\n"; *)
              assert (not (List.mem (x, y) !empty_lst));
              assert (List.length (!empty_lst) <> (List.length ((x, y)::!empty_lst)));
               m.(x).(y) <- Miss;
               m
