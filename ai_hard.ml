open Gameboard
open Random

let empty_lst = ref []
let ship_lst = ref []
let fire_lst = [true; false]

(* [determine_hard_fire ()] has a 50% chance or returning true and a 50% chance
of returning false. *)
let determine_hard_fire () = 
  let elt = Random.int 2 in
  List.nth fire_lst elt

(* [get_all_empty_coords miss_coords] returns a list of coordinate pairs representing
the coordinates of type Empty. *)
let get_all_empty_coords m = 
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length m.(i) - 1 do
      match m.(i).(j) with
        | Empty ->  empty_lst := (i, j)::!empty_lst
        | _ -> ()
    done
  done;
  empty_lst

(* [remove_empty coord reflst] *)
let remove_empty coord reflst = 
  reflst := List.filter (fun lst -> if lst = coord then false else true) !reflst

(* [get_empty_coord miss_coords] returns a list of coordinate pairs representing
the coordinates of type Empty. *)
let get_empty_coord miss_coords = 
  let rnd_ind = Random.int (List.length !miss_coords) in
  let empty_coord = List.nth !miss_coords rnd_ind in
  remove_empty empty_coord empty_lst;
  empty_coord

(* [thd tup] returns the third element of [tup], which is represented by 
(x, y, len, orientation), thus returning the value of len *)
let thd tup = 
  match tup with
    | (_, _, t, _) -> t

(* [create_vertical_lst tup acc num] transforms [tup] into a list of coordinate pairs
that describe the position of each of the user's placed vertical ships.*)
let rec create_vertical_lst tup acc num = 
  match num with
    | num when num < (thd tup) -> 
      let (x, y, _, _) = tup in (create_vertical_lst tup ((x, y+num)::acc) (succ num))
    | _ -> ref (List.rev acc)

(* [create_horizontal_lst tup acc num] transforms [tup] into a list of coordinate pairs
that describe the position of each of the user's placed horizontal ships.*)
let rec create_horizontal_lst tup acc num = 
  match num with
    | num when num < (thd tup) -> 
      let (x, y, _, _) = tup in create_horizontal_lst tup ((x+num, y)::acc) (succ num)
    | _ -> ref (List.rev acc)

(* [ship_coordinates arr_to_lst acc] returns a reference of [arr_to_lst] whose elements
are references to coordinate positions of the user's placed ships *)
let rec ship_coordinates arr_to_lst acc = 
    match arr_to_lst with
      | [] -> acc
      | h::t -> let (_, _, _, orientation) = h in
                if orientation = Vertical then
                  (ship_coordinates t ((create_vertical_lst h [] 0)::acc))
                else
                  (ship_coordinates t ((create_horizontal_lst h [] 0)::acc))

let shuffle lst =
  let func = (fun tup -> (Random.bits (), tup)) in
  let new_lst = List.map func lst in
  let sort = List.sort compare new_lst in
  List.map snd sort

(* [current_ship_index] is a reference to a value between 0 and 4 inclusive*)
let next_index = ref 1

let current_ship_index = ref 0

(* [new_index bound] is an integer to represent a new index into the list of ships
to be fired at. *)
let new_index bound = 
  if !next_index < bound then
    current_ship_index := !next_index;
    next_index := succ (!next_index)

(* lst will be current lst being fired at *)
let remove_coord_from_ship shipref coord = 
  shipref := List.filter (fun c -> if c = coord then false else true) !shipref

let remove_ship_if_empty () = 
  ship_lst := List.filter (fun reflst -> if !reflst = [] then false else true) !ship_lst

let update_curr_ship_index () = 
  if !(List.nth !ship_lst (!current_ship_index)) = [] then 
  begin
    remove_ship_if_empty ();
    new_index (List.length !ship_lst);
    !current_ship_index
  end
  else
    !current_ship_index

let get_curr_ship ships = 
  List.nth ships (update_curr_ship_index ())

(* bangships should be !ship_lst *)
let rec ai_win_condition bangships = 
  match bangships with
    | [] -> true
    | h::t -> if !h = [] then
                ai_win_condition t 
              else
                false
  
(* Returns coordinate to fire at. arr is an array of coordinates *)
let get_coord_of_hit reflst =
  let rnd_ind = Random.int (List.length !reflst) in
  List.nth !reflst rnd_ind

let ai_fire m coords = 
  ship_lst := ship_coordinates (Array.to_list(coords)) [];
  let curr_ship = get_curr_ship !ship_lst in
  match determine_hard_fire () with
    | true -> let (x, y) = (get_coord_of_hit curr_ship) in
              remove_coord_from_ship curr_ship (x, y);
              m.(x).(y) <- Hit;
              m
    | false -> let (x, y) = get_empty_coord (get_all_empty_coords m) in
              remove_empty (x, y) empty_lst;
               m.(x).(y) <- Miss;
               m
