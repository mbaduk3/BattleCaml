(* This file will have all of the gameboard mechanics. *)

type coord = (int * int)

type entry = BoardHit | Miss | Unhit | Empty | ShipHit

type t = entry Array.t array

type ship = t

type command = Fire of coord| Rotate | Hint | Quit

(** For Array.make_matrix x y elt, we would read it as x rows of length y whose
  elements are elt
  Array.(i).(j) gives the val at row i, column j *)

let init_matrix:t = Array.make_matrix 10 10 Empty

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

(*let set_ship_name = print_string "/nWhat Would You Like To Name Your Ship? /n";
  read_line () *)

(** A ship will always have 1 or n elements in the matrix. 1 if horizontal. n if vertical*)

let create_ship len : t = Array.make_matrix 1 len Unhit

let caml_5 = create_ship 5

let caml_4 = create_ship 4

let caml_3 = create_ship 3

let caml_3' = create_ship 3

let caml_2 = create_ship 2

let new_mod n m = (n + m) mod m

let get_row matrix num = matrix.(num)

(*Changes the given [row] to the [arr] within the specified range [i] to [j] (j not inclusive). 
The range must equal the length of [arr]. This allows you to modify a row in 0..n-1 *)
let modify_matrix matrix ship num = 
  for i = 0 to Array.length ship - 1 do
    for j = 0 to (Array.length ship.(0)) - 1 do 
      matrix.(num).(j) <- ship.(i).(j)
    done
  done

let demo_board = 
  modify_matrix init_matrix caml_5 0;
  modify_matrix init_matrix caml_4 1;
  modify_matrix init_matrix caml_3 2;
  modify_matrix init_matrix caml_3' 3;
  modify_matrix init_matrix caml_2 4

let win_condition matrix = matrix

(*[transpose matrix] returns a new [matrix] where the rows of the [matrix] become
  the columns. This is to be used for rotations of ships*)
let transpose matrix = 
  Array.init (Array.length matrix.(0)) (fun i -> 
    Array.init (Array.length matrix) (fun j -> matrix.(j).(i)))

(*if num > 0 then row.(num) <- arr.(num); change_row tl t (pred num); else change_row row ar*)

let get_val_of_coord (matrix:t) (x:coord) = matrix.(fst x).(snd x)

let fire (c:coord) matrix = 
  match get_val_of_coord matrix c with
  | Empty -> matrix.(fst c).(snd c) <- BoardHit; matrix
  | BoardHit | ShipHit -> print_string "You Already Hit This Place! Try Again!"; matrix
  | Miss -> print_string "Miss!"; matrix
  | Unhit -> matrix.(fst c).(snd c) <- ShipHit; matrix

let input_coordinates str : coord = 
let x = String.get str 1 in
let y = String.get str 4 in 
let int_x = Char.code x - 48 in 
let int_y = Char.code y - 48 in
(int_x, int_y)

let head = function
  | [] -> ""
  | h::t -> h

let second_elt lst = List.nth lst 1

let string_of_tuple tup = 
  let x = fst tup in 
  let y = snd tup in
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let give_hint matrix = 
  for i = 0 to Array.length matrix do
      if matrix.(i).(4) = Unhit then 
        print_string ("\nThere is an unhit caml at " ^ string_of_tuple (i, 4) ^ "\n")
      else
        print_string "\nCouldn't Find Anything For You. Just Keep Firing!\n"
  done

let parse str = 
  let str' = String.split_on_char ' ' str in 
  let hd = head str' in
  match hd with
  | "fire" -> Fire (input_coordinates (second_elt str'))
  | "hint" -> Hint
  | "rotate" -> Rotate
  | "" | "quit" | _ -> Quit


module Board = struct
  
end
