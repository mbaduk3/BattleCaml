(* This file will have all of the gameboard mechanics. *)

type coord = (int * int)

type entry = Hit | Miss | Unhit | Empty

type t = entry Array.t array

let init_matrix:t = Array.make_matrix 10 10 Empty

let get_coord_of_matrix (matrix:t) (x:coord) = matrix.(fst x).(snd x)

let fire (x:coord) matrix = 
  match get_coord_of_matrix matrix x with
  | Empty -> matrix.(fst x).(snd x) <- Hit; matrix
  | Hit -> print_string "You Already Hit This Place! Try Again!"; matrix
  | Miss -> print_string "Miss!"; matrix
  | Unhit -> matrix.(fst x).(snd x) <- Hit; matrix

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

let parse str = 
  let str' = String.split_on_char ' ' str in 
  let hd = head str' in
  match hd with
  | "" -> raise (Invalid_argument "You Need to Pass in an Order!")
  | "quit" -> exit 0
  | "fire" -> fire (input_coordinates (second_elt str'))
  | _ -> exit 0


module Board = struct
end