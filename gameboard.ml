(* This file will have all of the gameboard mechanics. *)
type entry = Hit | Miss | Unhit | Empty

exception No_Command

type coord = (int * int)

type t = entry Array.t array

let init_matrix:t = Array.make_matrix 10 10 Empty

let fire x:coord = x

let input_coordinates str = 

let head = function
  | [] -> ""
  | h::t -> h

let unparse_fire = 

let parse str = 
  let str' = String.split_on_char ' ' str in 
  let hd = head str' in
  match hd with
  | "" -> raise No_Command
  | "quit" -> exit 0
  | "fire" -> fire (1, 1)
  | _ -> (0, 0)


t