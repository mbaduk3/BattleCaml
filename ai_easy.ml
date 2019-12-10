open Gameboard
open Random

let coordlst_x = ref [0;1;2;3;4;5;6;7;8;9]
let coordlst_y = ref [0;1;2;3;4;5;6;7;8;9]

let filter coord coordlst = 
  let func = (fun lst -> if lst <> coord then true else false) in
  coordlst := List.filter func !coordlst;
  !coordlst

let choose_rnd_elt_in_lst lst = 
  let rnd_elt = Random.int (List.length !lst) in
  let (rnd_ind_x, rnd_ind_y) = List.nth !lst rnd_elt in
  (rnd_ind_x, rnd_ind_y)
 
 (* [cartesian_product x_lst y_lst] calculates the cartesian product of [x_lst]x[y_lst].
 The end result is a list of tuples of all different coordinate pair combinations *)
let rec cartesian_product x_lst y_lst = 
    match x_lst, y_lst with
    | [], _ | _, [] -> []
    | h1::t1, h2::t2 -> let fstlst = (cartesian_product [h1] t2) in
                        let sndlst = (cartesian_product t1 y_lst) in
                        let newlst = fstlst@sndlst in
                        (h1,h2)::newlst

let shuffle lst =
  let func = (fun tup -> (Random.bits (), tup)) in
  let new_lst = List.map func lst in
  let sort = List.sort compare new_lst in
  List.map snd sort

let generate_coordinates = 
  let prod = cartesian_product !coordlst_x !coordlst_y in
  (shuffle prod)

let list_generated = ref generate_coordinates

(* This method will prevent firing at the same place twice. *)
let ai_fire m = 
  if !list_generated <> [] then
    let (x, y) = choose_rnd_elt_in_lst list_generated in
    ignore (filter (x, y) list_generated);
    match m.(x).(y) with
      | Unhit ->  m.(x).(y) <- Hit; Contact m
      | Empty -> m.(x).(y) <- Miss; No_contact m
      | _ -> raise (Invalid_argument "This will not be possible")
    else
      raise (Invalid_argument "This will not be possible")