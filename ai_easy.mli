(** 
    The logic for easy version AI.

    This module handles the AI firing. This AI fires at random spots on the 
    board.
 *)

(** [ai_fire m] is a board with one entry randomly changed, corresponding to 
    the firing mechanism of Gameboard. *)
val ai_fire : Gameboard.entry array array -> Gameboard.entry array array

val filter : 'a -> 'a list ref -> 'a list

val shuffle : 'a list -> 'a list

(* [cartesian_product x_lst y_lst] calculates the cartesian product of 
    [x_lst]x[y_lst]. The end result is a list of tuples of all different 
    coordinate pair combinations *)
val cartesian_product : 'a list -> 'b list -> ('a * 'b) list