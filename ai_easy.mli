(** 
    The logic for easy version AI.

    This module handles the AI firing. This AI fires at random spots on the 
    board.
 *)

(** [ai_fire m] is a board with one entry randomly changed, corresponding to 
    the firing mechanism of Gameboard. *)
val ai_fire : Gameboard.entry array array -> Gameboard.entry array array