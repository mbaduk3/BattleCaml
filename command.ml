open Curses

type command = Fire | Rotate | Quit | Save | Up | Down | Left | Right | Other

let width = 10
let height = 10
let start_x win = ref (snd (getyx win))
let start_y win = ref (fst (getyx win))

(* let key_pad = keypad scr true

let no_delay = nodelay scr true *)

let get_key win = 
  match char_of_int (wgetch win) with
  | 'S' | 's' -> Down 
  | 'W' | 'w' -> Up
  | 'A' | 'a' -> Left
  | 'D' | 'd' -> Right
  | 'F' | 'f' -> Fire
  | 'R' | 'r' -> Rotate
  | 'Q' | 'q' -> Quit
  | _ -> Save