open Curses
open Gameboard

let scr = ref (initscr ());
ignore(Curses.cbreak ());
ignore(Curses.noecho ())
let b_win = ref (newwin 11 11 1 1)
let cur_x = ref 0
let cur_y = ref 0

let hit_ch = int_of_char 'X'
let miss_ch = int_of_char '0'
let empty_ch = int_of_char '.'

let incr_cur b = 
    cur_x := !cur_x + 1;
    if (!cur_x > Array.length b.(0)) then 
        cur_x := 0;
        cur_y := !cur_y + 1

let render_board b win = 
    cur_x := 0;
    cur_y := 0;
    for i = 0 to Array.length b - 1 do 
        for j = 0 to (Array.length b.(0) - 1) do 
            begin
            match b.(i).(j) with 
            | Hit -> 
                ignore(Curses.mvwaddch win !cur_y !cur_x hit_ch); 
                incr_cur b 
            | Miss -> 
                ignore(Curses.mvwaddch win !cur_y !cur_x miss_ch);
                incr_cur b 
            | _ -> 
                ignore(Curses.mvwaddch win !cur_y !cur_x empty_ch);
                incr_cur b
            end
        done
    done

let render b = 
    (*Curses.erase ();*)
    (*Curses.box !scr 0 0;*)
    Curses.box !b_win 0 0;
    render_board b !b_win;
    (* Curses.mvwaddch !b_win 10 10 empty_ch; *)
    Curses.wrefresh !b_win
    (*ignore(Curses.refresh ())*)



