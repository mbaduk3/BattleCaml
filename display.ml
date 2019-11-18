open Curses
open Gameboard

let scr = ref (initscr ());
ignore(Curses.cbreak ());
ignore(Curses.noecho ())
let b_win = ref (newwin 12 12 1 10)
let cur_x = ref 1
let cur_y = ref 1

let hit_ch = int_of_char 'X'
let miss_ch = int_of_char '0'
let unhit_ch = int_of_char '~'
let empty_ch = int_of_char '.'

let incr_cur b = 
    cur_x := !cur_x + 1;
    if (!cur_x > Array.length b.(0)) then 
        let () = cur_y := !cur_y + 1 in 
        cur_x := 1
    else if (!cur_y > Array.length b) then 
        cur_y := 1
        

let render_board b win = 
    cur_x := 1;
    cur_y := 1;
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
            | Unhit -> 
                ignore(Curses.mvwaddch win !cur_y !cur_x unhit_ch);
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
    Curses.wrefresh !b_win
    (*ignore(Curses.refresh ())*)



