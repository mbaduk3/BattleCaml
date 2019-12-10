open Curses
open Gameboard

let scr = ref (initscr ())
let () = 
  ignore(Curses.cbreak ());
  ignore(Curses.noecho ());
  ignore(Curses.curs_set 0); 
  ignore(Curses.nodelay !scr true)
let b_win = ref (newwin 12 (12 * 2 - 1) 1 5)
let ai_win = ref null_window
let () = 
  ignore(Curses.nodelay !b_win true)
let cur_x = ref 1
let cur_y = ref 1
let crosshair_x = ref 1
let crosshair_y = ref 1

let hit_ch = int_of_char 'X'
let miss_ch = int_of_char '0'
let unhit_ch = int_of_char '~'
let empty_ch = int_of_char '.'

let cur_timer = ref 0. 
let turn_count = ref 0

let incr_cur b = 
  cur_x := !cur_x + 1;
  if (!cur_x > Array.length b.(0)) then 
    let () = cur_y := !cur_y + 1 in 
    cur_x := 1
  else if (!cur_y > Array.length b) then 
    cur_y := 1

let placement_init () = 
  ignore(mvwin !b_win 1 17);
  ignore(refresh)

let play_init () = 
  ignore(mvwin !b_win 1 5);
  ai_win := (newwin 12 (12 * 2 - 1) 1 30);
  ignore(refresh)

let render_board b win dt =
  cur_x := 1;
  cur_y := 1;
  cur_timer := !cur_timer +. dt;
  for i = 0 to Array.length b - 1 do 
    for j = 0 to (Array.length b.(0) - 1) do 
      begin
        if (!cur_x = !crosshair_x && !cur_y = !crosshair_y) then
          begin
          if (1000. *. !cur_timer < 35.) then 
            begin
            ignore(wattroff win Curses.WA.protect);
            ignore(wattron win Curses.WA.standout)
            end;
          if (1000. *. !cur_timer > 50.) then 
            begin
            ignore(wattr_off win Curses.WA.standout);
            ignore(cur_timer := 0.);
            ignore(wattron win Curses.WA.protect)
            end
          end;
        match b.(i).(j) with 
        | Hit -> 
          ignore(Curses.mvwaddch win !cur_y (!cur_x*2) hit_ch); 
          incr_cur b;
          ignore(wattroff win Curses.WA.standout)
        | Miss -> 
          ignore(Curses.mvwaddch win !cur_y (!cur_x*2) miss_ch);
          incr_cur b;
          ignore(wattroff win Curses.WA.standout)
        | Unhit -> 
          ignore(Curses.mvwaddch win !cur_y (!cur_x*2) unhit_ch);
          incr_cur b;
          ignore(wattroff win Curses.WA.standout)
        | _ -> 
          ignore(Curses.mvwaddch win !cur_y (!cur_x*2) empty_ch);
          incr_cur b;
          ignore(wattroff win Curses.WA.standout)
      end
    done
  done;
  (* Use to render cur_time: *)
  (ignore(mvwaddstr win 9 1 (string_of_float !cur_timer)))

let render_ai_board b win dt = 
  cur_x := 1;
  cur_y := 1;
  for i = 0 to Array.length b - 1 do 
    for j = 0 to (Array.length b.(0) - 1) do 
    begin 
    match b.(i).(j) with 
        | Hit -> 
          ignore(Curses.mvwaddch win !cur_y (!cur_x*2) hit_ch); 
          incr_cur b;
          ignore(wattroff win Curses.WA.standout)
        | Miss -> 
          ignore(Curses.mvwaddch win !cur_y (!cur_x*2) miss_ch);
          incr_cur b;
          ignore(wattroff win Curses.WA.standout)
        | Unhit -> 
          ignore(Curses.mvwaddch win !cur_y (!cur_x*2) unhit_ch);
          incr_cur b;
          ignore(wattroff win Curses.WA.standout)
        | _ -> 
          ignore(Curses.mvwaddch win !cur_y (!cur_x*2) empty_ch);
          incr_cur b;
          ignore(wattroff win Curses.WA.standout)
    end
    done
  done

let render b dt = 
  Curses.box !b_win 0 0;
  Curses.box !ai_win 0 0;
  render_board b !b_win dt;
  render_board b !ai_win dt;
  Curses.wrefresh !b_win;
  Curses.wrefresh !ai_win;
  Sys.time ()

let exit_display () = 
  endwin (); exit 0




