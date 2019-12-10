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
let score_win = ref (newwin 3 15 3 54)
let meta_win = ref (newwin 9 15 6 54)
let err_win = ref (newwin 3 43 17 15)
let () = 
  ignore(Curses.nodelay !b_win true)
let cur_x = ref 1
let cur_y = ref 1
(* Crosshair x and y refer to the top-left coord of the crosshair matrix *)
let crosshair_x = ref 1 
let crosshair_y = ref 1
(* The crosshair matrix. It can be any pattern, not just 1x1 *)
let crosshair_mat = ref (Array.make_matrix 2 1 1)

let hit_ch = int_of_char 'X'
let miss_ch = int_of_char '0'
let unhit_ch = int_of_char '~'
let empty_ch = int_of_char '.'

let blue = Curses.Color.blue
let red = Curses.Color.red
let green = Curses.Color.green
let bkgd_color win = Curses.getbkgd win

let color_pair1 win = init_pair 1 (bkgd_color win) red

let cur_timer = ref 0. 
let turn_count = ref 0
let scr_width = ref 0

let incr_cur b = 
  cur_x := !cur_x + 1;
  if (!cur_x > Array.length b.(0)) then 
    let () = cur_y := !cur_y + 1 in 
    cur_x := 1
  else if (!cur_y > Array.length b) then 
    cur_y := 1

let placement_init () = 
  let y,x = getmaxyx !scr in
  ignore(mvwin !b_win 3 29);
  ignore(refresh)

let play_init () = 
  ignore(mvwin !b_win 3 45);
  ai_win := (newwin 12 (12 * 2 - 1) 3 20);
  ignore(mvwin !score_win 3 70);
  ignore(mvwin !meta_win 6 70);
  ignore(wclear !scr);
  ignore(wrefresh !scr)

(* True if the drawing cursor coordinates are equal to a crosshair coord.
   False otherwise *)
let check_cross () = 
  if (!cur_x < !crosshair_x || 
      !cur_x > (!crosshair_x + (Array.length !crosshair_mat) - 1))
  then 
    false
  else if (!cur_y < !crosshair_y || 
      !cur_y > (!crosshair_y + (Array.length !crosshair_mat.(0)) - 1))
  then 
    false
  else
    let rel_x = !cur_x - !crosshair_x in 
    let rel_y = !cur_y - !crosshair_y in 
    if (!crosshair_mat.(rel_x).(rel_y) = 0) 
    then false
    else true

let handle_hit b win dt = 
  ignore (color_pair1 win);
  ignore (wattroff win Curses.WA.standout);
  if (check_cross ()) then
  ignore (wattron win (Curses.WA.color_pair 1));
  ignore(Curses.mvwaddch win !cur_y (!cur_x*2) hit_ch);
  incr_cur b;
  ignore (wattroff win (Curses.WA.color_pair 1))

let handle_miss b win dt = 
  ignore (color_pair1 win);
  ignore (wattroff win Curses.WA.standout);
  if (check_cross ()) then
  ignore (wattron win (Curses.WA.color_pair 1));
  ignore(Curses.mvwaddch win !cur_y (!cur_x*2) miss_ch);
  incr_cur b;
  ignore (wattroff win (Curses.WA.color_pair 1))

let handle_unhit b win dt = 
  ignore(Curses.mvwaddch win !cur_y (!cur_x*2) unhit_ch);
  incr_cur b;
  ignore(wattroff win Curses.WA.standout)

let handle_misc b win dt = 
  ignore(Curses.mvwaddch win !cur_y (!cur_x*2) empty_ch);
  incr_cur b;
  ignore(wattroff win Curses.WA.standout)

let cur_blink_helper b win dt = 
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

let render_board b win dt =
  cur_x := 1;
  cur_y := 1;
  ignore (start_color ());
  cur_timer := !cur_timer +. dt;
  for i = 0 to Array.length b - 1 do 
    for j = 0 to (Array.length b.(0) - 1) do 
      begin
        if (check_cross ()) then
          cur_blink_helper b win dt;
        match b.(i).(j) with 
        | Hit -> handle_hit b win dt
        | Miss -> handle_miss b win dt
        | Unhit -> handle_unhit b win dt
        | _ -> handle_misc b win dt
      end
    done
  done
  (* Use to render cur_time: *)
  (* (ignore(mvwaddstr win 9 1 (string_of_float !cur_timer))) *)

let handle_hit_ai b win dt = 
  ignore(Curses.mvwaddch win !cur_y (!cur_x*2) hit_ch); 
  incr_cur b;
  ignore(wattroff win Curses.WA.standout)

let handle_miss_ai b win dt = 
  ignore(Curses.mvwaddch win !cur_y (!cur_x*2) miss_ch);
  incr_cur b;
  ignore(wattroff win Curses.WA.standout)

let handle_unhit_ai b win dt = 
  ignore(Curses.mvwaddch win !cur_y (!cur_x*2) unhit_ch);
  incr_cur b;
  ignore(wattroff win Curses.WA.standout)

let handle_misc_ai b win dt = 
  ignore(Curses.mvwaddch win !cur_y (!cur_x*2) empty_ch);
  incr_cur b;
  ignore(wattroff win Curses.WA.standout)

let render_ai_board b win dt = 
  cur_x := 1;
  cur_y := 1;
  for i = 0 to Array.length b - 1 do 
    for j = 0 to (Array.length b.(0) - 1) do 
    begin 
    match b.(i).(j) with 
        | Hit -> handle_hit_ai b win dt
        | Miss -> handle_miss_ai b win dt
        | Unhit -> handle_unhit_ai b win dt
        | _ -> handle_misc_ai b win dt
    end
    done
  done
(* Use to render cur_time: *)
  (* ignore(mvwaddstr win 9 1 (string_of_float !cur_timer)) *)

let render_names_placement () = 
  mvwaddstr !scr 2 30 "My board:"

let render_names_play () = 
  mvwaddstr !scr 2 46 "My board:";
  mvwaddstr !scr 2 19 "AI board:"

let render_names phase = 
  match phase with 
  | 0 -> ignore(render_names_placement ())
  | 1 -> ignore(render_names_play ())
  | _ -> ()

let render_score score = 
  ignore(mvwaddstr !score_win 1 1 ("Score: " ^ (string_of_int score)))

let render b opp_b phase dt = 
  (* Curses.wborder !scr 0 0 0 0 0 0 0 0; *)
  Curses.wborder !b_win 0 0 0 0 0 0 0 0;
  Curses.box !ai_win 0 0;
  Curses.box !score_win 0 0;
  Curses.box !meta_win 0 0;
  Curses.box !err_win 0 0;
  if (phase = 0) then 
    begin
    render_board b !b_win dt;
    render_ai_board opp_b !ai_win dt
    end
  else 
    begin
    render_board opp_b !ai_win dt;
    render_ai_board b !b_win dt
    end;
  render_names phase;
  render_score 0;
  Curses.wrefresh !b_win;
  Curses.wrefresh !score_win;
  Curses.wrefresh !ai_win;
  Curses.wrefresh !err_win;
  Curses.wrefresh !meta_win;
  Curses.wrefresh !scr;
  Sys.time ()

let exit_display () = 
  endwin (); exit 0




