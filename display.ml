open Curses
open Gameboard

let scr = ref (initscr ())
(* Max x, y are the dimensions of the terminal window *)
let max_x = ref 0
let max_y = ref 0
let () = 
  ignore(Curses.cbreak ());
  ignore(Curses.noecho ());
  ignore(Curses.curs_set 0); 
  ignore(Curses.nodelay !scr true)
let b_win = ref null_window
let ai_win = ref null_window
let score_win = ref null_window
let meta_win = ref null_window
let err_win = ref null_window
let sel_win = ref null_window
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

let battlecaml_str = "
  ____        _   _   _       ____                _ 
 | __ )  __ _| |_| |_| | ___ / ___|__ _ _ __ ___ | |
 |  _ \ / _` | __| __| |/ _ \ |   / _` | '_ ` _ \| |
 | |_) | (_| | |_| |_| |  __/ |__| (_| | | | | | | |
 |____/ \__,_|\__|\__|_|\___|\____\__,_|_| |_| |_|_|"

let p_to_play_str = "
                            _ _     _ _   _                _             
                           ( | )   ( | ) | |              | |            
  _ __  _ __ ___  ___ ___   V V_ __ V V  | |_ ___    _ __ | | __ _ _   _ 
 | '_ \| '__/ _ \/ __/ __|    | '_ \     | __/ _ \  | '_ \| |/ _` | | | |
 | |_) | | |  __/\__ \__ \    | |_) |    | || (_) | | |_) | | (_| | |_| |
 | .__/|_|  \___||___/___/    | .__/      \__\___/  | .__/|_|\__,_|\__, |
 | |                          | |                   | |             __/ |
 |_|                          |_|                   |_|            |___/ 
"

let incr_cur b = 
  cur_x := !cur_x + 1;
  if (!cur_x > Array.length b.(0)) then 
    let () = cur_y := !cur_y + 1 in 
    cur_x := 1
  else if (!cur_y > Array.length b) then 
    cur_y := 1

(* Initalize the placement phase windows *)
let placement_init () = 
  ignore(wclear !scr);
  let y,x = getmaxyx !scr in
  b_win := (newwin 12 (12 * 2 - 1) 1 5);
  ignore(Curses.nodelay !b_win true);
  score_win := (newwin 3 15 3 54);
  meta_win := (newwin 9 15 6 54);
  err_win := (newwin 3 40 15 29);
  ignore(mvwin !b_win 3 29);
  ignore(wrefresh !scr)

(* Initalize the play phase windows *)
let play_init () = 
  ignore(mvwin !b_win 3 45);
  ai_win := (newwin 12 (12 * 2 - 1) 3 20);
  ignore(mvwin !score_win 3 70);
  ignore(mvwin !meta_win 6 70);
  ignore(mvwin !err_win 15 20);
  ignore(wresize !err_win 3 65);
  ignore(wclear !err_win);
  ignore(wrefresh !err_win);
  ignore(wclear !meta_win);
  ignore(wrefresh !meta_win);
  ignore(wclear !scr);
  ignore(wrefresh !scr)

(* Initalize the menu phase windows *)
let menu_init () = 
  sel_win := (newwin 20 80 3 3)

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

let handle_unhit b win phase dt = 
  if (phase = 1) then 
    ignore(Curses.mvwaddch win !cur_y (!cur_x*2) empty_ch)
  else 
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

let render_board b win phase dt =
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
        | Unhit -> handle_unhit b win phase dt
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

let handle_unhit_ai b win phase dt = 
  ignore(Curses.mvwaddch win !cur_y (!cur_x*2) unhit_ch);
  incr_cur b;
  ignore(wattroff win Curses.WA.standout)

let handle_misc_ai b win dt = 
  ignore(Curses.mvwaddch win !cur_y (!cur_x*2) empty_ch);
  incr_cur b;
  ignore(wattroff win Curses.WA.standout)

let render_ai_board b win phase dt = 
  cur_x := 1;
  cur_y := 1;
  for i = 0 to Array.length b - 1 do 
    for j = 0 to (Array.length b.(0) - 1) do 
    begin 
    match b.(i).(j) with 
        | Hit -> handle_hit_ai b win dt
        | Miss -> handle_miss_ai b win dt
        | Unhit -> handle_unhit_ai b win phase dt
        | _ -> handle_misc_ai b win dt
    end
    done
  done
(* Use to render cur_time: *)
  (* ignore(mvwaddstr win 9 1 (string_of_float !cur_timer)) *)

let render_names_placement () = 
  mvwaddstr !scr 2 30 "My board:"

let render_names_play () = 
  ignore(mvwaddstr !scr 2 46 "My board:");
  mvwaddstr !scr 2 19 "AI board:"

let render_names phase = 
  match phase with 
  | 0 -> ignore(render_names_placement ())
  | 1 -> ignore(render_names_play ())
  | _ -> ()

let render_score score = 
  ignore(mvwaddstr !score_win 1 1 ("Score: " ^ (string_of_int score)))

let render_err err = 
  ignore(mvwaddstr !err_win 1 1 err)

let render_turn turn = 
  let turn' = turn / 2 in 
  ignore(mvwaddstr !meta_win 1 1 ("Turn #: " ^ string_of_int turn'))

let str_of_phase = function 
  | 0 -> "Placement"
  | 1 -> "Play" 
  | 2 -> "Menu"
  | _ -> raise Out_of_bounds

let render_phase phase = 
  ignore(mvwaddstr !meta_win 2 1 phase)

let menu_refresh () = 
  let h,w = getmaxyx !sel_win in 
  ignore(mvwin !sel_win (!max_y / 2 - (h / 2)) (!max_x / 2 - (w / 2)));
  ignore(mvwhline !scr 3 0 0 1000);
  ignore(mvwhline !scr (!max_y - 3) 0 0 1000);
  ignore(mvwaddstr !sel_win 1 1 battlecaml_str);
  ignore(mvwaddstr !sel_win 8 1 p_to_play_str);
  ignore(box !sel_win 0 0)

let menu_helper win phase dt = 
  menu_refresh ()
  
(* Refreshes max_x, max_y to the current terminal size *)
let update_maxs () =
  let y,x = getmaxyx !scr in 
  max_x := x;
  max_y := y

let render b opp_b phase turn dt =
  begin
  update_maxs ();
  match phase with 
    | 0 -> 
      Curses.wborder !b_win 0 0 0 0 0 0 0 0;
      Curses.box !score_win 0 0;
      Curses.box !meta_win 0 0;
      Curses.box !err_win 0 0;
      render_board b !b_win phase dt;
      render_names phase;
      render_score 0;
      render_turn turn;
      render_err "Welcome to battleCaml!";
      render_phase (str_of_phase phase);
    | 1 -> 
      Curses.wborder !b_win 0 0 0 0 0 0 0 0;
      Curses.box !ai_win 0 0;
      Curses.box !score_win 0 0;
      Curses.box !meta_win 0 0;
      Curses.box !err_win 0 0;
      render_names phase;
      render_score 0;
      render_turn turn;
      render_err "Welcome to battleCaml!";
      render_phase (str_of_phase phase);
      render_board opp_b !ai_win phase dt;
      render_ai_board b !b_win phase dt
    | 2 -> menu_helper !scr phase dt
    | _ -> ()
  end;
  Curses.wrefresh !b_win;
  Curses.wrefresh !score_win;
  Curses.wrefresh !ai_win;
  Curses.wrefresh !err_win;
  Curses.wrefresh !meta_win;
  Curses.wrefresh !sel_win;
  Curses.wrefresh !scr;
  Sys.time ()

let exit_display () = 
  endwin (); exit 0




