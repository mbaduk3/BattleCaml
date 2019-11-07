open Gameboard

let rec play_game board = 
  print_string "> ";
  parse (read_line ()) board |> play_game

let main () = 
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to BattlCaml.\n");
  print_endline "Please enter your first command.\n";
  print_string  "> ";
  play_game init_matrix

let () = main ()