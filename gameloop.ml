open Gameboard

let rec play_game = 
  print_string "> ";
  read_line () |> parse;
  play_game

let main () = 
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to BattlCaml.\n");
  print_endline "Please enter your first command.\n";
  print_string  "> ";
  play_game

let () = main ()