open OUnit2
open Gameboard

(* ---------------------------- Test Plan ----------------------------------- *)

(*
  Our game consists of several files, each of which contains the logic for a 
  different part of the system. As such, we tested each file differently. 

  The gameboard contains the logic for the main game data structure. This 
  structure is mutable, so testing the board required that we kept track of how 
  the board was being modified in between tests, so that we would know what the 
  expected results should be. As such, we developed most of the tests using 
  a glass-box testing approach, so that we would be able to examine all of the 
  different scenarios that might occur in the game. After writing these tests, 
  we also tested by playing the game extensively, just to ensure the correctness
  of the system. 

  
*)




(* ------------------- Gameboard Tests ----------------------- *)

let list_a = [1;2;3]
let array_a = Array.make 5 "a"
let array_b = Array.copy array_a
let () = array_b.(1) <- "b"; array_b.(2) <- "c"
let array_c = Array.make 3 "a"
let () = array_c.(1) <- "b"; array_c.(2) <- "c"
let array_empty = Array.make 0 "a"
let board_a = Array.make_matrix 10 10 Empty
let board_b = Array.make_matrix 10 10 Empty
let () = board_b.(2).(3) <- Miss
let board_c = Array.make_matrix 10 10 Empty
let () = board_c.(5).(5) <- Hit

let reset_board_a () = 
  for i = 0 to 9 do 
    for j = 0 to 9 do 
      board_a.(i).(j) <- Empty
    done
  done

let make_init_matrix_test 
  (name: string) : test = 
  name >:: (fun _ -> 
    let m = init_matrix () in 
    assert_equal 10 (Array.length m);
    assert_equal 10 (Array.length m.(0)))

let make_index_test 
  (name: string)
  (input_lst : 'a list)
  (input_elem : 'a)
  (expected_index: int) : test = 
  name >:: (fun _ -> 
    let i = index input_lst input_elem 0 in 
    assert_equal expected_index i)

let make_get_array_from_test
  (name: string)
  (input_i: int)
  (input_j: int)
  (input_arr: 'a array)
  (expected_arr: 'a array) : test = 
  name >:: (fun _ -> 
    let m = get_array_from input_i input_j input_arr in 
    assert_equal expected_arr m)

let make_new_mod_test 
  (name: string)
  (input_n: int)
  (input_m: int)
  (expected: int) : test = 
  name >:: (fun _ -> 
    let res = new_mod input_n input_m in 
    assert_equal ~printer:(string_of_int) expected res)

let make_string_of_tup_test 
  (name: string)
  (input_tup: int*int)
  (expected: string) : test = 
  name >:: (fun _ -> 
    let res = string_of_tuple input_tup in 
    assert_equal expected res)

let make_get_val_of_coord
  (name: string)
  (input_m: Gameboard.t)
  (input_coord: int*int)
  (expected: Gameboard.entry) : test = 
  name >:: (fun _ -> 
    let res = get_val_of_coord input_m input_coord in 
    assert_equal ~printer:(string_of_entry) expected res)

let make_fire_test 
  (name: string)
  (input_c: coord)
  (input_m: Gameboard.t)
  (expected: string) : test = 
  name >:: (fun _ -> 
    format input_m;
    let res = fire input_c input_m in 
    format input_m;
    assert_equal expected (string_of_response res))


let gameboard_tests = [
  make_init_matrix_test "init_matrix ()";
  make_index_test "first index" list_a 1 0;
  make_index_test "middle index" list_a 2 1;
  make_index_test "lase index" list_a 3 2;
  make_index_test "empty list" [] 2 0;
  make_get_array_from_test "i=j" 2 2 array_a array_empty;
  make_get_array_from_test "i=0" 0 2 array_a (Array.make 2 "a");
  make_get_array_from_test "i=0 2" 0 3 array_b array_c;
  make_new_mod_test "0 mod" 0 3 0;
  make_new_mod_test "regular mod" 5 3 2;
  make_new_mod_test "negative mod" (-3) 2 (-1);
  make_new_mod_test "mod by negative" 5 (-3) (2);
  make_string_of_tup_test "tuple test" (5, 5) "(5, 5)";
  make_string_of_tup_test "negative tup" (-2, 3) "(-2, 3)";
  make_get_val_of_coord "empty val" board_a (2, 3) Empty;
  make_fire_test "empty fire" (2, 3) board_a "no contact";
  make_get_val_of_coord "hit val" board_c (5, 5) Hit;
  make_fire_test "miss fire" (2, 3) board_b "already miss";
]



(* ---------------------- End Testing ----------------------- *)
let suite = 
  "test suite for battleCaml" >::: List.flatten [
    gameboard_tests;
  ]

let _ = run_test_tt_main suite