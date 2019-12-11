open OUnit2
open Gameboard

(* ------------------- Gameboard Tests ----------------------- *)

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

let list_a = [1;2;3]
let array_a = Array.make 5 "a"
let array_b = Array.copy array_a
let () = array_b.(1) <- "b"; array_b.(2) <- "c"
let array_c = Array.make 3 "a"
let () = array_c.(1) <- "b"; array_c.(2) <- "c"
let array_empty = Array.make 0 "a"

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
]









(* ---------------------- End Testing ----------------------- *)
let suite = 
  "test suite for battleCaml" >::: List.flatten [
    gameboard_tests;
  ]

let _ = run_test_tt_main suite