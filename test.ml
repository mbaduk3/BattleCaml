open OUnit2
open Gameboard

let make_init_matrix_test 
  (name: string) : test = 
  name >:: (fun _ -> 
    let m = init_matrix () in 
    assert_equal 10 (Array.length m);
    assert_equal 10 (Array.length m.(0)))

let gameboard_tests = [
  make_init_matrix_test "init_matrix ()"
]

let suite = 
  "test suite for battleCaml" >::: List.flatten [
    gameboard_tests;
  ]

let _ = run_test_tt_main suite