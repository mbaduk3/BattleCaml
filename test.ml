open OUnit2
open Gameboard
open Gameloop
open Ai_hard
open Ai_easy
(* ---------------------------- TEST PLAN ----------------------------------
   Testing for a game is rather difficult to do, so for testing, we decided to
   essentially run a single player game of BattleCaml. This single player game
   tests all of the functionality that is manually done by the player. This is done
   using OUnit, and it ultimately proves part of system correctness because it
   accurately simulates how one would fire in the game using the cursor. Of course,
   BattleCaml has an AI aspect of it as well, which is also tested using OUnit.

   I created two helper functions for testing the randomness functionality. If two
   lists are the same, but in different order, then their difference should be an
   empty list. 
   ---------------------------END TEST PLAN -------------------------------*)

(* [intersect l1 l2] returns a list of the shared elements between [l1] and [l2] *)
let intersect l1 l2 =
  List.rev ( List.fold_left (fun acc x -> if (List.exists (fun y -> y = x) l1) 
                              then x::acc else acc) [] l2)

(* [diff l1 l2] returns a list whose elements are in [l1] but not [l2] *)
let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let ship_lst_test = ref [[(0,0); (0, 1);]; [(0, 2); (0, 4); 
                                            (0, 5)]; [(1, 1); (1, 2); (1, 3); (1, 4)]]

let matrix_ex = [|[|Empty; Empty; Empty; Empty; Empty; Empty|];
                  [|Unhit; Unhit; Unhit; Unhit; Unhit; Unhit|];
                  [|Empty; Empty; Empty; Empty; Unhit; Unhit|]|]

let empty_coords = [(0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0);
                    (0, 2); (1, 2); (2, 2); (3, 2)]

let empty_lst_ex = ref [(0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0);
                        (0, 2); (1, 2); (1, 2); (1, 3)]

let empty_lst_copy = ref [(0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0);
                          (0, 2); (1, 2); (1, 2); (1, 3)]

let vertical_lst = [|("making", 1, 5, Vertical); ("sure", 2, 5, Vertical); 
                     ("x coord", 3, 1, Vertical); ("not used", 4, 7, Vertical)|]

let fst_elem_vert = [("making", 1); ("making", 2); ("making", 3); ("making", 4); ("making", 5)]

let snd_elem_vert = [("sure", 2); ("sure", 3); ("sure", 4); ("sure", 5); ("sure", 6)]

let thd_elem_vert = [("x coord", 3)]

let frth_elem_vert = [("not used", 4); ("not used", 5); ("not used", 6); ("not used", 7);
                      ("not used", 8); ("not used", 9); ("not used", 10)]

let horizontal_lst = [|(1, "making", 5, Horizontal); (2, "sure", 5, Horizontal); 
                       (3, "y coord", 1, Horizontal); (4, "not used", 7, Horizontal)|]

let fst_elem_hoz = [(1, "making"); (2, "making"); (3, "making"); 
                    (4, "making"); (5, "making")]

let snd_elem_hoz = [(2, "sure"); (3, "sure"); (4, "sure"); (5, "sure"); (6, "sure")]

let thd_elem_hoz = [(3, "y coord")]

let frth_elem_hoz = [(4, "not used"); (5, "not used"); (6, "not used"); (7, "not used");
                     (8, "not used"); (9, "not used"); (10, "not used")]

let example_opp_ships = [|(1, 1, 5, Vertical); (2, 2, 5, Horizontal); 
                          (3, 3, 1, Vertical); (4, 4, 7, Horizontal)|]

let make_thd_test
    (name : string)
    (expected_output : ('c))
    (input : 'a * 'b * 'c * 'd) : test = 
  name >:: fun _ ->
    assert_equal expected_output (thd input)

let make_get_miss_coord_test (name : string) : test =
  name >:: fun _ ->
    let difference = diff !empty_lst_copy !empty_lst_ex in
    assert_equal 1 (List.length difference)

let make_create_vertical_lst_test
    (name : string)
    (expected_output : ('a * int) list)
    (input1 : 'a * int * int * 'b)
    (input2 : ('a * int) list)
    (input3 : int) : test = 
  name >:: fun _ ->
    assert_equal expected_output (create_vertical_lst input1 input2 input3)

let make_create_horizontal_lst_test
    (name : string)
    (expected_output : (int * 'a) list)
    (input1 : int * 'a * int * 'b)
    (input2 : (int * 'a) list)
    (input3 : int) : test = 
  name >:: fun _ ->
    assert_equal expected_output (create_horizontal_lst input1 input2 input3)

let make_update_ship_lst_test
    (name : string)
    (expected_output : (int * int) list list) : test = 
  name >:: fun _ -> (update_ship_lst ship_lst_test);
    assert_equal expected_output !ship_lst_test

let make_get_all_empty_coords_test
    (name : string)
    (expected_output : (int * int) list)
    (input : Gameboard.entry array array) : test = 
  name >:: fun _ ->
    assert_equal expected_output (get_all_empty_coords input (ref []))

let make_filter_test
    (name : string)
    (expected_output : 'a list)
    (input1 : 'a)
    (input2 : 'a list ref) : test =
  name >:: fun _ ->
    assert_equal expected_output (Ai_easy.filter input1 input2)

let make_cartesian_product_test
    (name : string)
    (expected_output : ('a * 'b) list)
    (input1 : 'a list)
    (input2 : 'b list) : test = 
  name >:: fun _ -> 
    let cp = cartesian_product input1 input2 in
    assert_equal [] (diff cp expected_output)

let make_shuffled_lst_eq_test
    (name : string)
    (test: 'a list)
    (input : 'a list) : test = 
  name >:: fun _ ->
    assert_equal [] (diff (shuffle input) test)

let make_shuffled_lst_len_eq_test
    (name : string)
    (test: 'a list)
    (input : 'a list) : test = 
  name >:: fun _ ->
    assert_equal (List.length test) (List.length (shuffle input))

let make_hor_to_ver_test
    (name : string)
    (ships : ('a * orientation) array)
    (ship : int) : test =
  handle_rotate ships ship;
  name >:: fun _ -> assert_equal (snd ships.(ship)) Vertical

let make_ver_to_hor_test
    (name : string)
    (ships : ('a * orientation) array)
    (ship : int) : test = 
  handle_rotate ships ship;
  name >:: fun _ -> assert_equal (snd ships.(ship)) Horizontal

let ai_functionality_tests = [
  make_thd_test "Test on String" "3" (1, 2, "3", 4);
  make_thd_test "Test on Int" 3 (1, 2, 3, 4);
  make_create_vertical_lst_test "1st Elem" fst_elem_vert vertical_lst.(0) [] 0;
  make_create_vertical_lst_test "2nd Elem" snd_elem_vert vertical_lst.(1) [] 0;
  make_create_vertical_lst_test "3rd Elem" thd_elem_vert vertical_lst.(2) [] 0;
  make_create_vertical_lst_test "4th Elem" frth_elem_vert vertical_lst.(3) [] 0;
  make_create_horizontal_lst_test "1st Elem" fst_elem_hoz horizontal_lst.(0) [] 0;
  make_create_horizontal_lst_test "2nd Elem" snd_elem_hoz horizontal_lst.(1) [] 0;
  make_create_horizontal_lst_test "3rd Elem" thd_elem_hoz horizontal_lst.(2) [] 0;
  make_create_horizontal_lst_test "4th Elem" frth_elem_hoz horizontal_lst.(3) [] 0;
  make_shuffled_lst_eq_test "Empty Lists" [] [];
  make_shuffled_lst_eq_test "Non-Empty Lists" [1;2;3] [1;2;3];
  make_shuffled_lst_len_eq_test "Empty Lists" [] [];
  make_shuffled_lst_len_eq_test "Non-Empty Lists" [1;2;3] [1;2;3];
  make_filter_test "Filter Test 1" [] (0,0) (ref [(0,0)]);
  make_filter_test "Filter Test 2" [(0, 0); (2, 0)] (1,0) (ref [(0,0); (1, 0); (2, 0)]);
  make_filter_test "Filter Test 3" [(0, 0); (1, 0); (2, 0)] (3, 0) (ref [(0,0); (1, 0); (2, 0)]);
  make_cartesian_product_test "CP 1 Elem List" [(3, 3)] [3] [3];
  make_cartesian_product_test "CP 2 Elem List" [(0, 0); (0, 1); (1, 0); (1, 1)] [0; 1] [0; 1];
  make_hor_to_ver_test "Horizontal to vertical rotation" (Array.make 1 (0, Horizontal)) 0;
  make_ver_to_hor_test "Vertical to horizontal rotation" (Array.make 1 (0, Vertical)) 0;
  (* make_get_all_empty_coords_test "Empty Coords" empty_coords matrix_ex; *)
  (* make_update_ship_lst_test "Ship List Iter 1" [[(0, 1);]; [(0, 2); (0, 4); 
                          (0, 5)]; [(1, 1); (1, 2); (1, 3); (1, 4)]]; *)
  (* make_update_ship_lst_test "Ship List Iter 2" [[]; [(0, 2); (0, 4); 
                          (0, 5)]; [(1, 1); (1, 2); (1, 3); (1, 4)]];
     make_update_ship_lst_test "Ship List Iter 2" [[]; [(0, 4); (0, 5)]; 
                                      [(1, 1); (1, 2); (1, 3); (1, 4)]] *)

]

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
    ai_functionality_tests
  ]
let _ = run_test_tt_main suite