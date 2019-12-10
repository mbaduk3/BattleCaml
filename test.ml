open OUnit2
open Ai_hard
open Ai_medium
open Ai_easy
open Gameboard

let make_thd_test
  (name : string)
  (expected_output : ('c))
  (input : 'a * 'b * 'c * 'd) : test = 
    name >:: fun _ ->
    assert_equal expected_output (thd input)

let make_ge_miss_coord_test
  (name : string)
  (expected_output : int * int)
  (input : (int * int) list ref) : test = 
    name >:: fun _ ->
    assert_equal expected_output (get_empty_coord input)

let make_create_vertical_lst_test
  (name : string)
  (expected_output : ('a * int) list ref)
  (input1 : 'a * int * int * 'b)
  (input2 : ('a * int) list)
  (input3 : int) : test = 
    name >:: fun _ ->
    assert_equal expected_output (create_vertical_lst input1 input2 input3)

let make_create_horizontal_test
  (name : string)
  (expected_output : (int * 'a) list ref)
  (input1 : int * 'a * int * 'b)
  (input2 : (int * 'a) list)
  (input3 : int) : test = 
    name >:: fun _ ->
    assert_equal expected_output (create_horizontal_lst input1 input2 input3)

let make_change_ship_index_if_empty_test
  (name : string)
  (expected_output : int) : test = 
    name >:: fun _ ->
    assert_equal expected_output (change_ship_index_if_empty ())

let make_get_all_empty_coords_test
  (name : string)
  (expected_output : (int * int) list)
  (input : Gameboard.entry array array) : test = 
    name >:: fun _ ->
    assert_equal expected_output (!(get_all_empty_coords input))

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
    assert_equal expected_output (cartesian_product input1 input2)
