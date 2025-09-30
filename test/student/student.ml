open OUnit2
open Basics

(* very small tests, just to see if things work at all *)

let test_sanity _ =
  assert_equal 1 1

let test_rev_tup _ =
  assert_equal (2, 1) (rev_tup (1, 2))

let test_rev_triple _ =
  assert_equal (3, 2, 1) (rev_triple (1, 2, 3))

let test_is_odd _ =
  assert_equal true (is_odd 1);
  assert_equal false (is_odd 4)

let test_is_older _ =
  assert_equal true (is_older (2021,1,14) (2021,1,15));
  assert_equal false (is_older (2022,1,1) (2021,12,31))

let test_to_us_format _ =
  assert_equal (1, 15, 2021) (to_us_format (2021, 1, 15))

let test_pow _ =
  assert_equal 9 (pow 3 2)

let test_fac _ =
  assert_equal 120 (fac 5)

let test_get_nth _ =
  assert_equal "b" (get_nth (1, ["a"; "b"; "c"]))

let test_larger _ =
  assert_equal [1;2;3] (larger [1;2;3] [4])

let test_sum _ =
  assert_equal 6 (sum [1;2;3] [])

let suite =
  "student" >::: [
    "sanity" >:: test_sanity;
    "rev_tup" >:: test_rev_tup;
    "rev_triple" >:: test_rev_triple;
    "is_odd" >:: test_is_odd;
    "is_older" >:: test_is_older;
    "to_us_format" >:: test_to_us_format;
    "pow" >:: test_pow;
    "fac" >:: test_fac;
    "get_nth" >:: test_get_nth;
    "larger" >:: test_larger;
    "sum" >:: test_sum;
  ]

let _ = run_test_tt_main suite
