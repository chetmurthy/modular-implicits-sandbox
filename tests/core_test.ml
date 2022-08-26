(* camlp5o *)
(* core_test.ml *)

open OUnit2
open Core_ops

let test_simple ctxt =
  assert_equal (-1.) (neg 1.)
  ; assert_equal (-1) (neg 1)
  ; assert_equal Complex.(neg one) (neg Complex.one)
  ; assert_equal 2 (1 + 1)
  ; assert_equal 2. (1. + 1.)
  ; assert_equal Complex.one (Complex.one + Complex.zero)
;;

let suite = "core_opps" >::: [
      "test_simple"        >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
