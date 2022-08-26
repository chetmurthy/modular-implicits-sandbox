(* camlp5o *)
(* core_test.ml *)

open OUnit2

let test_simple ctxt =
  ()
;;

let suite = "core_opps" >::: [
      "test_simple"        >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
