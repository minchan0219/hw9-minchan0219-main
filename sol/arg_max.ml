open! Src.Cs17_setup_hw09

(*input: a tuple pair (list, f) where list is a non-empty 'a list and 
         f is of 'a -> int
  output:an element of list which the f of the element is the greatest*)
let arg_max = function
  | the_first :: the_rest, f ->
      let rec highest = function
        | [], max, _ -> max
        | first :: rest, max, maximum ->
            let first_val = f first in
            if first_val > maximum then highest (rest, first, first_val)
            else highest (rest, max, maximum)
      in
      highest (the_rest, the_first, f the_first)

let annual_return = function
  | "fuzzbook" -> 6
  | "twotter" -> 3
  | "giggle" -> 1
  | "yourSpace" -> 7
  | _ -> 0
;;

check_expect_int (arg_max ([ 42 ], fun x -> x * 2), 42, "single");;
check_expect_int (arg_max ([ -99 ], fun x -> x), -99, "single negative");;

check_expect_int
  ( annual_return
      (arg_max
         ([ "fuzzbook"; "twotter"; "giggle"; "yourSpace" ], annual_return)),
    7,
    "end" )
;;

check_expect_int
  ( annual_return
      (arg_max
         ([ "yourSpace"; "twotter"; "giggle"; "fuzzbook" ], annual_return)),
    7,
    "start" )
;;

check_expect_int (arg_max ([ 5; 2; 9; 4 ], fun x -> -x), 2, "negative function")
;;
check_expect_int (arg_max ([ -7; -3; -10 ], fun x -> x), -3, "negatives");;

check_expect_int
  (arg_max ([ 1; 5; 3; 5; 2 ], fun x -> x), 5, "duplicate max elements")
