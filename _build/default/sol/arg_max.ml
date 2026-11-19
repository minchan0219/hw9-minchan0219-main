open! Src.Cs17_setup_hw09

let arg_max = function
  | the_first :: the_rest, f ->
      let rec highest = function
        | [], max -> max
        | first :: rest, max ->
            if f first > f max then highest (rest, first)
            else highest (rest, max)
      in
      highest (the_first :: the_rest, f the_first)

let annual_return = function
  | "fuzzbook" -> 6
  | "twotter" -> 3
  | "giggle" -> 1
  | "yourSpace" -> 7
  | _ -> 0
;;

check_expect_string
  ( arg_max ([ "fuzzbook"; "twotter"; "giggle"; "yourSpace" ], annual_return),
    "yourSpace",
    "stocks: end" )
;;

check_expect_string
  ( arg_max ([ "yourSpace"; "twotter"; "giggle"; "fuzzbook" ], annual_return),
    "yourSpace",
    "stocks: start" )
