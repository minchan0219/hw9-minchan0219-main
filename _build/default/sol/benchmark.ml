(* open! Trees;;

   (* 1. Generate a list of length n consisting of (int * float) pairs *)
   let rec generate_list = function
   | 0 -> []
   | n -> (Random.int (n * 10), Random.float 100.0) :: generate_list (n - 1)

   (* 2. Perform k lookups in the BST using keys from lst and report average time *)
   let benchmark_lookups = function (lst, tree, k) ->
   let rec run = function
    | (0, _) -> ()
    | (remaining, []) -> run (remaining, lst)
    | (remaining, (key, _) :: rest) ->
      ignore (search (tree, key));
      run (remaining - 1, rest)
   in
   let t_start = Sys.time () in
   run (k, lst);
   let t_end = Sys.time () in
   let avg_time = (t_end -. t_start) /. float_of_int k in
   Printf.printf "Average time per lookup: %f seconds\n" avg_time;
   avg_time

   (* 3. Build both trees from the same data and compare lookup times *)
   let compare_builders = function (n, k) ->
   let lst = generate_list n in
   let sorted = mergesort lst in
   let bad_tree = bad_build_from_sorted sorted in
   let good_tree = build_from_sorted sorted in
   Printf.printf "n = %d, %d lookups\n" n k;
   print_string "  bad_build_from_sorted:  ";
   ignore (benchmark_lookups (lst, bad_tree, k));
   print_string "  build_from_sorted:      ";
   ignore (benchmark_lookups (lst, good_tree, k))
   ;;

   Random.init 17;;
   compare_builders (20000, 10000);; *)