open! Src.Cs17_setup_hw09

type ('key, 'value) bst =
  | Empty
  | Node of ('key * 'value) * ('key, 'value) children

and ('key, 'value) children = {
  left : ('key, 'value) bst;
  right : ('key, 'value) bst;
}

(* input: an ('a, 'b) bst and a key of type 'a to search for
   output: a 'b option representing the key associated with the 'a value, None 
   the key is not in the bst *)
let rec search = function
  | Empty, _ -> None
  | Node ((k, v), ch), x ->
      if x = k then Some v
      else if x < k then search (ch.left, x)
      else search (ch.right, x)

(* input: a list consisting of key and value tuples of type ('a, 'b) 
   output: a bst resulting from the list
*)
let rec bad_build_from_sorted = function
  | [] -> Empty
  | (key, value) :: rest ->
      Node ((key, value), { left = Empty; right = bad_build_from_sorted rest })

(*input: a ('a, 'b) bst, lower bound, upper bound
    output: a boolean indicating whether or not the bst is in proper order
  *)
let rec helper_1 = function
  | Empty, _, _ -> true
  | Node ((key, _), children), min_bound, max_bound ->
      let check_min =
        match min_bound with None -> true | Some min -> key > min
      in
      let check_max =
        match max_bound with None -> true | Some max -> key < max
      in
      check_min && check_max
      && helper_1 (children.left, None, Some key)
      && helper_1 (children.right, Some key, None)

let check_bst = function tree -> helper_1 (tree, None, None);;

check_expect_bool (check_bst Empty, true, "empty check");;

check_expect_bool
  ( check_bst (bad_build_from_sorted [ (1, "a"); (2, "b"); (3, "c"); (4, "d") ]),
    true,
    " bad_build nodes " )
;;

check_expect_bool
  ( check_bst (Node ((10, "a"), { left = Empty; right = Empty })),
    true,
    "no left and right" )
;;

check_expect_bool
  ( check_bst
      (Node
         ( (10, "root"),
           {
             left = Node ((12, "L"), { left = Empty; right = Empty });
             right = Empty;
           } )),
    false,
    "not built correctly" )
;;

(*input: a sorted list consisting of key and value tuples of type ('a, 'b)
  output: a proper bst resulting from the list*)
let rec build_from_sorted = function
  | [] -> Empty
  | lst -> (
      let mid = List.length lst / 2 in
      let lower = List.take mid lst in
      let higher = List.drop mid lst in
      match higher with
      | [] -> Empty
      | (key, value) :: rest ->
          Node
            ( (key, value),
              { left = build_from_sorted lower; right = build_from_sorted rest }
            ))
;;

check_expect_bool
  ( check_bst (build_from_sorted [ (1, "a") ]),
    true,
    "build_from_sorted: single element" )
;;

check_expect_bool
  (check_bst (build_from_sorted [ (1, "a"); (2, "b") ]), true, "two elements")
;;

check_expect_bool
  (check_bst (build_from_sorted [ (1, "a"); (2, "b"); (3, "c") ]), true, "odd")
;;

check_expect_bool
  ( check_bst (build_from_sorted [ (1, "a"); (2, "b"); (3, "c"); (4, "d") ]),
    true,
    "even" )
;;

check_expect_bool
  ( check_bst (build_from_sorted [ (-3, "a"); (0, "b"); (5, "c") ]),
    true,
    "negative" )
;;

let rec merge = function
  | lst, [] -> lst
  | [], lst2 -> lst2
  | f1 :: r1, f2 :: r2 ->
      if f1 < f2 then f1 :: merge (r1, f2 :: r2) else f2 :: merge (f1 :: r1, r2)

let rec mergesort = function
  | [] -> []
  | [ x ] -> [ x ]
  | lst ->
      let k = List.length lst / 2 in
      merge (mergesort (List.take k lst), mergesort (List.drop k lst))

(*input: an unsorted list consisting of key and value tuples of type ('a, 'b)
  output: a proper bst resulting from sorting the list and constructing a tree*)
let build = function [] -> Empty | lst -> build_from_sorted (mergesort lst);;

check_expect_bool (check_bst (build []), true, "empty list");;

check_expect_bool
  (check_bst (build [ (3, "c"); (1, "a"); (2, "b") ]), true, "unsorted list")
;;

check_expect_bool
  ( check_bst (build [ (5, "e"); (1, "a"); (4, "d"); (2, "b"); (3, "c") ]),
    true,
    "odd" )
;;

check_expect_bool
  ( check_bst
      (build [ (6, "f"); (2, "b"); (5, "e"); (1, "a"); (4, "d"); (3, "c") ]),
    true,
    "even" )
