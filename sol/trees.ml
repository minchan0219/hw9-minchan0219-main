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
let rec helper = function
  | Empty, _, _ -> true
  | Node ((key, _), children), min_bound, max_bound ->
      let check_min =
        match min_bound with None -> true | Some min -> key > min
      in
      let check_max =
        match max_bound with None -> true | Some max -> key < max
      in
      check_min && check_max
      && helper (children.left, Some key, None)
      && helper (children.right, None, Some key)

let check_bst = function tree -> helper (tree, None, None);;

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
  ( check_bst Node
      ( (10, "root"),
        {
          left = Node ((12, "L"), { left = Empty; right = Empty });
          right = Empty;
        } ),
    false,
    "not built correctly" )
