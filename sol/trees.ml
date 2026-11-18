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
      Node (
        (key, value),
          {left = Empty;
           right = bad_build_from_sorted rest});;


let rec helper = function
  | Empty,_,_-> true
  | Node ((key, _), children), min_bound, max_bound ->
      let min = children.left 
      
      let max = children.right

let check_bst : ('key, 'value) bst = function
  | tree -> helper (tree, None, None)