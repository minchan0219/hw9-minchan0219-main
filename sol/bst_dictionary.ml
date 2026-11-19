open! Src.Cs17_setup_hw09
open Trees
open Src.Dictionary_interface

module Dictionaried : Dictionary = struct
  type ('key, 'value) dict = ('key, 'value) Trees.bst

  let lookup = function tree, key -> Trees.search (tree, key)
  let build = function pairs -> Trees.build pairs
end
