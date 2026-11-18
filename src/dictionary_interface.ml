module type Dictionary = sig
  type ('key, 'value) dict
  val lookup: ('key, 'value) dict * 'key -> 'value option
  val build: ('key * 'value) list -> ('key, 'value) dict
end