module type INTEGER =
sig
  type t
  val zero : t
  val one : t
  val minus_one : t

  val succ : t -> t
  val pred : t -> t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val (mod) : t -> t -> t

  val (land) : t -> t -> t
  val (lor) : t -> t -> t
  val (lxor) : t -> t -> t

  val equal : t -> t -> bool
  val lt : t -> t -> bool

  val of_int : int -> t
  val of_string : string -> t
  val to_string : t -> string
end

(** Implementations **)

module Int : INTEGER =
struct
  type t = int
  let zero = 0
  let one = 1
  let minus_one = -1

  let succ = (+) 1
  let pred = (-) 1

  let add = (+)
  let sub = (-)
  let mul = ( * )
  let div = (/)
  let (mod) = (mod)

  let (land) = (land)
  let (lor) = (lor)
  let (lxor) = (lxor)

  let equal : t -> t -> bool = (=)
  let lt : t -> t -> bool = (<)
  let of_int : t -> t = (fun x -> x)
  let of_string = int_of_string
  let to_string = string_of_int
end

module BigInt : INTEGER =
struct
  type t = Big_int.big_int
  let zero = Big_int.zero_big_int
  let one = Big_int.unit_big_int
  let minus_one = Big_int.minus_big_int one

  let succ = Big_int.succ_big_int
  let pred = Big_int.pred_big_int

  let add = Big_int.add_big_int
  let sub = Big_int.sub_big_int
  let mul = Big_int.mult_big_int
  let div = Big_int.div_big_int
  let (mod) = Big_int.mod_big_int

  let (land) = Big_int.and_big_int
  let (lor) = Big_int.or_big_int
  let (lxor) = Big_int.xor_big_int

  let equal = Big_int.eq_big_int
  let lt = Big_int.lt_big_int

  let of_int = Big_int.big_int_of_int
  let of_string = Big_int.big_int_of_string
  let to_string = Big_int.string_of_big_int
end

module Z : INTEGER = Z
