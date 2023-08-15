
signature BLACK_BOX_OPT =
sig
  structure Circuit : CIRCUIT
  type t
  val init : unit -> t
  val apply_greedy : t -> Circuit.circuit * Time.time option -> Circuit.circuit option
  val apply_all : t -> Circuit.circuit * Time.time -> Circuit.circuit option
  val apply_both : t -> Circuit.circuit * Time.time -> Circuit.circuit option
  val best_equivalent : t -> Circuit.circuit -> Circuit.circuit option
  val max_breadth : t -> int
  val max_size : t -> int -> int
  val preprocess : Circuit.raw_circuit -> Circuit.raw_circuit
  val optlog : t -> (Time.time * int) -> string
end
