
signature BLACK_BOX_OPT =
sig
  structure Circuit : CIRCUIT
  type t
  val init : unit -> t
  val best_equivalent : t -> Circuit.circuit -> Circuit.circuit option
  val max_breadth : t -> int
  val max_size : t -> int -> int
  val preprocess : Circuit.raw_circuit -> Circuit.raw_circuit
end
