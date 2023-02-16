(* signature CIRCUIT =
sig
  type gate
  type layer
  type circuit
  val labelToGate : string -> gate
  val eval_raw_sequence : gate Seq.t -> ComplexMatrix.t
  val from_raw_sequence : gate Seq.t -> circuit
  val eval_mat : circuit -> ComplexMatrix.t
  val depth : circuit -> int
end *)


signature BLACK_BOX_OPT =
sig
  structure Circuit : CIRCUIT
  type t
  val init : unit -> t
  val best_equivalent : t -> Circuit.circuit -> Circuit.circuit option
  val max_breadth : t -> int
  val max_depth : t -> int -> int
end
