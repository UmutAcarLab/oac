
signature CIRCUIT_OPT =
sig
  type gate
  type layer
  type circuit
  val labelToGate : string * Qubit.qubit List.list -> gate
  val eval_raw_sequence : int * gate Seq.t -> ComplexMatrix.t
  val cprint : circuit -> unit
  val from_qasm : string -> circuit
  val eval_circuit : circuit -> ComplexMatrix.t
  val support : circuit -> QSet.t
  val gate : circuit -> layer -> Qubit.qubit -> gate
  val optimize : circuit -> circuit
  val preprocess : circuit -> circuit
  val size : circuit -> int
  val dump : circuit -> string -> unit
end
