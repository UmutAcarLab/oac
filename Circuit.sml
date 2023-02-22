signature CIRCUIT =
sig
  type gate
  type layer
  type circuit
  val labelToGate : (String.t, int List.list) -> gate
  val eval_raw_sequence : gate Seq.t -> ComplexMatrix.t
  val from_raw_sequence : gate Seq.t -> circuit
  val eval_circuit : circuit -> ComplexMatrix.t
  val depth : circuit -> int
  val load_circuit : string -> circuit
end
