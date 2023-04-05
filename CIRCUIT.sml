
signature CIRCUIT =
sig
  type gate
  type layer
  type circuit
  type raw_circuit = int * gate Seq.t

  val labelToGate : string * Qubit.qubit List.list -> gate
  val size : circuit -> int
  val eval_raw_sequence : raw_circuit -> ComplexMatrix.t
  val size_raw : raw_circuit -> int

  val from_raw_sequence : raw_circuit -> circuit
  val from_raw_sequence_with_set : QSet.t * gate Seq.t -> circuit
  val from_raw_sequence_with_relabel : raw_circuit * (Qubit.qubit -> Qubit.qubit) -> circuit

  val cprint : circuit -> unit
  val cstring : circuit -> string -> string
  val eval_circuit : circuit -> ComplexMatrix.t
  val support : circuit -> QSet.t

  val num_layers : circuit -> int
  val layer : circuit -> int -> layer

  val num_qubits : circuit -> int

  val split : circuit -> int -> (circuit * circuit)
  val prepend : circuit * circuit -> circuit

  val idx_inverse : circuit -> (int -> Qubit.qubit)

  val gate : circuit -> layer -> Qubit.qubit -> gate
  val is_id : gate -> bool
  val id_gate : Qubit.qubit -> gate
  val gate_support : gate -> Qubit.qubit List.list
  val gate_support_unordered : gate -> QSet.t

  val make_circuit : QSet.t -> (int QMap.map) -> (int) * (int * int -> gate) -> circuit
  val patch_circuit : circuit -> ((Qubit.qubit -> int) * int) -> circuit -> unit
end

