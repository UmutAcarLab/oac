
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
  val from_qasm : char Seq.t -> raw_circuit
  val raw_to_qasm : raw_circuit -> string

  val cprint : circuit -> unit
  val cstring : circuit -> string -> string
  val to_qasm : circuit -> string
  val to_raw_sequence : circuit -> raw_circuit

  val eval_circuit : circuit -> ComplexMatrix.t
  val right_leaning : circuit -> circuit
  val support : circuit -> QSet.t

  val num_layers : circuit -> int
  val layer : circuit -> int -> layer

  val num_qubits : circuit -> int

  val split : circuit -> int -> (circuit * circuit)
  val splitEnd : circuit -> int -> (circuit * circuit)
  val prepend : circuit * circuit -> circuit

  val idx_inverse : circuit -> (int -> Qubit.qubit)

  val gate : circuit -> layer -> Qubit.qubit -> gate
  val is_id : gate -> bool
  val id_gate : Qubit.qubit -> gate
  val gate_str : gate -> string
  val gate_support : gate -> Qubit.qubit List.list
  val gate_support_unordered : gate -> QSet.t

  val make_circuit : QSet.t -> (int QMap.map) -> (int) * (int * int -> gate) -> circuit
  val patch_circuit : circuit -> ((Qubit.qubit -> int) * int) -> circuit -> unit


  val raw_str : raw_circuit -> string -> string
  (* relabels the qubits of the raw_circuit to the qubits in the second circuit.*)
  val reindex : raw_circuit * circuit -> circuit

  val dump : circuit -> string -> unit
end

