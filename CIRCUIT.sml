
signature CIRCUIT =
sig
  type gate
  type layer
  type circuit
  type raw_circuit = int * gate Seq.t

  val labelToGate : string * int List.list -> gate
  val size : circuit -> int
  val eval_raw_sequence : raw_circuit -> ComplexMatrix.t
  val size_raw : raw_circuit -> int

  val from_raw_sequence : raw_circuit -> circuit
  val from_raw_sequence_with_set : QSet.t * gate Seq.t -> circuit
  val from_raw_sequence_with_idx : raw_circuit * (int -> int) -> circuit

  val cprint : circuit -> unit
  val cstring : circuit -> string -> string
  val eval_circuit : circuit -> ComplexMatrix.t
  val support : circuit -> QSet.t

  val num_layers : circuit -> int
  val layer : circuit -> int -> layer

  val num_qubits : circuit -> int

  val split : circuit -> int -> (circuit * circuit)
  val prepend : circuit * circuit -> circuit

  val reverse_idx : circuit -> (int -> int)

  val gate : circuit -> layer -> int -> gate
  val is_id : gate -> bool
  val id_gate : int -> gate
  val gate_support : gate -> QSet.t

  val make_circuit : QSet.t -> (int Seq.t) -> (int) * (int * int -> gate) -> circuit
  val patch_circuit : circuit -> ((int -> int) * int) -> circuit -> unit
end

