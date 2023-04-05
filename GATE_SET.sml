signature GATE_SET =
sig
  type gate
  val num_gates : int
  val labelToGate : string * Qubit.qubit List.list -> gate
  val str : gate -> string
  val inverse : gate -> gate
  val support : gate -> Qubit.qubit List.list
  val gate_matrix : gate -> ComplexMatrix.t
  val map_support : gate -> (Qubit.qubit -> Qubit.qubit) -> gate
end
