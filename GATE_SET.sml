signature GATE_SET =
sig
  type gate
  val labelToGate : string * Qubit.qubit List.list -> gate
  val str : gate -> string
  val inverse : gate -> gate
  val support : gate -> Qubit.qubit List.list
  val gate_matrix : gate -> ComplexMatrix.t
  val map_support : gate -> (Qubit.qubit -> Qubit.qubit) -> gate
  val gate_cost : gate -> int
end
