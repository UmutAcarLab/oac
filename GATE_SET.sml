signature GATE_SET =
sig
  type gate
  val num_gates : int
  val labelToGate : string * int List.list -> gate
  val str : gate -> string
  val inverse : gate -> gate
  val gate_matrix : gate -> ComplexMatrix.t
end