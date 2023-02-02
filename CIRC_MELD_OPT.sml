signature CIRC_MELD_OPT =
sig
  type circuit
  val init : string Seq.t -> circuit
  val optimize : GateSet.t ->circuit -> circuit
end

