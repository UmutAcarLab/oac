
functor CircuitOPT (structure GateSet : GATE_SET) : CIRCUIT_OPT =
struct
  structure Circuit = CircuitFun (structure GateSet = GateSet)

  structure BlackBoxOpt = BlackBoxOptFun (structure Circuit = Circuit)
  structure MeldOpt = MeldOptFun (structure BlackBoxOpt = BlackBoxOpt)


  open Circuit

  fun from_qasm f =
    let
      val parse_gate = Circuit.labelToGate
      val (nq, raw_str) = ParseQASM.readQASM f
      val raw_gate_seq = Seq.map parse_gate raw_str
    in
      Circuit.from_raw_sequence (nq, raw_gate_seq)
    end

  val bbopt = BlackBoxOpt.init ()
  val optimize = MeldOpt.optimize bbopt
end