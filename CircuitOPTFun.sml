
functor CircuitOPT (structure GateSet : GATE_SET) : CIRCUIT_OPT =
struct
  structure Circuit = CircuitFun (structure GateSet = GateSet)

  structure BlackBoxOpt = QuartzBB (structure Circuit = Circuit)
  structure MeldOpt = MeldOptFun (structure BlackBoxOpt = BlackBoxOpt)

  open Circuit
  val bbopt = BlackBoxOpt.init ()

  fun from_qasm f =
    let
      val rawc = Circuit.from_qasm (ReadFile.contentsSeq f)
      val rawc' = BlackBoxOpt.preprocess rawc
    in
      Circuit.from_raw_sequence rawc'
    end

  val optimize = MeldOpt.optimize bbopt
end