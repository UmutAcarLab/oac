
functor CircuitOPT (structure GateSet : GATE_SET) : CIRCUIT_OPT =
struct
  structure Circuit = CircuitFun (structure GateSet = GateSet)

  structure BlackBoxOpt = BlackBoxOptFun (structure Circuit = Circuit)
  structure MeldOpt = MeldOptFun (structure BlackBoxOpt = BlackBoxOpt)

  open Circuit

  val bbopt = BlackBoxOpt.init ()
  val optimize = MeldOpt.optimize bbopt

end