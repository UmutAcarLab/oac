
functor CircuitFun (structure GateSet : GATE_SET) : CIRCUIT =
struct
  exception Unimplemented

  type gate = GateSet.gate
  type layer = gate Seq.t
  type circuit = (int * layer Seq.t)

  val labelToGate = GateSet.labelToGate

  fun layer (_, c) x = Seq.nth c x
  fun init_layer n = ArraySlice.full (ForkJoin.alloc n)
  fun add_to_layer l i g = ArraySlice.update (l, i, g)

  fun from_layers ll n = (n, Seq.fromList ll)

  fun gate l q = Seq.nth l q

  fun size (c : circuit) = Seq.length (#2 c)

  fun support (c : circuit) = #1 c

  fun depth _ = raise Unimplemented

  fun eval_circuit _ = raise Unimplemented

  fun from_raw_sequence _ = raise Unimplemented


  fun load_circuit f =
    let
      val (sgc, circuit) = ParseQASM.readQASM f
    in
      Seq.map GateSet.labelToGate circuit
    end

  fun eval_raw_sequence s = raise Unimplemented
end


functor CircuitOPT (structure GateSet : GATE_SET) : CIRCUIT_OPT =
struct
  structure Circuit = CircuitFun (structure GateSet = GateSet)

  structure BlackBoxOpt = BlackBoxOptFun (structure Circuit = Circuit)
  structure MeldOpt = MeldOptFun (structure BlackBoxOpt = BlackBoxOpt)

  open Circuit

  val bbopt = BlackBoxOpt.init ()
  val optimize = MeldOpt.optimize bbopt

end
