
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

  fun gen_bench () =
    let
      val (nq, raw_optc) = Circuit.from_qasm (ReadFile.contentsSeq "out_unopt.qasm")
      val cseq = Seq.tabulate (fn i => raw_optc) (CommandLineArgs.parseInt "rep" 2)
      val cred = Seq.flatten cseq
    in
      Circuit.dump (Circuit.from_raw_sequence (nq, cred)) "out.qasm"
    end

  (* val _ = gen_bench() *)
end