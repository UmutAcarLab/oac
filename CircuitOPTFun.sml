
functor CircuitOPT (structure GateSet : GATE_SET) : CIRCUIT_OPT =
struct
  structure Circuit = CircuitFun (structure GateSet = GateSet)

  structure BlackBoxOpt = PyzxBB (structure Circuit = Circuit)
  structure MeldOpt = VerticalMeldFun (structure BlackBoxOpt = BlackBoxOpt)

  open Circuit
  val bbopt = BlackBoxOpt.init ()

  fun from_qasm f =
    let
      val cs = run "reading file" (fn _ => ReadFile.contentsSeq f)
      val rawc = run "parsing" (fn _ => Circuit.from_qasm (cs))
    in
      Circuit.from_raw_sequence rawc
    end


  val preprocess = MeldOpt.preprocess bbopt
  val greedy_optimize = MeldOpt.greedy_optimize bbopt
  val search = MeldOpt.search bbopt
  val combined_opt = MeldOpt.combined_opt bbopt

  val optlog = BlackBoxOpt.optlog bbopt

  fun gen_bench () =
    let
      val (nq, raw_optc) = Circuit.from_qasm (ReadFile.contentsSeq "out_unopt.qasm")
      val cseq = Seq.tabulate (fn i => raw_optc) (CommandLineArgs.parseInt "rep" 2)
      val cred = Seq.flatten cseq
    in
      Circuit.dump (Circuit.from_raw_sequence (nq, cred)) "out.qasm"
    end

  fun compress (c : circuit) =
    let
      val c' = from_raw_sequence (to_raw_sequence c)
    in
      c'
    end

end
