signature MELD_OPT =
sig
  structure Circuit : CIRCUIT
  structure BlackBoxOpt : BLACK_BOX_OPT
  val preprocess : BlackBoxOpt.t -> Circuit.circuit -> Circuit.circuit
  val greedy_optimize : BlackBoxOpt.t -> Circuit.circuit -> Circuit.circuit
  val optimize : BlackBoxOpt.t -> Circuit.circuit -> Circuit.circuit
end

functor VerticalMeldFun (structure BlackBoxOpt : BLACK_BOX_OPT) : MELD_OPT =
struct
  exception Unimplemented

  structure BlackBoxOpt = BlackBoxOpt
  structure Circuit = BlackBoxOpt.Circuit
  structure CLA = CommandLineArgs
  type oracle = Circuit.circuit -> Circuit.circuit option

  fun meld wsz (optfun : oracle) (c1 : Circuit.circuit, c2: Circuit.circuit) =
    if wsz = 0 then Circuit.prepend (c1, c2)
    else let
      fun loop (c1, c2) =
        if Circuit.size c1 = 0 then c2
        else if Circuit.size c2 = 0 then c1
        else let
          val c1 = if CLA.isArg "rl" then Circuit.right_leaning c1 else c1
          val (c1p, c1s) = Circuit.splitEnd c1 (Int.min (wsz, Circuit.size c1))
          val (c2p, c2s) = Circuit.split c2 (Int.min (wsz, Circuit.size c2))
          val cwd = Circuit.prepend (c1s, c2p)
        in
          case (optfun cwd) of
            NONE => Circuit.prepend (c1p, Circuit.prepend (cwd, c2s))
          | SOME cwd' => loop (c1p, loop (cwd', c2s))
        end
    in
      loop (c1, c2)
    end

  (* assume grain > wsz *)
  fun apply_opt_par (wsz, grain) (optfun: oracle) c =
    let
      val meld = meld wsz optfun
      val csize = Circuit.size c
    in
      if csize <= grain then
        case (optfun c) of SOME x => x | NONE => c
      else let
        val (c1, c2) = Circuit.split c (csize div 2)
        val (opc1, opc2) = ForkJoin.par (fn _ => apply_opt_par (wsz, grain) optfun c1, fn _ => apply_opt_par (wsz, grain) optfun c2)
      in
        if wsz <> 0 then meld (opc1, opc2)
        else Circuit.prepend (opc1, opc2)
      end
    end

  fun mapg grain f s =
    ArraySlice.full (SeqBasis.tabulate grain (0, Seq.length s) (f o (Seq.nth s)))

  fun apply_opt_par' (wsz, grain) (optfun: oracle) c =
    let
      fun splits c cl =
        if Circuit.size c < grain then c::cl
        else let
          val (peel, c') = Circuit.split c grain
        in
          splits c' (peel::cl)
        end
      val cl = splits c []
      val cseq = Seq.rev (Seq.fromList cl)
      val cseq_opt = mapg 1 (fn c => case (optfun c) of SOME c' => c' | NONE => c) cseq
      val meld = meld wsz optfun
      val empty_circuit = Circuit.from_raw_sequence (Circuit.num_qubits c, Seq.empty ())
    in
      SeqBasis.reduce 2 meld empty_circuit (0, Seq.length cseq_opt) (Seq.nth cseq_opt)
    end

  fun apply_opt_seq (wsz, grain) (optfun : oracle) c =
    let
      val meld = meld wsz optfun
      fun absorb c1 c2 =
        if Circuit.size c2 = 0 then c1
        else let
          val (c2p, c2s) = Circuit.split c2 (Int.min (grain, Circuit.size c2))
          val _ = print ("c2p size= " ^ (Int.toString (Circuit.size c2p) ^ "\n"))
          val _ = print ("c2s size= " ^ (Int.toString (Circuit.size c2s) ^ "\n"))
        in
          case optfun c2p of
            NONE => absorb (Circuit.prepend (c1, c2p)) c2s
          | SOME c2p' => absorb (meld (c1, c2p')) c2s
        end
    in
      absorb (Circuit.from_raw_sequence (Circuit.num_qubits c, Seq.empty())) c
    end

  fun gvopt bbopt c = BlackBoxOpt.apply_greedy bbopt c
    (* case  of
      NONE => (c, false)
    | SOME c' => (c', )
    let
      fun loop (c, opt) =
        let
          val nl = Circuit.num_layers c
          (* val cut = Int.min (prefix_sz, nl) *)
          (* val (subckt, ctxt) = Circuit.split c cut *)
        in
          case (BlackBoxOpt.apply_greedy bbopt c) of
            NONE => (c, opt)
          | SOME c' => (c', true)
        end
    in
      if prefix_sz = 0 then (c, false)
      else loop (c, false)
    end *)

  fun vopt bbopt timeout c =
    (print ("circuit size = " ^ Int.toString (Circuit.size c) ^ "\n");BlackBoxOpt.apply_all bbopt (c, timeout))
    (* let
      fun loop (c, opt) =
        let
          val nl = Circuit.num_layers c
          val cut = Int.min (prefix_sz, nl)
          val (subckt, ctxt) = Circuit.split c cut
        in
          case (BlackBoxOpt.apply_all bbopt (subckt, timeout), cut = nl) of
            (NONE, _) => (c, opt)
          | (SOME c', _) => (Circuit.prepend (c', ctxt), true)
          (* | (SOME c', false) => loop (Circuit.prepend (c', ctxt), true) *)
        end
    in
      if prefix_sz = 0 then (c, false)
      else loop (c, false)
    end *)

  fun vpreprocess bbopt c =
    SOME (Circuit.from_raw_sequence (BlackBoxOpt.preprocess (Circuit.to_raw_sequence c)))

  fun preprocess bbopt c =
    let
      val _ = print ("size = " ^ (Int.toString (Circuit.size c)) ^ "\n")
      val wsz = 0
      val grain = 200
    in
      apply_opt_par (wsz, grain) (vpreprocess bbopt) c
    end

  fun greedy_optimize bbopt c =
    let
      val nq = Circuit.num_qubits c
      val wsz =  nq * (BlackBoxOpt.max_size bbopt 1)
      val grain = Int.max (100, wsz)
      val _ = print ("size = " ^ (Int.toString (Circuit.size c)) ^ "\n")
    in
      (* apply_opt_seq (wsz, grain) (gvopt bbopt) c *)
      apply_opt_par (wsz, grain) (gvopt bbopt) c
    end

  fun optimize bbopt c =
    let
      val nq = Circuit.num_qubits c
      val wsz =  2 * (BlackBoxOpt.max_size bbopt 1)
      val grain = Int.max (CLA.parseInt "grain" 40, wsz)
      val timeout = CLA.parseInt "timeout" 1
      val _ = print ("size = " ^ (Int.toString (Circuit.size c)) ^ "\n")
    in
      apply_opt_par' (wsz, grain) (vopt bbopt timeout) c
    end

end