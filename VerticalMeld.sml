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
    let
      fun loop (c1, c2) =
        let
          val c1 = if CLA.isArg "rl" then Circuit.right_leaning c1 else c1
          val (c1p, c1s) = Circuit.splitEnd' c1 (Int.min (wsz, Circuit.num_layers c1))
          val (c2p, c2s) = Circuit.split' c2 (Int.min (wsz, Circuit.num_layers c2))
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
      val num_layers = Circuit.num_layers c
    in
      if num_layers <= grain then
        case (optfun c) of SOME x => x | NONE => c
      else let
        val (c1, c2) = Circuit.split' c (num_layers div 2)
        val (opc1, opc2) = ForkJoin.par (fn _ => apply_opt_par (wsz, grain) optfun c1, fn _ => apply_opt_par (wsz, grain) optfun c2)
      in
        if wsz <> 0 then meld (opc1, opc2)
        else Circuit.prepend (opc1, opc2)
      end
    end

  fun apply_opt_seq (wsz, grain) (optfun : oracle) c =
    let
      val meld = meld wsz optfun
      fun absorb c1 c2 =
        if Circuit.size c2 = 0 then c1
        else let
          val (c2p, c2s) = Circuit.split' c2 (Int.min (grain, Circuit.num_layers c2))
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
          (* val (subckt, ctxt) = Circuit.split' c cut *)
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
    (print ("num_layers = " ^ Int.toString (Circuit.num_layers c) ^ "\n");BlackBoxOpt.apply_all bbopt (c, timeout))
    (* let
      fun loop (c, opt) =
        let
          val nl = Circuit.num_layers c
          val cut = Int.min (prefix_sz, nl)
          val (subckt, ctxt) = Circuit.split' c cut
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
      val _ = print ("num_layers = " ^ (Int.toString (Circuit.num_layers c)) ^ "\n")
      val wsz = 0
      (* val grain = 4 * (CLA.parseInt "grain" 200) *)
      val grain = 200
    in
      apply_opt_par (wsz, grain) (vpreprocess bbopt) c
    end

  fun greedy_optimize bbopt c =
    let
      val _ = print ("num_layers = " ^ (Int.toString (Circuit.num_layers c)) ^ "\n")
      val wsz = BlackBoxOpt.max_size bbopt 1
      val grain = CLA.parseInt "grain" 200
    in
      apply_opt_seq (wsz, grain) (gvopt bbopt) c
      (* apply_opt_par (wsz, grain) (gvopt bbopt) c *)
    end

  fun optimize bbopt c =
    let
      val _ = print ("num_layers = " ^ (Int.toString (Circuit.num_layers c)) ^ "\n")
      val wsz = BlackBoxOpt.max_size bbopt 1
      val timeout = CLA.parseInt "timeout" 1
      val grain = (CLA.parseInt "grain" 200)
    in
      apply_opt_par (wsz, grain) (vopt bbopt timeout) c
    end

end