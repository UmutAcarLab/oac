signature MELD_OPT =
sig
  structure Circuit : CIRCUIT
  structure BlackBoxOpt : BLACK_BOX_OPT
  type circuit = Circuit.circuit
  val preprocess : BlackBoxOpt.t -> circuit -> circuit
  val greedy_optimize : BlackBoxOpt.t -> circuit -> Time.time -> circuit
  val search : BlackBoxOpt.t -> circuit -> Time.time -> circuit
  val combined_opt : BlackBoxOpt.t -> circuit -> real -> Time.time -> circuit
end

functor VerticalMeldFun (structure BlackBoxOpt : BLACK_BOX_OPT) : MELD_OPT =
struct
  exception Unimplemented
  exception BadSequence

  structure BlackBoxOpt = BlackBoxOpt
  structure Circuit = BlackBoxOpt.Circuit
  structure CLA = CommandLineArgs
  type circuit = Circuit.circuit
  type oracle = (circuit * Time.time) -> circuit option
  (* Unfortunately, Time.fromReal (Real.posInf) overflows *)
  (* So, I am assuming we will not run this code for longer than three hundred years, it is configurable :=) *)
  val maxTime = Time.fromReal (10000000000.0)

  datatype param = P of {wsz: int, grain: int, wdtime : Time.time option, total : Time.time}
  val str_time = Real.toString o Time.toReal

  fun tabulateg grain f n = ArraySlice.full (SeqBasis.tabulate grain (0, n) f)
  fun mapg grain f s = tabulateg grain (f o (Seq.nth s)) (Seq.length s)

  (* creates a sequence of size n, using f (i) if there is budget, and q (i) when there isn't *)
  fun timed_tabulate (costf : Time.time, budget : Time.time) (f : int -> 'a, q : int -> 'a) (n : int) : 'a Seq.t =
    let
      val result = ForkJoin.alloc n
      fun loop (idx, b) =
        if idx = n then ArraySlice.full result
        else if Time.< (b, Time.zeroTime) then (Array.update (result, idx, q (idx)); loop (idx + 1, b))
        (* else if Time.< (b, costf) then (Array.update (result, idx, q (idx)); loop (idx + 1, b)) *)
        else let
          val (res, tm) = Util.getTime (fn _ => f (idx))
          val _ = Array.update (result, idx, res)
        in
          loop (idx + 1, Time.- (b, tm))
        end
    in
      loop (0, budget)
    end
  fun timed_map (costf, budget) (f, q) s =
    timed_tabulate (costf, budget) (f o (Seq.nth s), q o (Seq.nth s)) (Seq.length s)

  fun seq_to_circuit cseq =
    let
      val nq =
        if (Seq.length cseq = 0) then raise BadSequence
        else Circuit.num_qubits (Seq.nth cseq 0)
      val b = (Circuit.from_raw_sequence (nq, Seq.empty()))
    in
      Seq.reduce (Circuit.prepend) b cseq
    end

  datatype step = OPT of circuit | MELD of {prefix : circuit, window : circuit, suffix : circuit}
  fun stepMeld wsz (optfun : circuit -> circuit option) (c1: circuit, c2: circuit) =
    if wsz = 0 then OPT (Circuit.prepend (c1, c2))
    else if (Circuit.size c1 = 0) then OPT (c2)
    else if (Circuit.size c2 = 0) then OPT (c1)
    else let
      val c1 = if CLA.isArg "rl" then Circuit.right_leaning c1 else c1
      val (c1p, c1s) = Circuit.splitEnd c1 (Int.min (wsz, Circuit.size c1))
      val (c2p, c2s) = Circuit.split c2 (Int.min (wsz, Circuit.size c2))
      val cwd = Circuit.prepend (c1s, c2p)
    in
      case (optfun cwd) of
        NONE => OPT (Circuit.prepend (c1p, Circuit.prepend (cwd, c2s)))
      | SOME cwd' => MELD ({prefix = c1p, window = cwd', suffix = c2s})
    end

  fun meldSeq (wsz, wt, total) (optfun : oracle) cseq =
    let
      val to = fn c => optfun (c, wt)
      fun optstep (c1: circuit, c2 : circuit) : circuit Seq.t =
        case stepMeld wsz to (c1, c2) of
          OPT c => Seq.fromList [c]
        | MELD {prefix, window, suffix} => Seq.fromList [prefix, window, suffix]
      val quickstep = (fn (c1, c2) =>  Seq.fromList ([Circuit.prepend (c1, c2)]))

      fun loop (cseq, tm) =
        if (Seq.length cseq = 1) orelse (Time.< (tm, Time.zeroTime)) then cseq
        else let
          val n = Seq.length cseq
          val argsi = fn i => (Seq.nth cseq (2*i), Seq.nth cseq (2*i + 1))
          val (stepseq, spent) = Util.getTime (fn _ => timed_tabulate (wt, tm) (optstep o argsi, quickstep o argsi) (n div 2))
          val cseq' =
            if (n mod 2) = 0 then Seq.flatten stepseq
            else Seq.append (Seq.flatten stepseq, Seq.fromList ([Seq.nth cseq (n - 1)]))
        in
          loop (cseq', Time.- (tm, spent))
        end
    in
      seq_to_circuit (loop (cseq, total))
    end

  fun meld wsz (optfun : oracle) (c1 : circuit, c2: circuit) = meldSeq wsz optfun (Seq.fromList [c1, c2])

  fun apply_opt_seq repeat (P {wsz, grain, wdtime, total}) (optfun: oracle) c =
    let
      fun splits c cl =
        if Circuit.size c <= grain then c::cl
        else let
          val (peel, c') = Circuit.split c grain
        in
          splits c' (peel::cl)
        end
      val cl = splits c []
      val cseq = Seq.rev (Seq.fromList cl)
      (* TODO: maybe use different window times for leaves and meld *)
      val wt = case wdtime of
          SOME wd => wd
        | NONE => Time.fromReal (Real.max (1.0, Real./ ((Time.toReal total), Real.fromInt (2 * (Seq.length cseq)))))
      val _ = print ("ttals = " ^ (str_time total) ^ "\n")
      val (cseq_opt, spent) = Util.getTime (fn _ => timed_map (wt, total) (fn c => case (optfun (c, wt)) of SOME c' => c' | NONE => c, fn c => c) cseq)
      val rem = Time.- (total, spent)
      (* val _ = print ("rem = " ^ (str_time rem) ^ "\n") *)
      val (copt, spent) = Util.getTime (fn _ => meldSeq (wsz, wt, rem) optfun cseq_opt)
      val rem = Time.- (rem, spent)
      (* calculate new window time wt', s.t., wt' >= 2 * wt.
       * I take the time spent with wt, and estimate wt' such that
       * all the remaining time will be spent in the recursive call.
       * To estimate, I guess that time spent scales linearly to wt.
      *)
      (* val _ = print ("rem = " ^ (str_time rem) ^ "\n") *)

      val wt' =
        let
          val tspent = Time.toReal (Time.- (total, rem))
          val wtr = Time.toReal wt
          val remr = Time.toReal (rem)
          val wtc = Real./ (Real.* (remr, wtr), 2.0 * tspent)
        in
          Time.fromReal (Real.max (2.0 * wtr, wtc))
        end
      val _ = print ("rem = " ^ (str_time rem) ^ " wt' = " ^ (str_time wt') ^ "\n")
    in
      if not (repeat) orelse Time.< (rem, wt') then copt
      else apply_opt_seq repeat (P {wsz = wsz, grain = grain, wdtime = SOME wt', total = rem}) optfun copt
    end

  fun apply_opt_seq' (P {wsz, grain, wdtime, total}) (optfun : oracle) c =
    let
      fun scale_time t k = Time.fromReal (Time.toReal t * (Real.fromInt k))
      val wdtime = Option.valOf wdtime
      val meld_time = scale_time wdtime 2
      val moptfun = fn c => optfun(c, meld_time)
      val grain_time = scale_time wdtime (Int.div(grain, wsz))
      val goptfun = fn c => optfun(c, grain_time)
      val start = Time.now()

      fun check_time t = Time.<= (Time.- (Time.now (), start), Time.- (total, t))

      fun meld_seq (c1, c2) =
        if check_time (meld_time) then
          case stepMeld wsz moptfun (c1, c2) of
            OPT c => c
          | MELD {prefix, window, suffix} => meld_seq (meld_seq (prefix, window), suffix)
        else Circuit.prepend (c1, c2)

      fun absorb c1 c2  =
        if not(check_time (grain_time)) then Circuit.prepend(c1, c2)
        else if Circuit.size c2 = 0 then c1
        else let
          val (c2p, c2s) = Circuit.split c2 (Int.min (grain, Circuit.size c2))
        in
          case goptfun c2p of
            NONE => absorb (Circuit.prepend (c1, c2p)) c2s
          | SOME c2p' => absorb (meld_seq (c1, c2p')) c2s
        end
    in
      absorb (Circuit.from_raw_sequence (Circuit.num_qubits c, Seq.empty())) c
    end

  fun apply_opt_flat (P {wsz, grain, wdtime, total}) (optfun : oracle) c =
    let
      fun scale_time t k = Time.fromReal (Time.toReal t * (Real.fromInt k))
      val wdtime = Option.valOf wdtime
      val meld_time = scale_time wdtime 2
      val moptfun = fn c => optfun(c, meld_time)
      val grain_time = scale_time wdtime (Int.div(grain, wsz))
      val goptfun = fn c => optfun(c, grain_time)
      val start = Time.now()

      fun splits c cl =
        if Circuit.size c <= grain then c::cl
        else let
          val (peel, c') = Circuit.split c grain
        in
          splits c' (peel::cl)
        end
      val cl = splits c []
      val cseq = Seq.rev (Seq.fromList cl)
      val (cseq_opt, spent) = Util.getTime (fn _ => Seq.map (fn c => case (goptfun c) of SOME c' => c' | NONE => c) cseq)


      fun check_time t = Time.<= (Time.- (Time.now (), start), Time.- (total, t))

      fun meld_step (c1, c2) =
        if check_time (meld_time) then
          case stepMeld wsz moptfun (c1, c2) of
            OPT c => Seq.fromList [c]
          | MELD {prefix, window, suffix} => Seq.fromList [prefix, window, suffix]
        else Seq.fromList [Circuit.prepend (c1, c2)]
      val _ = print ("time taken in flt pahse = "  ^ ((Real.toString (Time.toReal (Time.- (Time.now (), start))))) ^ "\n")
      val size =
        let
          fun loop sz (i, cseq) =
            if i = Seq.length cseq then sz
            else loop (sz + Circuit.size (Seq.nth cseq i)) (i + 1, cseq)
        in
          loop 0 (0, cseq_opt)
        end
      val _ = print ("size after flt pahse = "  ^ (Int.toString size) ^ "\n")

      fun loop (cseq : circuit Seq.t, numMelds : int, numLoops : int) =
        if (Seq.length cseq = 1) then (Seq.nth cseq 0, numMelds, numLoops)
        else let
          val n = Seq.length cseq
          val _ = print ("cseq lenght = " ^ (Int.toString n) ^ "\n")
          val argsi = fn i => (Seq.nth cseq (2*i), Seq.nth cseq (2*i + 1))
          val stepseq = tabulateg 1 (fn i => meld_step (argsi i)) (n div 2)
          val cseq' =
            if (n mod 2) = 0 then Seq.flatten stepseq
            else Seq.append (Seq.flatten stepseq, Seq.fromList ([Seq.nth cseq (n - 1)]))
        in
          loop (cseq', numMelds + (n div 2), numLoops + 1)
        end
      val (copt, numMelds, numLoops) = loop (cseq_opt, 0, 0)
      val _ = print ("num loops = " ^ (Int.toString numLoops) ^ " num melds = " ^ (Int.toString numMelds) ^ "\n")
      (*
      fun absorb c1 c2  =
        if not(check_time (grain_time)) then Circuit.prepend(c1, c2)
        else if Circuit.size c2 = 0 then c1
        else let
          val (c2p, c2s) = Circuit.split c2 (Int.min (grain, Circuit.size c2))
        in
          case goptfun c2p of
            NONE => absorb (Circuit.prepend (c1, c2p)) c2s
          | SOME c2p' => absorb (meld_seq (c1, c2p')) c2s
        end *)
    in
      (* meld_loop (Circuit.from_raw_sequence (Circuit.num_qubits c, Seq.empty())) (0, cseq_opt) *)
      copt
    end


  fun gvopt bbopt (c, t) = BlackBoxOpt.apply_greedy bbopt (c, NONE)

  fun vopt bbopt (c, t) =
    (print ("circuit size = " ^ Int.toString (Circuit.size c) ^ "\n");
      print ("timeout given = " ^ (Real.toString (Time.toReal t)) ^ "\n");
      BlackBoxOpt.apply_all bbopt (c, t))

  fun vpreprocess bbopt (c, t) =
    SOME (Circuit.from_raw_sequence (BlackBoxOpt.preprocess (Circuit.to_raw_sequence c)))

  fun preprocess bbopt c =
    let
      val _ = print ("size = " ^ (Int.toString (Circuit.size c)) ^ "\n")
    in
      apply_opt_seq false (P {wsz = 0, grain = Circuit.size c, wdtime = NONE, total = maxTime}) (vpreprocess bbopt) c
    end

  fun greedy_optimize bbopt c timeout =
    let
      val nq = Circuit.num_qubits c
      val wsz = nq * (CLA.parseInt "size" 6)
      val grain = CLA.parseInt "grain" (5 * wsz)
      val _ = print ("size = " ^ (Int.toString (Circuit.size c)) ^ "\n")
      val nc =  apply_opt_seq false (P {wsz = wsz, grain = grain, wdtime = NONE, total = timeout}) (gvopt bbopt) c
    in
      (* apply_opt_seq (wsz, grain) (gvopt bbopt) c *)
      nc
    end

  fun search bbopt c timeout =
    let
      val nq = Circuit.num_qubits c
      val wsz =  Int.min (nq * (BlackBoxOpt.max_size bbopt 1), 20)
      (* val wsz = nq * (CLA.parseInt "size" 6) *)
      val grain = CLA.parseInt "grain" (2 * wsz)
      val wt = SOME (Time.fromReal (Real.fromInt (CLA.parseInt "wt" 1)))
      val _ = print ("window size = " ^ (Int.toString wsz) ^ " grain = " ^ (Int.toString grain) ^ "\n")
      val _ = print ("size = " ^ (Int.toString (Circuit.size c)) ^ "\n")
    in
      apply_opt_seq true (P {wsz = wsz, grain = grain, wdtime = wt, total = timeout}) (vopt bbopt) c
    end

  fun gcvopt bbopt (c, t) = BlackBoxOpt.apply_greedy bbopt (c, SOME t)
  fun cvopt bbopt (c, t) = BlackBoxOpt.apply_all bbopt (c, t)

  fun combined_opt bbopt c (gt : real) timeout =
    let
      val nq = Circuit.num_qubits c
      val wsz = nq * (BlackBoxOpt.max_size bbopt 1)
      val grain =
        let
          val gi = CLA.parseInt "grain" (4 * wsz)
        in
          if gi >= 1600 then (1 + 1000 div wsz) * wsz
          else gi
        end
      val wt = SOME (Time.fromReal (gt * (Real.fromInt wsz)))
      val (c', tm) =
        Util.getTime (fn _ => apply_opt_seq' (P {wsz = wsz, grain = grain, wdtime = wt, total = timeout}) (gcvopt bbopt) c)
      val _ = print ("greedy done opts = " ^ (Int.toString(Circuit.size c' - Circuit.size c) ^ "\n"))
      val gtspent = Real./ (Time.toReal tm, Real.fromInt (Circuit.size c))
      val gt = Real.- (gt, gtspent)
      val wsz =  Int.min (nq * (BlackBoxOpt.max_size bbopt 1), 20)
      val _ = print ("total time (per gate) remaining = " ^ (Real.toString gt) ^ "\n")
      val grain = CLA.parseInt "grain" (2 * wsz)
      val wtr = (gt * (Real.fromInt wsz))
      val c'' =
        if wtr >= 1.0 then
          apply_opt_seq'
            (P {wsz = wsz, grain = grain, wdtime = SOME (Time.fromReal wtr), total = Time.-(timeout, tm)})
            (cvopt bbopt) c'
        else c'
    in
      c''
    end

end


