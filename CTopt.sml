structure CLA = CommandLineArgs



structure CliffordT =
struct
  open Math
  val (cos45, sin45) = (sqrt(0.5), sqrt(0.5))
  val x = pi/(8.0)
  val H = ComplexMatrix.fromList [[( 0.0, cos45), (0.0, sin45)], [(0.0, cos45), (0.0, ~sin45)]]
  val S = ComplexMatrix.fromList [[( cos45, ~sin45), (  0.0, 0.0)], [(  0.0, 0.0), ( cos45,  sin45)]]
  val T = ComplexMatrix.fromList [[(cos(x), ~(sin(x))), (  0.0, 0.0)], [(  0.0, 0.0), (cos(x), sin(x))]]
  val H_dag = SUM.dagger H
  val S_dag = SUM.dagger S
  val T_dag = SUM.dagger T
  val gates = Seq.fromList [H, S, T, H_dag, S_dag, T_dag]

  exception InvalidGate

  fun labelToIdx g =
    case g of
        "h" => 0
      | "s" => 1
      | "t" => 2
      | "hdg" => 3
      | "sdg" => 4
      | "tdg" => 5
      | _ => raise InvalidGate

  fun strtoidx sss =
    let
      fun hs s = Seq.map labelToIdx s
    in
      Seq.map (fn ss => Seq.map hs ss) sss
    end

  fun lteq (c, d) =
    if (Seq.length c > Seq.length d) then false
    else if (Seq.length d > Seq.length c) then true
    else
      let
        fun countT s idx cnt =
          if idx = Seq.length s then cnt
          else if (Seq.nth s idx = 2 orelse Seq.nth s idx = 5) then countT s (idx + 1) (cnt + 1)
          else countT s (idx + 1) cnt
        val (tc, td) = (countT c 0 0, countT d 0 0)
      in
        if tc > td then false
        else if td > tc then true
        else true
      end

  fun makeRepFirst sss =
    let
      fun ltIndx ss i j = lteq (Seq.nth ss i, Seq.nth ss j)
      fun repIdx ss currMin idx =
        if idx = Seq.length ss then currMin
        else if ltIndx ss currMin idx then repIdx ss currMin (idx + 1)
        else repIdx ss idx (idx + 1)
      fun swap ss i j =
        if i = j then ()
        else let
          val si = Seq.nth ss i
          val sj = Seq.nth ss j
        in
          (ArraySlice.update (ss, i, sj); ArraySlice.update (ss, j, si))
        end
      val minIndices = Seq.map (fn ss => repIdx ss 0 0) sss
    in
      Seq.foreach sss (fn (i, ss) => swap ss 0 (Seq.nth minIndices i))
    end

  fun get_rep_seq ss =
    Seq.map
    (fn rep =>
      let
        val grep = Seq.map labelToIdx rep
      in
        (GateSet.perm_to_mat_gates gates grep, grep)
      end
    ) ss

  fun maxlen sss =
    let
      val x = Seq.map (fn ss => Seq.map (Seq.length) ss) sss
      val y = Seq.map (fn ss => Seq.reduce Int.max 0 ss) x
      val z = Seq.reduce Int.max 0 y
    in
      z
    end

  fun insert sss t =
    let
      (* trie insertions need to be sequential *)
      fun nonpar_foreach s f =
        let
          fun loop idx max_len =
            if (idx >= Seq.length s) then max_len
            else loop (idx + 1) (Int.max (max_len, f (idx, Seq.nth s idx)))
        in
          loop 0 0
        end

      fun ins (_, ss) =
        let
          val rep = Seq.toList (Seq.nth ss 0)
        in
          nonpar_foreach ss (fn (i, s) => if (i = 0) then (Seq.length s) else (BlackBoxOpt.insert t (Seq.toList s, rep); Seq.length s))
        end

    in
      nonpar_foreach sss ins
    end

  val f = "../sk-ml/" ^ (CLA.parseString "filename" "test_all_5_1_complete_ECC_set.json")
  val sss = strtoidx (ParseQuartz.parse f)
  val _ = makeRepFirst sss
  val max_len = maxlen sss
  val optc = BlackBoxOpt.mkOpt max_len
  val useopt = CommandLineArgs.parseBool "useopt" true
  val _ = if useopt then insert sss optc else 0


  fun load_circuit f =
    let
      val (sgc, circuit) = ParseQASM.readQASM f
    in
      Seq.map (fn (label, _) => labelToIdx label) circuit
    end

  (* val frep = CLA.parseString "filerep" "test_12_1_representative_set.json"
  val (ssrep, tm) = Util.getTime (fn _ => ParseQuartz.parse_rep frep)
  val _ = print ("parsed filerep in " ^ Time.fmt 4 tm ^ "s\n")
  val _ = if (CLA.parseBool "dumpeq" false) then print (ParseQuartz.str_rep (ssrep)) else ()
  val ml = DelayedSeq.reduce Int.max 0 (DelayedSeq.map (fn p => Seq.length p) (DelayedSeq.fromArraySeq ssrep))
  val optbrute = {eq = get_rep_seq ssrep, max_len = ml, eqdagger = ref (Seq.empty())} *)
(*
  fun analyze_dist_brute {eq = mpseq, max_len = _} =
    let
      val num_dists = (Seq.length mpseq) * (Seq.length mpseq)
      val dists = ForkJoin.alloc num_dists
      val _ = ForkJoin.parfor 5000 (0, num_dists)
        (fn i =>
          let
            val (x, y) = (i mod (Seq.length mpseq), i div (Seq.length mpseq))
            val ((mx, _), (my, _)) = (Seq.nth mpseq x, Seq.nth mpseq y)
          in
          Array.update (dists, i, SUM.proj_trace_dist (mx, my))
          end
        )
      (* val _ = Seq. *)

      val avg_dist = Real./ (Seq.reduce (Real.+) (0.0) (ArraySlice.full dists), Real.fromInt num_dists)
    in
      print ("avg dist = " ^ (Real.toString avg_dist) ^ "\n")
    end *)


  val cliffordt_gates : GateSet.t =
  {
    gates = gates,
    labels = Seq.fromList ["h", "s", "t", "hdg", "sdg", "tdg"] ,
    order = Seq.fromList [2, 4, 8, 2, 4, 8],
    inverses = Seq.fromList [3, 4, 5, 0, 1, 2],
    size = 6
  }

  fun idxtostr sss =
    let
      fun hs s = Seq.map (GateSet.label cliffordt_gates) s
    in
      Seq.map (fn ss => Seq.map hs ss) sss
    end

  val sss = idxtostr sss

  structure M = MeldOpt (structure OptC = BlackBoxOpt)

  val optimize = M.optimize cliffordt_gates optc

end

fun RX thet =
  let
    val t = thet/2.0
    val (c, s) = (Math.cos t, Math.sin t)
  in
    ComplexMatrix.fromList [[(c, 0.0), (0.0, ~1.0 * s)], [(0.0, ~1.0 * s), (c, 0.0)]]
  end

fun RY thet =
  let
    val t = thet/2.0
    val (c, s) = (Math.cos t, Math.sin t)
  in
    ComplexMatrix.fromList [[(c, 0.0), (~1.0 * s, 0.0)], [(s, 0.0), (c, 0.0)]]
  end

fun RZ thet =
  let
    val t = thet/2.0
    val (c, s) = (Math.cos t, Math.sin t)
  in
    ComplexMatrix.fromList [[(c, ~1.0 * s), (0.0, 0.0)], [(0.0, 0.0), (c, s)]]
  end

val _ = if (CLA.parseBool "dumpeq" false) then print (ParseQuartz.str (CliffordT.sss)) else ()

val f = CLA.parseString "bench" "test-single.qasm"
val s = CliffordT.load_circuit f
val s' = CliffordT.optimize s
val _ = print ("length after optimization = " ^ (Int.toString (Seq.length s')) ^ "\n")