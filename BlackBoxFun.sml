
functor BlackBoxOptFun (structure Circuit : CIRCUIT) : BLACK_BOX_OPT =
struct


  type mcseq = (ComplexMatrix.t * Circuit.raw_circuit) Seq.t

  exception Unimplemented
  structure Circuit = Circuit
  structure CLA = CommandLineArgs

  fun phase_canonical m =
    let
      val n = #1 (ComplexMatrix.dimension m)
      val mij = ComplexMatrix.nth m
      fun next i j =
        if j = n - 1 then (i + 1, 0)
        else (i, j + 1)

      val z = (0.0, 0.0)
      fun first_nonzero (i, j) =
        if Complex.equiv (mij (i, j), z) then first_nonzero (next i j)
        else mij (i, j)

      val (_, ethet) = Complex.polar_form (first_nonzero (0, 0))
      val ethet' = Complex.conjugate ethet
    in
      ComplexMatrix.map (fn c => Complex.multiply (c, ethet')) m
    end

  structure QuartzInterface =
  struct
    fun load (f : string, nq : int) : (mcseq * int) =
      let
        val _ = print ("parsing quartz = " ^ (f) ^ "\n")
        val (ssrep, tm) = Util.getTime (fn _ => ParseQuartz.parse_rep_multi f)
        fun form_tuple rep =
          let
            val grep = Seq.map (Circuit.labelToGate) rep
            val c = (nq, grep)
            val m = Circuit.eval_raw_sequence c
          in
            (phase_canonical m, c)
          end
        val ss = Seq.map form_tuple ssrep
        val ml = DelayedSeq.reduce Int.max 0 (DelayedSeq.map (fn p => Seq.length p) (DelayedSeq.fromArraySeq ssrep))
      in
        (ss, ml)
      end

    fun convert (f : string, f' : string) : unit = raise Unimplemented

    fun equivalent_up_to_phase p (a, b) : bool =
      let
        val c = ComplexMatrix.* (a, (ComplexMatrix.dagger b))
        (* c should be of the form k * I *)
        val _ = if p then print (ComplexMatrix.str c) else ()
        val tolerance = 1E~15
        val c' = ComplexMatrix.scale (ComplexMatrix.nth c (0, 0)) (ComplexMatrix.id (#1 (ComplexMatrix.dimension c)))
      in
        ComplexMatrix.norm (ComplexMatrix.- (c, c')) <= tolerance
      end

    exception RaiseDiff
    fun find_approx (circuits : mcseq) (m, c) : Circuit.raw_circuit option =
      let
        val slop' = 1E~12
        fun within_slop m (m', _) = equivalent_up_to_phase false (m, m')
        fun within_slop' m (m', c') =
          let
            val w = ComplexMatrix.norm (ComplexMatrix.- (m, m')) <= slop'
            val w' = within_slop m (m', c')
          in
            if w = w' then w
            else (
            print (ComplexMatrix.str m); print ("\n\n\n"); print (ComplexMatrix.str m');
            if w then print ("\ncanonical says equal\n")
            else print ("\n upto_phase says equal\n");
            equivalent_up_to_phase true (m, m');
            print ((Circuit.raw_str c' "; "));
            print ((Circuit.cstring c "; "));
            raise RaiseDiff)
          end

        fun find_best m =
          let
            val candidates = Seq.filter (within_slop' (phase_canonical m)) circuits
            fun select_smaller ((ma, ca), (mb, cb)) =
              if (Circuit.size_raw ca < Circuit.size_raw cb) then (ma, ca)
              else (mb, cb)
          in
            if (Seq.length candidates = 0) then NONE
            else
              SOME (Seq.reduce select_smaller ((Seq.nth candidates 0)) candidates)
          end

        val candidate = find_best m
      in
        case candidate of
          NONE => NONE
        | SOME (_, c') =>
            if Circuit.size_raw c' >= Circuit.size c then NONE
            else SOME c'
      end
  end

  type store = (ComplexMatrix.t, Circuit.raw_circuit) Hashtable.hashtable

  type collection = {max_size : int, cstore : store}
  type t = collection Seq.t

  fun init_hash_table (sz, capacity) =
    let
      val hvec =
        let
          val rd = Random.rand (123, 456)
          fun next_complex _ = (Random.randReal rd, Random.randReal rd)
        in
          ArraySlice.full (Array.tabulate (sz, next_complex))
        end

      fun hash m =
        let
          val svec = ComplexMatrix.matVec (m, hvec)
          (* multiplying by idx differentiates b/w permutations of the same matrix *)
          (* taking the real part differentiates b/w swapping real and img elements of a matrix *)
          val svec_mod = Seq.mapIdx (fn (idx, c) => Real.fromInt (idx) * Complex.real (c)) svec
          val sum_mod = 7917.0 * (Seq.reduce (Real.+) 0.0 svec_mod)
          (* val _ = print ((Real.toString sum_mod) ^ "\t ") *)
        in
          Real.round sum_mod
        end
      fun eq (m1, m2) = ComplexMatrix.compare (m1, m2) = EQUAL
    in
      Hashtable.make ({hash = hash, eq = eq, capacity = capacity})
    end

  fun load f nq =
    let
      val (ss, ml) = QuartzInterface.load (f, nq)
      val sz = Real.toInt(IEEEReal.TO_NEAREST) (Math.pow(2.0, Real.fromInt(nq)))
      val ht = init_hash_table (sz, 5 * (Seq.length ss))
      val _ = Seq.foreach ss (fn (_, (m, c)) => Hashtable.insert ht (m, c))
    in
      {max_size = ml, cstore = ht}
    end

  fun init () =
    let
      val max_breadth = CLA.parseInt "max_breadth" 1
      fun loop idx acc =
        if idx = 0 then acc
        else
          let
            val flagName = "filerep" ^ (Int.toString idx)
            val sc = (CLA.parseString flagName "")
            val _ = print ("f = " ^ sc ^ "\n")
            val c =
              case (CLA.parseString flagName "") of
                "" => {max_size = 0, cstore = init_hash_table (0, 0)}
              | f => load f idx
          in
            loop (idx - 1) (c::acc)
          end
    in
      Seq.fromList (loop max_breadth [])
    end


  (* fun remove_inverses gs s =
    let
      val s' = Seq.map (fn x => SOME x) s
      val slen = Seq.length s
      fun is_inverse (i, j) = (GateSet.inverse gs i) = j
      val _ = Seq.iterate
        (fn (_, i) =>
          if i < slen - 1
            andalso is_inverse (Seq.nth s i, Seq.nth s (i + 1))
            andalso (Option.isSome (Seq.nth s' i)) then
            (ArraySlice.update (s', i, NONE); ArraySlice.update (s', i + 1, NONE))
          else ()
        )
        () s
    in
      Seq.mapOption (fn x => x) s'
    end *)
  fun max_breadth opt = Seq.length opt
  (* fun max_breadth opt = 2 *)
  fun max_size (opt: t) x =
    if (x <= max_breadth opt) then #max_size (Seq.nth opt (x - 1))
    else 0

  fun reindex (c' : Circuit.raw_circuit, c : Circuit.circuit) =
    let
      val get_idx = (fn x => Qubit.to_int x)
      val get_qubit = Circuit.idx_inverse c
      val qrelabel = (get_qubit o get_idx)
      val c'' = Circuit.from_raw_sequence_with_relabel (c', qrelabel)
    in
      c''
    end

  fun find ({max_size, cstore} : collection) (m, c) =
    let
      val m' = phase_canonical m
      val c' = Hashtable.lookup cstore m'
    in
      case c' of
        NONE => NONE
      | SOME c' =>
          if Circuit.size_raw c' >= Circuit.size c then NONE
          else SOME (reindex (c', c))
    end

  fun best_equivalent opt c =
    let
      val qsz = QSet.size (Circuit.support c)
    in
      if (qsz <= max_breadth opt) then
        find (Seq.nth opt (qsz - 1)) (Circuit.eval_circuit c, c)
      else NONE
    end

end
