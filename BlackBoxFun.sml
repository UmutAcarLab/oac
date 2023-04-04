
functor BlackBoxOptFun (structure Circuit : CIRCUIT) : BLACK_BOX_OPT =
struct

  exception Unimplemented
  structure Circuit = Circuit
  structure CLA = CommandLineArgs

  type collection = {circuits : (ComplexMatrix.t * Circuit.raw_circuit) Seq.t, max_size : int}
  type t = collection Seq.t

  fun load f nq =
    let
      val _ = print ("parsing quartz = " ^ (f) ^ "\n")
      val (ssrep, tm) = Util.getTime (fn _ => ParseQuartz.parse_rep_multi f)
      fun form_tuple rep =
        let
          val grep = Seq.map (Circuit.labelToGate) rep
          val c = (nq, grep)
        in
          (Circuit.eval_raw_sequence c, c)
        end
      val ss = Seq.map form_tuple ssrep
      val ml = DelayedSeq.reduce Int.max 0 (DelayedSeq.map (fn p => Seq.length p) (DelayedSeq.fromArraySeq ssrep))
    in
      {circuits = ss, max_size = ml}
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
                "" => {circuits = Seq.empty(), max_size= 0}
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

  fun equivalent_up_to_phase p (a, b) : bool =
    let
      val c = ComplexMatrix.* (a, ComplexMatrix.trans(ComplexMatrix.dagger b))
      (* val _ = if (p) then print ("c is " ^ (ComplexMatrix.str c) ^ "\n") else () *)
      val tolerance = 1E~15
      fun diag c i = ComplexMatrix.nth c (i, i)
      val eqs = Seq.tabulate (fn i => Complex.equivt tolerance (diag c 0, diag c i)) (#1 (ComplexMatrix.dimension c))
    in
      Seq.reduce (fn (a, b) => a andalso b) true eqs
    end


  fun proj_trace_dist (m1, m2) =
    let
      fun dim m = #1 (ComplexMatrix.dimension m)
      fun rot_in d m i = ComplexMatrix.scale (Complex.ein d i) m
      val d = dim m1
      (* compute |M1 e^itheta - M2| for all theta = 2*i*pi/n *)
      (* val all_rot = Seq.tabulate (fn i => Matrix.norm (Matrix.- (Matrix.scale (Complex.ein d i, m1), m2))) dim *)
      val all_rot = Seq.tabulate (fn i => ComplexMatrix.norm (ComplexMatrix.- (rot_in d m1 i, m2))) d
    in
      Seq.reduce Real.min Real.posInf all_rot
    end

   fun find_approx ({circuits, max_size} : collection) (m, c) =
      let
        (* val _ = print ("finding opt for " ^ (ComplexMatrix.str m)) *)
        val slop' = 1E~12
        fun within_slop m (m', _) = equivalent_up_to_phase false (m, m')
        fun find_best m =
          let
            val candidates = Seq.filter (within_slop m) circuits
            fun select_smaller ((ma, ca), (mb, cb)) =
              if (Circuit.size_raw ca < Circuit.size_raw cb) then (ma, ca)
              else (mb, cb)
          in
            if (Seq.length candidates = 0) then NONE
            else
              SOME (Seq.reduce select_smaller ((Seq.nth candidates 0)) candidates)
          end

        val candidate = find_best m
        (* val _ = if (ComplexMatrix.equiv (slop) (m, m')) then () else *)
        (* (print (ComplexMatrix.str m); print (ComplexMatrix.str m'); print "problem in find_approx\n"; raise WrongOpt) *)
        (* val _ = print ("best equiv is " ^ (Circuit.cstring c' "; ") ^ "\n")
        val _ = print ("m is " ^ (ComplexMatrix.str m) ^ "\n")
        val _ = print ("m' is " ^ (ComplexMatrix.str m') ^ "\n")
        val _ = print ("eq = " ^ (Bool.toString (equivalent_up_to_phase true (m, m'))) ^ "\n") *)
       (* fun printSeq s = print ((Seq.reduce (fn (a, b) => a ^ " " ^ b) "" (Seq.map (label gs) s)) ^ "\n") *)
      in
        case candidate of
          NONE => NONE
        | SOME (_, c') =>
            if Circuit.size_raw c' >= Circuit.size c then NONE
            else
              let
                val cidx = Circuit.reverse_idx c
                val c' = Circuit.from_raw_sequence_with_idx (c', cidx)
                val _ = print ("using equality " ^ (Circuit.cstring c "; ") ^ "\t == " ^ (Circuit.cstring c' "; ") ^ "\n")
              in
                SOME c'
              end
      end

  fun best_equivalent opt c =
    let
      (* val _ = print ("finding best equiv for " ^ (Circuit.cstring c "; ") ^ "\n") *)
      val qsz = QSet.size (Circuit.support c)
    in
      if (qsz <= max_breadth opt) then
        find_approx (Seq.nth opt (qsz - 1)) (Circuit.eval_circuit c, c)
      else NONE
    end

  (* fun max_size (opt: t) x = 5 *)

end
