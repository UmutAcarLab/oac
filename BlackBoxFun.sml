
functor BlackBoxOptFun (structure Circuit : CIRCUIT) : BLACK_BOX_OPT =
struct

  exception Unimplemented
  structure Circuit = Circuit
  structure CLA = CommandLineArgs

  type collection = {circuits : (ComplexMatrix.t * Circuit.circuit) Seq.t, max_size : int}
  type t = collection Seq.t

  fun load f =
    let
      val (ssrep, tm) = Util.getTime (fn _ => ParseQuartz.parse_rep_multi f)
      fun form_tuple rep =
        let
          val grep = Seq.map (Circuit.labelToGate) rep
          val c = Circuit.from_raw_sequence grep
        in
          (Circuit.eval_circuit c, c)
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
            val c =
              if CLA.parseFlag flagName then load (CLA.parseString flagName "")
              else {circuits = Seq.empty(), max_size= 0}
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


   (* fun find_approx (gs : t) (m, p) slop =
      let
        val {eq = mats, max_size = mlen, ...} = #optbrute gs
        val slop' = slop/3.0
        fun within_slop m (m', _) = abs (ComplexMatrix.proj_trace_dist (m, m') - slop') < slop'
        fun find_best (m, p) =
          Seq.reduce (fn ((ma, pa), (mb, pb)) => if (Seq.length pa < Seq.length pb) then (ma, pa) else (mb, pb)) (m, p)
          (Seq.filter (within_slop m) mats)
        val (m', p') = find_best (m, p)
        val _ = if (ComplexMatrix.equiv (slop) (m, m')) then () else
        (print (ComplexMatrix.str m); print (ComplexMatrix.str m'); print "problem in find_approx\n"; raise WrongOpt)

       fun printSeq s = print ((Seq.reduce (fn (a, b) => a ^ " " ^ b) "" (Seq.map (label gs) s)) ^ "\n")
      in
        if (Seq.length p = Seq.length p') then NONE
        else (
          (* (printSeq p); print (ComplexMatrix.str m); (printSeq p');print (ComplexMatrix.str m');  *)
          SOME (m', p', Seq.length p - Seq.length p'))
      end *)
  fun best_equivalent opt c = raise Unimplemented
    (* let
      bindings
    in
      body
    end *)

  fun max_breadth opt = Seq.length opt
  fun max_size (opt: t) x = #max_size (Seq.nth opt x)

end