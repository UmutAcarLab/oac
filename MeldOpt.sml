
signature MELD_OPT =
sig
  structure Circuit : CIRCUIT
  structure BlackBoxOpt : BLACK_BOX_OPT
  val optimize : BlackBoxOpt.t -> Circuit.circuit -> Circuit.circuit
end

functor MeldOptFun (structure BlackBoxOpt : BLACK_BOX_OPT) : MELD_OPT =
struct
  exception Unimplemented
  structure BlackBoxOpt = BlackBoxOpt
  structure Circuit = BlackBoxOpt.Circuit

  val optimize = (fn _ => raise  Unimplemented)
end

(* functor MeldOptFun (structure BlackBoxOpt : BLACK_BOX_OPT) : MELD_OPT =
struct



  type circuit = int Seq.t
  datatype circtree =
    PAR of node
  | CONCAT of node
  | LEAF of (circuit * int)
  | PLEAF of (circuit * int)
  withtype node = {left : circtree, right : circtree, size : int}

  exception Unimplemented
  exception WrongOpt


  fun init s = LEAF (s)

  fun size c =
    case c of
      PAR n => #size n
    | CONCAT n => #size n
    | LEAF (_, sz) => sz
    | PLEAF (_, sz) => sz

  (* naive partition strategy *)
  fun partition k c =
    let
      val sz = Seq.length c
      val tk = sz div 2
    in
      if sz > k then
        CONCAT {left = partition k (Seq.take c tk), right = partition k (Seq.drop c tk), size = sz}
      else LEAF (c, sz)
    end

  fun flatten c =
    case c of
      PAR _ => raise Unimplemented
    | CONCAT {left, right, size} => Seq.append (flatten left, flatten right)
    | LEAF (c, _) => c
    | PLEAF (c, _) => c


  (* fun lease gs c del k front =
    let
      val clen = Circuit.size c
      fun layeri i = if front then Circuit.layer c i else Circuit.layer c (clen - i - 1)

      (* inactive_bits is a non-functional set that is modified in place *)
      val inactive_bits = QSet.init n

      (* peel off one layer *)
      fun peel_layer sl =
        let
          val layer = layeri sl
          val inactive =  QSet.contains inactive_bits

          (* sbits is also a non-functional set that is modified in place *)
          val sbits = QSet.init n
          val new_layer = Circuit.init_layer n
          (* loop invariant: sbits \intersect inactive_bits = \empty *)
          fun loop qi =
            if qi = n then ()
            else if inactive qi then loop (qi + 1)
            else if (QSet.contains sbits qi) then loop (qi + 1)
            else
              let
                val (g, qbits) = Circuit.gate layer qi
                val id_gate = (GateSet.id_idx gs, qbits)
              in
                if (QSet.intersect_count qbits inactive_bits = 0) then
                  (QSet.foreach qbits (fn i => Circuit.add_to_layer new_layer i g);
                  (QSet.absorb sbits qbits; loop (qi + 1)))
                else
                  (QSet.foreach qbits (fn i => Circuit.add_to_layer new_layer i id_gate);
                  QSet.absorb inactive_bits qbits)
              end
        in
        end
    in
      body
    end *)

  fun meldc c1 c2 =
    let
      val n = Circuit.support c1

      (* returns a sequence of n qubits *)
      fun lease c k front max_depth =
        let
          val clen = Circuit.size c
          fun layeri i = if front then Circuit.layer c i else Circuit.layer c (clen - i - 1)
          val sets = Seq.tabulate (fn i => QSet.init n) (max_depth * n)
          fun m i q = Seq.nth sets (i * n + q)

          fun label_layer i max =
            if i = max then ()
            else
              let
                val layer = layeri i
                fun determine j =
                  if j = n then ()
                  else
                  let
                    val (g, qbits) = Circuit.gate layer j
                    val qf = QSet.init n
                    val _ = QSet.iterate qbits (fn q => QSet.absorb qf (m (i - 1) q))
                  in
                    (ArraySlice.update (sets, i * n + j, qf); determine (j + 1))
                  end
              in
                (determine 0; label_layer (i+1) max)
              end

          (* non-functional set *)
          (* val claimed_bits = QSet.init n *)
          val _ = label_layer 0 max_depth

          fun find_support qi =
            let
              fun loop i =
                if i = max_depth then NONE
                else if QSet.size (m i qi) = k then SOME i
                else loop (i + 1) qs
            in
              loop 0
            end

          val shortest_k_circuit =
            let
              fun loop qi =
                if qi = n then NONE
                else case find_support qi of
                  SOME idx => SOME (qi, idx)
                | NONE => loop (qi + 1)
            in
              loop 0
            end

          val maximal_circuit =
            case shortest_k_circuit of
              NONE => NONE
            | SOME (qi, idx) =>
              let
                val max_layers = Seq.tabulate (fn i => 0) n
                val support = m idx qi
                fun explore j q =
                  let
                    val g = Circuit.gate (layeri j) q
                  in
                    if Circuit.
                  end
              in

              end
        in

        end

      fun meldk c1 seam c2 k =
        let
          val c1_scar = lease c k
        in
          body
        end
    in
      body
    end

  fun meld_nodes gs bopt c =
    let
      val fopt = OptC.best_equivalent bopt gs

      fun fbest c =
        case fopt c of
          NONE => c
        | SOME c' => c'

      val slop = 1E~15
      fun meldc (c1: int Seq.t) (c2: int Seq.t) =
        let
          val sz = 2 * (OptC.max_depth bopt 1)
          fun append3 (a, b, c) =
            let
              val (sa, sb, sc) = (Seq.length a, Seq.length b, Seq.length c)
            in
              Seq.tabulate
              (fn i =>
                if i < sa then Seq.nth a i
                else if i < sa + sb then Seq.nth b (i - sa)
                else Seq.nth c (i - sa - sb))
              (sa + sb + sc)
            end
          fun printSeq s = print ((Seq.reduce (fn (a, b) => a ^ " " ^ b) "" (Seq.map (Int.toString) s)) ^ "\n")

          (* start with a default credit of 3 --- max number of times we can lookup without size reduction *)
          val ic = 3
          fun opt_loop p1 p2 seam credits =
            if credits = 0 orelse sz < 1 then append3 (p1, seam, p2)
            else if (Seq.length p1 = 0) orelse (Seq.length p2 = 0) then append3 (p1, seam, p2)
            else let
              val t1 = Int.min (sz, Seq.length p1)
              val t2 = Int.min (sz, Seq.length p2)
              val (s1, s2) =  (Seq.drop p1 ((Seq.length p1) - t1), Seq.take p2 t2)
              val s = append3 (s1, seam, s2)
              val nr = fopt s
              (* val new_rep' = approx_slices gs (GateSet.perm_to_mat gs s, s) slop *)
              (* val nr =
                case (new_rep, new_rep') of
                  (NONE, _) => new_rep'
                | (_, NONE) => new_rep
                | (SOME s, SOME s') =>
                  if (Seq.length s >= Seq.length s') then (print "approx wins\n"; SOME s') else SOME s *)
            in
              case nr of
                NONE => append3 (p1, seam, p2)
              | SOME s =>
                let
                  val ssize = Seq.length s
                  fun count_common (a, aidx) (b, bidx) inc cnt lim =
                    if cnt >= lim then cnt
                    else if (Seq.nth a aidx = Seq.nth b bidx) then
                      count_common (a, inc aidx) (b, inc bidx) inc (cnt + 1) lim
                    else cnt

                  val t1' = count_common (p1, (Seq.length p1) - t1) (s, 0) (fn i => i + 1) 0 (Int.min (t1, ssize))
                  val rem_size = ssize - t1'
                  val t2' = count_common (p2, t2 - 1) (s, ssize - 1) (fn i => i - 1) 0 (Int.min (t2, ssize - t1'))
                  val p1' =  (Seq.take p1 ((Seq.length p1) - t1 + t1'))
                  val p2' = (Seq.drop p2 (t2 - t2'))
                  val seam' = (Seq.subseq s (t1', ssize - t2' - t1'))
                  val len_reduced = ((t1 - t1') + (t2 - t2') + ((Seq.length seam) - (Seq.length seam'))) > 0

                  fun verify (p1, s, p2) (p1', s', p2') =
                    let
                      val ma = GateSet.perm_to_mat gs (append3 (p1, s, p2))
                      val mb = GateSet.perm_to_mat gs (append3 (p1', s', p2'))
                    in
                      if (ComplexMatrix.equiv slop (ma, mb)) then () else
                        (print "inverif\n"; print (Real.toString (ComplexMatrix.proj_trace_dist (ma, mb)) ^ "\n") ; print (ComplexMatrix.str ma); print (ComplexMatrix.str mb); raise WrongOpt)
                    end
                  (* val _ = verify (p1, seam, p2) (p1', seam', p2') *)
                in
                  opt_loop p1' p2' seam' (if len_reduced then ic else (credits - 1))
                end
            end
          val s = opt_loop c1 c2 (Seq.empty()) ic
          val s' = GateSet.append_perm gs c1 c2
          val red = (Seq.length c1) + (Seq.length c2) - (Seq.length s)
          fun verify s s' =
            let
              val m = GateSet.perm_to_mat gs s
              val m' = GateSet.perm_to_mat gs s'
            in
              if (ComplexMatrix.equiv (2.0 * slop) (m, m')) then ()
              else ((printSeq s); (printSeq s'); raise WrongOpt)
            end
          (* val _ = verify s s' *)
        in
          (s, Seq.length s)
        end

      fun meldp c1 c2 = raise Unimplemented

      fun ml c =
        case c of
          PAR {left, right, ...} => (meldp (ml left) (ml right))
        | CONCAT {left, right, ...} =>
            let val (ml, mr) = (ml left, ml right)
            in  case (ml, mr) of
              (LEAF (c1, _), LEAF (c2, _)) => LEAF (meldc c1 c2)
            | _ => CONCAT {left = ml, right = mr, size = size ml + size mr}
            end
        | LEAF (c, _) =>
          let
            val newc = fbest c
          in LEAF (newc, Seq.length newc)
          end
        | _ => c
    in
      ml c
    end



  fun optimize gs bopt c =
    let
      fun koptimize k c =
        if (k = 0) then c
        else
          let
            val _ = print ("partition size = " ^ (Int.toString (OptC.max_depth bopt k)) ^ "\n")
            val sc = partition (OptC.max_depth bopt k) c
            val optsc = meld_nodes gs bopt sc
          in
            koptimize (k-1) (flatten optsc)
          end
    in
      koptimize (OptC.max_breadth bopt) c
    end

end *)
