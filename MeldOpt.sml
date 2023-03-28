
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
  structure T = TreeFun (structure Circuit = Circuit)
  structure S = SupportClosedFun (structure Circuit = Circuit)

  (*  *)

  (* gen_all_subckt qs *)
  (* for each gate in frontier:
    if support(gate) \subset qs then include g; add rught neightbors of g to the froniter
  *)


  open T
  fun gen_all_subckt c (qs, av) (supp_size, max_size) =
    let
      val ms = case max_size of SOME (m) => m | NONE => (Option.valOf Int.maxInt)
      val n = QSet.size qs + QSet.size av
      val (all, k) = case supp_size of SOME (k) => (false, k) | NONE => (true, n)
      val sc = S.parse c ms

      fun create_circuit qs ms =
        let
          (* val _ = print ("calling create with qset = " ^ (QSet.str qs)) *)
          val (cqs, sz, ctxtfront) = S.gen_max_subckt sc qs
          (* val _ = print ("created circuit of size = " ^ (Int.toString (sz)) ^ "\n") *)
        in
          if sz = 0 then []
          else [(cqs, ctxtfront)]
        end

      fun drop_some (qs : QSet.t) av =
        let
          val q = QSet.some av
        in
          (QSet.add (qs, q), QSet.subtract (av, q))
        end

      fun loop_all qs av =
        if (QSet.size av) = 0 then []
        else
          let
            val l0 = create_circuit qs ms
            val (qs', av') = drop_some qs av
            val (l1, l2) = (loop_all qs av', loop_all qs' av')
          in
            l0@l1@l2
          end

      (* wksize = |qs| + |av| *)
      (* |av| always decreases *)
      fun loopk qs av wksize =
        if QSet.size qs = k then create_circuit qs ms
        else if wksize < k then []
        else
          let
            val (qs', av') = drop_some qs av
            val (l1, l2) = (loopk qs av' (wksize - 1), loopk qs' av' wksize)
          in
            l1@l2
          end
    in
      (* TODO: figure out the right arguments here *)
      if all then loop_all qs av
      else loopk qs av (QSet.size qs + QSet.size av)
    end


  fun subckt (c : Circuit.circuit) q k max_size =
    let
      val candidates = gen_all_subckt c (QSet.empty , Circuit.support c) (SOME k, SOME max_size)
      (* val _ = print ("num subckts = " ^ (Int.toString (List.length candidates)) ^ "\n") *)
      (* val _ = List.app (Circuit.cprint o S.to_norm_circuit o (#1)) candidates *)
    in
      case candidates of
        nil => NONE
      | (x, ctxt) :: lx => SOME (S.to_norm_circuit x, ctxt)
    end


    (* let
      val n = Circuit.num_qubits n
      val sc = S.parse c max_size
      val dpseq = Seq.tabulate (fn i => NONE) (n * (Circuit.num_layers c))
      fun dpread (l, q) = Seq.nth dpseq (l * n + q)
      fun dpwrite (l, q, v) = ArraySlice.update(dpseq, (l * n + q), SOME v)
      val gate = Circuit.gate c

      fun loop (l, q) k =
        case dpread (l, q) of
          SOME _ => Seq.empty()
        | NONE =>
          let
            val g = gate (l, q)
            val scg = S.support sc (l, q)
            val sz = QSet.size scg
          in
            if (sz <= k) then
              let
                val circuits = ForkJoin.alloc sz
                val _ = QSet.foreach
                  (fn q => let val res = loop (l + 1, q) k in ArraySlice.update (circuits, q, res) end) scg
                fun combine_leaders circuits =
                  let
                    val q = Seq.length circuits
                  in
                    body
                  end
              in

              end

            else (dpwrite (l, q, true); Seq.empty ())
          end
    in
      body
    end *)

  fun patch_left (c : Circuit.circuit) (c' : circtree) =
    case c' of
      LEAF x => LEAF c
    | CONCAT {left, right, size} =>
      let
        val l' = patch_left c left
        val s' = size - (tree_size left) + (tree_size l')
      in
        CONCAT {left = l', right = right, size = s'}
      end
    | PAR {left, right, size} =>
      let
        val l' = patch_left c left
        val s' = size - (tree_size left) + (tree_size l')
      in
        PAR {left = l', right = right, size = s'}
      end

  fun optimize_base bbopt (c : Circuit.circuit) =
    let
      val n = Circuit.num_qubits c
      val max_breadth = BlackBoxOpt.max_breadth bbopt

      fun loop_size q sz (change, c) =
        if (sz > max_breadth) orelse (Circuit.size c = 0) then (change, c)
        else let
          val d = (BlackBoxOpt.max_size bbopt sz)
          val sub' =
            case subckt c q sz d of
              SOME (s, cf) =>
                (case BlackBoxOpt.best_equivalent bbopt s of
                  NONE => NONE
                | SOME s' => SOME (s', (cf, Circuit.size s)))
            | NONE => NONE
        in
          case sub' of
            NONE => (loop_size q (sz + 1) (change, c))
          | SOME (s', ctxt_and_size) =>
            let
              (* val _ = (print "before "; Circuit.cprint c) *)
              val _ = Circuit.patch_circuit c ctxt_and_size s'
              (* val _ = (print "after "; Circuit.cprint c) *)
            in
              loop_size q (sz + 1) (true, c)
            end
        end

      fun loop_bit q (change, c) =
        if q = n orelse (Circuit.size c = 0) then (change, c)
        else let
          val (change', c') = loop_size q 0 (false, c)
        in
          loop_bit (q + 1) (change orelse change', c')
        end

      fun loop (c, opt) =
        let
          val (change, c') = loop_bit 0 (false, c)
        in
          if change andalso (Circuit.size c' <> 0) then
            loop (c', (opt orelse change))
          else (c, opt)
        end
    in
      loop (c, false)
    end

  (* fun meld c1 c2 = raise Unimplemented *)

  (* fun optimize_tree bbopt c =
    case c of
      PAR n => PAR n
    | CONCAT ({left, right, size}) =>
        let
          val (l', r')  = (optimize_tree left, optimize_tree right)
        in
          meld l' r'
        end
    | LEAF c => LEAF (optimize_base bbopt c) *)

  fun meld bbopt c1 c2 =
    let
      fun meld_rec nl c1 c2 =
        (* TODO: stop early once there are no new circuits, for example,
         * if you can't optimize after d peels, stop
        *)
        if nl = 0 then c2
        else
          let
            val (c1, peel) = Circuit.split c1 (nl - 1)
            val c2' = Circuit.prepend (peel, c2)
            val (c2'_opt, _) = (optimize_base bbopt c2)
          in
            meld_rec (nl - 1) c1 c2'_opt
          end
    in
      meld_rec (Circuit.num_layers c1) c1 c2
    end

  fun simple_opt bbopt c =
    let
      val num_layers = Circuit.num_layers c
    in
      if num_layers < 2 then #1 (optimize_base bbopt c)
      else
        let
          val (c1, c2) = Circuit.split c (num_layers div 2)
          val (opc1, opc2) = (simple_opt bbopt c1, simple_opt bbopt c2)
        in
          meld bbopt opc1 opc2
        end
    end

  fun optimize bbopt c =
    let
      val c' = simple_opt bbopt c
      val _  = (print "after all "; Circuit.cprint c')
    in
      c'
    end

end

(*

 functor MeldOptFun (structure BlackBoxOpt : BLACK_BOX_OPT) : MELD_OPT =
struct



  type circuit = int Seq.t
  datatype circtree =
    PAR of node
  | CONCAT of node
  | LEAF of (circuit * int)
  | PLEAF of (circuit * int)
  withtype

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
      fun lease c k front max_size =
        let
          val clen = Circuit.size c
          fun layeri i = if front then Circuit.layer c i else Circuit.layer c (clen - i - 1)
          val sets = Seq.tabulate (fn i => QSet.init n) (max_size * n)
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
          val _ = label_layer 0 max_size

          fun find_support qi =
            let
              fun loop i =
                if i = max_size then NONE
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
          val sz = 2 * (OptC.max_size bopt 1)
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
            val _ = print ("partition size = " ^ (Int.toString (OptC.max_size bopt k)) ^ "\n")
            val sc = partition (OptC.max_size bopt k) c
            val optsc = meld_nodes gs bopt sc
          in
            koptimize (k-1) (flatten optsc)
          end
    in
      koptimize (OptC.max_breadth bopt) c
    end

end *)
