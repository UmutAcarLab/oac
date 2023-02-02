
structure SMatrix = SUM

functor MeldOpt (structure OptC : BLACK_BOX_OPT) =
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
                      if (SMatrix.equiv slop (ma, mb)) then () else
                        (print "inverif\n"; print (Real.toString (SMatrix.proj_trace_dist (ma, mb)) ^ "\n") ; print (SMatrix.str ma); print (SMatrix.str mb); raise WrongOpt)
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
              if (SMatrix.equiv (2.0 * slop) (m, m')) then ()
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

end
