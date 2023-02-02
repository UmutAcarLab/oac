
structure SMatrix = SUM

structure GateSet =
struct

  type red = {eq : (SMatrix.t * int Seq.t) Seq.t, max_len : int, eqdagger : (SMatrix.t * int Seq.t) Seq.t ref}

  type t = {gates: SMatrix.t Seq.t, labels: string Seq.t, order: int Seq.t, inverses: int Seq.t, size: int}

  fun gates (gs: t) = (#gates gs)
  fun size (gs: t) = Seq.length (#gates gs)
  fun gatem (gs: t) i = Seq.nth (#gates gs) i
  fun order (gs : t) i = Seq.nth (#order gs) i
  fun inverse (gs: t) i = Seq.nth (#inverses gs) i
  fun perm_to_mat (gs : t) perm = Seq.reduce SMatrix.multiply (SMatrix.id(2)) (Seq.map (gatem gs) perm)
  fun perm_to_mat_gates gates perm = Seq.reduce SMatrix.multiply (SMatrix.id(2)) (Seq.map (Seq.nth gates) perm)
  fun inverse_ord (gs: t) i =
    if (order gs i = 2) then i
    else inverse gs i

  fun id_idx (gs : t) = 0

  fun label (gs: t) idx = Seq.nth (#labels gs) idx

  fun perm_to_string (gs: t) perm =
    if (Seq.length perm = 0) then "I"
    else Seq.reduce (fn (a, b) => a ^ " " ^ b) "" (Seq.map (label gs) perm)

  fun invert_perm (gs: t) perm =
    Seq.tabulate (fn i => inverse_ord gs (Seq.nth perm (Seq.length perm - 1 - i))) (Seq.length perm)

  fun append_perm (gs: t) p1 p2 = Seq.append (p1, p2)
(*
  fun find_approx (gs : t) (m, p) slop =
    let
      val {eq = mats, max_len = mlen, ...} = #optbrute gs
      val slop' = slop/3.0
      fun within_slop m (m', _) = abs (SMatrix.proj_trace_dist (m, m') - slop') < slop'
      fun find_best (m, p) =
        Seq.reduce (fn ((ma, pa), (mb, pb)) => if (Seq.length pa < Seq.length pb) then (ma, pa) else (mb, pb)) (m, p)
        (Seq.filter (within_slop m) mats)
      val (m', p') = find_best (m, p)
      val _ = if (SMatrix.equiv (slop) (m, m')) then () else
      (print (SMatrix.str m); print (SMatrix.str m'); print "problem in find_approx\n"; raise WrongOpt)

      fun printSeq s = print ((Seq.reduce (fn (a, b) => a ^ " " ^ b) "" (Seq.map (label gs) s)) ^ "\n")
    in
      if (Seq.length p = Seq.length p') then NONE
      else (
        (* (printSeq p); print (SMatrix.str m); (printSeq p');print (SMatrix.str m');  *)
        SOME (m', p', Seq.length p - Seq.length p'))
    end *)


  (* fun tabulate f n = ArraySlice.full (SeqBasis.tabulate 20 (0, n) f) *)
  (* fun approx_slices (gs : t) (m, p) slop = NONE *)
    (* let
      val pz = Seq.length p
      (* val msz = Int.max (3 * (Int.div (Seq.length p, 4)), #max_len (#optbrute gs)) *)
      val start_sz =  #max_len (#optbrute gs)
      val end_sz = if pz > start_sz then pz else start_sz
      (* val num_slices = Seq.tabulate (fn ssz => ) (end_sz - start_sz + 1) *)


      fun check_slice idx msz =
        let
          val ssz = Int.min (msz, pz - idx)
          val sl =  Seq.subseq p (idx, ssz)
        in
          case find_approx gs (perm_to_mat gs sl, sl) slop of
            NONE => NONE
          | SOME b => SOME (idx, b)
        end

      val num_slices = (end_sz - start_sz + 1) * (Seq.length p)
      val _ = print ("checking " ^ (Int.toString (Seq.length p) ^ " slices\n"))
      val new_reps = tabulate (fn i => check_slice (i mod (Seq.length p)) (start_sz + i div (Seq.length p))) num_slices
      val best_sub = Seq.reduce
      (fn (a, b) =>
        case (a, b) of
          (_, NONE) => a
        | (NONE, _) => b
        | (SOME (_, (_, _, opa)), SOME (_, (_, _, opb))) => if opa > opb then a else b)
        NONE
        new_reps
    in
      case best_sub of
        NONE => NONE
      | SOME (i, (m', p', red)) =>
        let
          val sub_size = Seq.length p'
          fun g n =
            if n < i then Seq.nth p n
            else if (n - i < sub_size) then Seq.nth p' (n - i)
            else Seq.nth p (n + red)
          val new_rep = Seq.tabulate g (pz - red)
          val _ = print ("approx red = " ^ (Int.toString red)  ^ " slop used = " ^ (Real.toString (SMatrix.proj_trace_dist (m, perm_to_mat gs new_rep)))
            ^ " allowed slop = " ^ (Real.toString slop) ^ "\n")
          (* val _ = print ("best slice size = " ^ (Int.toString (sub_size + red))) *)
        in
          SOME (new_rep)
        end

    end *)

  fun check_prefix_id (gs : t) perm =
    let
      val mats = Seq.map (gatem gs) perm
      val (mat_val, fin) = Seq.scan SMatrix.multiply (SMatrix.id (2)) mats
      fun check_id m = SMatrix.equiv 1E~12 (SMatrix.id(2), m)
      fun find_longest_id idx curr =
        if (idx >= Seq.length mat_val) then curr
        else if check_id (Seq.nth mat_val idx) then
          find_longest_id (idx + 1) idx
        else find_longest_id (idx + 1) curr
      val x = if (check_id fin) then Seq.length perm
              else find_longest_id 0 0
    in
      if x = 0 then NONE
      else SOME (Seq.drop perm x)
    end

  val useopt = CommandLineArgs.parseBool "useopt" true
  val useopt2 = CommandLineArgs.parseBool "useopt2" true
  (* could add optimizations here *)
  (* take t from p1 and t' from p2 such that t + t' <= sz *)
  (* fun append_perm_opt (gs : t) slop p1 p2  =
    if (not useopt2) then append_perm gs p1 p2
    else let
      val optc = (#optc gs)
      val sz = 52
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
      fun printSeq s = print ((Seq.reduce (fn (a, b) => a ^ " " ^ b) "" (Seq.map (label gs) s)) ^ "\n")

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
          (* val _ = print "feeding to opt \t" *)
          (* val _ = printSeq s *)
          val new_rep = OptC.lookup optc (Seq.length s, Seq.nth s)
          val new_rep' = approx_slices gs (perm_to_mat gs s, s) slop
          (* val _ = print ("slop = " ^ (Real.toString slop) ^ "\n") *)
          val nr =
            case (new_rep, new_rep') of
              (NONE, _) => new_rep'
            | (_, NONE) => new_rep
            | (SOME s, SOME s') =>
              if (Seq.length s >= Seq.length s') then (print "approx wins\n"; SOME s') else SOME s
          (* val nr' = check_prefix_id gs s
          val new_rep =
            case (nr', new_rep) of
              (NONE, _) => new_rep
            | (_, NONE) => nr'
            | (SOME s, SOME s') =>
              if (Seq.length s > Seq.length s') then SOME s'
              else SOME s *)
        in
          case nr of
            NONE => append3 (p1, seam, p2)
          | SOME s =>
            let
              val ssize = Seq.length s
              (* val _ = print "got from opt \t" *)
              (* val _ = printSeq s *)
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
                  val ma = perm_to_mat gs (append3 (p1, s, p2))
                  val mb = perm_to_mat gs (append3 (p1', s', p2'))
                in
                  if (SMatrix.equiv slop (ma, mb)) then () else
                    (print "inverif\n"; print (Real.toString (SMatrix.proj_trace_dist (ma, mb)) ^ "\n") ; print (SMatrix.str ma); print (SMatrix.str mb); raise WrongOpt)
                end
              (* val _ = verify (p1, seam, p2) (p1', seam', p2') *)
            in
              opt_loop p1' p2' seam' (if len_reduced then ic else (credits - 1))
            end
        end
      val s = opt_loop p1 p2 (Seq.empty()) ic
      val s' = append_perm gs p1 p2
      val red = (Seq.length p1) + (Seq.length p2) - (Seq.length s)
      fun verify s s' =
        let
          val m = perm_to_mat gs s
          val m' = perm_to_mat gs s'
        in
          if (SMatrix.equiv (2.0 * slop) (m, m')) then ()
          else ((printSeq s); (printSeq s'); raise WrongOpt)
        end
      (* val _ = verify s s' *)
    in
      (
        (* print ("red = " ^(Int.toString red)^ "\n\n");  *)
      s)
    end


  fun perm_compare (p1, p2) =
    let
      val n1 = Seq.length p1
      val n2 = Seq.length p2
    in
      case Int.compare (n1, n2) of
        EQUAL =>
          let
            fun loop i =
              if (i >= n1) then EQUAL
              else
                let
                  val (i1, i2) = (Seq.nth p1 i, Seq.nth p2 i)
                in
                  case Int.compare (i1, i2) of
                    EQUAL => loop (i + 1)
                  | lg => lg
                end
          in
            loop 0
          end
      | lg => lg
    end

  fun perm_new (gs: t) (perm_list, m) =
    let
      fun list_nth l idx = List.nth (l, idx)
      fun check_prefix () =
        if (List.length perm_list < 1) then true
        else
            let
              val l = List.length perm_list
              val head = list_nth perm_list 0
              fun inverse_check () =
                if l < 2 then true
                else (list_nth perm_list 1 <> inverse gs head)
              val ord = Seq.nth (#order gs) head
              fun order_check2 ord =
                if ord <> 2 then true
                else (inverse gs head >= head)

              fun order_check ord =
                (* assume l >= ord *)
                if ord = 0 then false
                else if (list_nth perm_list (ord - 1) <> head) then true
                else order_check (ord - 1)
            in
              inverse_check () andalso (l < ord orelse order_check ord) andalso (order_check2 ord)
            end
      val opt_perm = if useopt then OptC.lookup (#optc gs) (List.length perm_list, list_nth perm_list) else NONE
    in
      case opt_perm of
        SOME x => false
      | NONE => check_prefix ()
    end *)
end
