
functor CircuitFun (structure GateSet : GATE_SET) : CIRCUIT =
struct
  exception Unimplemented

  datatype gate = P of GateSet.gate | I of Qubit.qubit
  type layer = gate Seq.t
  type circuit = {qset : QSet.t, layers: layer Seq.t, idx : int QMap.map, size : int ref}
  type raw_circuit = int * gate Seq.t

  fun size {qset, layers, idx, size} = !size

  fun is_id g =
    case g of
      P _ => false
    | I _ => true

  fun labelToGate (s, ql) =
    case s of
      "I" => I (List.hd ql)
    | _ =>  P (GateSet.labelToGate (s, ql))


  fun id_gate x = I (x)

  fun layer {qset, layers, idx, ...} x = Seq.nth layers x
  fun init_layer n = ArraySlice.full (ForkJoin.alloc n)

  fun gate {qset, layers, idx, ...} l (q : Qubit.qubit) = Seq.nth l (QMap.lookup (idx, q))

  fun gate_str (g) =
    case g of
      P g => GateSet.str g
    | I x => ""

  fun gate_matrix g =
    case g of
      P g => GateSet.gate_matrix (g)
    | I x => ComplexMatrix.id (2)


  fun gate_support g =
    case g of
      I x => [x]
    | P g => GateSet.support g

  fun gate_support_unordered g = QSet.from_list (gate_support g)

  fun num_qubits (c : circuit) = QSet.size (#qset c)
  fun support (c : circuit) = (#qset c)
(*
  fun mat_gate g (nq, idx) =
    let
      val perm = Seq.tabulate (fn i => ~i) nq
      val supp = gate_support g
      val supp_list = QSet.to_list (supp)
      val sz = List.length supp

      fun loop buck l =
        if buck = sz then ()
        else
          case l of
            nil =>
              (if Seq.nth perm buck < 0 then ArraySlice.update (perm, buck, buck) else ()
              ; loop (buck + 1) nil)
          | q :: rl =>
            let
              val qidx = Seq.nth idx q
            in
              if Seq.nth perm buck = ~buck andalso (not (QSet.contains )) then
                (ArraySlice.update (perm, buck ))
              else loop (buck + 1) rl
            end

    in
      body
    end *)
  fun from_qasm (chars : char Seq.t) =
    let
      val (nq, raw_str) = ParseQASM.parse chars
      val raw_gate_seq = Seq.map labelToGate raw_str
    in
      (nq, raw_gate_seq)
    end

  fun to_raw_sequence (c : circuit) =
    let
      val {qset, layers, idx, ...} = c
      val num_qubits = QSet.size qset
      fun layer_to_lines l =
        let
          val done = Seq.tabulate (fn x => false) num_qubits

          fun loop qidx acc =
            if qidx = num_qubits then acc
            else if Seq.nth done qidx then loop (qidx + 1) acc
            else let
              val g = Seq.nth l qidx
              val qs = gate_support_unordered g
              val _ = QSet.foreach qs (fn q =>
                (* (print ("q = " ^ (Int.toString q) ^ "\n"); print (" qidx = " ^ (Int.toString (Seq.nth idx q) ^ "\n"));  *)
                ArraySlice.update (done, QMap.lookup(idx, q), true))
                (* ) *)
            in
              case g of
                P g' => loop (qidx + 1) (g::acc)
              | _ => loop (qidx + 1) acc
            end
        in
          Seq.fromList (loop 0 [])
        end
      val gseqs = Seq.map layer_to_lines layers
      val c = Seq.flatten gseqs
    in
      (num_qubits, c)
    end


  (* cnot (0, 3), x(1), x(2) *)
  (* perm(1, 3) * cnot (0, 1) * perm (1, 3),  x(1), x(2) *)
  fun eval_gate_sequence (fidx : Qubit.qubit -> int) (nq, gqasm) =
  (* !size=3 *)
    let
      val width = Real.toInt(IEEEReal.TO_NEAREST) (Math.pow(2.0, Real.fromInt(nq)))
      (* print(GateSet.str(Seq.nth gqasm 0)) *)
      fun tensorize_gate (g : gate) =
        let
          val supp = QSet.to_intset fidx (gate_support_unordered g)
          val support_size = IntSet.size supp
          fun shift_left n i =
            let
              val n_word = Word.fromInt n
              val i_word = Word.fromInt i
              val result_word = Word.<< (n_word, i_word)
            in
              Word.toInt result_word
            end
          fun shift_right n i =
            let
              val n_word = Word.fromInt n
              val i_word = Word.fromInt i
              val result_word = Word.>> (n_word, i_word)
            in
              Word.toInt result_word
            end
          fun bit_and a b =
            let
              val a_word = Word.fromInt a
              val b_word = Word.fromInt b
              val result_word = Word.andb (a_word, b_word)
            in
              Word.toInt result_word
            end
          fun bit_or a b =
            let
              val a_word = Word.fromInt a
              val b_word = Word.fromInt b
              val result_word = Word.orb (a_word, b_word)
            in
              Word.toInt result_word
            end
          val gmatrix = gate_matrix g
          fun get_entry (ket, bra) =
            let
              fun fun_i iter =
                if iter = 0 then false
                else let
                  val i = nq - iter
                  val q_arr_i = nq - 1 - i
                  val bra_i_state = bit_and (shift_right bra i) 1
                  val ket_i_state = bit_and (shift_right ket i) 1
                in
                  if not (IntSet.contains (supp, q_arr_i)) andalso not (bra_i_state = ket_i_state) then true
                  else fun_i (iter-1)
                end
              val is_zero = fun_i nq
              val supp_seq =
                let
                  val qgate_seq = Seq.fromList (gate_support g)
                in
                  Seq.map fidx qgate_seq
                end
              fun get_indexes iter bra_index ket_index =
                if iter = 0 then (bra_index, ket_index)
                else let
                  val j = support_size - iter
                  val i = nq - 1 - (Seq.nth supp_seq j)
                  val bra_i_state = bit_and (shift_right bra i) 1
                  val new_bra_index = bit_or bra_index (shift_left bra_i_state (support_size - 1 - j))
                  val ket_i_state = bit_and (shift_right ket i) 1
                  val new_ket_index = bit_or ket_index (shift_left ket_i_state (support_size - 1 - j))
                in
                  get_indexes (iter-1) new_bra_index new_ket_index
                end
              val (bra_index, ket_index) = get_indexes support_size 0 0
            in
              if is_zero then
                (0.0, 0.0)
              else
                Array2.sub (gmatrix, ket_index, bra_index)
            end
        in
          (* T(0) @ I(1), S(0) @ I(1), H(0) @ I(1)
          matrices are all correct
          https://colab.research.google.com/drive/1taOfQhSk_-SXuBL2Zja_YgGb2wWl1KlH?usp=sharing *)
          ComplexMatrix.tabulate (width, width) get_entry
        end
      val g_matrices = Seq.map tensorize_gate gqasm
      val final_matrix = Seq.reduce ComplexMatrix.* (ComplexMatrix.id (width)) (Seq.rev g_matrices)
    in
      final_matrix
    end

  val eval_raw_sequence = eval_gate_sequence Qubit.to_int
  fun eval_circuit c = eval_gate_sequence (fn q => QMap.lookup (#idx c, q)) (to_raw_sequence c)

  fun cprint (c : circuit) =
    let
      val {qset, layers, idx, ...} = c
      val (nq, gqasm) = to_raw_sequence c
      val gstr = Seq.map gate_str gqasm
      val str = Seq.reduce (fn (a, b) => a ^ "\n" ^ b) "" gstr
    in
      print ("circuit  = " ^ str ^ "\n")
    end

  fun raw_str (nq, gseq) sep =
    let
      val gstr = Seq.map (fn g => gate_str(g) ^ sep) gseq
      val str = Seq.reduce (fn (a, b) => a ^ b) "" gstr
    in
      str
    end

  fun raw_to_qasm ((nq, gseq) : raw_circuit) =
    let
      val header = "OPENQASM 2.0;\ninclude \"qelib1.inc\";\nqreg q[" ^ (Int.toString (nq)) ^ "];\n"
    in
      header ^ (raw_str (nq, gseq) ";\n")
    end

  fun size_raw (_, gseq) = Seq.length gseq

  fun cstring (c : circuit) sep = raw_str (to_raw_sequence c) sep
  fun to_qasm (c : circuit) = raw_to_qasm (to_raw_sequence c)

  fun dump (c:circuit) (f) =
    let
      open Posix.FileSys
      val file =  createf (f, O_WRONLY, O.fromWord 0w0, S.irwxu)
      val s =  to_qasm c
      val s' = Word8Vector.tabulate (String.size s, fn i => Word8.fromInt (Char.ord (String.sub (s,i))))
    in
      (Posix.IO.writeVec (file, Word8VectorSlice.full s'); ())
    end

  fun from_raw_sequence_with_set (qs : QSet.t, (gseq: gate Seq.t)) =
    let
      val (qubits, idx) = QSet.gen_seq_and_map qs
      fun q_to_qidx q = QMap.lookup (idx, q)
      fun qidx_to_q qidx = Seq.nth qubits qidx

      val nq = Seq.length qubits
      val gate_count = Seq.length gseq
      exception LayerFull of int
      fun gen_layers lest =
        (let
          val layers = Seq.tabulate (fn i => Seq.map (fn q => id_gate (q)) qubits) lest
          val frontier = Seq.tabulate (fn i => 0) nq
          fun loop idx nl =
            if idx = gate_count then nl
            else
              let
                val g = Seq.nth gseq idx
                val supp = gate_support_unordered g
                val layer_num = QSet.fold (fn (q, max) => Int.max (Seq.nth frontier (q_to_qidx q), max)) 0 supp
                val _ = if layer_num >= lest then (raise LayerFull (lest)) else ()
                (* val _ = if nl >= layer_num then () else (fill_layers (nl + 1, layer_num)) *)
                val layer = Seq.nth layers layer_num
                val _ =
                  QSet.foreach supp (fn q =>
                    let
                      val qidx = q_to_qidx q
                    in
                      (ArraySlice.update (layer, qidx, g); ArraySlice.update (frontier, qidx, 1 + layer_num))
                    end
                    )
              in
                loop (idx + 1) (Int.max (1 + layer_num, nl))
              end

          val nl = loop 0 0
        in
          Seq.take layers nl
        end) handle LayerFull lest => (gen_layers (2 * lest))
      val layers =
        (gen_layers (2 * (gate_count div nq) + 1))
    in
      {qset = qs, layers = layers, idx = idx, size = ref (Seq.length gseq)}
    end

  fun from_raw_sequence (nq, gseq) =
    from_raw_sequence_with_set ((QSet.from_seq (Qubit.enumerate nq)), gseq)

  fun from_raw_sequence_with_relabel ((nq, gseq), q_label) =
    let
      val qs = QSet.from_seq (Seq.tabulate (fn x => (q_label o Qubit.from_int) x) nq)
      fun relabel g =
        case g of
          P g => P (GateSet.map_support g q_label)
        | I q => I (q_label q)
      val gates = Seq.map relabel gseq
    in
      from_raw_sequence_with_set (qs, gates)
    end

  fun idx_inverse (c as {qset, layers, idx, ...}) =
    if size (c) = 0 then (fn x => Qubit.from_int x)
    else
      let
        val nq = num_qubits c
        fun get_act_qubit qidx =
          let
            val candidates = gate_support_unordered (Seq.nth (Seq.nth layers 0) qidx)
          in
            Option.valOf (QSet.find (fn q => QMap.lookup(idx, q) = qidx) candidates)
          end
      in
        Seq.nth (Seq.tabulate get_act_qubit nq)
      end

  fun num_layers {qset, layers, idx, ...} = Seq.length layers

  fun layer_size num_qubits idx ly =
    let
      fun loop qidx count qidx_set =
        if qidx = num_qubits then count
        else if IntSet.contains (qidx_set, qidx) then loop (qidx + 1) count qidx_set
        else if is_id (Seq.nth ly qidx) then loop (qidx + 1) count qidx_set
        else
          let
            val g = Seq.nth ly qidx
            val qidx_set' = IntSet.union (QSet.to_intset (fn q => QMap.lookup (idx, q)) (gate_support_unordered g), qidx_set)
          in
            loop (qidx + 1) (count + 1) qidx_set'
          end
    in
      loop 0 0 (IntSet.empty)
    end

  fun make_circuit qs idx (num_layers, f) =
    let
      val num_qubits = QSet.size qs
      (* fun layer_size ly =
        let
          fun loop qidx count qs =
            if qidx = num_qubits then count
            else if QSet.contains (qs, qidx) then loop (qidx + 1) count qs
            else if is_id (Seq.nth ly qidx) then loop (qidx + 1) count qs
            else
              let
                val g = Seq.nth ly qidx
                val qs' = QSet.fold (fn (q, qs') => QSet.add (qs', Seq.nth idx q)) qs (gate_support g)
              in
                loop (qidx + 1) (count + 1) (qs')
              end
        in
          loop 0 0 (QSet.empty)
        end *)
      fun tab_layer l = Seq.tabulate (fn qidx => f (l, qidx)) num_qubits

      val (sz, layers) =
        let
          val layers = Seq.tabulate tab_layer num_layers
          val layer_counts = Seq.map (layer_size num_qubits idx) layers
        in
          (Seq.reduce op+ 0 layer_counts, layers)
        end

    in
      {qset = qs, layers = layers, idx = idx, size = ref (sz)}
    end

  exception PatchUnimplemented


  fun patch_circuit (c : circuit) (ctxtfrontier : (Qubit.qubit -> int), ctxtsize : int) (nc : circuit) =
    let
      val support = support nc
      val num_layers_new = num_layers nc

      fun new_gate (l : int) (q : Qubit.qubit) =
        if l >= num_layers_new then id_gate (q)
        else gate nc (layer nc l) q

      fun non_id_depth q =
        let
          fun loop lidx =
            if lidx < 0 then 0
            else if is_id (gate nc (layer nc lidx) q) then loop (lidx - 1)
            else (lidx + 1)
        in
          loop (num_layers_new - 1)
        end

      fun replace_gates q =
        let
          val flen = ctxtfrontier q
          val _ = if (non_id_depth q) > flen then raise PatchUnimplemented else ()
          val qidx = QMap.lookup((#idx c), q)
          fun loop lidx =
            if lidx = flen then ()
            else (ArraySlice.update (layer c lidx, qidx, new_gate lidx q); loop (lidx + 1))
        in
          loop 0
        end

      val _ = QSet.foreach support replace_gates
      val szref = #size c
      val _ = (szref := !szref - ctxtsize + (size nc))
    in
      ()
    end



  exception InvalidIdx

  fun split (c : circuit) i =
    let
      val {qset, layers, idx, size} = c
      val nq = QSet.size qset
      val _ = if (Seq.length layers < i) then raise InvalidIdx else ()
      val (l1, l2) = (Seq.take layers i, Seq.drop layers i)

      val sub_size = fn l => Seq.reduce op+ 0 (Seq.map (layer_size nq idx) l)
      val (s1, s2) =
        if 2 * i < Seq.length layers then
          let
            val s1 = sub_size l1
          in
            (s1, !size - s1)
          end
        else
          let
            val s2 = sub_size l2
          in
            (!size - s2, s2)
          end

      val c1 = {qset = qset, layers = l1, idx = idx, size = ref s1}
      val c2 = {qset = qset, layers = l2, idx = idx, size = ref s2}
    in
      (c1, c2)
    end

  fun splitEnd c n =
    let
      val nl = num_layers c
    in
      if nl < n orelse n < 0 then raise InvalidIdx
      else split c (nl - n)
    end

  exception PrependIncompat
  fun prepend (c1 : circuit, c2: circuit) =
    let
      val {qset = q1, layers = l1, idx = idx1, size = s1} = c1
      val {qset = q2, layers = l2, idx = idx2, size = s2} = c2
      val compatible = QSet.is_subset (q1, q2) andalso (QSet.size q1 = QSet.size q2)
      val _ = if (not compatible) then raise PrependIncompat else ()
      val nq = QSet.size q1
      val (nl1, nl2) = (Seq.length l1, Seq.length l2)
      val qubits = QSet.to_seq q1

      val layer_fun =
        let
          val idx_cmp = Seq.map (fn q => QMap.lookup (idx1, q) = QMap.lookup (idx2, q)) qubits
          val idx_compat = Seq.reduce (fn (b1, b2) => b1 andalso b2) true idx_cmp
          val permute_layer =
            if idx_compat then (fn ly => ly)
            else
              let
                val s = ArraySlice.full (ForkJoin.alloc nq)
                val _ = QSet.foreach q2
                  (fn q => ArraySlice.update (s, QMap.lookup(idx2, q), QMap.lookup(idx1, q)))
              in
                (fn ly => Seq.tabulate (fn i => Seq.nth ly (Seq.nth s i)) nq)
              end
        in
          (fn i =>
            if i >= nl1 then Seq.nth l2 (i - nl1)
            else permute_layer (Seq.nth l1 i))
        end

      val layers = Seq.tabulate layer_fun (nl1 + nl2)
    in
      {qset = q2, layers = layers, idx = idx2, size = ref (!s1 + !s2)}
    end

  fun reindex (c' : raw_circuit, c : circuit) =
    let
      val get_idx = (fn x => Qubit.to_int x)
      val get_qubit = idx_inverse c
      val qrelabel = (get_qubit o get_idx)
      val c'' = from_raw_sequence_with_relabel (c', qrelabel)
    in
      c''
    end

end
