
functor CircuitFun (structure GateSet : GATE_SET) : CIRCUIT =
struct
  exception Unimplemented

  datatype gate = P of GateSet.gate | I of int
  type layer = gate Seq.t
  type circuit = {qset : QSet.t, layers: layer Seq.t, idx : int Seq.t, size : int ref}

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

  fun gate {qset, layers, idx, ...} l q =
    let
      val qidx = (Seq.nth idx q)
    in
      Seq.nth l qidx
    end


  fun gate_support g =
    case g of
      I x => QSet.singleton (x)
    | P g => GateSet.support g


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


  fun eval_circuit (c as {qset, layers, idx, size}) = raise Unimplemented
    (* let

      val nq = num_qubits c

      (* cnot (0, 3), x(1), x(2) *)
      (* perm(1, 3) * cnot (0, 1) * perm (1, 3),  x(1), x(2) *)
      fun eval_layer l =
        let
          val perm =
            let
              val p = Seq.tabulate (fn i => i) nq
              fun loop q =
                if q = nq then ()
                else
                  let
                    val g =
                  in
                    body
                  end
            in

            end

        in
          body
        end

      val ml = Seq.map eval_layer layers
    in
      Seq.reduce Matrix.* Matrix.id(size c) ml
    end *)

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
              val qs = gate_support g
              val _ = QSet.foreach qs (fn q =>
                (* (print ("q = " ^ (Int.toString q) ^ "\n"); print (" qidx = " ^ (Int.toString (Seq.nth idx q) ^ "\n"));  *)
                ArraySlice.update (done, Seq.nth idx q, true))
                (* ) *)
            in
              case g of
                P g' => loop (qidx + 1) (g'::acc)
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

  fun cprint (c : circuit) =
    let
      val {qset, layers, idx, ...} = c
      val (nq, gqasm) = to_raw_sequence c
      val gstr = Seq.map GateSet.str gqasm
      val str = Seq.reduce (fn (a, b) => a ^ "\n" ^ b) "" gstr
    in
      print ("circuit  = " ^ str ^ "\n")
    end

  fun gen_idx qs =
    let
      val qubits = QSet.to_seq qs
      val max_qubit = Seq.reduce (Int.max) 0 qubits
      val bools = Seq.tabulate (fn i => if QSet.contains (qs, i) then 1 else 0) (1 + max_qubit)
    in
      (qubits, #1 (Seq.scan op+ 0 bools))
    end

  fun from_raw_sequence_with_set (qs : QSet.t, (gseq: gate Seq.t)) =
    let
      val (qubits, idx) = gen_idx qs
      fun q_to_qidx q = Seq.nth idx q
      fun qidx_to_q qidx = Seq.nth qubits qidx

      val nq = Seq.length qubits
      val gate_count = Seq.length gseq
      exception LayerFull of int
      fun gen_layers lest =
        let
          val layers = Seq.tabulate (fn i => Seq.map (fn q => id_gate (q)) qubits) lest
          val frontier = Seq.tabulate (fn i => 0) nq
          fun loop idx nl =
            if idx = gate_count then nl
            else
              (let
                (* fun fill_layers (st, last) =
                  if (st > last) then ()
                  else (ArraySlice.update (layers, st, Seq.tabulate (fn q => id_gate (q)) nq); fill_layers (st + 1, last)) *)

                val g = Seq.nth gseq idx
                val supp = gate_support g
                val layer_num = QSet.fold (fn (q, max) => Int.max (Seq.nth frontier (q_to_qidx q), max)) 0 supp
                val _ = if layer_num >= lest then raise LayerFull (lest) else ()
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
              end)

          val nl = loop 0 0
        in
          Seq.take layers nl
        end
      val layers =
        (gen_layers (2 * (gate_count div nq)))
        handle LayerFull lest => gen_layers (2 * lest)
    in
      {qset = qs, layers = layers, idx = idx, size = ref (Seq.length gseq)}
    end

  fun from_raw_sequence (nq, gseq) =
    from_raw_sequence_with_set ((QSet.from_seq (Seq.tabulate (fn x => x) nq)), gseq)

  fun load_circuit f =
    let
      val (sgc, circuit) = ParseQASM.readQASM f
      fun to_circ_gate g = P (g)
    in
      Seq.map (to_circ_gate o GateSet.labelToGate) circuit
    end

  fun num_layers {qset, layers, idx, ...} = Seq.length layers

  fun make_circuit qs idx (num_layers, f) =
    let
      val num_qubits = QSet.size qs
      fun layer_size ly =
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
        end
      fun tab_layer l = Seq.tabulate (fn qidx => f (l, qidx)) num_qubits

      val (sz, layers) =
        let
          val layers = Seq.tabulate tab_layer num_layers
          val layer_counts = Seq.map layer_size layers
        in
          (Seq.reduce op+ 0 layer_counts, layers)
        end

    in
      {qset = qs, layers = layers, idx = idx, size = ref (sz)}
    end

  exception PatchUnimplemented


  fun patch_circuit (c : circuit) (ctxtfrontier : (int -> int), ctxtsize : int) (nc : circuit) =
    let
      val support = support nc
      val num_layers_new = num_layers nc

      fun new_gate (l : int) (q : int) =
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
          val qidx = Seq.nth (#idx c) q
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

  fun eval_raw_sequence s = raise Unimplemented
end
