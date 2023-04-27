functor SupportClosedFun (structure Circuit : CIRCUIT) =
  struct
    (* gate type of this representation need not carry what qubits its working on *)
    type gate = Circuit.gate
    (* maybe layers can lists *)

    type layer = (QSet.t * gate) Seq.t
    type circuit = {qset : QSet.t, layers: layer Seq.t, idx : int QMap.map}


    exception InvalidQSet

    fun layer {qset, layers, idx} l = Seq.nth layers l
    fun gate (c as {qset, layers, idx}) (l, q) = Seq.nth (layer c l) (QMap.lookup (idx, q))
    fun num_layers (c as {qset, layers, idx}) = Seq.length layers

    fun closed_support (c : circuit) (l, q) =
      let
        val (supp, g) = (gate c (l, q))
      in
        supp
      end

    val gen_idx = QSet.gen_seq_and_map

    fun to_norm_circuit (c as {qset, layers, idx} : circuit) =
      let
        fun gateqidx (c as {qset, layers, idx}) (l, qidx) = Seq.nth (layer c l) qidx
      in
        Circuit.make_circuit qset idx (Seq.length layers, fn (l, qidx) => #2 (gateqidx c (l, qidx)))
      end

    datatype front_state = A | P of {idx : int, gate : QSet.t * gate}

    fun fr_to_idx d fs =
      case fs of
        A => d
      | P {idx, gate} => idx

    fun filter_and_map_idx (p: 'a -> bool) (f: int * 'a -> 'b) (s: 'a Seq.t): 'b Seq.t =
     ArraySlice.full (SeqBasis.filter 10000 (0, Seq.length s) (fn i => f (i, Seq.nth s i)) (p o Seq.nth s))

    fun subckt_feasible (c : circuit) (qs : QSet.t) =
      let
        val {qset, layers, idx} = c
        val _ = if QSet.is_subset (qs, qset) then () else raise InvalidQSet
        val nl = Seq.length layers
        fun check_qubit (lidx, q) =
          if lidx = nl then false
          else let
            val (supp, g) = gate (c) (lidx, q)
          in
            if Circuit.is_id g then check_qubit (lidx + 1, q)
            else if QSet.is_subset (supp, qs) then true
              (* ((print ("witness for qubit" ^ (Qubit.str q) ^ ("is gate " ^ (Circuit.gate_str g)) ^ " and are subsets = " ^ (QSet.str supp) ^ " " ^ (QSet.str qs) ^ "\n")); true) *)
            else false
          end
        val f = QSet.is_subset (qs, qset)
        val f' = f andalso (QSet.fold (fn (q, acc) => acc andalso check_qubit (0, q)) true qs)
      in
        f'
      end

    (* TODO: stop early when you know qs can't be included *)
    fun gen_max_subckt (c : circuit) (qs : QSet.t) =
      if not (subckt_feasible c qs) then NONE
      else let
        val {qset, layers, idx} = c

        val (qubits, idx') = gen_idx qs
        val num_qubits = (Seq.length qubits)
        val num_layers = Seq.length layers
        val frontier = Seq.tabulate (fn i => A) num_qubits
        exception LoopInvariant
        (* ll: list of layers,  lidx : next layer idx, sz : number of non_id gates, av : active qubits *)
        fun loop (ll, lidx) sz (aq : QSet.t) =
          if lidx = num_layers then
            (Seq.rev (Seq.fromList ll), sz)
          else let
            val new_layer = ArraySlice.full (ForkJoin.alloc num_qubits)

            fun qidx'_to_q qidx' = Seq.nth qubits qidx'
            fun q_to_qidx' q = QMap.lookup(idx', q)

            fun qidx'_to_qidx qidx' = QMap.lookup(idx, (qidx'_to_q qidx'))

            val _ = Seq.foreach frontier
              (fn (qidx', fr) =>
                case fr of
                  P {idx, gate} => (ArraySlice.update (new_layer, qidx', gate))
                | A =>
                  let
                    (* val _ = print ("layer = " ^ (Int.toString lidx) ^ " qidx' = " ^ (Int.toString qidx') ^ "\n") *)
                    (* val _ = print ("supp = " ^ (QSet.str supp)) *)
                    (* val (supp, og) = Seq.nth (Seq.nth layers frq) (qidx'_to_qidx qidx') *)

                    val q = qidx'_to_q qidx'
                    val extend = QSet.is_subset (closed_support c (lidx, q), aq)
                    val g =
                      if extend then (gate c (lidx, q))
                      else if lidx > 0 then
                        case ll of
                          l::_ => (#1(Seq.nth l qidx'), Circuit.id_gate (q))
                        | _ => raise LoopInvariant
                      else (QSet.singleton q, Circuit.id_gate (q))
                    val _ = ArraySlice.update (new_layer, qidx', g)
                    val _ = if (not extend) then ArraySlice.update (frontier, qidx', P({idx = lidx, gate =  g})) else ()
                  in
                    ()
                  end
              )

            val non_id_gates =
              DelayedSeq.reduce op+ 0 (DelayedSeq.map (fn (supp, g) => if Circuit.is_id g then 0 else 1) (DelayedSeq.fromArraySeq new_layer))

            val active_seq = filter_and_map_idx (fn fr => case fr of A => true | _ => false) (fn (qidx', _) => qidx'_to_q qidx') frontier
            val aq = QSet.from_seq active_seq

            (* (Seq.reduce (Int.max) 0 (Seq.map (fr_to_idx (num_layers + 1)) frontier)) = (num_layers + 1) *)
            (* val lidx = *)
              (* DelayedSeq.reduce (Int.min) (num_layers) (DelayedSeq.map (fn fr => case fr of P _ => num_layers | A x => x) (DelayedSeq.fromArraySeq frontier)) *)
          in
            if (QSet.size aq > 0) then loop ((new_layer::ll), (lidx + 1)) (sz + non_id_gates)  aq
            else (Seq.rev (Seq.fromList ll), sz + non_id_gates)
          end
        val (layers, sz) = loop ([], 0) 0 qs
        val gate_len = Seq.map (fr_to_idx num_layers) frontier
        val c' = {qset = qs, layers = layers, idx = idx'}
      in
        SOME (c', sz, fn q => Seq.nth gate_len (QMap.lookup (idx', q)))
      end

    fun parse (c : Circuit.circuit) max_size =
      let
        val num_layers = Circuit.num_layers c
        val (qubits, idx) = gen_idx (Circuit.support c)
        val n = Seq.length qubits
        (* maps q \in qubits to [0, ... n - 1] *)
        val csizes = Seq.tabulate (fn i => 0) n
        fun layeri i = Circuit.layer c i
        fun act_qubit qidx = Seq.nth qubits qidx
        fun gateqidx l qidx = Circuit.gate c l (act_qubit qidx)

        fun create_layers i min ll =
          if min = max_size orelse i = num_layers then Seq.rev (Seq.fromList ll)
          else
            let
              val layer = layeri i
              (* skip qbits which have exceeded size limit or have an identity gate *)
              fun scope qidx = Seq.nth csizes qidx < max_size andalso not (Circuit.is_id (gateqidx layer qidx))
              val new_layer = Seq.tabulate
                (fn qidx =>
                  let
                    val in_scope = true
                    val g =
                      if in_scope then gateqidx layer qidx
                      else (Circuit.id_gate (act_qubit qidx))
                    (* val _ = print ("parse layer = " ^ (Int.toString i) ^ " qidx = " ^ (Int.toString qidx) ^ "\n") *)
                    val (supp : QSet.t) =
                      case ll of
                        nil => Circuit.gate_support_unordered g
                      | l::_ =>
                        let
                          val curr_supp = (Circuit.gate_support_unordered g)
                          fun past_suppq q = #1 (Seq.nth l (QMap.lookup (idx, q)))
                          val past_supp = QSet.fold (fn (q, qss) => QSet.union (past_suppq q, qss)) QSet.empty curr_supp
                        in
                          QSet.union (curr_supp, past_supp)
                        end
                    (* val _ = print ("i = " ^ (Int.toString i) ^ "\n") *)
                    (* val _ = print ("parse gate = " ^ (Circuit.gate_str g) ^ "parse supp = " ^ (QSet.str supp) ^ "\n") *)
                  in
                    (supp, g)
                  end
                ) n
              val _ = Seq.foreach csizes
                (fn (qidx, sz) =>
                  if scope qidx then
                    let
                      val g = gateqidx layer qidx
                      val supp = Circuit.gate_support_unordered g
                      val new_size = 1 + (QSet.fold (fn (q', acc) => Int.max (acc, Seq.nth csizes (QMap.lookup (idx, q')))) 0 supp)
                    in
                      ArraySlice.update (csizes, qidx, new_size)
                    end
                  else ())
              val min_size = Seq.reduce (Int.min) max_size csizes
            in
              create_layers (i + 1) min_size (new_layer::ll)
            end
        val layers = create_layers 0 0 []
      in
        {qset = Circuit.support c, layers = layers, idx =idx}
      end
  end