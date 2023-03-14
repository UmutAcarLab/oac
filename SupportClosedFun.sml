functor SupportClosedFun (structure Circuit : CIRCUIT) =
  struct
    (* gate type of this representation need not carry what qubits its working on *)
    type gate = Circuit.gate
    (* maybe layers can lists *)

    type layer = (QSet.t * gate) Seq.t
    type circuit = {qset : QSet.t, layers: layer Seq.t, idx : int Seq.t}


    exception InvalidQSet

    fun layer {qset, layers, idx} l = Seq.nth layers l
    fun gate (c as {qset, layers, idx}) (l, q) = Seq.nth (layer c l) (Seq.nth idx q)
    fun closed_support (c : circuit) (l, q) =
      let
        val (supp, g) = (gate c (l, q))
      in
        supp
      end

    fun gen_idx qs =
      let
        val max_qubit = QSet.fold (Int.max) 0 qs
        val bools = Seq.tabulate (fn i => if QSet.contains (qs, i) then 1 else 0) (1 + max_qubit)
      in
        #1 (Seq.scan op+ 0 bools)
      end

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

    fun gen_max_subckt (c : circuit) (qs : QSet.t) =
      let
        val {qset, layers, idx} = c
        val _ = if QSet.is_subset (qs, qset) then () else raise InvalidQSet

        val qubits = QSet.to_seq qs
        val num_qubits = (Seq.length qubits)
        val num_layers = Seq.length layers
        val frontier = Seq.tabulate (fn i => A) num_qubits
        val idx' = gen_idx qs

        exception LoopInvariant
        (* ll: list of layers,  lidx : next layer idx, sz : number of non_id gates, av : active qubits *)
        fun loop (ll, lidx) sz (aq : QSet.t) =
          if lidx = num_layers then
            (Seq.rev (Seq.fromList ll), sz)
          else let
            val new_layer = ArraySlice.full (ForkJoin.alloc num_qubits)

            fun qidx'_to_q qidx' = Seq.nth qubits qidx'
            fun q_to_qidx' q =  Seq.nth idx' q

            fun qidx'_to_qidx qidx' =  Seq.nth idx (qidx'_to_q qidx')

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

            val active_seq = Seq.mapIdx (fn (qidx', fr) => case fr of A => qidx'_to_q qidx' | _ => ~1) frontier
            val aq = (QSet.from_seq (Seq.filter (fn x => x >= 0) active_seq))

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
        (c', sz, fn q => Seq.nth gate_len (Seq.nth idx' q))
      end

    fun parse (c : Circuit.circuit) max_size =
      let
        val num_layers = Circuit.num_layers c
        val qubits = QSet.to_seq (Circuit.support c)
        val n = Seq.length qubits
        (* maps q \in qubits to [0, ... n - 1] *)
        val idx = gen_idx (Circuit.support c)
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
                    val supp =
                      case ll of
                        nil => Circuit.gate_support g
                      | l::_ => QSet.union (Circuit.gate_support g, #1 (Seq.nth l qidx))
                    (* val _ = print ("parse supp = " ^ (QSet.str supp)) *)
                  in
                    (supp, g)
                  end
                ) n
              val _ = Seq.foreach csizes
                (fn (qidx, sz) =>
                  if scope qidx then
                    let
                      val g = gateqidx layer qidx
                      val supp = Circuit.gate_support g
                      val new_size = 1 + (QSet.fold (fn (q', acc) => Int.max (acc, Seq.nth csizes (Seq.nth idx q'))) 0 supp)
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