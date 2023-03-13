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

    fun gen_map_seq qs max_qubit =
      let
        val bools = Seq.tabulate (fn i => if QSet.contains (qs, i) then 1 else 0) max_qubit
        val (f, offs) = Seq.scan op+ 0 bools
      in
        f
      end

    fun to_norm_circuit (c as {qset, layers, idx} : circuit) =
      let
        fun gateqidx (c as {qset, layers, idx}) (l, qidx) = Seq.nth (layer c l) qidx
      in
        Circuit.make_circuit qset idx (Seq.length layers, fn (l, qidx) => #2 (gateqidx c (l, qidx)))
      end

    fun gen_max_subckt (c : circuit) (qs : QSet.t) =
      let
        val {qset, layers, idx} = c
        val _ = if QSet.is_subset (qs, qset) then () else raise InvalidQSet

        val qubits = QSet.to_seq qs
        val num_qubits = (Seq.length qubits)
        val frontier = Seq.tabulate (fn i => 0) num_qubits

        val num_layers = Seq.length layers

        val idx' =
          let
            val max_qubit = Seq.reduce (Int.max) 0 qubits
          in
            gen_map_seq qs (1 + max_qubit)
          end

        fun loop ll sz =
          let
            fun scope qidx = Seq.nth frontier qidx >= 0
            val new_layer = ArraySlice.full (ForkJoin.alloc num_qubits)

            fun qidx'_to_q qidx' = Seq.nth qubits qidx'
            fun qidx'_to_qidx qidx' =  Seq.nth idx (qidx'_to_q qidx')

            val _ = Seq.foreach frontier
              (fn (qidx', frq) =>
                let
                  val in_scope = scope qidx'
                  val (supp, og) = Seq.nth (Seq.nth layers frq) (qidx'_to_qidx qidx')
                  val (g, bump) =
                    let
                      val common = QSet.intersect (supp, qs)
                      val sz = QSet.size common
                    in
                      if (in_scope andalso sz = QSet.size supp) then ((supp, og), true)
                      else ((common, Circuit.id_gate (qidx'_to_q qidx')), false)
                    end
                  val _ = ArraySlice.update (new_layer, qidx', g)
                in
                  if bump andalso (frq < num_layers - 1) then ArraySlice.update (frontier, qidx', 1 + frq)
                  else (ArraySlice.update (frontier, qidx', ~1))
                end
              )
            val non_id_gates = DelayedSeq.reduce op+ 0 (DelayedSeq.map (fn (supp, g) => if Circuit.is_id g then 0 else 1) (DelayedSeq.fromArraySeq new_layer))
            val some_in_scope =
              DelayedSeq.reduce (fn (a, b) => a orelse b) false (DelayedSeq.map (fn fr => fr >= 0) (DelayedSeq.fromArraySeq frontier))
          in
            if some_in_scope then loop (new_layer::ll) (sz + non_id_gates)
            else (Seq.fromList (new_layer::ll), sz + non_id_gates)
          end
        val (layers, sz) = loop [] 0
        val c' = {qset = qs, layers = layers, idx = idx'}
      in
        (c', sz, fn q => Seq.nth frontier (Seq.nth idx' q))
      end

    fun parse (c : Circuit.circuit) max_size =
      let
        val num_layers = Circuit.num_layers c
        val qubits = QSet.to_seq (Circuit.support c)
        val n = Seq.length qubits
        (* maps q \in qubits to [0, ... n - 1] *)
        val idx =
          let
            val max_qubit = Seq.reduce (Int.max) 0 qubits
          in
            gen_map_seq (Circuit.support c) (1 + max_qubit)
          end

        val csizes = Seq.tabulate (fn i => 0) n
        fun layeri i = Circuit.layer c i
        fun act_qubit qidx = Seq.nth qubits qidx
        fun gateqidx l qidx = Circuit.gate c l (act_qubit qidx)

        fun create_layers i min ll =
          if min = max_size orelse i = num_layers then Seq.fromList ll
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
                    val supp = QSet.empty
                      (* case ll of
                        nil => Circuit.gate_support g
                      | l::_ => QSet.union (Circuit.gate_support g, #1 (Seq.nth l qidx)) *)
                  in
                    (supp, g)
                  end
                ) n
              val _ = print ("new_layer done\n")
              val _ = Seq.foreach csizes
                (fn (qidx, sz) =>
                  if scope qidx then
                    let
                      val g = gateqidx layer qidx
                      val supp = Circuit.gate_support g
                      val new_size = 1 + (QSet.fold (fn (acc, q') => Int.max (acc, Seq.nth csizes (Seq.nth idx q'))) 0 supp)
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