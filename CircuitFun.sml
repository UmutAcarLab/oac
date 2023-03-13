
functor CircuitFun (structure GateSet : GATE_SET) : CIRCUIT =
struct
  exception Unimplemented

  datatype gate = P of GateSet.gate | I of int
  type layer = gate Seq.t
  type circuit = {qset : QSet.t, layers: layer Seq.t, idx : int Seq.t}

  fun is_id g =
    case g of
      P _ => false
    | I _ => true

  fun labelToGate (s, ql) =
    case s of
      "I" => I (List.hd ql)
    | _ =>  P (GateSet.labelToGate (s, ql))

  fun id_gate x = I (x)

  fun layer {qset, layers, idx} x = Seq.nth layers x
  fun init_layer n = ArraySlice.full (ForkJoin.alloc n)

  fun gate {qset, layers, idx} l q =
    let
      val qidx = (Seq.nth idx q)
    in
      Seq.nth l qidx
    end


  fun gate_support g =
    case g of
      I x => QSet.singleton (x)
    | P g => GateSet.support g

  fun size (c : circuit) = raise Unimplemented

  fun num_qubits (c : circuit) = QSet.size (#qset c)
  fun support (c : circuit) = (#qset c)

  fun eval_circuit _ = raise Unimplemented

  fun to_raw_sequence (c : circuit) =
    let
      val {qset, layers, idx} = c
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
              val _ = QSet.foreach qs (fn q => ArraySlice.update (done, Seq.nth idx q, true))
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

  fun cprint (c) =
    let
      val {qset, layers, idx} = c
      val (nq, gqasm) = to_raw_sequence c
      val gstr = Seq.map GateSet.str gqasm
      val str = Seq.reduce (fn (a, b) => a ^ "\n" ^ b) "" gstr
    in
      print ("circuit  = " ^ str ^ "\n")
    end

  fun from_raw_sequence (nq, (gseq: gate Seq.t)) =
    let
      val idx = Seq.tabulate (fn i => i) nq
      val gate_count = Seq.length gseq
      val _ = print ("gate count = " ^ (Int.toString gate_count) ^ "\n")
      fun gen_layers lest =
        let
          exception LayerFull
          val layers = Seq.tabulate (fn i => Seq.tabulate (fn q => id_gate (q)) nq) lest
          val frontier = Seq.tabulate (fn i => 0) nq
          fun loop idx nl =
            if idx = gate_count then nl
            else
              let
                (* fun fill_layers (st, last) =
                  if (st > last) then ()
                  else (ArraySlice.update (layers, st, Seq.tabulate (fn q => id_gate (q)) nq); fill_layers (st + 1, last)) *)

                val g = Seq.nth gseq idx
                val supp = gate_support g
                val layer_num = QSet.fold (fn (max, q) => Int.max (Seq.nth frontier q, max)) 0 supp
                val _ = if layer_num >= lest then raise LayerFull else ()
                (* val _ = if nl >= layer_num then () else (fill_layers (nl + 1, layer_num)) *)
                val layer = Seq.nth layers layer_num
                val _ = QSet.foreach supp (fn q => (ArraySlice.update (layer, q, g); ArraySlice.update (frontier, q, 1 + layer_num)))
              in
                loop (idx + 1) (Int.max (1 + layer_num, nl))
              end
          (* handle LayerFull => gen_layers (2 * lest) *)

          val nl = loop 0 0
        in
          Seq.take layers nl
        end
      val layers = gen_layers (2 * (gate_count div nq))
    in
      {qset = QSet.from_seq idx, layers = layers, idx = idx}
    end

  fun load_circuit f =
    let
      val (sgc, circuit) = ParseQASM.readQASM f
      fun to_circ_gate g = P (g)
    in
      Seq.map (to_circ_gate o GateSet.labelToGate) circuit
    end

  fun num_layers {qset, layers, idx} = Seq.length layers

  fun make_circuit qs idx (num_layers, f) =
    let
      val num_qubits = Seq.length idx
      fun tab_layer l = Seq.tabulate (fn qidx => f (l, qidx)) num_qubits
      val layers = Seq.tabulate tab_layer num_layers
    in
      {qset = qs, layers = layers, idx = idx}
    end

  fun patch_circuit (c : circuit) (ctxtfrontier : int -> int) (nc : circuit) =
    let
      val support = support nc
      val num_layers_new = num_layers nc

      fun new_gate (l : int) (q : int) =
        if l >= num_layers_new then id_gate (q)
        else gate nc (layer nc l) q

      fun replace_gates q =
        let
          val flen = ctxtfrontier q
          val qidx = Seq.nth (#idx c) q
          fun loop lidx =
            if lidx = flen then ()
            else (ArraySlice.update (layer c lidx, qidx, new_gate lidx q); loop (lidx + 1))
        in
          loop 0
        end
    in
      QSet.foreach support replace_gates
    end

  fun eval_raw_sequence s = raise Unimplemented
end
