structure CLA = CommandLineArgs

structure TwoGateSet : GATE_SET =
struct
  exception InvalidGate
  exception Unimplemented
  open Math

  type qubit = Qubit.qubit

  datatype gate =
    H of qubit
  | S of qubit
  | T of qubit
  | HD of qubit
  | SD of qubit
  | TD of qubit
  | CNOT of qubit * qubit
  | CCZ of qubit * qubit * qubit
  | RZ of (string * qubit)

  fun map_support g fidx =
    case g of
      H(x) => H (fidx x)
    | S(x) => S (fidx x)
    | T(x) => T (fidx x)
    | HD(x) => HD (fidx x)
    | SD(x) => SD (fidx x)
    | TD(x) => TD (fidx x)
    | CNOT (x, y) => CNOT (fidx x, fidx y)
    | CCZ (x, y, z) => CCZ (fidx x, fidx y, fidx z)
    | RZ (s, x) => RZ (s, fidx x)

  fun support g =
    case g of
      H(x) => [x]
    | S(x) => [x]
    | T(x) => [x]
    | HD(x) => [x]
    | SD(x) => [x]
    | TD(x) => [x]
    | CNOT (x, y) => [x, y]
    | CCZ (x, y, z) => [x, y, z]
    | RZ(_, x) => [x]

  fun labelToGate g =
    case g of
      ("h", [x]) => H (x)
    | ("s", [x]) => S (x)
    | ("t", [x]) => T (x)
    | ("hdg", [x]) => HD (x)
    | ("sdg", [x]) => SD (x)
    | ("tdg", [x]) => TD (x)
    | ("cx", [x, y]) => CNOT (x, y)
    | ("ccz", [x, y, z]) => CCZ (x, y, z)
    | (rz_some, [x]) =>
      if (String.isPrefix "rz" rz_some) then RZ(rz_some, x)
      else labelToGate (rz_some, [x, x])
    | _ => (print (#1 g ^ " " ^ (Int.toString (List.length (#2 g))) ^ "\n"); raise InvalidGate)

  fun str g =
    case g of
      H(x) => "h q[" ^ (Qubit.str x) ^ "]"
    | S(x) => "s q[" ^ (Qubit.str x) ^ "]"
    | T(x) => "t q[" ^ (Qubit.str x) ^ "]"
    | HD(x) => "hdg q[" ^ (Qubit.str x) ^ "]"
    | SD(x) => "sdg q[" ^ (Qubit.str x) ^ "]"
    | TD(x) => "tdg q[" ^ (Qubit.str x) ^ "]"
    | CNOT(x, y) => "cx q[" ^ (Qubit.str x) ^ "], q[" ^ (Qubit.str y) ^ "]"
    | CCZ(x, y, z) => "ccz q[" ^ (Qubit.str x) ^ "], q[" ^ (Qubit.str y) ^ "], q[" ^ (Qubit.str z) ^ "]"
    | RZ(s, x) => s ^ " q[" ^ (Qubit.str x) ^ "]"

  fun inverse g =
    case g of
      H(x) => HD(x)
    | S(x) => SD(x)
    | T(x) => TD(x)
    | HD(x) => H(x)
    | SD(x) => S(x)
    | TD(x) => T(x)
    | CNOT _ => g
    | CCZ _ => g
    | RZ _ => raise Unimplemented

  val num_gates = 7

  val gate_matrix =
    let
      val (cos45, sin45) = (sqrt(0.5), sqrt(0.5))
      val x = pi/(8.0)
      val (one, z) = ((1.0, 0.0), (0.0, 0.0))
      val hm = ComplexMatrix.fromList [[( cos45, 0.0), (sin45, 0.0)], [(cos45, 0.0), (~sin45, 0.0)]]
      val sm = ComplexMatrix.fromList [[one, z], [z, (0.0, 1.0)]]
      val tm = ComplexMatrix.fromList [[one, z], [z, (cos45, sin45)]]
      val hdm = ComplexMatrix.dagger hm
      val sdm = ComplexMatrix.dagger sm
      val tdm = ComplexMatrix.dagger tm
      val cnotm = ComplexMatrix.fromList [[one, z, z, z], [z, one, z, z], [z, z, z, one], [z, z, one, z]]
      val cczm =
        let
          fun eij (i, j) =
            if i <> j then z
            else if i < 7 then one
            else (~1.0, 0.0)
        in
          ComplexMatrix.tabulate (8, 8) eij
        end
    in
      fn g =>
        case g of
          H _ => hm
        | S _ => sm
        | T _ => tm
        | HD _ => hdm
        | SD _ => sdm
        | TD _ => tdm
        | CNOT _ => cnotm
        | CCZ _ => cczm
        | RZ _ => raise Unimplemented
    end

end


structure Experiment =
struct
  structure TwoOPT = CircuitOPT (structure GateSet = TwoGateSet)
  val cqasm = CLA.parseString "circuit" "test-small.qasm"
  val print_out = CLA.isArg "outfile"
  val outfile = CLA.parseString "outfile" "out.qasm"

  fun optimize () =
    let
      val c = TwoOPT.from_qasm cqasm
      val c' = Benchmark.run "optimizing" (fn _ => TwoOPT.optimize c)
      val _ = (print ("shrank circuit by " ^ (Int.toString (TwoOPT.size c - TwoOPT.size c') ^ "\n"));
      print ("new size =  " ^ (Int.toString (TwoOPT.size c') ^ "\n")))
    in
      if print_out then TwoOPT.dump c' outfile
      else ()
    end

  fun preprocess () =
    let
      val c = TwoOPT.from_qasm cqasm
    in
      if print_out then TwoOPT.dump c outfile
      else ()
    end

end


val _ = if CLA.isArg "preprocess" then Experiment.preprocess ()
        else Experiment.optimize()

(* val _ = TwoOPT.cprint c *)
(* val _ = print(ComplexMatrix.str (TwoOPT.eval_circuit c)) *)
(* val _ = print(ComplexMatrix.str (TwoOPT.eval_circuit c)) *)
(* val _ = print "after circuit\n" *)
(* val _ = print (ComplexMatrix.str (TwoOPT.eval_circuit c')) *)
