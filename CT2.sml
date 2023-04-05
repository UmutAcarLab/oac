structure CLA = CommandLineArgs

structure TwoGateSet : GATE_SET =
struct
  exception InvalidGate
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

  fun map_support g fidx =
    case g of
      H(x) => H (fidx x)
    | S(x) => S (fidx x)
    | T(x) => T (fidx x)
    | HD(x) => HD (fidx x)
    | SD(x) => SD (fidx x)
    | TD(x) => TD (fidx x)
    | CNOT (x, y) => CNOT (fidx x, fidx y)

  fun support g =
    case g of
      H(x) => [x]
    | S(x) => [x]
    | T(x) => [x]
    | HD(x) => [x]
    | SD(x) => [x]
    | TD(x) => [x]
    | CNOT (x, y) => [x, y]

  fun labelToGate g =
    case g of
      ("h", [x]) => H (x)
    | ("s", [x]) => S (x)
    | ("t", [x]) => T (x)
    | ("hdg", [x]) => HD (x)
    | ("sdg", [x]) => SD (x)
    | ("tdg", [x]) => TD (x)
    | ("cx", [x, y]) => CNOT (x, y)
    | _ => (print (#1 g ^ " " ^ (Int.toString (List.length (#2 g))) ^ "\n"); raise InvalidGate)

  fun str g =
    case g of
      H(x) => "h (" ^ (Qubit.str x) ^ ")"
    | S(x) => "s (" ^ (Qubit.str x) ^ ")"
    | T(x) => "t (" ^ (Qubit.str x) ^ ")"
    | HD(x) => "hdg (" ^ (Qubit.str x) ^ ")"
    | SD(x) => "sdg (" ^ (Qubit.str x) ^ ")"
    | TD(x) => "tdg (" ^ (Qubit.str x) ^ ")"
    | CNOT(x, y) => "cx (" ^ (Qubit.str x) ^ ", " ^ (Qubit.str y) ^ ")"

  fun inverse g =
    case g of
      H(x) => HD(x)
    | S(x) => SD(x)
    | T(x) => TD(x)
    | HD(x) => H(x)
    | SD(x) => S(x)
    | TD(x) => T(x)
    | CNOT _ => g

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
    end

end


structure TwoOPT = CircuitOPT (structure GateSet = TwoGateSet)
val f = CLA.parseString "circuit" "test-small.qasm"
val c = TwoOPT.from_qasm f
val _ = TwoOPT.cprint c
(* val _ = print(ComplexMatrix.str (TwoOPT.eval_circuit c)) *)
val c' = TwoOPT.optimize c

val _ = print "before circuit\n"
val _ = print(ComplexMatrix.str (TwoOPT.eval_circuit c))
val _ = print "after circuit\n"
val _ = print (ComplexMatrix.str (TwoOPT.eval_circuit c'))
