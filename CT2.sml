structure CLA = CommandLineArgs

structure TwoGateSet : GATE_SET =
struct
  exception InvalidGate
  open Math

  datatype gate =
    H of int
  | S of int
  | T of int
  | HD of int
  | SD of int
  | TD of int
  | CNOT of int * int

  fun support g =
    case g of
      H(x) => QSet.from_list ([x])
    | S(x) => QSet.from_list ([x])
    | T(x) => QSet.from_list ([x])
    | HD(x) => QSet.from_list ([x])
    | SD(x) => QSet.from_list ([x])
    | TD(x) => QSet.from_list ([x])
    | CNOT (x, y) => QSet.from_list ([x, y])

  fun labelToGate g =
    case g of
        ("h", [x]) => H (x)
      | ("s", [x]) => S (x)
      | ("t", [x]) => T (x)
      | ("hdg", [x]) => HD (x)
      | ("sdg", [x]) => SD (x)
      | ("tdg", [x]) => TD (x)
      | ("cnot", [x, y]) => CNOT (x, y)
      | _ => (print (#1 g ^ " " ^ (Int.toString (List.length (#2 g))) ^ "\n"); raise InvalidGate)

  fun str g =
    case g of
      H(x) => "h (" ^ (Int.toString x) ^ ")"
    | S(x) => "s (" ^ (Int.toString x) ^ ")"
    | T(x) => "t (" ^ (Int.toString x) ^ ")"
    | HD(x) => "hdg (" ^ (Int.toString x) ^ ")"
    | SD(x) => "sdg (" ^ (Int.toString x) ^ ")"
    | TD(x) => "tdg (" ^ (Int.toString x) ^ ")"
    | CNOT(x, y) => "cnot (" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")"

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
      val hm = ComplexMatrix.fromList [[(0.0, cos45), (0.0, sin45)], [(0.0, cos45), (0.0, ~sin45)]]
      val sm = ComplexMatrix.fromList [[(cos45, ~sin45), z], [z, (cos45, sin45)]]
      val tm = ComplexMatrix.fromList [[(cos(x), ~(sin(x))), z], [z, (cos(x), sin(x))]]
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
val c' = TwoOPT.optimize c
