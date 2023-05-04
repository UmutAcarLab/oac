structure CLA = CommandLineArgs

structure TwoGateSet : GATE_SET =
struct
  exception InvalidGate
  exception Unimplemented
  open Math

  type qubit = Qubit.qubit

  structure Unint =
  struct
    datatype t =
      RZ of (string * qubit)
    | RX of (string * qubit)
    | U1 of (string * qubit)

    fun map_support u fidx =
      case u of
        RZ (s, x) => RZ (s, fidx x)
      | RX (s, x) => RX (s, fidx x)
      | U1 (s, x) => U1 (s, fidx x)

    fun support u =
      case u of
        RZ(_, x) => [x]
      | RX(_, x) => [x]
      | U1(_, x) => [x]

    fun labelToGate g =
      case g of
        (r_some, [x]) =>
          if (String.isPrefix "rz" r_some) then SOME (RZ(r_some, x))
          else if (String.isPrefix "u1" r_some) then SOME (U1(r_some, x))
          else if (String.isPrefix "rx" r_some) then SOME (RX(r_some, x))
          else NONE
      | _ => NONE
    fun str u =
      case u of
        RZ(s, x) => s ^ " q[" ^ (Qubit.str x) ^ "]"
      | RX(s, x) => s ^ " q[" ^ (Qubit.str x) ^ "]"
      | U1(s, x) => s ^ " q[" ^ (Qubit.str x) ^ "]"
  end


  datatype gate =
    H of qubit
  | S of qubit
  | T of qubit
  | HD of qubit
  | SD of qubit
  | TD of qubit
  | X of qubit
  | CNOT of qubit * qubit
  | CCZ of qubit * qubit * qubit
  | UNINT of Unint.t



  fun map_support g fidx =
    case g of
      H(x) => H (fidx x)
    | S(x) => S (fidx x)
    | T(x) => T (fidx x)
    | HD(x) => HD (fidx x)
    | SD(x) => SD (fidx x)
    | TD(x) => TD (fidx x)
    | X (x) => X (fidx x)
    | CNOT (x, y) => CNOT (fidx x, fidx y)
    | CCZ (x, y, z) => CCZ (fidx x, fidx y, fidx z)
    | UNINT x => UNINT (Unint.map_support x fidx)

  fun support g =
    case g of
      H(x) => [x]
    | S(x) => [x]
    | T(x) => [x]
    | HD(x) => [x]
    | SD(x) => [x]
    | TD(x) => [x]
    | X(x) => [x]
    | CNOT (x, y) => [x, y]
    | CCZ (x, y, z) => [x, y, z]
    | UNINT x => Unint.support x

  fun labelToGate g =
    case g of
      ("h", [x]) => H (x)
    | ("s", [x]) => S (x)
    | ("t", [x]) => T (x)
    | ("hdg", [x]) => HD (x)
    | ("sdg", [x]) => SD (x)
    | ("tdg", [x]) => TD (x)
    | ("x", [x]) => X (x)
    | ("cx", [x, y]) =>  (if (not (Qubit.eq (x, y))) then () else (print "x =y label\n";raise InvalidGate); CNOT (x, y))
    | ("ccz", [x, y, z]) => CCZ (x, y, z)
    | x =>
      case Unint.labelToGate x of
        SOME y => UNINT (y)
      | NONE => (print (#1 g ^ " " ^ (Int.toString (List.length (#2 g))) ^ "\n"); raise InvalidGate)
(*
    (rz_some, [x]) =>
      if (String.isPrefix "rz" rz_some) then RZ(rz_some, x)
      else labelToGate (rz_some, [x, x]) *)

  fun str g =
    case g of
      H(x) => "h q[" ^ (Qubit.str x) ^ "]"
    | S(x) => "s q[" ^ (Qubit.str x) ^ "]"
    | T(x) => "t q[" ^ (Qubit.str x) ^ "]"
    | HD(x) => "hdg q[" ^ (Qubit.str x) ^ "]"
    | SD(x) => "sdg q[" ^ (Qubit.str x) ^ "]"
    | TD(x) => "tdg q[" ^ (Qubit.str x) ^ "]"
    | X(x) => "x q[" ^ (Qubit.str x) ^ "]"
    | CNOT(x, y) =>
    (if (not (Qubit.eq (x, y))) then () else (print "x =y!!\n";raise InvalidGate); "cx q[" ^ (Qubit.str x) ^ "], q[" ^ (Qubit.str y) ^ "]")
    | CCZ(x, y, z) => "ccz q[" ^ (Qubit.str x) ^ "], q[" ^ (Qubit.str y) ^ "], q[" ^ (Qubit.str z) ^ "]"
    | UNINT x => Unint.str x

  fun inverse g =
    case g of
      H(x) => HD(x)
    | S(x) => SD(x)
    | T(x) => TD(x)
    | HD(x) => H(x)
    | SD(x) => S(x)
    | TD(x) => T(x)
    | X _ => g
    | CNOT _ => g
    | CCZ _ => g
    | UNINT _ => raise Unimplemented


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
      val xm = ComplexMatrix.fromList[[z, one], [one, z]]
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
        | X _ => xm
        | CNOT _ => cnotm
        | CCZ _ => cczm
        | UNINT _ => raise Unimplemented
    end

end


structure Experiment =
struct
  structure TwoOPT = CircuitOPT (structure GateSet = TwoGateSet)
  val cqasm = CLA.parseString "circuit" "test-small.qasm"
  val print_out = CLA.isArg "outfile"
  val outfile = CLA.parseString "outfile" "out.qasm"
  val no_preprocess = CLA.isArg "nopp"

  fun run msg f =
    let
      val _ = print (msg ^ "\n")
      val t0 = Time.now ()
      val result =  f ()
      val t1 = Time.now ()
      val diff = Time.toReal(Time.- (t1, t0))
    in
      (print ("time taken = " ^ Real.fmt (StringCvt.FIX (SOME 4)) diff ^ "s\n")
      ; result)
    end

  fun optimize () =
    let
      val c =
        let val nppc = (TwoOPT.from_qasm cqasm) in
          if no_preprocess then nppc
          else (run "preprocessing" (fn _ => TwoOPT.preprocess nppc))
        end
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
      val c' = run "preprocessing" (fn _ => TwoOPT.preprocess c)
    in
      if print_out then TwoOPT.dump c' outfile
      else ()
    end

end


val _ = if CLA.isArg "pponly" then Experiment.preprocess ()
        else Experiment.optimize()

(* val _ = TwoOPT.cprint c *)
(* val _ = print(ComplexMatrix.str (TwoOPT.eval_circuit c)) *)
(* val _ = print(ComplexMatrix.str (TwoOPT.eval_circuit c)) *)
(* val _ = print "after circuit\n" *)
(* val _ = print (ComplexMatrix.str (TwoOPT.eval_circuit c')) *)
