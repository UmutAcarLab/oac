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
  | Y of qubit
  | CNOT of qubit * qubit
  | CCZ of qubit * qubit * qubit
  | CCX of qubit * qubit * qubit
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
    | Y (x) => Y (fidx x)
    | CNOT (x, y) => CNOT (fidx x, fidx y)
    | CCZ (x, y, z) => CCZ (fidx x, fidx y, fidx z)
    | CCX (x, y, z) => CCX (fidx x, fidx y, fidx z)
    | UNINT x => UNINT (Unint.map_support x fidx)

  fun support g =
    case g of
      H(x) => [x]
    | S(x) => [x]
    | T(x) => [x]
    | HD(x) => [x]
    | SD(x) => [x]
    | TD(x) => [x]
    | X (x) => [x]
    | Y (x) => [x]
    | CNOT (x, y) => [x, y]
    | CCZ (x, y, z) => [x, y, z]
    | CCX (x, y, z) => [x, y, z]
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
    | ("y", [x]) => Y (x)
    | ("cx", [x, y]) =>  (if (not (Qubit.eq (x, y))) then () else (print "x =y label\n";raise InvalidGate); CNOT (x, y))
    | ("ccz", [x, y, z]) => CCZ (x, y, z)
    | ("ccx", [x, y, z]) => CCX (x, y, z)
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
    | Y(x) => "y q[" ^ (Qubit.str x) ^ "]"
    | CNOT(x, y) =>
    (if (not (Qubit.eq (x, y))) then () else (print "x =y!!\n";raise InvalidGate); "cx q[" ^ (Qubit.str x) ^ "], q[" ^ (Qubit.str y) ^ "]")
    | CCZ(x, y, z) => "ccz q[" ^ (Qubit.str x) ^ "], q[" ^ (Qubit.str y) ^ "], q[" ^ (Qubit.str z) ^ "]"
    | CCX(x, y, z) => "ccx q[" ^ (Qubit.str x) ^ "], q[" ^ (Qubit.str y) ^ "], q[" ^ (Qubit.str z) ^ "]"
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
    | Y _ => g
    | CNOT _ => g
    | CCZ _ => g
    | CCX _ => g
    | UNINT _ => raise Unimplemented

  fun gate_cost g = 1
    (* case g of
      T _ => 1
    | TD _ => 1
    | _ => 0 *)

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
      val ym = ComplexMatrix.fromList[[z, (0.0, ~1.0)], [(0.0, 1.0), z]]
      val cnotm = ComplexMatrix.fromList [[one, z, z, z], [z, one, z, z], [z, z, z, one], [z, z, one, z]]
      val ccxm =
        let
          fun eij (i, j) =
            if j = 3 then if i = 7 then one else z
            else if j = 7 then if i = 3 then one else z
            else if i <> j then z
            else one
        in
          ComplexMatrix.tabulate (8, 8) eij
        end
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
        | Y _ => xm
        | CNOT _ => cnotm
        | CCZ _ => cczm
        | CCX _ => ccxm
        | UNINT _ => raise Unimplemented
    end

end


structure Experiment =
struct
  structure TwoOPT = CircuitOPT (structure GateSet = TwoGateSet)
  val qasm_file = CLA.parseString "circuit" "test-small.qasm"
  val print_out = CLA.isArg "outfile"
  val outfile = CLA.parseString "outfile" "out.qasm"
  val no_preprocess = CLA.isArg "nopp"
  val timeout = Time.fromReal (Real.fromInt (CLA.parseInt "timeout" 3600))
  val logfile = CLA.parseString "logfile" "out.lopt.log"

  val circ_name =
    Substring.string (#2 (Substring.splitr (fn c => c <> #"/") (Substring.full qasm_file)))

  fun preprocess () =
    let
      val c = TwoOPT.from_qasm qasm_file
      val c' = run "preprocessing" (fn _ => TwoOPT.preprocess c)
    in
      if print_out then TwoOPT.dump c' outfile
      else ()
    end

  fun greedy_optimize () =
    let
      val c =
        let val nppc = (TwoOPT.from_qasm qasm_file) in
          if no_preprocess then nppc
          else (run "preprocessing" (fn _ => TwoOPT.preprocess nppc))
        end
      val rellog = (Time.now(), TwoOPT.size c)
      val c' = Benchmark.run "greedy optimization" (fn _ => TwoOPT.greedy_optimize c timeout)
      val _ = (print ("shrank circuit by " ^ (Int.toString (TwoOPT.size c - TwoOPT.size c') ^ "\n"));
      print ("new size =  " ^ (Int.toString (TwoOPT.size c') ^ "\n")))
      val _ = WriteFile.dump ("logs/" ^ (circ_name)^".greedy.log", (TwoOPT.optlog rellog) ^ "\n")
    in
      if print_out then TwoOPT.dump c' outfile
      else ()
    end

  fun optimize () =
    let
      val c =
        let val nppc = (TwoOPT.from_qasm qasm_file) in
          if no_preprocess then nppc
          else (run "preprocessing" (fn _ => TwoOPT.preprocess nppc))
        end
      val _ = print ("circuit size after preprocessing = " ^ (Int.toString (TwoOPT.size c)) ^ "\n")
      val _ = print ("greedy optimize\n")
      val rellog = (Time.now(), TwoOPT.size c)
      val (c', tm) = Util.getTime (fn _ => TwoOPT.greedy_optimize c timeout)
      val grec = (Time.now(), TwoOPT.size c')
      val c'' =
        if Time.< (tm, timeout) then run "search optimization" (fn _ => TwoOPT.search c' (Time.- (timeout, tm)))
        else c'
      val _ = print ("greedy shrank circuit by " ^ (Int.toString (TwoOPT.size c - TwoOPT.size c') ^ "\n"))
      val _ = print ("search shrank circuit by " ^ (Int.toString (TwoOPT.size c' - TwoOPT.size c'') ^ "\n"))
      val _ = print ("new size =  " ^ (Int.toString (TwoOPT.size c'') ^ "\n"))
      val logstr =
        let
          val (it, _) = rellog
          val (tg, szg) = grec
          val t = Time.toReal (Time.-(tg, it))
        in
          (TwoOPT.optlog rellog) ^ "\n" ^ ("(" ^ (Real.toString t) ^ ", " ^ (Int.toString szg) ^ ");\n")
        end

      val _ = WriteFile.dump (logfile, logstr)
    in
      if print_out then TwoOPT.dump c'' outfile
      else ()
    end

end


val _ = if CLA.isArg "pponly" then Experiment.preprocess ()
        else if CLA.isArg "greedyonly" then Experiment.greedy_optimize()
        else Experiment.optimize()

(* val _ = TwoOPT.cprint c *)
(* val _ = print(ComplexMatrix.str (TwoOPT.eval_circuit c)) *)
(* val _ = print(ComplexMatrix.str (TwoOPT.eval_circuit c)) *)
(* val _ = print "after circuit\n" *)
(* val _ = print (ComplexMatrix.str (TwoOPT.eval_circuit c')) *)
