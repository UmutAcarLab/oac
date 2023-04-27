structure ParseQASM =
struct

  exception InvalidFormat

  (* fun writeToQASM f gs {perm, M} =
    let
      fun writeFile (f: string) (s: string) : unit =
          let val os = TextIO.openOut f
          in (TextIO.output(os,s); TextIO.closeOut os)
            handle X => (TextIO.closeOut os; raise X)
          end

      val header = "OPENQASM 2.0; \ninclude \"qelib1.inc\"; \nqreg q[1]; \n"
      fun write_gate i = (GateSet.label gs i) ^ " q[0];\n"
      val gate_s = Seq.reduce (fn (a, b) => a ^ b) "" (Seq.map write_gate perm)
    in
      writeFile f (header ^ gate_s)
    end *)
  fun seq_to_str s = CharVector.tabulate (Seq.length s, (fn i => Seq.nth s i))

  fun parse chars =
    let
      fun split chars char =
        let
          fun isChar i = Seq.nth chars i = char
          val charPos = ArraySlice.full (SeqBasis.filter 10000 (0, Seq.length chars) (fn i => i) isChar)
          fun splitStart i = if i = 0 then 0 else 1 + Seq.nth charPos (i-1)
          fun splitEnd i = if i = Seq.length charPos then Seq.length chars else Seq.nth charPos i
        in
          DelayedSeq.tabulate
            (fn i => Seq.subseq chars (splitStart i, splitEnd i - splitStart i))
            (1 + Seq.length charPos)
        end
      val lines = split chars (#"\n")
      fun line i = DelayedSeq.nth lines i

      fun parse_qbit c =
        case Parse.parseInt ((DelayedSeq.nth (split (Seq.drop c 1) (#"]")) 0)) of
          SOME n => Qubit.from_int n
        | NONE => (print ("invalid line = " ^ (seq_to_str c) ^ "\n"); raise InvalidFormat)

      (* given a line it returns all integers i such that q[i] is in the line*)
      fun get_qubits line =
        let
          val qsp = split line (#"q")
          val numinputs = DelayedSeq.length qsp - 1
        in
          List.tabulate (numinputs, fn i => parse_qbit (DelayedSeq.nth qsp (i + 1)))
        end

      (* from qreg q[n], retrieve n *)
      val nqubits =
        case get_qubits (Seq.drop (line 2) 5) of
          [x] => Qubit.to_int x
        | _ => raise InvalidFormat
      val head_off = 3
      fun parseGateLine i =
        let
          val i = i + head_off
          (* gate is the string before the first space. *)
          val gate = Parse.parseString (DelayedSeq.nth (split (line i) (#" ")) 0)
          val bits = Seq.drop (line i) (String.size gate)
          val qsplits = (split bits (#"q"))
          (* val numinputs = DelayedSeq.length qsplits - 1 *)
          fun all_white_space l = List.length (String.tokens (fn c => c = #" ") (Parse.parseString l)) = 0
        in
          if all_white_space (line i) then NONE
          else
            SOME (gate, get_qubits bits)
            (* List.tabulate (numinputs, (fn i => get_qubit (DelayedSeq.nth line_split (1 + i))))) *)
        end

      val numLines = DelayedSeq.length lines
      val c = Seq.tabulate parseGateLine (numLines - head_off)
      val circuit = Seq.mapOption (fn x => x) c
    in
      (nqubits, circuit)
    end

end