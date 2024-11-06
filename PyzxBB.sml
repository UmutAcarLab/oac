structure Log =
struct
  type optlog = (Time.time * int) List.list ref
  type timelog = Time.time ref
  type t = optlog * timelog

  fun init () = (ref [], ref (Time.zeroTime))
  fun time_str (t) = Real.toString (Time.toReal (!t))

  fun str (r, t) (it, isz) =
    let
      val l =
        case !r of
          [] => [(it, 0), (Time.now (), 0)]
        | (t, fsz)::_ => (it, 0)::(List.rev ((Time.now(), fsz)::(!r)))
      val s = (Seq.fromList l)
      val s' = Seq.map (fn (t, sz) => (Time.toReal (Time.-(t, it)), isz - sz)) s
      val ss' = Seq.map (fn (t, sz) => "(" ^ (Real.toString t) ^ ", " ^ (Int.toString sz) ^ ");") s'
      val _ = print ("pyzx time = " ^ (time_str t) ^ "\n")
    in
      Seq.reduce (fn (a, b) => a ^ b) "" ss'
    end

  fun register_opt ((r, _), sz) =
    let
      val te = Time.now ()
      val ol = !r
      val nl =
        case ol of
          [] => [(te, sz)]
        | (te', sz')::_ => (te, sz + sz') :: ol
    in
      r := nl
    end

  fun register_time ((_, r), tm) = (r := Time.+ (!r, tm))


end

functor PyzxBB (structure Circuit : CIRCUIT) : BLACK_BOX_OPT =
struct
type t = Log.t
exception Unintialized
exception Unimplemented
structure Circuit = Circuit

(* fun proc_id () = MLton.Parallel.processorNumber ()
val P = MLton.Parallel.numberOfProcessors *)
val pyzx_exec = "python3 pyzx/pyzx_lopt.py"
val qasm_file = CommandLineArgs.parseString "circuit" "test-small.qasm"
val circ_name =
    Substring.string (#2 (Substring.splitr (fn c => c <> #"/") (Substring.full qasm_file)))

fun preprocess _ = raise Unimplemented

val ques_log = CommandLineArgs.parseString "qlogfile" "pyzx_log.log"
fun call_pyzx log cqasm timeout =
  let
    val tmstr = Real.toString (Time.toReal (Time.now()))
    val in_file = circ_name ^ ".temp_pyzx.qasm." ^ tmstr
    val out_file = "optimized_nam_" ^ in_file
    val _ = Circuit.dump cqasm in_file
    val command = pyzx_exec ^ " " ^ (in_file) ^ " " ^ (out_file) ^ " " ^ (Int.toString timeout)
    val _ = print ("running " ^ command ^ "\n")
    val (_, tm) = Util.getTime(fn _ => OS.Process.system command)
    val _ = Log.register_time (log, tm)
    val _ = print ("time taken = " ^ ((Real.toString o Time.toReal) (tm)) ^ "\n")
    val cseq = (ReadFile.contentsSeq out_file)
    val cleanup = ("rm -f " ^ in_file ^ " " ^ out_file)
    val _ = print("removing by command " ^ cleanup ^ "\n")
    val _ = OS.Process.system (cleanup)
  in
    SOME cseq
  end

fun init () = Log.init()
  (* let
    val (eccs, sz) = load_eqset ()
  in *)

  (* end *)

fun seq_to_str s = CharVector.tabulate (Seq.length s, (fn i => Seq.nth s i))

fun cstr s = s ^ (Char.toString (#"0"))

exception InvalidPreprocess

fun apply_ c timeout log =
  let
  fun select (c, cqasm') =
    case cqasm' of
      NONE => (NONE, 0)
    | SOME charseq =>
        let
          (* val _ = print ("back from quartz = " ^ (seq_to_str charseq)) *)
          val (c' : Circuit.raw_circuit) = Circuit.from_qasm (charseq)
          val szd = (Circuit.cost_raw c' - Circuit.cost c)
          val _ = print ("Circuit optimzied from " ^ (Int.toString (Circuit.cost c) ^ " to " ^ (Int.toString(Circuit.cost_raw c')) ^ "\n"))
        in
          if (szd >= 0) then (NONE, szd)
          else (SOME (Circuit.reindex (c', c)), szd)
        end
    val cqasm' = call_pyzx log c (Real.ceil (Time.toReal timeout))
    val (res, sd) = select (c, cqasm')
    val _ = if (Option.isSome res) then Log.register_opt (log, ~sd) else ()
  in
    res
  end

fun apply_greedy log (c, topt) =
  case topt of
    NONE => apply_ c ((Time.fromReal 1.0)) log
  | SOME t => apply_ c t log

fun apply_all log (c, timeout) = apply_ c timeout log


fun apply_both log (c, timeout) = apply_ c timeout log

fun best_equivalent log c = apply_greedy log (c, NONE)

fun max_breadth x = 5
val sz = CommandLineArgs.parseInt "size" 6
fun max_size x i = sz
fun optlog log (it, isz) = Log.str log (it, isz)

end

