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
      val _ = print ("queso time = " ^ (time_str t) ^ "\n")
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

functor QuesoBBInt (structure Circuit : CIRCUIT) : BLACK_BOX_OPT =
struct
type t = (MLton.Pointer.t option * Log.t)
exception Unintialized
exception Unimplemented
structure Circuit = Circuit

(* fun proc_id () = MLton.Parallel.processorNumber ()
val P = MLton.Parallel.numberOfProcessors *)


val ffi_start_vm = _import "startVM" : unit -> unit;
val ffi_thread_init = _import "initialize" : string * string * MLton.Pointer.t ref -> unit;
val ffi_optimize = _import "opt_circuit" : string * int * char array * int * MLton.Pointer.t -> Int32.int;

fun preprocess _ = raise Unimplemented

(* Assuming single-threaded for now *)
(* Multi-threaded attachment to Java VM needs a little more work
 * and a much better accounting of JNI behavior *)
fun init () =
  if CommandLineArgs.isArg ("pponly") then (NONE, Log.init())
  else let
    val _ = ffi_start_vm ()
    val rule_file = CommandLineArgs.parseString "rule_file" "./lib/queso/rules_q3_s6_nam.txt"
    val symb_rule_file = CommandLineArgs.parseString "rule_file" "./lib/queso/rules_q3_s3_nam_symb.txt"
    val td_ref = ref (MLton.Pointer.null)
    val _ = ffi_thread_init (rule_file, symb_rule_file, td_ref)
  in
    (SOME (!td_ref), Log.init())
  end


fun call_queso f cqasm =
  let
    fun loop_buff_size sz =
      let
        val buffer = ForkJoin.alloc sz
        (* val _ = print ("calling quartz\n") *)
        val tmp = f (buffer, sz)
        (* val _ = print ("quartz returned\n") *)
        val sz' = Int32.toInt (tmp)
        (* val _ = print ("int form quartz = " ^ (Int.toString sz') ^"\n") *)
      in
        if sz' = ~1 then NONE
        else if sz' < 0 then (print "buffer overflow, calling again\n"; loop_buff_size (~sz'))
        else SOME (Seq.tabulate (fn i => Array.sub (buffer, i)) sz')
      end
  in
    loop_buff_size (2 * (String.size cqasm))
  end


fun seq_to_str s = CharVector.tabulate (Seq.length s, (fn i => Seq.nth s i))

fun cstr s = s ^ (Char.toString (#"0"))

exception InvalidPreprocess

fun apply_ greedy (SOME td, log) c timeout =
  let
  fun select (c, cqasm') =
    case cqasm' of
      NONE => (NONE, 0)
    | SOME charseq =>
        let
          (* val _ = print ("back from queso = " ^ (seq_to_str charseq)) *)
          val (c' : Circuit.raw_circuit) = Circuit.from_qasm (charseq)
          val szd = (Circuit.cost_raw c' - Circuit.cost c)
          val _ = print ("Circuit optimzied from " ^ (Int.toString (Circuit.cost c) ^ " to " ^ (Int.toString(Circuit.cost_raw c')) ^ "\n"))
        in
          if (szd >= 0) then (NONE, szd)
          else (SOME (Circuit.reindex (c', c)), szd)
        end

    val cqasm = (Circuit.to_qasm c) ^ (String.str (Char.chr 0))
    val timeout = Real.ceil (Time.toReal timeout)
    val stimeout = if greedy then (~1 * timeout) else timeout
    val cqasm' = call_queso (fn (b, bsize) => ffi_optimize (cqasm, stimeout, b, bsize, td)) cqasm
    val (res, sd) = select (c, cqasm')
    val _ = if (Option.isSome res) then Log.register_opt (log, ~sd) else ()
  in
    res
  end

val max_time = Time.fromReal (10000000000.0)
fun apply_greedy (td, log) (c, topt) =
  case topt of
    NONE => apply_ true (td, log) c (max_time)
  | SOME t => apply_ true (td, log) c t

fun apply_all (td, log) (c, timeout) = apply_ false (td, log) c timeout

fun apply_both (td, log) (c, timeout) = apply_ false (td, log) c timeout

fun best_equivalent (td, log) c = apply_greedy (td, log) (c, NONE)

fun max_breadth x = 5
val sz = CommandLineArgs.parseInt "size" 6
fun max_size x i = sz
fun optlog (td, log) (it, isz) = Log.str log (it, isz)

end

