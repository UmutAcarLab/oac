functor QuartzBB (structure Circuit : CIRCUIT) : BLACK_BOX_OPT =
struct

type transformer = MLton.Pointer.t * Word64.word
datatype store = GREEDY of transformer | BOTH of {greedy: transformer, all: transformer}
type t = store Seq.t option
exception Unintialized

structure Circuit = Circuit

val ffi_optimize = _import "opt_circuit" : string * int * char array * int * MLton.Pointer.t -> Int32.int;
val ffi_preprocess = _import "preprocess" : string * char array * int -> Int32.int;
val ffi_load_greedy_xfers = _import "load_greedy_xfers" : string *  MLton.Pointer.t ref -> Word64.word;
val ffi_load_xfers = _import "load_xfers" :
  string *  MLton.Pointer.t ref * Word64.word ref * MLton.Pointer.t ref * Word64.word ref -> unit;

(* fun load_greedy_xfers (eccs : MLton.Pointer.t) = *)
fun load_greedy_xfers (contents) =
  let
    val xfer_ptr = ref (MLton.Pointer.null)
    val sz = ffi_load_greedy_xfers (contents, xfer_ptr)
  in
    GREEDY (!xfer_ptr, sz)
  end

fun load_xfers (contents) =
  let
    val gxfer_ptr = ref (MLton.Pointer.null)
    val gwrd_ptr = ref (0w0)
    val axfer_ptr = ref (MLton.Pointer.null)
    val awrd_ptr = ref (0w0)
    val _ = ffi_load_xfers (contents, gxfer_ptr, gwrd_ptr, axfer_ptr, awrd_ptr)
  in
    BOTH {greedy = (!gxfer_ptr, !gwrd_ptr), all = (!axfer_ptr, !awrd_ptr)}
  end

val P = MLton.Parallel.numberOfProcessors

fun call_quartz f cqasm =
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

fun init () =
  if CommandLineArgs.isArg ("pponly") then NONE
  else let
    val greedy = CommandLineArgs.isArg ("greedyonly")
    val eq_file = CommandLineArgs.parseString "eqset" "Nam_4_3_complete_ECC_set.json"
    val contentsSeq = run ("reading xfers from " ^ eq_file) (fn _ => ReadFile.contentsSeq eq_file)
    val len = Seq.length contentsSeq
    val contents = CharVector.tabulate (len + 1, fn i => if (i = len) then (#"0") else Seq.nth contentsSeq i)
  in
    if greedy then SOME (ArraySlice.full (SeqBasis.tabulate 1 (0, P) (fn _ => load_greedy_xfers (contents))))
    else SOME (ArraySlice.full (SeqBasis.tabulate 1 (0, P) (fn _ => load_xfers (contents))))
  end
  (* let
    val (eccs, sz) = load_eqset ()
  in *)

  (* end *)

fun seq_to_str s = CharVector.tabulate (Seq.length s, (fn i => Seq.nth s i))

fun cstr s = s ^ (Char.toString (#"0"))

exception InvalidPreprocess

fun preprocess (c : Circuit.raw_circuit) =
  let
    val cq = (Circuit.raw_to_qasm c) ^ (String.str (Char.chr 0))
    val pid =  MLton.Parallel.processorNumber ()
    val cqasm = call_quartz (fn (b, bsize) => ffi_preprocess (cq, b, bsize)) cq
  in
    case cqasm of
      NONE => raise InvalidPreprocess
    | SOME charseq => Circuit.from_qasm (charseq)
  end

fun apply_ (t, tsz) c timeout =
  let
  fun select (c, cqasm') =
    case cqasm' of
      NONE => NONE
    | SOME charseq =>
        let
          (* val _ = print ("back from quartz = " ^ (seq_to_str charseq)) *)
          val (c' : Circuit.raw_circuit) = Circuit.from_qasm (charseq)
        in
          if (Circuit.size_raw c' >= Circuit.size c) then NONE
          else SOME (Circuit.reindex (c', c))
        end

    val cqasm = (Circuit.to_qasm c) ^ (String.str (Char.chr 0))
    val cqasm' =
      call_quartz (fn (b, bsize) => ffi_optimize (cqasm, timeout, b, bsize, t)) cqasm
  in
    select (c, cqasm')
  end

fun apply_greedy st c =
  case st of
    NONE => raise Unintialized
  | SOME st =>
    let
      val pid = MLton.Parallel.processorNumber ()
      val tfer =
        case Seq.nth st pid of
          GREEDY x => x
        | BOTH {greedy, all} => greedy
    in
      apply_ tfer c 0
    end

fun apply_all st (c, timeout) =
  case st of
    NONE => raise Unintialized
  | SOME st =>
    let
      val pid = MLton.Parallel.processorNumber ()
      val tfer =
        case Seq.nth st pid of
          GREEDY x => raise Unintialized
        | BOTH {greedy, all} => all
      (* fun loop_apply c cnt =
        case apply_ tfer c timeout of
            SOME c' => loop_apply c' (cnt + 1)
          | NONE => if cnt = 0 then NONE else SOME c *)
      (* loop_apply c 0 *)
    in
      apply_ tfer c timeout
    end

val best_equivalent = apply_greedy

fun max_breadth x = 5
val sz = CommandLineArgs.parseInt "size" 6
fun max_size x i = sz


end

