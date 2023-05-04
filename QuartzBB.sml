functor QuartzBB (structure Circuit : CIRCUIT) : BLACK_BOX_OPT =
struct

type t = (MLton.Pointer.t * Word64.word) Seq.t
structure Circuit = Circuit

val ffi_optimize = _import "opt_circuit" : string * char array * int * MLton.Pointer.t -> Int32.int;
val ffi_preprocess = _import "preprocess" : string * char array * int -> Int32.int;
val ffi_load_eqset = _import "load_eqset" : string *  MLton.Pointer.t ref -> Word64.word;
val ffi_load_greedy_xfers = _import "load_greedy_xfers" : string *  MLton.Pointer.t ref -> Word64.word;

fun load_eqset () =
  let
    val eq_ptr = ref (MLton.Pointer.null)
    val eq_file = CommandLineArgs.parseString "eqset" "Nam_4_3_complete_ECC_set.json"
    val sz = ffi_load_greedy_xfers (eq_file, eq_ptr)
  in
    (!eq_ptr, sz)
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
        else SOME (Seq.tabulate (fn i => Array.sub(buffer, i)) sz')
      end
  in
    loop_buff_size (2 * (String.size cqasm))
  end

fun init () = Seq.tabulate (fn i => load_eqset ()) P

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


fun best_equivalent st c =
  let
    val cqasm = (Circuit.to_qasm c) ^ (String.str (Char.chr 0))
    val pid =  MLton.Parallel.processorNumber ()
    val (t, tsz) = Seq.nth st pid
    val _ = check_qasm cqasm
    val cqasm' = call_quartz (fn (b, bsize) => ffi_optimize (cqasm, b, bsize, t)) cqasm
  in
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

  end

fun max_breadth x = 5
val sz = CommandLineArgs.parseInt "size" 10
fun max_size x i = sz


end

