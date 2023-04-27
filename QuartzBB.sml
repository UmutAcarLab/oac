functor QuartzBB (structure Circuit : CIRCUIT) : BLACK_BOX_OPT =
struct

type t = (MLton.Pointer.t * Word64.word)
structure Circuit = Circuit

val ffi_optimize = _import "opt_circuit" : string * char array * int * MLton.Pointer.t * Word64.word -> Int32.int;
val ffi_preprocess = _import "preprocess" : string * char array * int -> Int32.int;
val ffi_load_eqset = _import "load_eqset" : string *  MLton.Pointer.t ref -> Word64.word;

fun load_eqset () =
  let
    val eq_ptr = ref (MLton.Pointer.null)
    val sz = ffi_load_eqset ("Nam_6_3_complete_ECC_set.json", eq_ptr)
    val _ = print ("size obtained  = " ^ (Int.toString (Word64.toInt sz)) ^ "\n")
  in
    (!eq_ptr, sz)
  end


fun call_quartz f cqasm =
  let
    fun loop_buff_size sz =
      let
        val buffer = ForkJoin.alloc sz
        val sz' = Int32.toInt (f (buffer, sz))
        val _ = print ("int from quartz" ^ (Int.toString sz') ^ "\n")
      in
        if sz' = ~1 then NONE
        else if sz' < 0 then loop_buff_size (~sz')
        else SOME (Seq.tabulate (fn i => Array.sub(buffer, i)) sz')
      end
  in
    loop_buff_size (String.size cqasm)
  end

fun init () = load_eqset ()

fun seq_to_str s = CharVector.tabulate (Seq.length s, (fn i => Seq.nth s i))

fun cstr s = s ^ (Char.toString (#"0"))

fun preprocess (c : Circuit.raw_circuit) =
  let
    val cq = (Circuit.raw_to_qasm c) ^ (String.str (Char.chr 0))
    val _ = print "calling preprecss"
    val _ = print ("len of str = " ^ (Int.toString (String.size cq)) ^ "\n")

    val cqasm = call_quartz (fn (b, bsize) => ffi_preprocess (cq, b, bsize)) cq
  in
    case cqasm of
      NONE => c
    | SOME charseq => Circuit.from_qasm (charseq)
  end

fun best_equivalent (t, tsz) c =
  let
    val cqasm = (Circuit.to_qasm c) ^ (String.str (Char.chr 0))
    val cqasm' = call_quartz (fn (b, bsize) => ffi_optimize (cqasm, b, bsize, t, tsz)) cqasm
  in
    case cqasm' of
      NONE => NONE
    | SOME charseq =>
        let
          val _ = print ("back from quartz = " ^ (seq_to_str charseq))
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

