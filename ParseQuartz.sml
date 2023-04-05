signature PARSE_QUARTZ =
sig
  (* gives a sequence of equivalence classes.
   * each equivalence class is a sequence of circuits
   * each circuit is a sequence of labels
   *)
  val parse : string -> string Seq.t Seq.t Seq.t
  val parse_multi : string -> (string * Qubit.qubit List.list) Seq.t Seq.t Seq.t
  val str : string Seq.t Seq.t Seq.t -> string

  val parse_rep : string -> (string * Qubit.qubit List.list) Seq.t Seq.t
  val parse_rep_multi : string -> (string * Qubit.qubit List.list) Seq.t Seq.t
  val str_rep : string Seq.t Seq.t -> string
end

structure ParseQuartz : PARSE_QUARTZ =
struct
open JSON

exception InvalidFormat

fun int_from_string (s) =
  case Int.fromString s of
    NONE => raise InvalidFormat
  | SOME x => x

fun from_array v =
  case v of
    ARRAY x => x
  | _ => raise InvalidFormat

fun from_object v =
  case v of
    OBJECT x => x
  | _ => raise InvalidFormat

val from_string = JSONUtil.asString

fun list_nth l i = List.nth (l, i)

fun parse f =
  let
    val p = JSONParser.openFile f

    val lv = from_array (JSONParser.parse p)
    val d = from_object (list_nth lv 1)
    val sd = Seq.fromList d
    fun parseElement (_, v) =
      let
        val inner = from_array (v)
        val sinner = Seq.fromList inner
        fun parseInner v =
          let
            val v' = from_array v
            val v'' = from_array (list_nth v' 1)
            val seq_gates = DelayedSeq.fromList v''
            val seq_rem_arr = DelayedSeq.map from_array seq_gates
            val gate_arr = (DelayedSeq.map (fn v => from_string (list_nth v 0)) seq_rem_arr)
          in
            DelayedSeq.toArraySeq gate_arr
          end
      in
        Seq.map parseInner sinner
      end
  in
    Seq.map parseElement sd
  end


fun parse_gate_multi g =
  let
    val g = from_array g
    val gate = from_string (list_nth g 0)
    val app_seq = DelayedSeq.fromList (let val t = from_array (list_nth g 1) in t end)
    val s = DelayedSeq.map ((fn q => String.extract (q, 1, NONE)) o from_string) app_seq
  in
    (gate, DelayedSeq.toList (DelayedSeq.map (Qubit.from_int o int_from_string) s))
  end

fun parse_multi f =
  let
    val p = JSONParser.openFile f

    val lv = from_array(JSONParser.parse p)
    val d = from_object (list_nth lv 1)
    val sd = Seq.fromList d
    fun parseElement (_, v) =
      let
        val inner = from_array v
        val sinner = Seq.fromList inner
        fun parseInner v =
          let
            val v' = from_array v
            val v'' = from_array (list_nth v' 1)
            val seq_gates = Seq.fromList v''
          in
            Seq.map parse_gate_multi seq_gates
          end
      in
        Seq.map parseInner sinner
      end
  in
    Seq.map parseElement sd
  end

fun parse_rep f =
  let
    val p = JSONParser.openFile f
    val (lv, tm) = Util.getTime (fn _ =>  JSONParser.parse p)
    val _ = print ("json parsed in " ^ Time.fmt 4 tm ^ "s\n")
    val sd = Seq.fromList (from_array lv)
    fun parseElement e =
      let
        val e' = from_array (list_nth e 1)
        val ss = Seq.fromList e'
        fun parse_gates g = (from_string (list_nth g 0), [Qubit.from_int 0])
      in
        Seq.map (parse_gates o from_array) ss
      end
  in
    Seq.map (parseElement o from_array) sd
  end

fun parse_rep_multi f =
  let
    val p = JSONParser.openFile f
    val (lv, tm) = Util.getTime (fn _ =>  JSONParser.parse p)
    val _ = print ("json parsed in " ^ Time.fmt 4 tm ^ "s\n")
    val sd = Seq.fromList (from_array lv)
    fun parseElement e =
      let
        val e = from_array (e)
        val e' = from_array (list_nth e 1)
        val ss = Seq.fromList e'
      in
        Seq.map parse_gate_multi ss
      end
  in
    Seq.map parseElement sd
  end

fun printElement s = Seq.reduce (fn (a, b) => a ^ " " ^ b) "" s

fun str_rep ss = Seq.reduce (fn (a, b) => a ^ "\n" ^ b) "" (Seq.map printElement ss)

fun str sss =
  let
    fun printClass ss = Seq.reduce (fn (a, b) => a ^ "; " ^ b) "" (Seq.map printElement ss)
  in
    Seq.reduce (fn (a, b) => a ^ "\n" ^ b) "" (Seq.map printClass sss)
  end

end

