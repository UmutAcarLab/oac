signature QUBIT =
sig
  type qubit
  val compare : qubit * qubit -> order
  val str : qubit -> string
  val to_int : qubit -> int
  val from_int : int -> qubit
  val enumerate : int -> qubit Seq.t
  val eq : qubit * qubit -> bool
end


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


exception Unimplemented
structure IntToString =
struct
  fun digitToChar x =  String.sub ("0123456789", x)
  fun str (x : int) =
    if x < 0 then raise Unimplemented
    else if x < 10 then (CharVector.tabulate (1, fn i => digitToChar x))
    else let
      val charr = CharArray.array (x, #"\000")
      fun loop (n, chidx) =
        let
          val _ = CharArray.update (charr, chidx, digitToChar (Int.rem (n, 10)))
          val q = Int.quot (n, 10)
        in
          if q = 0 then
            CharArraySlice.vector (CharArraySlice.slice (charr, chidx, NONE))
          else loop (q, chidx - 1)
        end
    in
      loop (x, x - 1)
    end

end

structure Qubit :> QUBIT =
struct
  type qubit = int
  val compare = Int.compare
  fun eq (a, b) = (a = b)

  val str = IntToString.str
  fun to_int x = x
  fun from_int x = x
  fun enumerate n = Seq.tabulate (fn x => x) n
end

structure QMap = RedBlackMapFn (struct type ord_key = Qubit.qubit val compare = Qubit.compare end)

structure QSet =
struct
  structure S = RedBlackSetFn (struct type ord_key = Qubit.qubit val compare = Qubit.compare end)
  type t = S.set

  exception Unimplemented
  exception Empty

  val empty = S.empty
  val from_list = S.fromList
  val to_list = S.toList
  val contains = S.member
  val union = S.union
  val is_subset = S.isSubset
  val size = S.numItems
  val fold = S.foldl
  val singleton = S.singleton
  val from_seq = from_list o Seq.toList
  val to_seq = Seq.fromList o to_list
  val map = S.map
  fun foreach s f = S.app f s
  val add = S.add
  val subtract = S.subtract

  fun to_intset f s =
    let
      val l = to_list s
      val il = List.map f l
    in
      IntSet.from_list il
    end

  fun gen_map qs =
    let
      val qseq = to_seq qs
      val qseqm = Seq.mapIdx (fn (i, q) => QMap.singleton (q, i)) qseq
    in
      Seq.reduce (QMap.unionWith (fn (a, b) => a)) (QMap.empty) qseqm
    end

  fun gen_seq_and_map qs =
    let
      val qseq = to_seq qs
      val qseqm = Seq.mapIdx (fn (i, q) => QMap.singleton (q, i)) qseq
    in
      (qseq, Seq.reduce (QMap.unionWith (fn (a, b) => a)) (QMap.empty) qseqm)
    end

  fun str s : string =
    let
      val ss = to_seq s
      val sss = Seq.map (Qubit.str) ss
      val sr = Seq.reduce (fn (a, b) => a ^ ", " ^ b) "" sss
    in
      ("(" ^ sr ^ ")\n")
    end

  fun exists s p = S.find (fn x => p (x)) s
  val find = S.find

  fun some s =
    case S.find (fn _ => true) s of
      SOME e => e
    | NONE => raise Empty

  val intersect = S.intersection
end


(* structure QSet =
struct






  (* fun some_element s = raise Unimplemented *)
    (* let

    in
      body
    end *)

  (* fun contains s i = Seq.nth s i *)
  (* fun union (s1, s2) = Seq.tabulate (fn i => (Seq.nth s1 i) orelse (Seq.nth s2 i)) (Seq.length s1) *)
(*
  fun iterate s f = raise Unimplemented
  fun forall s f = raise Unimplemented
  fun remove s q = raise Unimplemented
  fun intersect_count s1 s2 = raise Unimplemented *)
end *)
