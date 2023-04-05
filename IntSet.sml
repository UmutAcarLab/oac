structure IntSet =
struct
  structure S = IntBinarySet
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

  fun str s : string =
    let
      val ss = to_seq s
      val sss = Seq.map (Int.toString) ss
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
