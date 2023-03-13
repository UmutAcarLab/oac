structure QSet =
struct
  exception Unimplemented
  exception Empty

  structure S =  IntRedBlackSet
  type t = S.set

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

  fun foreach s f = S.app f s
  val add = S.add
  val subtract = S.subtract

  fun some s =
    case S.find (fn _ => true) s of
      SOME e => e
    | NONE => raise Empty

  fun some_element s = raise Unimplemented
    (* let

    in
      body
    end *)

  (* fun contains s i = Seq.nth s i *)
  (* fun union (s1, s2) = Seq.tabulate (fn i => (Seq.nth s1 i) orelse (Seq.nth s2 i)) (Seq.length s1) *)
  val intersect = S.intersection

  fun iterate s f = raise Unimplemented
  fun forall s f = raise Unimplemented
  fun remove s q = raise Unimplemented
  fun intersect_count s1 s2 = raise Unimplemented
end