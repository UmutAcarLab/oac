structure QSet =
struct
  exception Unimplemented
  type t = bool Seq.t
  fun init n = Seq.tabulate (fn i => false) n
  fun add s i = ArraySlice.update (s, i, true)
  fun copy s = Seq.map (fn v => v) s

  fun some_element s = raise Unimplemented
    (* let

    in
      body
    end *)

  fun size s = raise Unimplemented
  fun contains s i = Seq.nth s i
  fun union (s1, s2) = Seq.tabulate (fn i => (Seq.nth s1 i) orelse (Seq.nth s2 i)) (Seq.length s1)
  fun intersect (s1, s2) = raise Unimplemented

  fun absorb s1 s2 =
    Seq.foreach s2 (fn (i, _) => if (not (Seq.nth s1 i)) then ArraySlice.update (s1, i, Seq.nth s2 i) else ())

  fun foreach s f = raise Unimplemented
  fun iterate s f = raise Unimplemented
  fun forall s f = raise Unimplemented
  fun fold b f s = raise Unimplemented


  fun intersect_count s1 s2 = raise Unimplemented
  fun from_list q = raise Unimplemented

  fun toSeq s = raise Unimplemented
  fun singleton x = raise Unimplemented


  fun subset s1 s2 = raise Unimplemented
    (* SeqBasis.reduce (fn (a, b) => a andalso b) true (0, Seq.length s1) (fn i => not (Seq.nth s1 i) orelse (Seq.nth s2 i)) *)

  fun diff s1 s2 =
    Seq.tabulate (fn i => (Seq.nth s1 i) andalso not(Seq.nth s2 i)) (Seq.length s1)
end