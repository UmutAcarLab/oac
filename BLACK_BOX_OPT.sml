signature BLACK_BOX_OPT =
sig
  type t
  val mkOpt : int -> t
  val insert : t -> int List.list * int List.list -> unit
  val best_equivalent : t -> GateSet.t -> int Seq.t -> int Seq.t option
  val max_breadth : t -> int
  val max_depth : t -> int -> int
end

structure BlackBoxOpt : BLACK_BOX_OPT =
struct
  open TrieOpt

  fun remove_inverses gs s =
    let
      val s' = Seq.map (fn x => SOME x) s
      val slen = Seq.length s
      fun is_inverse (i, j) = (GateSet.inverse gs i) = j
      val _ = Seq.iterate
        (fn (_, i) =>
          if i < slen - 1
            andalso is_inverse (Seq.nth s i, Seq.nth s (i + 1))
            andalso (Option.isSome (Seq.nth s' i)) then
            (ArraySlice.update (s', i, NONE); ArraySlice.update (s', i + 1, NONE))
          else ()
        )
        () s
    in
      Seq.mapOption (fn x => x) s'
    end

  fun best_equivalent opt gs s =
    let
      val s' = remove_inverses gs s
    in
      case lookup opt (Seq.length s', Seq.nth s') of
        NONE => if (Seq.length s' < Seq.length s) then SOME s'
                else NONE
      | x => x
    end

  fun max_breadth _ = 1
  fun max_depth opt _ = max_len opt
end