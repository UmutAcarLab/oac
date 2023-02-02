functor SUN (Matrix : COMPLEX_MATRIX) : SU =
struct
  type t = Matrix.t
  type coord =  {t : Int16.int, x : Int16.int, y : Int16.int, z : Int16.int}
  exception Unimplemented
  exception BadUnitary

  fun id d = Matrix.id (d)
  val multiply = Matrix.*

  fun nth m i j = Matrix.nth m (i, j)
  fun dim m = #1 (Matrix.dimension m)

  fun canonical U = U


  (* fun toSU2 m = (nth m 0 0, nth m 0 1 , nth m 1 0, nth m 1 1)
  fun coordinate m i =
    if (dim m = 2) then SU2.coordinate (toSU2 m) i
    else raise Unimplemented
  fun all_coordinates m i =
    if (dim m = 2) then SU2.all_coordinates (toSU2 m) i
    else raise Unimplemented

  fun coord_eq (c1, c2) = SU2.coord_eq (c1, c2)

  fun coord_str c = SU2.coord_str c

  fun hash c = SU2.hash c *)

  fun rot_in d m i = Matrix.scale (Complex.ein d i) m

  fun proj_trace_dist (m1, m2) =
    let
      val d = dim m1
      (* compute |M1 e^itheta - M2| for all theta = 2*i*pi/n *)
      (* val all_rot = Seq.tabulate (fn i => Matrix.norm (Matrix.- (Matrix.scale (Complex.ein d i, m1), m2))) dim *)
      val all_rot = Seq.tabulate (fn i => Matrix.norm (Matrix.- (rot_in d m1 i, m2))) d
    in
      Seq.reduce Real.min Real.posInf all_rot
    end


  fun equiv epsilon (m1, m2) =
    Real.<= (proj_trace_dist (m1, m2), epsilon*10.0)

  fun min_multiply (m1, m2) =
    let
      val m = Matrix.* (m1, m2)
      val d = dim m
      val I = id d
      val all_rot = Seq.tabulate (fn i =>
        let
          val mi = rot_in d m i
        in
          (mi, Matrix.norm (Matrix.- (mi, I)))
        end) d
    in
      #1 (Seq.reduce
        (fn ((m, v), (m', v')) =>
          case Real.compare (v, v') of
            LESS => (m, v)
          | _ => (m', v')
        )
        (I, Real.posInf) all_rot)
    end

  fun order m =
		let
			fun loop ord m' =
				if ord > 50 then Option.valOf (Int.maxInt)
				else if equiv 1E~12 (m, m') then ord
        else loop (ord + 1) (Matrix.* (m, m'))
		in
			loop 2 (Matrix.* (m, m))
		end

  fun fromSU2 (a, b, c, d) = Matrix.fromList ([[a, b], [c, d]])

  val dagger = Matrix.dagger
  val str = Matrix.str
  val det = Matrix.det
  val compare = Matrix.compare


  fun check_unitary U =
    let
      val UD = dagger U
      val d = dim U
      val I = id d
      val I' = multiply (U, UD)
    in
      if equiv 1E~12 (I, I') then ()
      else (print (str U); print (str I'); print (Real.toString (proj_trace_dist (I, I')) ^ "\n");
        let
          val all_rot = Seq.tabulate (fn i => (Real.toString o Matrix.norm) (Matrix.- (rot_in d I i, I'))) d
          val s = Seq.reduce (fn (a, b) => a ^ "\t" ^ b) "\t" (Seq.tabulate (fn i => str (Matrix.-(rot_in d I i, I'))) d)
          val _ = print (s ^ "\n")
          val s = Seq.reduce (fn (a, b) => a ^ "\t" ^ b) "\t" all_rot
        in
          print (s ^ "\n")
        end;

      raise BadUnitary)
    end

end

structure SUM = SUN (ComplexMatrix)
