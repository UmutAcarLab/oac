structure Complex =
struct
  type complex = (real * real)
  fun real (c : complex)  = #1 c
  fun img (c : complex)  = #2 c

  fun conjugate (c: complex) = (real c, ~ (img c))

  fun modulus ((a, b) : complex) = Math.sqrt (a * a + b * b)

  fun polar_form ((a, b) : complex) =
    let
      val r = modulus (a, b)
    in
      (r, (a/r, b/r))
    end


  fun add ((a1, b1) : complex, (a2, b2) : complex) : complex = (a1 + a2, b1 + b2)

  fun cdiv ((a, b) : complex, (c, d) : complex) : complex =
    let
      val common_denom = c * c + d * d
      val real_num = a * c + b * d
      val i_num = b * c - a * d
    in
      (real_num/common_denom, i_num/common_denom)
    end

  fun sub ((a1, b1) : complex, (a2, b2) : complex) : complex = (a1 - a2, b1 - b2)

  fun multiply ((a1, b1) : complex, (a2, b2) : complex) : complex =
    (Real.*(a1, a2) - Real.*(b1, b2), Real.*(a1, b2) + Real.* (a2, b1))

  fun str (a, b) = (Real.toString a) ^ " + i" ^ (Real.toString b)


  fun round p (a, b) =
    let
      fun round_part v = if abs (v) < p then 0.0 else v
    in
      (round_part a, round_part b)
    end

  fun equiv (a, b) =
    let
      val (r, c) = round 1E~14 (sub (a, b))
    in
      Real.== (r, 0.0) andalso Real.== (c, 0.0)
    end

  (* fun compare (c1, c2) =
    if equiv (c1, c2) then EQUAL else (GREATER) *)
    (* (print (str c1); print ("\t");print (str c2); print ("\t");
    case  (real c1, real c2) of
      EQUAL => Real.compare (img c1, img c2)
    | lg => (print "this case\n"; lg)) *)

  fun equivt t (a, b) =
    let
      val (r, c) = round t (sub (a, b))
    in
      Real.== (r, 0.0) andalso Real.== (c, 0.0)
    end

  fun ein n i =
    let
      val ag = Real./ (2.0 * (Math.pi), Real.fromInt n)
      val agi = (Real.fromInt (i)) * ag
    in
      (Math.cos agi, Math.sin agi)
    end

end

