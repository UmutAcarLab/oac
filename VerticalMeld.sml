signature MELD_OPT =
sig
  structure Circuit : CIRCUIT
  structure BlackBoxOpt : BLACK_BOX_OPT
  val preprocess : BlackBoxOpt.t -> Circuit.circuit -> Circuit.circuit
  val greedy_optimize : BlackBoxOpt.t -> Circuit.circuit -> Circuit.circuit
  val optimize : BlackBoxOpt.t -> Circuit.circuit -> Circuit.circuit
end

functor VerticalMeldFun (structure BlackBoxOpt : BLACK_BOX_OPT) : MELD_OPT =
struct
  exception Unimplemented

  structure BlackBoxOpt = BlackBoxOpt
  structure Circuit = BlackBoxOpt.Circuit
  structure CLA = CommandLineArgs

  (* assume grain > wsz *)
  fun apply_opt_fun (wsz, grain) optfun c =
    let
      fun meld (c1 : Circuit.circuit, c2: Circuit.circuit) =
        let
          fun loop (c1, c2) wsz =
            let
              val peel_size = Int.min (wsz div 2, Circuit.num_layers c1)
              val (c1ctxt, c1slice) = Circuit.splitEnd c1 peel_size
              val (c2'_opt, opt) =
                let
                  val c1slice_c2 =  Circuit.prepend (c1slice, c2)
                  val psz = Int.min (wsz, Circuit.num_layers c1slice_c2)
                in
                  optfun (psz, c1slice_c2)
                end
            in
              if opt then loop (c1ctxt, c2'_opt) (2 * wsz)
              else Circuit.prepend (c1ctxt, c2'_opt)
            end
        in
          loop (c1, c2) wsz
        end
      val num_layers = Circuit.num_layers c
    in
      if num_layers < grain then #1 (optfun (num_layers, c))
      else let
        val (c1, c2) = Circuit.split c (num_layers div 2)
        val (opc1, opc2) = ForkJoin.par (fn _ => apply_opt_fun (wsz, grain) optfun c1, fn _ => apply_opt_fun (wsz, grain) optfun c2)
      in
        if wsz <> 0 then meld (opc1, opc2)
        else Circuit.prepend (opc1, opc2)
      end
    end

  fun gvopt bbopt (prefix_sz, c) =
    let
      fun loop (c, opt) =
        let
          val nl = Circuit.num_layers c
          val cut = Int.min (prefix_sz, nl)
          val (subckt, ctxt) = Circuit.split c cut
        in
          case (BlackBoxOpt.apply_greedy bbopt subckt, cut = nl) of
            (NONE, _) => (c, opt)
          | (SOME c', _) => (Circuit.prepend (c', ctxt), true)
          (* | (SOME c', false) => loop (Circuit.prepend (c', ctxt), true) *)
        end
    in
      if prefix_sz = 0 then (c, false)
      else loop (c, false)
    end

  fun vopt bbopt timeout (prefix_sz, c) =
    let
      fun loop (c, opt) =
        let
          val nl = Circuit.num_layers c
          val cut = Int.min (prefix_sz, nl)
          val (subckt, ctxt) = Circuit.split c cut
        in
          case (BlackBoxOpt.apply_all bbopt (subckt, timeout), cut = nl) of
            (NONE, _) => (c, opt)
          | (SOME c', _) => (Circuit.prepend (c', ctxt), true)
          (* | (SOME c', false) => loop (Circuit.prepend (c', ctxt), true) *)
        end
    in
      if prefix_sz = 0 then (c, false)
      else loop (c, false)
    end

  fun vpreprocess bbopt (prefix_sz, c) =
    if prefix_sz = 0 then (c, false)
    else (Circuit.from_raw_sequence (BlackBoxOpt.preprocess (Circuit.to_raw_sequence c)), true)


  fun preprocess bbopt c =
    let
      val _ = print ("num_layers = " ^ (Int.toString (Circuit.num_layers c)) ^ "\n")
      val wsz = 0
      val grain = CLA.parseInt "grain" 200
    in
      apply_opt_fun (wsz, grain) (vpreprocess bbopt) c
    end

  fun greedy_optimize bbopt c =
    let
      val _ = print ("num_layers = " ^ (Int.toString (Circuit.num_layers c)) ^ "\n")
      val wsz = BlackBoxOpt.max_size bbopt 1
      val grain = CLA.parseInt "grain" 200
    in
      apply_opt_fun (wsz, grain) (gvopt bbopt) c
    end

  fun optimize bbopt c =
    let
      val _ = print ("num_layers = " ^ (Int.toString (Circuit.num_layers c)) ^ "\n")
      val wsz = BlackBoxOpt.max_size bbopt 1
      val timeout = CLA.parseInt "timeout" 1
      val grain = (CLA.parseInt "grain" 200)
    in
      apply_opt_fun (wsz, grain) (vopt bbopt timeout) c
    end

end