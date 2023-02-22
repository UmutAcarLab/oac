
signature MELD_OPT =
sig
  structure Circuit : CIRCUIT
  structure BlackBoxOpt : BLACK_BOX_OPT
  val optimize : BlackBoxOpt.t -> Circuit.circuit -> Circuit.circuit
end
