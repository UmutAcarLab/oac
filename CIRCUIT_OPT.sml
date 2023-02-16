
signature CIRCUIT_OPT =
sig
  include CIRCUIT
  val optimize : circuit -> circuit
end
