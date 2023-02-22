functor TreeRep (structure Circuit : CIRCUIT) =
struct
  datatype circtree =
    PAR of node
  | CONCAT of node
  | LEAF of circuit
  withtype node = {left : circtree, right : circtree, size : int}

  fun tree_size c = raise Unimplemented

  fun parse (c : circuit) = raise Unimplemented

  fun flatten (c : circtree) = raise Unimplemented

end