val ( $$ ) : Lambda.C.term -> Lambda.C.term -> Lambda.C.term
val ( $ ) : Lambda.term -> Lambda.term -> Lambda.term
val clam : string -> Lambda.C.term -> Lambda.C.term
val lam : Lambda.term -> Lambda.term
exception InvalidArgument of string
val church' : int -> Lambda.C.term
val church : int -> Lambda.term
val cZero' : Lambda.C.term
val cOne' : Lambda.C.term
val cTwo' : Lambda.C.term
val cThree' : Lambda.C.term
val cFour' : Lambda.C.term
val cFive' : Lambda.C.term
val cSeven' : Lambda.C.term
val cTwelf' : Lambda.C.term
val cZero : Lambda.term
val cOne : Lambda.term
val cTwo : Lambda.term
val cThree : Lambda.term
val cFour : Lambda.term
val cFive : Lambda.term
val cSeven : Lambda.term
val cTwelf : Lambda.term
val succ' : Lambda.C.term
val plus' : Lambda.C.term
val times' : Lambda.C.term
val cTrue' : Lambda.C.term
val cFalse' : Lambda.C.term
val bool_and' : Lambda.C.term
val is_zero' : Lambda.C.term
val pred' : Lambda.C.term
val y' : Lambda.C.term
val sum_u' : Lambda.C.term
val sum' : Lambda.C.term
val sum2' : Lambda.C.term
val calc' : Lambda.C.term
val sum : Lambda.term
val sum2 : Lambda.term
val calc : Lambda.term
