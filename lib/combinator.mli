val ( $$ ) : Lambda.C.term -> Lambda.C.term -> Lambda.C.term
val ( $ ) : Lambda.term -> Lambda.term -> Lambda.term
val clam : string -> Lambda.C.term -> Lambda.C.term
val lam : Lambda.term -> Lambda.term
val k : Lambda.term
val s : Lambda.term
val test : Lambda.C.term -> Lambda.term -> bool
val t_first : Lambda.C.term
val sk_first : Lambda.term
val t_second : Lambda.C.term
val sk_second : Lambda.term
val sk_second' : Lambda.term
val t_id : Lambda.C.term
val sk_id : Lambda.term
val i : Lambda.term
val t_ap : Lambda.C.term
val sk_ap : Lambda.C.term
val t_ex1 : Lambda.C.term
val sk_ex1 : Lambda.C.term
val t_ex2 : Lambda.C.term
val sk_ex2 : Lambda.C.term
module Systematic_SK :
  sig
    type expr = App of expr * expr | S | K
    val to_lambda : expr -> Lambda.term
    exception HaveFreeVar
    val from_lambda : Lambda.term -> expr
  end
