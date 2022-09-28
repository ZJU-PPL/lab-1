exception Todo
module C :
  sig
    type term = Var of string | Lam of string * term | Ap of term * term
    val st_first : term
    val st_second : term
    val st_id : term
    val st_with_free : term
    val st_ex1 : term
    val st_ex2 : term
    val pretty : term -> string
  end
type var = Free of string | Bound of int
type term = Var of var | Lam of term | Ap of term * term
exception BadRepresentation of string
val to_locally_nameless : C.term -> term
val t_first : term
val t_second : term
val t_id : term
val t_with_free : term
module Our_own_operators :
  sig
    val ( @$ ) : ('a -> 'b) -> 'a -> 'b
    val ( |/ ) : 'a -> ('a -> 'b) -> 'b
    val t_id' : term
    val t_with_free' : term
  end
val syn_equal : term -> term -> bool
val free_var : term -> string list
val from_locally_nameless : term -> C.term
val subst_free : term -> string -> term -> term
val subst_bound : term -> term -> term
val nf_wrong : term -> term
module Good_nf :
  sig
    type dvar = DIndex of int | DLevel of int | DFree of string
    type dterm = DVar of dvar | DLam of dterm | DAp of dterm * dterm
    val dsubst_bound : dterm -> dterm -> dterm
    val to_dterm : term -> dterm
    val from_dterm : dterm -> term
    val nf : term -> term
  end
val nf : term -> term
val equiv : term -> term -> bool
