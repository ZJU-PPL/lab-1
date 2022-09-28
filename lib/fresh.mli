open Base

type t = {
  base : string;
  counter : int;
  forbid : (string, String.comparator_witness) Set.t;
}

(* 
构造一个 fresh variable name 生成器.
- base 是前缀, 比如 ~base:"x" 的话生成出来的变量名就是 "x0" "x1" "x2" ...
- forbid 是不允许出现的变量名, 比如 ~forbid:["x1" ; "a" ; "x3"], 那么生成
  的过程中就会跳过 "x1" 和 "x3".
*)
val init : base:string -> forbid:string list -> t

(*
从生成器获取一个新名字, 并返回改变后的生成器. 注意, 
原来的生成器不会改变, 即你在原本的生成器上再 get 一次,
得到的是一样的东西.
*)
val get : t -> string * t
