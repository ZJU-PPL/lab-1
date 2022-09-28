open Lambda

[@@@warning "-39"] 
[@@@warning "-27"]

(* Bonus: Combinator Theory *)

(* 定义

k = λ x . λ y . x
s = λ x . λ y . λ z . (x y) (x z)

上面定义的两个 lambda term 分别叫做 k combinator 和 s combinator.

再定义几个说法:
- Closed lambda term: 不含 free variables 的 lambda term.
- SK expression: 由 s, k, 和 Application 构成的表达式.
  例如 "s (k k) (s (k s) k)"
- 下面说的等价均指 beta-equivalence.

本部分将带领大家探索这样一件事:
  任意的 closed lambda term 都有等价的 SK expression.

这说明:
  仅由 s, k, 和 application 就能构造与 lambda calculus 
  同等强大的计算系统! 关键在于, 这里再也没有"讨厌的"绑定变量了!

是不是很有意思!

想了解更多, 可看如下资料
- https://people.cs.uchicago.edu/~odonnell/Teacher/Lectures/Formal_Organization_of_Knowledge/Examples/combinator_calculus/
- Stephen Wolfram: https://writings.stephenwolfram.com/2020/12/combinators-a-centennial-view/
- Lambda-Calculus and Combinators: An Introduction by J. Roger Hindley, Jonathan P. Seldin
  https://www.cin.ufpe.br/~djo/files/Lambda-Calculus%20and%20Combinators.pdf
*)

(*    
需要澄清一下, 虽然此处我们用 lambda calculus 来定义了 s 和 k, 但其实它们是可以独立于 lambda calculus 的.

独立地来说, combinators 的计算规则是
- k a b --> a
- s a b c --> (a c) (b c)

为了防止误会, 把 application 显式地用 app 写出来,
- app(app(k, a), b) --> a
- app(app(app(s, a), b), c) --> app(app(a, c), app(b, c))

上面的箭头 "-->" 表达的意思跟 beta-reduction 类似, 就是说
经过一步变换, 左边可以 reduce 为右边.

这是一套独立的变换系统, 可以与 lambda calculus 完全没有关系.

只是我们在整个实验过程中已经开发好了 lambda calculus 的系统,
所以我们不妨就在 lambda calculus 的环境里讨论 SK combinators.
*)

(* 先同样地定义一些辅助运算符, 方便一些. *)
let ($$) x y = C.Ap (x , y)
let ($) x y = Ap (x , y)
let clam s t = C.Lam (s , t)
let lam t = Lam t

(* k = λ x . λ y . x *)
let k = clam "x" @@ clam "y" @@ Var "x" |> to_locally_nameless

(* s = λ x . λ y . λ z . (x y) (x z) *)
let s = clam "x" @@ clam "y" @@ clam "z" @@ ((Var "x" $$ Var "z") $$ (Var "x" $$ Var "z")) |> to_locally_nameless

(* 一个比较 C.term 和 term 是否等价的函数, 只是 equiv 和 to_locally_nameless 的包装, 方便一些. *)
let test ct t = equiv (to_locally_nameless ct) t

(* 
接下来我们会尝试去找若干个 closed lambda term 的等价的 SK expression.
*)

(* first  = λ x . λ y . x *)
(* first' = k *)
let t_first = clam "x" @@ clam "y" @@ Var "x"
let sk_first = k

(* second  = λ x . λ y . y *)
(* second' = s k *)
(* 请你动手验证一下, 这两个确实是等价的 *)
let t_second = C.(Lam ("x" , Lam ("y" , Var "y")))
let sk_second = Ap (s , k)
let sk_second' = s $ k

(* 上面两个表达式为什么分别叫 first, second?
   可不是第一题和第二题的意思哦.
   first 输入两个参数, 返回第一个;
   second 输入两个参数, 返回第二个. *)

(* id  = λ x . x *)
(* id' = s k k *)
(* 为什么 id' = id ?
   - 从 lambda calculus 的角度来说,
     id' = s k k = λ z . (k z) (k z) = λ z . z = id
   - 从 s k 系统本身的规则来说, 我们有
     s k k z = (k z) (k z) = z
   - 如果用上前面的 second = s k, 可以更简便地看出
     s k k z = (s k) k z = second k z = z
     这样来说, 其实 s k <anything> z = second <anything> z = z
     换句话说, s k <anything> 都等价于 id.
*)
(* 与 id 等价的这个这个 SK expression 叫做 i.
   顺便提一句, 我们前面构造出来的 sk_something
   在后面构造 SK expressions 时都是允许使用的哦. *)
let t_id = clam "x" @@ Var "x"
let sk_id = s $ k $ k

(* 
我们讨论的是 SK expressions, 但你也会看到有人讨论 SKI expresions.
其实就是他们的系统中 I 也是原始符号, 定义为 I z = z.
但我们已经看到, I 可以由 S 和 K 定义, 所以我们就只说 SK expressions 了.
*)
let i = s $ k $ k

(*
顺便说, 上面的 sk_something 和 i 在下面都是可以用的喔.
*)

(* 现在到你啦! 仿照上面 sk_something 的做法, 
   为下面三个 lambda 表达式找到 SK expressions.  *)
(* ap f x === f x *)
(* ap  = λ a . λ b . a b *)
(* ap' = ? *)
let t_ap = clam "a" @@ clam "b" @@ (Var "a" $$ Var "b")
let sk_ap = C.Var "Todo"

(* 下面来个难点的. 你可能需要**好好思考一下**如何做这个转换. *)
(* ex1  = λ a . a (λ c . a c) *)
(* ex1' = ? *)
let t_ex1 = clam "a" @@ (Var "a" $$ (clam "c" @@ (Var "a" $$ Var "c")))
let sk_ex1 = C.Var "Todo"

(* 下面来个更难点的. *)
(* ex2  = λ a . λ b . b a (λ c . a c) *)
(* ex2' = ? *)
let t_ex2 = C.(Lam ("a" , Lam ("b" , Ap (Ap (Var "b" , Var "a") , Lam ("c" , Ap (Var "a" , Var "c"))))))
let sk_ex2 = C.Var "Todo"


(* 经过上面两个比较难的例子, 不知道你是否已经发现了一些规律
下面要做的事情, 就是系统地解决这个问题:

  想一个办法, 把任意的 closed lambda term 转换成 SK expressions.   
*)

module Systematic_SK = struct

  (* expr 类型捕捉了 SK expression 的概念:
     由 S, K, 以及 application 组成的表达式. *)
  type expr = 
    | App of expr * expr (* we call it App so that the names are not ambiguous. *)
    | S
    | K

  (* 把 SK expressions 转换成 lambda term 是容易的,
     只要把 S 变成 lambda term s, K 变成 lambda term k,
     App 变成 Ap, 就行啦. *)
  let rec to_lambda e = 
    match e with
    | S -> s
    | K -> k
    | App (e1 , e2) -> Ap (to_lambda e1 , to_lambda e2)
  
  exception HaveFreeVar

  (* 
     接下来就是你大展身手的时候了! 请你思考并实现 
     from_lambda : term -> expr, 使得对任意
     closed lambda term t, 都有
     to_lambda (from_lambda t) === t.
     换句话说, 把 t 转换成与 t 等价的 SK expression!

     没有你想像的那么难哦!

     tips
     - 如果你发现输入的 t 不是 closed term,
       请抛出上面定义的 exception HaveFreeVar (即 raise HaveFreeVar).
  *)
  let from_lambda (t : term) : expr = S (* Todo *)

end