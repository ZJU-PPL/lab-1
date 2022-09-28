open Base

(* 取消两种 warning.
    27 是 "unused variable"
    32 是 "unused value declaration"
    39 是 "unused rec flag"
   现在你不需要太关心这件事, 不过如果你想知道
   为什么要取消这三个 warning, 你可以注释掉这两行,
   看看会发生什么事.
*)
[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-39"] 

(* Todo 是一个 "异常". 如果你不了解 OCaml 中的异常, 你可以
   + 与 Python 中的 Exception, C++ 中的异常类比. 但要注意, OCaml 中的异常与前两者的异常有很大的区别, 区别就在于它是一等公民.
     所有异常的类型都是 exn, 所以 Todo : exn. 你可以定义接收 exn 值的函数 f : exn -> int -> int. 总之就是, 异常可以像
     普通的值一样被传来传去.
   + 参考 https://dev.realworldocaml.org/error-handling.html . 
   
   为什么要定义这个 Todo 异常呢? 很简单, 下面所有写着 raise Todo 的地方, 就是你要写代码的地方啦!
   这也是 OCaml 编程中的一个技巧 -- 用 raise Todo 这样的东西来代表你还未完成的函数. 
   但我们还是把所有 "let something = raise Todo" 都注释掉了, 原因是下面在使用 utop 的时候, 如果
   有未完成的 Todo, 它会直接异常退出. 
   *)
exception Todo 

(* 
  module C 定义了类型 term, 它表达了 lambda 表达式的抽象语法.
  类型 term 的一个值就是一个 lambda 表达式(的 AST 表示).
*)

module C = struct
  type term = 
    | Var of string
    | Lam of string * term
    | Ap of term * term

  (* 下面是一些例子. 作为练习, 你可以画出每个 lambda 表达式的树的形式. *)

  (* first = λ x . λ y . x *)
  let st_first = Lam ("x" , Lam ("y" , Var "x"))

  (* second = λ x . λ y . y *)
  let st_second = Lam ("x" , Lam ("y" , Var "y"))

  (* id = λ x . x *)
  let st_id = Lam ("x" , Var "x")

  (* 作为一个一般约定, 在文本书写时, 
     + application 的优先级高于 lambda abstraction;
     + lambda abstraction 是右结合的.
     在下面几个例子中, 我们把省略括号的式子和加上括号的式子都写出来, 便于掌握. *)
  (* with_free = x (λ y . y z)
               = x ( λ y . (y z) ) *)
  let st_with_free = Ap (Var "x" , Lam ("y" , Ap (Var "y" , Var "z")))

  (* exercise 1 = λ x . (λ y . x y) (λ z . z)
                = λ x . ( ( λ y . (x y) ) (λ z . z) ) *)
  let st_ex1 = Var "Todo" 

  (* exercise 2 = λ f . λ x . f x 
                = λ f . ( λ x . (f x) ) *)
  let st_ex2 = Var "Todo"

  (* pretty: term -> string . 故名思义, 它把一个 lambda term 
     变成一个漂亮的字符串. 
     
     将命令行切换到 project 根目录 (dune-project 文件所在的目录), 输入 "dune build" 就能编译. 

     输入 "dune utop" 就能打开一个交互式的 REPL (read-evaluate-print loop, 就像你直接在命令行里输入 python 时
     出来的那个交互式的界面. Utop 启动时, project 根目录下的 ".ocamlinit" 文件会被执行. 目前里面写着两句 open. 
     这与代码里的 open 意义是一样的, 打开 module, 可以不带前缀地访问里面的值. 

     在 utop 里敲 "C.pretty C.st_with_free" 试试! )
     *)
  let rec pretty t = 
    match t with
    | Var s -> s
    | Ap (t1 , t2) -> 
      let par1 = begin
        match t1 with
        | Var _ | Ap _ -> false
        | Lam _ -> true
      end in
      let par2 = begin
        match t2 with
        | Var _ -> false
        | Ap _ | Lam _ -> true
      end in
      let s1 = pretty t1 in
      let s2 = pretty t2 in
      let s1' = if par1 then "(" ^ s1 ^ ")" else s1 in
      let s2' = if par2 then "(" ^ s2 ^ ")" else s2 in
      s1' ^ " " ^ s2'
    | Lam (s , t') -> 
      let s' = pretty t' in
      Printf.sprintf "λ %s . %s" s s'
 
end

(* 上面的 lambda term 定义是不够好的. 为什么? 
   回顾课堂, 我们知道 lambda term 里被绑定的变量的名字 (比如 "x (λ y . y z)" 中的第二个 "y") 本质上表达了
   变量的绑定关系, 即这个变量对应的是哪个 "λ". 然而, 使用名字就会产生重名的问题, 在处理表达式的过程中
   就需要考虑名字冲突和换名. 这并不是一种优雅的解决方式.
    
   我们回归本质, 既然绑定了的变量的名字是用来标志绑定位置的, 我们何不直接记录一个变量的绑定位置?
   (请看 supplement 1 左侧) 用树的视角来说, 一个变量要么是自由变量, 要么被它到根路径上的一个 "λ" 绑定着. 
   我们可以采取这样的表达方式: 如果是自由变量, 还是记录它的名字; 如果是绑定变量, 就记录一个非负整数,
   表示此处的变量是被往上第几个 "λ" 绑定着 (从 0 开始计数). 这样得到 supplement 1 右侧的表示.
   这种表示称为 locally nameless representation, 因为 local (局部) 变量是没有名字的 (用了数字), 
   而自由变量仍然是有名字的.

   接下来我们实现 locally nameless representation.
    *)

(* 首先, 现在一个变量有可能是
   - 一个自由的 (即没有绑定的) 变量, 我们仍用名字来代表, 是为 Free of string ;
   - 一个绑定变量, 用到绑定位置的 "λ" 个数, 从 0 计数来表示, 是为 Bound of int . *)

type var =
  | Free of string
  | Bound of int

(* term 的定义与上面的区别在于 Var 里由 string 换成了 var, Lam 里不再需要记录绑定变量名了. *)
type term =
  | Var of var
  | Lam of term
  | Ap of term * term

(* 需要注意, 一个 term 的值不一定是合法的, 例如 λ . λ . 5 ,
   说要 5 个 λ 之上的绑定变量, 但其实只有两个 λ (所以最多只能写 0 或 1). 
   这里定义的 BadRepresentation 异常就是在遇到不合法的 term 时抛出的. *)
exception BadRepresentation of string

(* 你的任务来了! 请完成下面的函数 to_locally_nameless, 把一个 C.term, 即最开始定义的绑定变量
   用名字指代的那种 term, 处理成 locally nameless 表示的 term. 
   
   几个小 tips
   - 因为我们在 module C 外面, 所以 C.term 的 variants 现在用 C.Var , C.Lam, C.Ap 访问.
   - 你可能需要使用 Map 数据结构. 可以在这里学到它的用法: https://dev.realworldocaml.org/maps-and-hashtables.html .
   - 你可能需要定义辅助函数. 函数只是一个值而已, 所以可以用 
       let rec auxiliary_func x y = ... in 
     这样的方式在函数表达式内定义辅助函数, 在之后调用.

   你可以使用下面的框架, 也可以不使用. 但你需要保证 to_locally_nameless 的类型是 C.term -> term.
   *)

(* need test: 10 *)
(* 删掉 [ Var (Free "_") ], 取消下面的注释  *)
let to_locally_nameless (t : C.term) : term = Var (Free "_")
  (* let rec aux t d m : term =
    match t with
    | C.Var s -> raise Todo
    | C.Lam (s , t') -> raise Todo
    | Ap (t1 , t2) -> raise Todo
    in 
  aux t 0 (Map.empty (module String)) *)

(* 可以在 utop 中查看下面的值, 看看是否符合预期. 
   写好一个函数后在小例子上试一试看看对不对, 这种活动叫做 sanity check,
   翻译成中文可以是 "神志清醒检验", 即检查是否出现了神志不清的情况. *)
let t_first = to_locally_nameless C.st_first
let t_second = to_locally_nameless C.st_second

(* 顺便介绍两个函数作用的运算符.

   一般把 f 函数作用在 x 上, 我们就写 f x.

   然而, 如果我们要把 f, g, h 依次作用在 x 上, 我们就得写 h (g (f x)).
   这样写括号比较麻烦, 有个办法是写成 h @@ g @@ f @@ x. 顺序没变, 只是
   换成用 @@ 运算符了. 这个运算符其实啥也没干, f @@ x 就等于 f x. 它的特别之处
   其实是它的结合性. 我们知道函数调用是左结合的, h g f x 会变成 (((h g) f ) x). 
   而 @@ 运算符是右结合的, 所以 h @@ g @@ f @@ x 会变成 h @@ (g @@ (f @@ x)), 
   也就是 h (g (f x)) 啦. (
   
   还有一种有趣的函数作用方式, 与 bash 命令行中的管道 "|" 很类似. 上面的函数作用可以写作
     x |> f |> g |> h
   "|>" 是一个左结合运算符, x |> f = f x. 因此上面的式子就是
     ((x |> f) |> g) |> h = ((f x) |> g) |> h = (g (f x)) |> h = h (g (f x)).
   这种写法感觉就很帅.

   下面演示一下.
   *)

let t_id = to_locally_nameless @@ C.st_id
let t_with_free = C.st_with_free |> to_locally_nameless

(* 再顺便一提, 你觉得上面的 "@@" 和 "|>" 运算符是什么语言本身自带的, 神圣的东西吗?
   完全不是. 你如果在使用有溯源功能的编辑器, 可以对这两个运算符溯源 
   (VSCode 就是按住 Ctrl 然后鼠标点击那个运算符). 你会发现它们不过是 Base 库里定义的东西.
   你也可以定义自己的运算符!

   想了解更多运算符相关, 请看
   - https://v2.ocaml.org/manual/expr.html
   - https://blog.shaynefletcher.org/2016/09/custom-operators-in-ocaml.html

   下面是个例子.
*)

module Our_own_operators = struct 
  let (@$) f x = f x
  let (|/) x f = f x

  let t_id' = to_locally_nameless @$ C.st_id
  let t_with_free' = C.st_with_free |/ to_locally_nameless
end

(* 由于 locally nameless 下绑定变量的名字已经消解了, 所以如果
   两个 locally nameless term 是语法上等同的 (syntactically equal),
   即一模一样的, 那么它们就表示相同的 lambda expression.

   下面这个函数判断两个 locally nameless term 是否一模一样.

   其中只有一个 case 需要你来实现, 因为我们想通过这个函数教你一些东西
   - match 里面是可以同时 match 多个的, 用逗号把被 match 的表达式和
     下面的 pattern 分别分隔开就行了.
   - String.(s = s') 是一个有趣的表达式. String 是一个 module, 其中定义了
     (=) 这个运算符来比较字符串的相等. 这个表达式 **局部打开了这个 module** (locally open),
     String.( ... ) 括号里的东西就像在 open String 之后的环境里一样. 
     因此在其中用 (=) 这个运算符, 用的就是 String module 里的那个.

     能不能直接写 s = s' 呢? 你可以试试! 编译器说什么?
     核心其实是这个: 请问 "=" 是什么? 就是一个普通的函数! 只是它长成运算符的样子而已.
*)

(* need test: 2 *)
let rec syn_equal (t : term) (t' : term) : bool =
  match t , t' with
  | Var (Bound k) , Var (Bound k') -> k = k'
  | Var (Free s) , Var (Free s') -> String.(s = s')
  | Lam r , Lam r' -> syn_equal r r'
  | Ap (e1 , e2) , Ap (e1' , e2') -> raise Todo
  | _ -> false



(* 请实现下面这个函数 free_var : term -> string list.
   它的功能是求出 locally nameless term 中的所有 free variables,
   组成一个 list (不需要排序或者去除重复, 只要这个 list 包含且仅包含所有的
   free variables 即可). *)

(* need test: 4 *)
let rec free_var (t : term) : string list = raise Todo

(* 下面要做的是 to_locally_nameless 的反向, 即
   将 locally nameless representation (term) 变回
   用名字指代绑定变量的表示 (C.term).
   
   你可能已经发现了问题: locally nameless representation
   中绑定变量的名字是消解掉的, 不重要的, 那么变回 C.term
   时绑定变量名哪里来呢? 

   由于 lambda term 中具体的名字不重要 (α-equivalence),
   我们随便起名就好啦! 然而, 必须要小心, 不要把自由变量给捕获了.

   举例来说, "λ . λ . 1 a b", 转换为 "λ x . λ y . x a b"
   "λ u . λ v . u a b" 都是正确的, 因为绑定结构 (binding structure)
   是一致的. 然而, 转换成 "λ a . λ c . a a b" 就不对了, 
   因为 "a a b" 中的第二个 "a" 原本是自由变量, 现在却被第一个 λ 绑定了,
   即 lambda term 的含义变了.

   你应该能想到我们的解决方案: 既然名字不重要, 我们就主动生成一些
   互不相同的, 整个表达式里没有出现过的名字. 我们已经写好了一个 module fresh
   来帮助你做这件事. 请看 fresh.mli (fresh module 的 interface file) 了解如何
   使用它. 你可以在 utop 里先试用一下.

   一些小 tips
   - 你可能需要使用 Map
   - 遇到你认为不合法的情况, 可以抛出异常 raise (BadRepresentation "some description")

   *)

(* need test: 10 *)
let from_locally_nameless (t : term) : C.term = raise Todo

(* write a QuickCheck to check 
   t |> from_locally_nameless |> to_locally_nameless = t
    *)

(* 
接下来就是激动人心的 substitution 了!
所有 substitution 都在 locally nameless representation 上做,
因为这种表示就是为了方便 substitution 等操作的.
相信大家在课上已经理解了 substitution 的想法, 这里就不赘述.

我们分两种, 一种是把 term 中的自由变量替换掉.
subst_free (λ . λ . x y 1) "x" b = (λ . λ . b y 1)
*)

let rec subst_free (a : term) (x : string) (b : term) : term = raise Todo

(* 
另一种是用于 application 的计算.
我们本来有 (λ x . a) b = [b / x] a,
现在实现的 subst_bound 满足 subst_bound a b = [b / x] a.
例如, subst_bound (0 (λ . u 1)) b = b (λ . u b).
*)
let rec subst_bound (a : term) (b : term) : term = raise Todo

(* 
接下来就是激动人心的 normalization 了!

请先阅读助教写的讲义 (supplement 2) 了解 normalization,
然后阅读这部份的说明 (supplement 3), 了解具体实现上的问题.

*)

(* The following implementation is wrong. Why? 
   [Hint: consider normalizing "λ y . λ z . (λ a . λ b . a) (z (λ c . c))".] *)
let nf_wrong (t : term) : term =
  let rec aux (t : term) (l : term list) : term =
    match t with
    | Ap (t1 , t2) -> aux t1 (t2 :: l) 
    | Lam t' ->
      begin
        match l with
        | [] -> Lam (aux t' [])
        | u :: l' -> aux (subst_bound t' u) l'
      end
    | Var _ ->
      let norm_l = List.map l ~f:(fun t -> aux t []) in
      List.fold norm_l ~init:t ~f:(fun u v -> Ap (u , v))
    in
  aux t []

module Good_nf = struct
  
  type dvar = 
    | DIndex of int
    | DLevel of int
    | DFree of string

  type dterm = 
    | DVar of dvar
    | DLam of dterm
    | DAp of dterm * dterm

  (* 你可能需要的 dterm 版的 subst_bound *)
  let dsubst_bound (a : dterm) (b : dterm) : dterm = raise Todo

  (* 你可能需要的 dterm 和 term 之间的转换函数 *)
  let rec to_dterm (t : term) : dterm = raise Todo
  let rec from_dterm (t : dterm) : term = raise Todo

  (* 你要实现的 normalization 函数. 
     前面几个 "你可能需要的" 函数, 如果你不需要, 可以直接删掉.
     总之, 你需要实现一个正确的 nf : term -> term, 它也是
     最终测试的目标之一.
  *)

  let nf (t : term) : term = raise Todo

end

let nf = Good_nf.nf

(* 如果两个 lambda term 都有 normal form, 
   那么当两个 normal form 语法等同 (长得一模一样) 时,
   它们在等式语义下就是等价的. 下面的函数判断此事. *)
let equiv t t' = syn_equal (nf t) (nf t')