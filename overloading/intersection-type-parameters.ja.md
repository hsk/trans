# First Class Overloading via Insersection Type Parameters <a name="r_ast"></a>[*](#ast)

  Elton Cardoso 2 , Carlos Camarão 1 , and Lucilia Figueiredo 2
  
  1 Universidade Federal de Minas Gerais,

  camarao@dcc.ufmg.br
  
  2 Universidade Federal de Ouro Preto

  eltonm.cardoso@gmail.com, luciliacf@gmail.com

## 要約

  Hindley-Milner 型システムは、関数パラメータが単相型でなければならないという制約を課します。
  この制限を解除し、システムF の "ファーストクラス" 多相型を提供することは明らかに望ましいことですが、 システムF の型推論は決定不能です。
  近年、システムFに依存するが、多相型パラメータを有する関数の定義のための型注釈を必要とする高ランクの型を組み込んだより実用的な型システムが提案されています。
  しかし、これらの型注釈は必然的に高階関数のいくつかの可能な使用を許しません。
  この問題を回避し、コードの再利用を促進するために、関数本体内で多相的に使用される関数パラメータの型を指定するための交差型を使用して、ポリモーフィックまたはオーバーロードされた引数のアプリケーションに対してこのような関数を柔軟に使用できるようにします。

## 1 はじめに

  Hindley-Milner 型システム <a name="r9"></a>[[9]](#9) (HM) は、 Haskell <a name="r23"></a>[[23]](#23) や ML <a name="r20"></a>[20](#20) のような最近の関数型プログラミング言語の型システムの基礎として用いられています。
  これは、コンパイラがプログラマの助けを借りずに、あらゆる言語表現の主要型を推論できるという顕著な特性のためで、 型推論アルゴリズム <a name="r5"></a>[[5]](#5) は比較的単純です。
  しかしながら、いくつかの制約を課すことによってこれが達成されており、関数パラメータが単相型でなければならないという主な制約があります。

  たとえば、 HM 型システム では、次の定義は使用できません:

    foo g = (g [True,False], g [’a’,’b’,’c’])

  パラメータ `g` は、関数の本体で異なる型 (ブール値のリストと文字のリストの両方に適用される) で使用されるため、その型は単相的ではありません。したがって、 foo のこの定義は HM で型付けできません。

  対照的に、 Girard-Reynolds システム F \[<a name="r7"></a>[7](#7), <a name="r25"></a>[25](#25)\] では、型の中に汎用的な量指定子を使用することができます。
  システム F に基づく言語では、 foo を割り当てることができ、 たとえば、次のように型付けします

      (∀a.[a] → [a]) → ([Bool],[Char])

  ----

  <a name="ast"></a>[\*](#r_ast) この研究は、 FAPEMIG によって部分的に支援されています。

  ----

<!-- page 2 -->

  関数 `foo` は、例えば、関数適用 `(foo reverse)` で使用することができます。ここでは、 `∀a.[a] → [a]` 型の逆の list を計算します。

  `foo` の上記の型は、関数の矢印の左側に量子を含むので、 rank-2 型です。
  他の高階の型も `foo` に割り当てることができます。
  例えば、`length` が `∀a.[a] → Int` 型であるとき、 `(foo length)` のような関数適用で `foo` に 型 `(∀a.[a] → Int) → (Int,Int)` を割り当てます。

  しかしながら、これらの2つの型は、システム F では比較できません。
  言い換えれば、システム F には式の主要型が欠けています。
  1つの式は、2つ以上の比類のない型を持つ型付け可能であり、どちらも他の型よりも一般的ではありません。
  結果として、型推論は常に単一の型を選択することはできず、レットバインド定義の範囲全体でそれを使用することはできません。 特に、上記の foo の定義には主要な型は存在せず、他のすべてのインスタンスは一連のインスタンス化と一般化によってそれに従います。

  プログラミング言語の基礎としてシステムFを使用するもう一つの欠点は、このシステムの完全な型推論は決定不能であることです [31]。 3
  これらの問題に対処するために、より実用的な型システムが、プログラミング言語におけるより高ランクの多相型のサポートのための基礎として最近提案されています。
  主な考え方は、プログラマが多相型パラメータを持つ関数の定義のための型注釈を提供することを要求することで、型の曖昧さを回避し、型推論の指針として使用できる型情報を提供することです。
  この行に沿ったいくつかの関連する作品は、MLF [15,16]、FPH [30]、および柔軟なタイプ[18,17]です。
  これらの型システムは、 Hindley-Milner 型システムに導入された変更、特に必要な型の注釈の選択と型推論に使用される方針によって異なります。

  関数型パラメータの単相性の制限を避けることに関連する主要型の欠如と型注釈の必要性だけではありません。
  関数の上位ランクの多相型の注釈 (または、関数パラメータの量化された型の注釈) は、関数が使用されるコンテキストを必然的に制限します。
  たとえば、 foo の注釈の型が `(∀a.[a] → [a]) → ([Bool],[Char])` の場合、次の関数適用のいずれも型検査できません:

    foo length     where length    has type ∀a.[a] → Int
    foo head       where head      has type ∀a.[a] → a
    foo listMaybe  where listMaybe has type ∀a.[a] → Maybe a

  現代の関数プログラミング言語は、すでに HM への他の有用な拡張を含んでおり、上位ランクの関数がこれらの拡張と共にうまく機能することが望まれています。特に、上位ランクの関数は、オーバーロードをサポートするために Haskell で使用される型クラス [10,8] のメカニズムと連携してうまく機能することが望ましいのです。

  ----------

  3 Kfoury と Tiuryn は、確かにランクが3以上のシステムFの任意のサブセットに対する型付け性の決定不能性を証明しています [13]。

  ----------

<!-- page 3 -->

  例えば、以下の Haskell の例で `foo` を使うことを考えてみましょう:

      foo allEqual  where allEqual :: ∀a. Eq a ⇒ [a] → Bool
      foo sort      where sort :: ∀a. Ord a ⇒ [a] → [a]
      foo nub       where nub :: ∀a. Eq a ⇒ [a] → [a]
      foo fromEnum  where fromEnum :: ∀a. Enum a ⇒ a → Int

  上記の各引数の型は、(述語型とも呼ばれる) 制約された多相型です。
  Haskell では、型クラスは、ある値 (メンバ関数) が定義されている型 (インスタンス) のファミリを表します。
  例えば、等価演算子 `(==)` は `∀a. Eq a ⇒ a → a → Bool` 型です。ここで、クラス制約 `Eq a` は、等価演算子がパラメトリック多相ではないことを示しますが、`Eq` クラスのインスタンスである型に対してのみ動作します。
  言い換えると、クラス制約は、量化された型変数のインスタンス化可能な型を制限します。
  オーバーロードされたシンボルの型に導入されたクラス制約は、これらのシンボルに関して定義された式の型にも伝播されます。
  例えば ​​`(==)` 型の制約は、与えられたリストのすべての要素が等しいかどうかをチェックする関数` allEqual` の型に、また関数 `nub`の型にも渡されます。 指定されたリスト内の重複要素を削除します。 4

  型システム QMLF [19] は、私たちの知る限りでは、より高いランクの多相性がどのように制約付き多相型と連携して機能するかを調べる唯一の研究です。
  これは MLF 型システムを拡張し、上位クラスの型で述語制約 (クラス制約など) を指定できるようにします。
  たとえば、このシステムでは、 foo には型の注釈を付けることができます

      (∀a. Ord a ⇒ [a] → [a]) → ([Bool],[Char])

そして、QMLFでは `(foo sort)` と `(foo reverse)` の両方の関数適用が許可されています。`reverse` の型は `∀a.Ord a ⇒ [a] → [a]` と型付けできます。
  しかし、 `foo` のこの型の注釈は、例えば、上記リストの他のすべての関数適用を禁止します。
  特に、関数適用 `foo nub` は、 `nub`の型がクラスの制約上の `foo` のパラメータの型と異なっているにもかかわらず、型検査を行いません。
  したがって、述語制約付きの上位ランクの型の型注釈を許可しても、上位ランクの関数の柔軟な使用に関するすべての問題は解決されません。

  これらの問題を解決し、コードの再利用を促進するために、関数の本体でさまざまな型で使用されている関数パラメータの型に対して、プログラマが交差タイプを注釈できるようにする、異なるアプローチを採用しています。
  交差型 [3] は、 Hindley-Milner 型システムよりも多くのプログラムを型付けしながら、システムFの忠実な多型に概念的に単純で扱いやすい代替を提供します。

  ----

  4 Haskell の型クラスのより詳細な記述は、例えば [23,1,28] で見つけることができます。

  ----

<!-- page 4 -->

  我々の研究の新規性は、交叉型の引数を用いて上位ランクの関数の定義を可能にするために、通常の Hindley-Milner 型と制約付き多相型との交差型の組み合わせと、多相型または多重定義引数の両方へのそのような関数の適用です。

  モダリティ抽象化、データ型不変量、動的型、および汎用（または多型）関数の定義を含む、上位ランクの多型を必要とするいくつかの興味深いアプリケーションが、 [26,22,29] などで説明されています。
  これらの例のいくつかでは、上位ランクの型注釈は、そのような関数の可能な引数の集合を制限する目的にまさに機能し、いくつかの望ましい特性を保証します (例えば、[3] の第3章を参照)。
  したがって、この種の多形性のプログラミング言語における有用性、すなわち私たちの研究が補完的であるということに関して、我々は主張しないことは明らかです。

  我々の型システムは、束縛された多相型と交差型を組み合わせた CTi という名前です。
  このシステムの背後にある直感的な考え方は第2章で説明されています。
  3章では、束縛したいと思う2つの主要なトピック、すなわち制限された多相性と交差型について簡単に説明し、システムの型言語の形式的な定義を提示します。
  型システム CTi はセクション4で定義され、セクション5は対応する型推論アルゴリズムを提示します。
  最後に、我々の結論をセクション6に示します。

  ## 2 交差パラメータと多相引数

  前述の引数のいずれかがこの関数に適用されることを可能にする、(最小の)主要型の関数 `foo` を検討しましょう。
  直感的に、関数適用 `(foo f)` は `f` が boolean のリストと char のリストに適用できる関数であれば有効です。
  また、 `(foo f)` は `(τ1,τ2)` 型でなければならず、ここで `τ1` は boolean のリストに対する `f` のアプリケーションの結果の型、`τ2` は `f` を文字のリストに追加します。
  交差タイプを使用すると、これは次のように書くことができます（参照の容易さのために、fooの定義は以下で繰り返されます）。

  例 1.

      foo :: ∀b, c.([Bool] → b ∧ [Char] → c) → (b,c)
      foo g = (g [True,False], g [’a’,’b’,’c’])

  交差の型の使用は、このパラメーターの有限な使用だけが起こる可能性があるので、関数の本体でさまざまな型で使用される関数パラメーターの型を表現するための自然な選択と思われます。

  私たちの型システムは、通常の交差型システム[3,14,4]とは異なり、(HM + 制約付き多相型) の控えめな拡張として意図されており、型注釈がない場合にこのシステムとの完全な互換性を維持します。
  関数の本体内で多相的に使用されるパラメータの型が交差型の形式で指定される上位ランクの関数の定義には、型注釈が必要です。

<!-- page 5 -->

  An intersection type `(τ1 ∧ τ2)` may occur in the type of an expression only
  to the left of the function type constructor, as, for example, in the above type
  annotation for foo. A type `(τ1 ∧ τ2)` may also occur in typing context assignments,
  being introduced in this context by means of a type annotation.

  Since intersection types may only occur to the left of function arrows, in-
  tersection type elimination is restricted, in our system, to the rule for type
  derivation of a term variable. Dually, intersection type introduction may occur
  only in the type derivation for the argument of an application: assuming a term
  t can be assigned type `(τ1 ∧ τ2) → τ` in a given typing context, an application
  `(t u)` will be well typed in this context, if `u` can be assigned, in this context, a
  type σ that can be instantiated both to `τ1` and to `τ2`.

  For example, application `(foo reverse)` is well typed according to this rule,
  since the type annotated for foo can be instantiated to `([Bool] → [Bool] ∧
  [Char] → [Char]) → ([Bool],[Char])`, and the type of reverse can be in-
  stantiated to both `[Bool] → [Bool]` and `[Char] → [Char]`.

  Analogously, application `(foo sort)` is well typed, in a context where Bool
  and Char are both instances of type class `Ord`, since the type of sort can be
  instantiated to both `[Bool] → [Bool]` and `[Char] → [Char]`, in this context.
  Each of the applications of foo discussed previously would also be valid, in a
  context where the constraints on the type of the arguments could be satisfied.

  The type for each use of an intersection type parameter in the body of the
  function is derived by means of intersection type elimination. The appropriate
  type is inferred according to the type required in each context where the pa-
  rameter is used, in the same way as it happens for uses of overloaded symbols
  in a type system for context-dependent overloading, such as Haskell’s type class
  system. For example, the type for each use of parameter `g` in the body of foo
  is inferred according to the type of the argument to which `g` is applied. Dually,
  the type for each use of parameter `x` in the body of function `f1`, defined below,
  is infered according to the types of the parameters of the function to which `x` is
  given as argument.

  Example 2.

      f1 :: (Bool ∧ Int) → Int
      f1 x = if x then (x+1) else (x-1)

  Function f1 could be applied, for example, to an overloaded symbol, `say o ::
  C a ⇒ a`, declared in type class `C`, provided that there exist instance definitions
  of this class for types `Bool` and `Int`.

  Consider now the definition of function `f2` below:

  Example 3.

      f2 :: (Int → Int ∧ Bool → Bool) → (Int ∧ Bool) → (Int, Bool)
      f2 h y = (h y, h y)
      
<!-- page 6 -->

  The types of parameters `h` and `y` on each application `h y`, in the body of func-
  tion `f2`, can be determined by the type of the result of this function. Therefore,
  type inference for function `f2` is defined so as to use the additional information
  provided by type annotations for selecting the appropriate type for `h` and `y` on
  each application `h y`. On the other hand, if type annotations were provided only
  for the parameters of `f2`, then four incomparable types could be derived for `f2`,
  namely, `(Int,Bool)`, `(Int,Int)`, `(Bool,Int)` and `(Bool,Bool)`; it would not
  be possible then to determine the types of `h` and `y` on each application. No type
  could then be infered for `f2`.

  As in the case for overloaded symbols, an expression involving a function
  parameter annotated with an to an intersection type may sometimes be ambigu-
  ous. As an example, consider application `s z` in the body of function `f3` defined
  below:

  Example 4.

      f3 ::(Int → Int ∧ Bool → Int) → (Int ∧ Bool) → Int
      f3 s z = s z

  In this case, there are two distinct possible type derivations for the body of `f3`, corresponding to the two different possible choices for the types of parameters `s` and `z` in application `s z`.

todo-->
  ## 3 Constrained Polymorphism and Intersection Types

  Haskell’s type class system is based on the more general theory of qualified
  types [10], which extends Hindley-Milner type expressions with type constraints
  (or predicates). A constrained polymorphic type has the form `∀a.P ⇒ τ`, where
  `P` is a (possibly empty) set of class constraints and `τ` is a monomorphic type.
  Here, and elsewhere, we use the notation `a` for a sequence of type variables
  `a1, ..., an`, for some `n ≥ 0`.

  Type `∀a.P ⇒ τ` denotes the set of instances of `τ` that satisfy the predicates
  in `P`. For example, type `Int → Int → Bool` is an instance of type `∀a. Eq a ⇒
  a → a → Bool`, if constraint `Eq Int` can be satisfied according to the visible
  class and instance declarations, that is, if there exists an instance definition of
  type `Int` for class `Eq`. In other words, each class constraint `C τ` is an assertion
  that type `τ` must be an instance of class `C`. 5

  Satisfiability of class constraints can be described using an entailment rela-
  tion, denoted by the symbol `|=`. If `P` and `Q` are finite sets of type predicates,
  then the assertion that `P |= Q` means that predicates in `Q` are satisfied whenever
  predicates in P are satisfied. 6

  ----

  5 For simplicity, we only consider here single parameter type classes, since the exten-
  sion to multiparameter type classes [12,11] is orthogonal to the problem considered
  in this paper.
  6 See [10] for the general assumptions made about the predicate entailment relation
  `P |= Q`, and for a definition of the entailment relation induced by Haskell class and
  instance declarations.

  ----

<!-- page 7 -->

  The type language of our system extends the language of constrained poly-
  morphic types, introducing intersection types. The syntax of types and expres-
  sions of system CTi is given in Figure 1. Types are divided into monointersection
  types (τ ), that are used as types of expressions, and intersection types (ι), which
  can be introduced only by a type annotation for a function parameter, and ap-
  pear in the type of an expression only to the left of a function arrow.
  Intersection types

  ι
  Monointersection types τ
  Polymorphic types
  σ
  ::= τ | ι ∧ τ
  ::= a | ι → τ
  ::= ∀a. P ⇒ τ
  Type constraint P, Q ::= | P, C τ
  Typing context Γ
  Terms t, u ::=
  |
  |
  |
  |
  |
  ::= | Γ, (x : σ) | Γ, (x : ι)
  x
  Variable
  λx. t
  Functional abstraction
  λ(x :: ρ). t
  Annotated functional abstraction
  tu
  Function application
  let x = u in t Let-binding
  t :: σ
  Type annotated expression (σ closed)

  Figure 1. Syntax of types and terms

  The intersection type constructor (∧) is considered, as usual, to be commu-
  tative and associative. We write τ 0 ∈ (τ 1 ∧ . . . ∧ τ n ) if τ 0 = τ i , for some 1 ≤ i ≤ n.
  A typing context Γ conveys the typings of in-scope variables; Γ binds a term
  variable, x, to its type, either σ or ι.
  We define f tv(σ) to be the set of free type variables of σ, and extend this
  function to typing contexts in the obvious way (note that an intersection type ι
  does not have bound type variables):

      f tv(Γ ) =
      [
      [
      {f tv(σ) | (x : σ) ∈ Γ } ∪
      {f tv(ι) | (x : ι) ∈ Γ }

  The language of terms of type system CTi is the usual language of HM aug-
  mented with type annotations on both terms (t :: σ) and lambda-bound variables
  (λ(x :: ι). t). We assume that type annotations σ are closed , that is, they have
  no free type variables. 7

  7
  Open type annotations, which require lexically-scoped type variables [27], are
  avoided, because this introduces complications that are out of the scope of this
  work.

<!-- page 8 -->

  ## 4 Typing Rules

  The rules of type system CTi are defined in Figure 2. A typing judgement has
  the form

      P ; Γ ` t : τ

  meaning that type τ may be assigned to term t, in typing context Γ , if all
  constraints in P are satisfied, according to the constraint entailment relation,
  P |= Q, defined by the program class and instance declarations.

  P ; Γ ` t : τ
  ` inst σ ≤ P ⇒ τ
  P ; Γ, (x : σ) ` x : τ
  P ; Γ, (x : τ 0 ) ` t : τ
  P ; Γ ` λx. t : τ 0 → τ
  τ ∈ ι
  P ; Γ, (x : ι) ` x : τ
  (VAR)
  (VARi)
  P ; Γ, (x : ι) ` t : τ
  P ; Γ ` λ(x :: ι). t : ι → τ
  (ABS)
  (ABSA)
  P ; Γ ` t : ι → τ
  Q; Γ ` ∧ I u : ι
  (APP)
  P, Q; Γ ` t u : τ
  Γ ` gen t : σ
  ` inst σ ≤ P ⇒ τ
  Γ ` gen u : σ
  P ; Γ, (x : σ) ` t : τ
  P ; Γ ` let x = u in t : τ
  (LET)
  (ANNOT)
  P ; Γ ` (t :: σ) : τ
  Γ ` gen t : σ
  ` inst σ ≤ P ⇒ τ
  P ; Γ ` t : τ
  a = f tv(P ⇒ τ ) − f tv(Γ )
  Γ ` gen t : ∀a. P ⇒ τ
  Q |= [a 7→ τ 0 ]P
  (GEN)
  (INST)
  ` inst ∀a. P ⇒ τ ≤ Q ⇒ [a 7→ τ 0 ]τ
  P ; Γ ` ∧ I t : ι
  P i ; Γ ` t : τ i , for i = 1, . . . , n
  (P 1 , . . . , P n ); Γ ` ∧ I t : τ 1 ∧ . . . ∧ τ n
  (GENi)

  Figure 2. Type system CTi

  The type system is presented in a syntax-directed form, where all type deriva-
  tions for a given term t (if there are any) have the same structure, uniquely 

<!-- page 9 -->

  determined by the syntax structure of t. A syntax directed-formulation for a type
  system is closer to type inference, making it easier to derive the type inference
  algorithm directly from the type system.
  The type rules presented in Figure 2 that are related to the handling of
  constraints are the usual ones in a syntax-directed type system for constrained
  polymorphism [10]. Type constraint introduction and polymorphic generalisation
  are restricted to the type derivation rule for let-bindings (LET), and are defined
  in Figure 2 by means of the auxiliary judgment
  Γ ` gen t : σ
  The rule for this judgment essentially says that all constraints on the type vari-
  ables of a type must be moved from the global context to this type before
  universal quantification of its type variables.
  Dually, polymorphic instantiation and type constraint elimination are re-
  stricted to rule (VAR). Constrained polymorphic instantiation is defined in Fig-
  ure 2 by means of the auxiliary judgement
  ` inst σ ≤ P ⇒ τ
  Notation [a 7→ τ 0 ], used in this rule, is an abreviation for [a 1 7→ τ 1 0 , . . . , a n 7→ τ n 0 ],
  which represents the capture-avoiding substitution that maps each type variable
  a i to monointersection type τ i , for i = 1, . . . , n.
  Rule (INST) formalizes constrained polymorphic type instantiation: type
  ∀a. P ⇒ τ may be instantiated to type Q ⇒ [α 7→ τ 0 ]τ , provided the set of
  constraints [α 7→ τ 0 ]P is entailed by the set of constraints Q, according to the
  assumed constraint entailment relation. For example, the usual class and in-
  stance definitions for class Eq a in Haskell induce the relation Eq a |= Eq [a],
  and thus type ∀a.Eq a ⇒ a → a → Bool can be instantiated to type Eq a ⇒
  [a] → [a] → Bool.

  The only new rules are (VARi), (ABSA) and (APP). Rule (VARi) defines the
  type derivation for a term variable that is bound to an intersection type in the
  typing context, by means of intersection type elimination. Rule (ABSA), very
  similar to the usual rule (ABS), defines type derivations for lambda-abstractions
  for which the type of the parameter is annotated.

  Type derivation for an application (t u) may require intersection type intro-
  duction for the derivation of the type of the argument (u), as discussed previously
  in Section 2. This is dealt with by means of the special judgment

  P ; Γ ` ∧ I u : ι

  The rule for the derivation of this judgment (GENi) formalizes the idea of in-
  tersection type introduction, also collecting the constraints that apply on each
  type τ ∈ ι into the global constraint context.

  Type system CTi is a conservative extension of the qualified type system [10],
  in the sense that, in the absence of type annotations, a judgment P ; Γ ` t : τ
  is derived in system CTi if and only if it is also derived in the qualified type

<!-- page 10 -->

  system. This can be proved by noticing that, in the absence of type annotations,
  no intersection type can be introduced in the typing context, neither can such a
  type occur in the type of an expression. Thus, the new rules (VARi) and (ABSA)
  of system CTi are never used for the derivation of the type of an expression
  that does not include intersection type annotations. Moreover, the type rule for
  application (APP) of system CTi reduces to the usual type rule for application
  in the qualified type system, when only such expressions are considered.

  Type inference can usually be derived almost directly from a syntax-directed
  formulation for a type system. The idea is to substitute each type that must
  be guessed in a type derivation rule by a fresh type variable, delaying guessing
  until these types can be later determined, by means of unification (see eg. [21]).
  This technique cannot yet be applied, however, to the rules defined in Figure 2,
  because a different source of type guessing arises: on intersection type elimina-
  tion, in rule (VARi), and on intersection type introduction, in the derivation of
  a judgment P ; Γ ` ∧ I u : ι, used in rule (APP). The idea is to use information
  provided by type annotations as an additional context for guessing the appro-
  priate type in these cases. Information provided by type annotations therefore
  must be propagated inwards in a type derivation. The idea of taking advantage
  of type annotations in this way is called bidirectional type inference [22], or local
  type inference [24]. Bidirectional type inference for system CTi is presented in
  the next section.

  ## 5 Bidirectional Type Inference

  The bidirectional type inference rules for system CTi are presented in Figure 3.
  The typing rules defined in this figure express the idea of propagating types
  inwards, and describe two similar typing judgments:

      P ; Γ ` ⇑ t : τ

  means that, in a typing context Γ , term t can be inferred to have type τ , provided
  that predicates in P are satisfiable.

      P ; Γ ` ⇓ t : τ

  means that, in a typing context Γ , term t can be checked to have type τ , provided
  that predicates in P are satisfiable. The up-arrow (⇑) suggests pulling a type
  up out of a term, whereas the down-arrow (⇓) suggests pushing a type down
  into a term. The auxiliary judgments for type generalization, Γ ` gen t : σ,
  polymorphic instantiation, ` inst σ ≤ P ⇒ τ , and intersection type introduction,
  P ; Γ ` ∧ I t : ι, are treated in the same way.

  The main idea of the bidirectional typing rules is that a term might be
  typeable in checking mode when it is not typeable in inference mode; for example
  the term (λ(x :: Int ∧ Bool. x) can be checked with type (Int ∧ Bool) → Int,
  but is not typeable in inference mode. On the other hand, if we infer the type
  for a term, we can always check that the term has that type. That is:

<!-- page 11 -->

  P ; Γ ` δ t : τ
  ` inst σ ≤ P ⇒ τ
  P ; Γ, (x : σ) ` δ x : τ
  P ; Γ ` δ λx. t : τ 0 → τ
  P, Q; Γ ` δ t u : τ
  τ ∈ ι
  P ; Γ, (x : ι) ` ⇓ x : τ
  (VAR)
  P ; Γ, (x : τ 0 ) ` δ t : (τ, P 0 , Γ 0 )
  P ; Γ ` ⇑ t : ι → τ
  I
  Q; Γ ` ∧
  ⇓ u : ι
  δ =⇑ ⇓
  (ABS)
  P ; Γ, (x : ι) ` δ t : τ
  P ; Γ ` δ λ(x :: ι). t : ι → τ
  (x : ι) ∈ Γ, for some ι
  Q; Γ ` ⇑ u : τ 0
  P ; Γ ` ⇓ x : τ 0 → τ
  (APP1)
  (VARi)
  P, Q; Γ ` δ x u : τ
  (ABSA)
  (APP2)
  (x 1 : ι 1 ) ∈ Γ and (x 2 : ι 2 ) ∈ Γ
  ∃ τ 0 . (τ 0 → τ ∈ ι 1 ) and (τ 0 ∈ ι 2 )
  (APP3)
  P ; Γ ` δ x 1 x 2 : τ
  P, Q 0 ; Γ ` ⇓ t : τ 0
  ` inst ∀a. Q 0 ⇒ τ 0 ≤ Q ⇒ τ
  Γ ` gen
  ⇑ u : σ
  P ; Γ, (x : σ) ` δ t : τ )
  P ; Γ ` δ let x = u in t : τ
  (LET)
  P, Q; Γ ` δ (t :: ∀a. Q 0 ⇒ τ 0 ) : τ
  Γ ` gen
  ⇑ t : σ
  ` inst
  σ ≤ ρ
  δ
  τ 0 = [a 7→ τ 00 ]τ
  Q |= [a 7→ τ 0 ]P
  P ; Γ ` ⇑ t : τ
  a = f tv(P ⇒ τ ) − f tv(Γ 0 )
  Γ ` gen
  ⇑ t : ∀a. P ⇒ τ
  (ANNOT)
  (GEN)
  ` inst
  ∀a. P ⇒ τ ≤ Q ⇒ τ 0
  δ
  (INST)
  P ; Γ ` ⇓ ∧ I t : ι
  P ; Γ ` ⇓ t : τ for each τ ∈ ι
  P ; Γ ` ⇓ ∧ I t : ι
  (GENi)

  Figure 3. Bidirectional type inference for type system CTi

      If P ; Γ ` ⇑ t : τ then P ; Γ ` ⇓ t : τ

  Furthermore, the checking mode allows us to give to a term types that are
  more specific than its most general type. In contrast, the inference mode may
  only produce the most general type. For example, if a variable has type (∀a. a),
  we can check that it has this type and also that it has types Int, Int → Int,
  ∀a. [a] → [a] etc. On the other hand, we will only be able to infer b, where b
  is a fresh type variable.

<!-- page 12 -->

  Most of the rules in Figure 3 are the same in any direction δ, namely (VAR),
  (ABS), (ABSA), (LET) and (ANNOT). They can be seen as shorthand for two rules
  that differ only in the arrow direction.

  Intersection type elimination, defined by rule (VARi), is not allowed in the
  inference mode, since this would imply a nondeterministic choice for the type
  of the variable, among the component types of the intersection type to which it
  is bound in the typing context. In type checking mode, on the other hand, this
  amounts to verifying that the type to be checked for the variable occurs in its
  intersection type.

  Analogously, intersection type introduction, defined by means of judgment
  P ; Γ ` ⇓ ∧ I t : ι is also only allowed in type checking mode, and amounts to
  checking that each type τ ∈ ι can be derived for term t in context (P ; Γ ).

  Type inference and checking for an application (t u) is now split into three
  rules, in order that cases that require intersection type elimination can be treated
  more properly. Rule (APP3) is used only for typing an application (t u) where
  both t and u are variables (intersection type parameters), namely, x 1 and x 2 ,
  bound to intersection types ι 1 and ι 2 in Γ , respectively. This case requires si-
  multaneous intersection type elimination for derivation of the types for both x 1
  and x 2 . This is the case, for example, of each application h y in the body of
  function f2 of Example 3, and also of application s z in the body of function
  f3 of Example 4, both discussed in Section 2. Using the information provided
  by the type annotation in the definition of function f2, one can check that the
  first application h y in the body of this function may be assigned type Int, and
  the second may be assigned type Bool. In a type inference algorithm based on
  the bidirectional system, application (s z), in the body of function f3, can be
  detected as ambiguous: this is done by imposing an additional condition on rule
  (APP3), for checking that there exists a unique type τ 0 such that τ 0 → τ ∈ ι 1 and
  τ 0 ∈ ι 2 , where τ is known, in check mode. The implementation of type inference
  and detection of ambiguity is briefly discussed at the end of this section.

  Rule (APP2) is used only for typing an application (t u) where t is variable,
  namely, x, bound to an intersection type ι in Γ . This is the case, for example,
  of each use of parameter g in the body of function foo of Example 1. In this
  case, intersection type elimination is required for inferring the type for x, and
  the type inferred for the argument u provides an additional type context for the
  intersection type elimination. In other words, a type τ 0 is firstly inferred for the
  argument u, and knowledge of this type is then used for checking the type of x,
  that is, for choosing a type τ 0 → τ among the component types of type ι bound
  to x, where type τ is known in checking mode. Note that this choice is guaranteed
  to be unique in check mode, but not in inference mode. This possible ambiguity
  on the choice of the type for x also can be detected by a type inference algorithm
  based on the bidirectional system, by imposing an additional condition on rule
  (VARi), that requires that this choice must be unique.

  Rule (APP1) is used in all other applications (t u) where the other two rules
  for application do not apply, that is, when the term t is not a variable, or it is
  a variable that is not bound to an intersection type in the given typing context.

<!-- page 13 -->

  In this case, we first infer a type ι → τ for function t, and then check that the
  argument u has type ι. In this way we take advantage of the information given
  by the parameter type of t, to provide an additional type context for determining
  the type for the argument u. Notice that, even in checking mode, we ignore the
  required type ι when inferring the type for the function t, because this additional
  information is not important in this case.

  Rule (APP1) accounts indeed for three distinct possible cases:

  1. The parameter type for t is indeed an intersection type ι. In this case, type
  checking for the type of u involves intersection type introduction, which is
  driven by the required type ι. This is the case, for example, of applications
  foo reverse and foo sort discussed in Section 2, where foo is the function
  defined in Example 1.

  2. The parameter type for t is not an intersection type, say, the type inferred
  for t is τ 0 → τ , and u is a variable bound to an intersection type ι u . This is
  the case, for example, of each use of parameter x in the body of function f1
  of Example 2. In this case, checking for the type of u involves intersection
  type elimination, which is driven by the required type τ 0 .

  3. The last case corresponds to usual applications where neither the function
  parameter type nor the argument type are an intersection type. Note that,
  in this case, rule (APP1) corresponds to the usual inference rule for an ap-
  plication of constrained polymorphic type systems.

  The correspondence between the bidirectional type inference defined in Fig-
  ure 3 and type system CTi defined in Figure 2, is stated by the following theorems
  (proofs are omitted for space reasons and will be provided in a technical report):

  Theorem 1 (Soundness). If P ; Γ ` ⇑ t : τ then P ; Γ ` t : τ .

  Theorem 2 (Completeness). If P ; Γ ` t : τ then P ; Γ ` ⇑ t : τ .

  A bidirectional type inference algorithm that infers principal types for ex-
  pressions of type system CTi can be derived directly from the rules of Fig-
  ure 3, using the ideas described in [22], and providing an implementation for
  the entailment relation P |= Q that is used in these rules. We have developed
  an implementation for this algorithm, written in Haskell, which is available at
  https://github.com/emcardoso/CTi.

  Another important aspect of the type inference is detection of ambiguity.
  There are two possible sources of ambiguity in system CTi: those arising from
  the use of parameters annotated with an intersection type, and those arising
  from the use of overloaded symbols. The first source of ambiguity is ruled out by
  imposing appropriate conditions on rules (APP3) and (VARi), for checking that
  when intersection type elimination is required, the appropriate type is uniquely
  determined. Ambiguity arising from overloading can be treated as usual, that is,
  by imposing a suitable syntactic condition over the principal type inferred for an
  expression, that guarantees that this expression is not ambiguous. In the case
  of a type system with single parameter type classes, a term t with inferred type

<!-- page 14 -->

  ∀a. P ⇒ τ can be guaranteed to be nonambiguous if f tv(P ) − f tv(τ ) = ∅, as
  proposed in [10]. For multiparameter type classes a more appropriate condition
  should be used (see, for example, [2] or [6]). A more detailed discussion of the
  implementation of the type inference algorithm and the detection of ambiguity
  will be presented in a further work.

  ## 6 Conclusion

  This paper presents a type system, called CTi, that is a conservative extension of
  the system Hindley-Milner plus type classes with the introduction of a restricted
  form of higher rank polymorphism that allows to specify parameter types with
  intersection types. A type inference algorithm that is sound and complete with
  respect to the type system is also presented. A prototype of the type inference
  algorithm has been implemented in Haskell. The higher rank polymorphism pro-
  vided by CTi is complementary to the usual higher rank polymorphism of type
  systems based on system F. There are expressions that can be typed in CTi that
  cannot be typed in these other higher rank systems, and vice-versa.

  ## References

  1. Richard Bird. Introduction to Functional Programming using Haskell. Prentice
  Hall, 2nd edition, 1998.
  2. Carlos Camarão, Rodrigo Ribeiro, Lucı́lia Figueiredo, and Cristiano Vasconcellos.
  A solution to haskell’s multiparamemeter type class dilemma. In Proceedings of
  13th Brazilian Symposium on Programming Languages, pages 19–21, 2009.
  3. M. Coppo and M. Dezani-Ciancaglini. An extension of the basic functionality
  theory for the λ-calculus. Notre Dame J. Formal Logic, 21(4):685–693, 1980.
  4. M Coppo, M Dezani-Ciancaglini, and B. Veneri. Principal type schemes and
  lambda-calculus semantics. Academic Press, 1980.
  5. L. Damas and R. Milner. Principal type-schemes for functional programs. In POPL
  ’82: Symposium on Principles of Programming Languages, pages 207–212. ACM
  Press, 1982.
  6. Simon Peyton Jones et. al. The glorious glasgow haskell compilation system
  user’s guide, version 4.0.6. available at http://www.haskell.org/ghc/docs/6.
  12.2/users_guide.pdf, 1999.
  7. Jean-Yves Girard. Interprétation fonctionnelle et élimination des coupures de
  l’arithmétique d’ordre supérieur. PhD thesis, Université Paris VII, 1972.
  8. Cordelia Hall, Kevin Hammond, Simon Peyton-Jones, and Philip Wadler. Type
  classes in haskell. ACM Transactions on Programming Languages and Systems
  (TOPLAS), 18(2):109–138, 1996.
  9. R. Hindley. The principal type-scheme of an object in combinatory logic. Trans-
  actions of the American Mathematical Society, 146, 1969.
  10. Mark P. Jones. Qualified Types: theory and practice. PhD thesis, University of
  Nottingham, Department of Computer Science, 1994.

<!-- page 15 -->

  11. Mark P. Jones. A system of constructor classes: overloading and implicit higher-
  order polymorphism. Journal of Functional Programming, 5:1–35, 1995.12. Simon Peyton Jones, Mark P. Jones, and Erik Meijer. Type classes: an exploration
  of the design space. In Haskell Workshop. ACM Press, June 1997.
  13. A. J. Kfoury and J. Tiuryn. Type reconstruction in finite fragments of second
  order lambda calculus. Information and Computation, 98(2):228–257, 1992.
  14. A. J. Kfoury and J. B. Wells. Principality and decidable type inference for finite-
  rank intersection types. In POPL’99: ACM Conference on Principles of Program-
  ming Languages, pages 161–174. ACM Press, 1999.
  15. Didier Le Botlan and Didier Remy. Mlf: Raising ML to the power of system F.
  In ICFP’2003: International Conference on Functional Programming, pages 27–38.
  ACM Press, 2003.
  16. Didier Le Botlan and Didier Remy. Recasting MLF. Information and Computaion,
  207(6):726–785, 2009.
  17. Daan Leijen. Hmf: Simple type inference for first-class polymorphism. In
  ICFP’2008: International Conference on Fucntional Programming, pages 283–294.
  ACM Press, 2008.
  18. Dann Leijen. Flexible types: robust type inference for first-class polymorphism. In
  POPL’2009: International Conference on Principles of Programming Languages,
  pages 66–77. ACM Press, 2009.
  19. Dann Leijen and Andres Löh. Qualified types for MLF. In ICFP’2005: Interna-
  tional Conference on Fucntional Programming, pages 144–155. ACM Press, 2005.
  20. R. Milner, M. Tofte, R. Harper, and D. MacQueen. The Definition of Standard
  ML, revised edition. MIT Press, 1997.
  21. John Mitchell. Foundations for Programming Languages. MIT Press, 1996.
  22. S. Peyton-Jones, D. Vytiniotis, S. Weirich, and M. Shields. Practical type inference
  for arbitrary rank types. Journal of Functional Programming, 17(1):1–82, 2007.
  23. Simon Peyton-Jones. Haskell 98 language and libraries: the revised report. Cam-
  bridge University Press, 4 edition, 2003.
  24. Benjamin C. Pierce and David N. Turner. Local type inference. ACM Trans.
  Program. Lang. Syst., 22(1):1–44, 2000.
  25. John C. Reynolds. Towards a theory of type structure. In Programming Sympo-
  sium, Proceedings Colloque sur la Programmation, pages 408–423, London, UK,
  1974. Springer-Verlag.
  26. C. Shan. Sexy types in action. ACM SIGPLAN Notices, 39(5):15–22, 2004.
  27. Mark Shields and Simon Peyton-Jones. Lexically scoped type variables. http:
  //research.microsoft.com/ ~simonpj/papers/scoped-tyvars/scoped.ps, 2004.
  28. Simon Thompson. Haskell: The Craft of Functional Programming. Addison-Wesley,
  2nd edition, 1999.
  29. Janis Voigtlander. Types for Programming Reasoning. PhD thesis, Technische
  Universität Dresden, 2009.
  30. Dimitrios Vytiniotis, Stephanie Weirich, and Simon Peyton Jones. FPH: first-class
  polymorphism for haskell. In ICFP08: Proc. of the 13th ACM SIGPLAN Int. Conf.
  on Functional Programming, pages 295–306. ACM Press, 2008.
  31. J. B. Wells. Typability and type checking in the second-order lambda-calculus are
  equivalent and undecidable. Annals of Pure and Applied Logic, 98(1–3):111–156,
  1999.
