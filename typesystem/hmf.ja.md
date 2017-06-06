# HMF: Simple type inference for first-class polymorphism

  Daan Leijen
  Microsoft Research
  daan@microsoft.com

## 要約

HMF は、Hindley-Milner 型推論とファーストクラスの多相型および通常のシステム F 型の保守的な拡張です。
このシステムは、単純な型付け規則と、通常の Damas-Milner アルゴリズムのわずかな拡張である非常に単純な型推論アルゴリズムを使用して、他の提案とは区別されます。
相対的にシンプルで表現力があれば、 HMF は実際には非常に魅力的な型システムであると感じています。
http://research.microsoft.com/users/daan/pubs.html にある型システムの参照実装があります。

## 1. はじめに

関数型言語の型推論は、 通常、 Hindley-Milner 型システム (Hindley 1969; Milner 1978; Damas and Milner 1982) に基づいています。
Hindley-Milner は、 単純な論理仕様と、 型推論アルゴリズムを備えており、 型のアノテーションなしで、 最も一般的な、 または主要な型を式のために自動的に推論することができます。

自動型推論を実現するために、 Hindley-Milner 型システムは、 関数の引数と構造の要素が単調にしかならない多相型を制限します。
形式的に言えば、 これは、 普遍的な量限定子が最も外側のレベルにしか現れない (すなわち、 より高いランクの型は許されない) ことを意味し、 定量化された変数は、 単相型でインスタンス化することができます。(不可解なインスタンス化は許可されません)。
これらは実際には厳しい制限です。
ファーストクラスの多相型の使用はまれではありますが、通常は良い代替案や回避策はありません (より良い概要は (Peyton Jones et al. 2007) を参照)。

ファーストクラス多相型の参照計算は、 明示的に型指定された システム F です。
Remy(2005) が述べたように、 System F の表現力と Hindley-Milner 型推論の利便性が組み合わされています。
残念なことに、 システム F の完全な型推論は決定不能です (Wells 1999)。
したがって、 私たちの目標を達成するための唯一の方法は、 一流の多相型を使ったプログラミングを楽しい経験にするためにプログラマーに与えられた注釈で、 Hindley-Milner 型推論を強化することです。

この分野にはかなりの研究がなされていますが (Peyton Jones et al. 2007; Remy 2005; Jones 1997; Le Botlan and Remy 2003; Le Botlan 2004; Odersky and Laufer 1996; Garrigue and Remy 1999a; Vytiniotis et al. 2006; Dijkstra 2005)、完全に満足できる解決策はまだ見出されていません。
提案された多くのシステムは非常に複雑で、 アルゴリズム仕様などを使用したり、 通常のシステムFタイプを超える新しい型を導入したりします。

この記事では、実際に少数の注釈を必要とするファーストクラスの多形性を持つ Hindley-Milner の単純で保守的な拡張である HMF を紹介します。
シンプルさと表現力の組み合わせは、 HMF を実際に Hindley-Milner の非常に魅力的な代替物にすることができます。
特に:

HMF は控えめな拡張です: Hindley-Milner でよく整形されたすべてのプログラムは、よく型付けされた HMF プログラムであり、そのようなプログラムでは型の注釈は決して必要ありません。
HMF は、型のアノテーションを使用して、より高いランクの System F 型とインフォーマシブなインスタンス化を持つファーストクラスの多相的値をサポートします。
 
- 実際には、 Hindley-Milner を超えたプログラムには型アノテーションはほとんど必要ありません。
多相的パラメータとあいまいな主観的インスタンス化に注釈を付ける必要があります。
両方のケースを明確に指定することができ、実際に適用するのは比較的簡単です。

HMF は抽象化に関して堅牢です。
関数適用 `e1 e2` が型付けされている場合はいつでも、抽象化には `e1 e2` が適用されるという顕著な特性があります。
これを重要な特性と考えています。一般的な多相性の値に対して一般的な多相性の抽象化を再利用できることを意味します。

- アルゴリズムW (Damas and Milner 1982) に類似した主要型を推論するシンプルで効果的な型推論アルゴリズムがあります。

次のセクションでは、実際に HMF の概要を示します。
セクション4は、 HMF の正式な論理型規則を提示し、 その後、 セクション6の型推論アルゴリズムについて説明します。
最後に、セクション5では、型の注釈について詳しく説明します。

## 2. 概要と背景

  HMF は、Hindley-Milner を、多相型の値がファーストクラスである通常の System F 型で拡張します。
  ファーストクラスの多相型をサポートするには、上位ランクの型と批判的なインスタンス化という2つの要素が必要です。

## 2.1 高ランク型

  Hindley-Milner は、さまざまな型のインスタンス化で定義を多相性で再利用できるようにします。
  たとえば、アイデンティティ関数を考えてみましょう:

      id :: ∀α. α → α (inferred)
      id x = x

  この関数はその引数で多相型であるため、任意の値に適用することができ、整数とブール値の両方に `id` が適用されるタプル式 `(id 1, id True)` が型定義されます。
  残念ながら、定義だけが多相的であり、構造のパラメータや要素はそうではありません。
  我々は、多相型パラメータを可能にするために、より高いランクのタイプを必要とします。

<!-- page 2 -->

    たとえば、次のプログラムを実行します。

      poly f = (f 1, f True) (rejected)

  Hindley-Milner ではこのプログラムは拒否されます。これは、パラメータ `f` が `Int` と `Bool` の両方に適用できるような単相型が存在しないからです。
  しかし、 HMF では、多相型のパラメータで明示的に注釈を付けることができます。
  例えば:

      poly (f :: ∀α. α → α) = (f 1, f True)

型 `(∀α. α → α) → (Int, Bool)` で HMF に型どりがよく、関数適用 `poly id` の型が整っています。
   `poly` の推論された型は、量化子が関数型の中にネストされているため、より上位の型です。
   パラメータ `f` には、多くの多相型を割り当てることができます。たとえば、 `∀α. α → α → α`、または `∀α. α → Int` であり、どちらも他方のインスタンスではありません。
   このため、 HMF は自動的にパラメータの多相型を自動的に推論することはできず、多相型を持つパラメータには注釈を付ける必要があります。

   より高位の多相型は、状態およびメモリトランザクションのタイプセーフカプセル化、データ構造の融合、および汎用プログラミングを含む、実際には多くの応用を有します。
   このような応用の概要については、興味のある読者は以下を参照してください (Peyton Jones et al. 2007)。

## 2.2 Impredicative instantiation

  Besides higher-rank types, HMF also supports the other ingredient for first-class polymorphism, namely impredicative instantiation,  where type variables can be instantiated with polymorphic types (instead of just monomorphic types).
  We believe that this is a crucial property that enables the use of normal polymorphic abstractions over general polymorphic values
  For example, if we define:

      apply :: ∀αβ.(α → β) → α → β (inferred)
      apply f x = f x

  then the expression

      apply poly id

  is well-typed in HMF, where the type variable α in the type of apply is impredicatively instantiated to the polymorphic type `∀α. α → α` (which is not allowed in Hindley Milner).
  Unfortunately, we cannot always infer impredicative instantiations automatically since this choice is sometimes ambiguous.

  Consider the function single :: ∀α. α → [α] that creates a
  singleton list (where we use the notation [α] for a list of elements of
  type α). In a predicative system like Hindley-Milner, the expression
  single id has type ∀α. [α → α]. In a system with impredicative
  instantiation, we can also a give it the type [∀α. α → α] where all
  elements are kept polymorphic. Unfortunately, neither type is an
  instance of the other and we have to disambiguate this choice.

  Whenever there is an ambiguous impredicative application,
  HMF always prefers the predicative instantiation, and always introduces
  the least inner polymorphism possible. Therefore, HMF is by
  construction fully compatible with Hindley-Milner and the type of
  single id is also ∀α. [α → α] in HMF. If the impredicative instantiation
  is wanted, a type annotation is needed to make this choice
  unambigious. For example, we can create a list of polymorphic
  identity functions as:1

      ids = (single :: (∀α. α → α) → [∀α. α → α]) id

  where ids has type [∀α. α → α]. Fortunately, ambiguous impredicative
  applications can only happen in few specific cases, namely
  when a function with a type of the form ∀α. α → ... is applied to a

  ----

  1 We can also write single `(id :: ∀α. α → α)` with rigid type annotations (Section 5.3)

  ----

  polymorphic argument whose outer quantifiers must not be instantiated
  (as in single id). In all other cases, the (impredicative) instantiations
  are always fully determined and an annotation is never
  needed. For example, we can create a singleton list with ids as its
  element without extra annotations:

      idss :: [[∀α. α → α]] (inferred)
      idss = single ids

  Moreover, HMF considers all arguments in an application to disambiguate
  instantiations and is not sensitive to the order of arguments.
  Consider for example reverse application defined as:

      revapp :: ∀αβ. α → (α → β) → β (inferred)
      revapp x f = f x

  The application revapp id poly is accepted without any annotation
  as the impredicative instantiation of the quantifier α in the type of
  revapp to ∀α. α → α is uniquely determined by considering both
  arguments.

  More generally, HMF has the property that whenever an application
  e1 e2 is well typed, than the expression apply e1 e2 is
  also well typed, and also the reverse application revapp e2 e1.
  We consider this an important property since it applies more generally
  for arbitrary functors (map) applying polymorphic functions
  (poly) over structures that hold polymorphic values (ids). A concrete
  example of this that occurs often in practice is the application
  of runST in Haskell. The function runST executes a state
  monadic computation in type safe way and its (higher-rank) type
  is:

      runST :: ∀α.(∀s. ST s α) → α

  Often, Haskell programmers use the application operator ($) to
  apply runST to a large computation as in:

      runST $ computation

  Given that ($) has the same type as apply, HMF accepts this application
  without annotation and impredicatively instantiates the α
  quantifier of apply to ∀s. ST s α. In practice, automatic impredicative
  instantiation ensures that we can also reuse many common
  abstractions on structures with polymorphic values without extra
  annotations. For example, we can apply length to a list with polymorphic
  elements,

      length ids

  or map the head function over a list of lists with polymorphic
  elements,

      map head (single ids)

  or similarly:

      apply (map head) (single ids)

  without giving any type annotation.

## 2.3 Robustness

  HMF is not entirely robust against small program transformations
  and sometimes requires the introduction of more annotations. In
  particular, η-expansion does not work for polymorphic parameters
  since these must always be annotated in HMF. For example,
  λf .poly f is rejected and we should write instead λ(f :: ∀α. α →
  α).poly f .

  Moreover, since HMF disambiguates impredicative instantiations
  over multiple arguments at once, we cannot always abstract
  over partial applications without giving an extra annotation. For
  example, even though revapp id poly is accepted, the ‘equivalent’
  program let f = revapp id in f poly is not accepted

<!-- page 3 -->

  without an extra annotation, since the type assigned to the partial
  application revapp id in isolation is the Hindley-Milner type
  ∀αβ.((α → α) → β) → β and the body f poly is now rejected.

  Nevertheless, we consider the latter program as being quite different
  from a type inference perspective since the partial application
  revapp id can now be potentially shared through f with
  different (polymorphic) types. Consider for example let f =
  revapp id in (f poly, f iapp) where iapp has type (Int →
  Int) → Int → Int. In this case, there does not exist any System
  F type for f to make this well-typed, and as a consequence we
  must reject it. HMF is designed to be modular and to stay firmly
  within regular System F types. Therefore f gets assigned the regular
  Hindley-Milner type. If the polymorphic instantiation is wanted,
  an explicit type annotation must be given.

## 3. A comparision with MLF and boxy types

  In this section we compare HMF with two other type inference systems
  that support first-class polymorphism, namely MLF (Le Botlan
  and Remy 2003; Le Botlan 2004; Le Botlan and R ´ emy 2007; ´
  Remy and Yakobowski 2007) and boxy type inference (Vytiniotis ´
  et al. 2006).

  MLF

  The MLF type system also supports full first-class polymorphism,
  and only requires type annotations for parameters that are used
  polymorphically. As a consequence, MLF is strictly more powerful
  than HMF, and every well-typed HMF program is also a well-typed
  MLF program. MLF achieves this remarkable feat by going beyond
  regular System F types and introduces polymorphically bounded
  types. This allows MLF to ‘delay’ instantiation and give a principal
  type to ambiguous impredicative applications. For example, in the
  program let f = revapp id in (f poly, f iapp), the type
  assigned to f is ∀(γ > ∀α. α → α). ∀β.(γ → β) → β, which
  can be instantiated to either ∀β.((∀α. α → α) → β) → β or
  ∀αβ.((α → α) → β) → β. Since applications never need an
  annotation, this makes MLF robust under rewrites. For example,
  when the application e1 e2 is well-typed, than so is apply e1 e2
  and also revapp e2 e1, and partial applications can always be
  abstracted by a let-binding.

  As shown in Section 2.1, inference for polymorphic parameters
  is not possible in general and we can therefore argue that MLF
  achieves optimal (local) type inference in the sense that it requires
  the minimal number of annotations possible. The drawback of
  MLF is that it goes beyond regular System F types which makes
  MLF considerably more complicated. This is not only the case
  for programmers that have to understand these types, but also for
  the meta theory of MLF, the implementation of the type inference
  algorithm, and the translation to System F (which is important for
  qualified types (Leijen 2007b; Leijen and Loh 2005)). ¨

  HMF represents a different point in the design space and only
  uses regular System F types. As shown in Section 2.2, HMF does
  this at the price of also requiring annotations on ambiguous impredicative
  applications. In return for those annotations, we get a
  simpler system than MLF where programmers can work with normal
  System F types and where the inference algorithm is a small
  extension of algorithm W (which also makes it easier to extend
  HMF with qualified types for example).

  Boxy type inference

  The GHC compiler supports first-class polymorphism using boxy
  type inference. This inference system is made principal by distinguishing
  between inferred ‘boxy types’ and checked annotated
  types. There are actually two variants of boxy type inference,
  namely basic boxy type inference, and the extension with ‘presubsumption’
  (Vytiniotis et al. 2006, Section 6). The basic version
  is quite weak cannot type simple applications like tail ids or prop-

  Figure 1. HMF types

  agate the annotation in single id :: [∀α. α → α]. Therefore, we
  only discuss the extended version with pre-subsumption (which is
  implemented in GHC).

  Unfortunately, there are no clear rules for programmers when
  annotations are needed with boxy type inference. In general, it is
  hard to characterize those situations precisely since they depend on
  the typing context, and the details of the boxy matching and presubsumption
  algorithms.

  In general, most polymorphic parameters and impredicative applications
  need an annotation with boxy type inference. However,
  due to the built-in type propagation, we can often just annotate
  the result type, as in (single id) :: [∀α. α → α] (which is rejected
  in HMF). Annotations can also be left out when the type is
  apparent from the context, as in foo (λf .(f 1, f True)) where
  foo has type ((∀α. α → α) → (Int, Bool)) → Int. Neither
  HMF nor MLF can type this example and need an annotation
  on f . Of course, local propagation of types is not robust under
  small program transformations. For example, the abstraction
  let poly = λf .(f 1, f True) in foo poly is not well-typed and
  the parameter f needs to be annotated in this case.

  In contrast to HMF, annotations are sometimes needed even if
  the applications are unambigious. Take for example the function
  choose with type ∀α. α → α → α, and the empty list null
  with type ∀α. [α]. Both the applications choose null ids and
  choose ids null are rejected with boxy type inference even though
  the instantiations are unambigious 2. Surprisingly, the abstraction
  let f = choose null in f ids is accepted due to an extra generalization
  step on let bindings. All of these examples are accepted
  without annotations in both HMF and MLF.

  Finally, even if an impredicative application e1 e2 is accepted,
  the abstraction apply e1 e2 (and revapp e2 e1) is still rejected with
  boxy type inference without an extra type annotation. For example,
  the application apply runST (return 1) must be annotated as
  (apply :: ((∀s. ST s Int) → Int) → (∀s. ST s Int) →
  Int) runST (return 1). We feel that this can be a heavy burden
  in general when abstracting over common polymorphic patterns.

## 4. Type rules

  HMF uses regular System F types as defined Figure 1. A type σ is
  either a quantified type ∀α. σ, a type variable α, or the application
  of a type constructor c. Since HMF is invariant, we do not treat the
  function constructor (→) specially and assume it is part of the type
  constructors c. The free type variables of a type σ are denoted as
  ftv(σ):

      ftv(α) = {α}
      ftv(c σ1 ... σn) = ftv(σ1)∪ ... ∪ ftv(σn)
      ftv(∀α. σ) = ftv(σ) − {α}

  and is naturally extended to larger constructs containing types.
  In the type rules, we sometimes distinguish between polymorphic
  types σ and monomorphic types. Figure 1 defines unquanti-

  ----

  2 GHC actually accepts the second expression due to a left-to-right bias in
  type propagation.

  ----

<!-- page 4 -->

  fied types ρ as types without an outer quantifier, and monomorphic
  types τ as types without any quantifiers at all (which correspond to
  the usual Hindley-Milner τ types).

## 4.1 Substitution

  A substitution S is a function that maps type variables to types.
  The empty substitution is the identity function and written as [ ].
  We write Sx for the application of a substitution S to x where
  only the free type variables in x are substituted. We often write a
  substitution as a finite map [α1 := σ1, ..., αn := σn ] (also written
  as [α := σ]) which maps αi to σi and all other type variables to
  themselves. The domain of a substitution contains all type variables
  that map to a different type: dom(S) = {α | Sα 6= α}. The
  codomain is a set of types and defined as: codom(S) = {Sα |
  α ∈ dom(S)}. We write (α := σ) ∈ S if α ∈ dom(S) and
  Sα = σ. The expression (S − α) removes α from the domain of
  S, i.e. (S − α) = [α := σ | (α := σ) ∈ S ∧ α /∈ α]. Finally, we
  only consider idempotent substitutions S where S(Sx ) = Sx (and
  therefore ftv(codom(S)) 6∩ dom(S)).

## 4.2 Type instance

  We use the regular System F polymorphic generic instance relation
  (v) on types, defined as:

      β 6∩ ftv(∀α. σ1)
      -------------------------
      ∀α. σ1 v ∀β. [α := σ]σ1

  where we write (6∩) for disjoint sets. Note that the generic instance
  relation can only instantiate the outer bound variables. Here are
  some examples:

      ∀α. α → α v Int → Int
      ∀α. α → α v ∀β. [∀α. α → β] → [∀α. α → β]

  Note that HMF is invariant since the instance relation can only
  instantiate outer quantifiers. Two types are considered equal if they
  are instances of each other:

      σ1 = σ2 , (σ1 v σ2 ∧ σ2 v σ1)

  This means that we can freely apply α-renaming, reorder quanti-
  fiers, and that unbound quantifiers are irrelevant. Finally, we write
  JσK for the polymorphic weight of a type, which is defined as the
  sum of all (non-instantiable) inner polymorphic types.

      J∀α. ρK = wt(ρ)
      where
      wt(α) = 0
      wt(c σ1 ... σn) = wt(σ1) + ... + wt(σn) + 0
      wt(∀α. σ) = wt(σ) iff α /∈ ftv(σ)
      wt(∀α. σ) = wt(σ) + 1 otherwise

  and extends naturally to structures containing types. For example,
  J∀α. [∀β. α → β]K is one, while Jτ K, the polymorphic weight of
  monomorphic types, is always zero. Note that the polymorphic
  weight is monotonically increasing with respect to instantiation, i.e.

  Property 1 (Polymorphic weight is stable):

      If σ1 v σ2 then Jσ1K 6 Jσ2K

  The polymorphic weight is used in the type rules to restrict derivations
  to have a minimal polymorphic weight, effectively preventing
  the introduction of arbitrary polymorphic types.

## 4.3 Type rules

  We first describe a simpler version of HMF, called Plain HMF, that
  does not consider multiple argument applications. In Section 4.5
  we describe the addition of a type rule for N-ary applications that
  is used for full HMF.

  Figure 2. Type rules for Plain HMF

  The type rules for Plain HMF are given in Figure 2. The expression
  Γ ` e : σ implies that under a type environment Γ we can
  assign a type σ to the expression e. The type environment Γ binds
  variables to types, where we use the expression Γ, x : σ to extend
  the environment Γ with a new binding x with type σ (replacing any
  previous binding for x ). Expressions e in HMF are standard and
  consist of variables x , applications e1 e2, functions λx .e, functions
  with an annotated parameter λ(x :: σ).e, and local bindings
  let x = e1 in e2.

  An important property for HMF is the existance of principal
  type derivations, i.e. for any derivation Γ ` e : σ', there also exists
  a derivation Γ ` e : σ with a unique most general type σ such that σ v σ'. In Section 6 we describe a type inference algorithm that
  infers precisely those principal types and is sound and complete
  with respect to the type rules.

  The rules VAR and GEN are standard and equivalent to the usual
  Hindley-Milner rules. The instantiation rule INST is generalized to
  use the System F generic instance relation.

  Just like Hindley-Milner, the function rule FUN restricts the type
  of the parameter x to a monomorphic type τ . As we have seen in the
  introduction, this is essential to avoid guessing polymorphic types
  for parameters. Furthermore, the type of the function body must
  be an unquantified type ρ. For example the expression λx .λy.x
  has the principal type ∀αβ. α → β → α in HMF. Without the
  restriction to unquantified types, the type ∀α. α → (∀β. β → α)
  could also be derived for this expression, and since neither of
  these types is an instance of each other, we would no longer have
  principal type derivations.

  In contrast, rule FUN-ANN binds the type of the parameter to a
  given polymorphic type σ. Again, the type of the function body
  must be an unquantified type ρ. For simplicity we consider only
  closed annotations in Plain HMF but we remove this restriction in
  Section 5.1. There is no special rule for type annotations since we
  can treat a type annotation (e ::σ) as an application to an annotated
  identity function: (λ(x :: σ).x ) e. Using this encoding, we can

<!-- page 5 -->

  derive the following rule for closed annotations:

      ANN? Γ ` e : σ
      -----------------------
      Γ ` (e :: σ) : σ

  using INST, GEN, FUN-ANN, and APP.

  The LET rule and application rule APP are standard except for
  their extra side conditions. Without these conditions the type rules
  are still sound and would reside between HMF and implicitly typed
  System F. Unfortunately this system would not have principal type
  derivations which precludes efficient type inference. The side conditions
  are therefore pragmatically chosen to be the simplest conditions
  such that HMF has principal type derivations, simple rules for
  type annotations, and a straightforward type inference algorithm.

  The application rule APP requires that the argument and parameter
  type are syntactically equivalent which can be full polymorphic
  types. Furthermore, the rule requires that the polymorphic
  weight of the function type is minimal, i.e. for any derivations
  `Γ |- e1 : σ2' → σ'` and `Γ |- e2 : σ2'`, we have that
  `[[σ2 → σ]] ≤ [[σ'2 → σ']]`. For convenience, we often use the shorthand
  minimal(`[[σ2 → σ]]`) to express this condition. Note that for
  monomorphic applications, the polymorphic weight is always zero
  and therefore always minimal. Effectively, the condition ensures
  that predicative instantation is preferred when possible and that no
  arbitrary polymorphism can be introduced. Take for example the
  derivation of the application single id from the introduction (using
  `τ` for `α → α`):

      Γ |- single : ∀α. α → [α]              Γ |- id : ∀α. α → α
      ∀α. α → [α] ⊑ (α → α) → [α → α]        ∀α. α → α ⊑ α → α
      --------------------------------        --------------------
      Γ |- single : (α → α) → [α → α]         Γ |- id : α → α
                            minimal([[ τ → [τ] ]])
      --------------------------------------------------------------
      Γ |- single id : [α → α]    α ∉ ftv(Γ)
      ----------------------------------------
      Γ |- single id : ∀α.[α → α]

  Without the condition for minimal polymorphic weights, the type
  [∀α. α → α] could also be derived for the application single id:

      Γ |- single : ∀α. α → [α]
      ∀α. α → [α] ⊑ (∀α. α → α) → [∀α. α → α]
      ------------------------------------------
      Γ |- single : (∀α. α → α) → [∀α. α → α]
          Γ |- id : ∀α. α → α
      ---------------------------------------
      Γ |- single id : [∀α. α → α]             wrong!

  where we would lose principal type derivations since the types
  `∀α. [α → α]` and `[∀α. α → α]` are not in an instance relation.
  The minimality condition ensures that the second derivation is
  disallowed, since the polymorphic weight `[[∀α. [α → α] ]` is smaller
  than `[[ [∀α. α → α] ]]`.

  It is important that the minimality condition ranges over the
  entire sub derivations of e1 and e2 since the ‘guessed’ polymorphism
  of the second derivation is introduced higher up the tree in
  the instantiation rule. As shown in these derivations, the condition
  disambiguates precisely those impredicative applications where a
  function of type `α → ...` is applied to a polymorphic argument. It
  is easy to see that the argument is always be (predicatively) instantiated
  in this case (if no annotation was given).

  Just like Hindley-Milner, the LET rule derives a polymorphic
  type for let-bound values. In addition, the rule requires that the type
  of the bound value is the most general type that can be derived,
  i.e. for any derivation `Γ |- e1 : σ1'`, we have that `σ1 ⊑ σ1'`.
  As a convenient shorthand, we often write mostgen(σ1) for this
  condition.

  The condition on let bindings is required to prevent the introduction
  of arbitrary polymorphism through polymorphic types in the
  type environment `Γ`. Without it, we could for example bind `single'`
  to single with the (polymorphically) instantiated type `(∀α. α → α) → [∀α. α → α]`, and derive for the application `single' id` the
  type `[∀α. α → α]` and lose principal type derivations again.

  We cannot just require that the let-bound values are of minimal
  polymorphic weight as in the application rule, since arbitrary
  polymorphism can also be introduced through the sharing of quantified
  type variables. Consider the expression (let foo x y =
  single y in foo ids id) where ids has type [∀α. α → α]. The
  principal type for this expression is ∀α. [α → α], where the type
  for foo is ∀αβ. β → α → [α]. Without the most general type
  restriction, we could also assign the type ∀α. [α] → α → [α]
  to foo and through arbitrary sharing derive the incomparable type
  [∀α. α → α] for the expression.

  The type rules of HMF allow principal derivations and are sound
  where well-typed programs cannot go ‘wrong’. We can prove this
  by showing that for every HMF derivation there is a corresponding
  System F term that is well-typed (Leijen 2007a). Furthermore,
  HMF is a conservative extension of Hindley-Milner. In HindleyMilner
  programs rule FUN-ANN does not occur and all instantiations
  are monomorphic. This implies that the types in an application
  are always monomorphic and therefore the minimality restriction
  is always satisfied. Since Hindley-Milner programs have principal
  types, we can also always satisfy the most general types restriction
  on let bindings. Finally, it is interesting that if we just restrict
  instantiation to monomorphic instantiation, we end up with a predicative
  type system for arbitrary rank type inference (Peyton Jones
  et al. 2007; Odersky and Laufer 1996). ¨

## 4.4 On the side conditions

  The LET rule restriction to most-general types is not new. It has
  been used for example in the typing of dynamics in ML (Leroy
  and Mauny 1991), local type inference for F6 (Pierce and Turner
  1998), semi-explicit first-class polymorphism (Garrigue and Remy ´
  1999b), and more recently for boxy type inference (Vytiniotis et al.
  2006). All of these systems require some form of minimal solutions
  in order to have principal type derivations.

  From a logical perspective though, the conditions on LET and
  APP are unsatisfactory since they range over all possible derivations
  at that point and can therefore be more difficult to reason about
  (even though they are still inductive). There exists a straighforward
  decision procedure however to fullfill the conditions by always
  using most general type derivations. This automatically satisfies the
  LET rule side condition, and due to Property 1 will also satisfy the
  minimality condition on the APP rule where only rule INST on e1
  and e2 needs to be considered (which is a key property to enable
  efficient type inference).

  It is interesting to note that the type rules without the side
  conditions are still sound, but would lack principal derivations,
  and the type inference algorithm would be incomplete. This is the
  approach taken by Pierce and Turner (1998) for local type inference
  for example which is only partially complete.

  Even though we are not fully satisfied with the side conditions
  from a logical perspective, we believe that the specification is still
  natural from a programmers perspective, with clear rules when annotations
  are needed. Together with the use of just regular System F
  types and a straightforward type inference algorithm, we feel that
  the practical advantages justify the use of these conditions in the
  specification of the type rules.

## 4.5 N-ary applications

  Since Plain HMF requires minimal polymorphic weight on every
  application node, it is sensitive to the order of the applications. For
  example, if e1 e2 is well-typed, so is apply e1 e2, but the reverse
  application, revapp e2 e1 is not always accepted. As a concrete
  example, revapp id poly is rejected since the principal type of the
  application revapp id in Plain HMF is `∀αβ.(α → α) → β → β`

<!-- page 6 -->

  and we cannot derive the (desired) type `∀β.(∀α. α → α) → β →
  β` since its polymorphic weight is larger.

  A solution to this problem is to allow the application rule to have
  a minimal polymorphic weight over multiple arguments. In particular,
  we extend Plain HMF to full HMF by adding the following
  rule for N-ary applications:

      APP-N
      Γ |- e : σ1 → ... → σn → σ    Γ |- e1 : σ1 ...    Γ |- en : σn
      ∀σ' σ1'..σn'. Γ |- e : −→σn' → σ' ∧ Γ |- e1 : σ1' ∧ .. ∧ Γ |- en : σn'
                          ⇒ [[−→σn → σ]] ≤ [[−→σn' → σ']]
      -------------------------------------------------------------------------
      Γ |- e e1 ... en : σ

  where we write −→σn for the type σ1 → ... → σn. With the rule
  APP-N, it becomes possible to accept the application revapp id poly
  since we can instantiate revapp to (∀α. α → α) → ((∀α. α →
  α) → (Int, Bool)) → (Int, Bool) which has a minimal polymorphic
  weight when both arguments are considered.

  Even though it is always best to consider the maximal number
  of arguments possible, the rule APP-N does not require to always
  consider all arguments in an application, and derivations for partial
  applications are still possible. In fact, it would be wrong to always
  consider full applications since functions can return polymorphic
  functions that need to be instantiated first using rule INST. As an
  example, consider the expression head ids 1. For this application,
  it is essential to consider the application head ids first in order to
  use INST to instantiate its polymorphic result ∀α. α → α to the
  required Int → Int type, and we cannot use APP-N directly.

## 5. About type annotations

  In principle HMF does not need any special rules for type annotations
  since we can type an annotation (e :: σ) as an application to
  a typed identity function: (λ(x :: σ).x ) e. However, in practice it
  is important to handle annotations with free variables and to propagate
  type annotation information to reduce the annotation burden.
  In this section we discuss these issues in more detail. Note that all
  three techniques described in this section are orthogonal to HMF as
  such, and can be applied in general to Hindley-Milner based type
  inference systems.

## 5.1 Partial annotations

  In order to give types to any subexpression, we need to be able to
  give partial type annotations (Remy 2005). We write ´ e :: ∃∃α. σ for
  a partial type annotation where the free variables α in σ are locally
  bound. We read the annotation as “for some (monomorphic) types
  α, the expression e has type σ” (and therefore call ∃∃ the ‘some’
  quantifier). As a practical example of such annotation, consider the
  type of runST:

      runST :: ∀α.(∀s. ST s α) → α

  If we define this function, the parameter needs a partial annotation:

      runST (x :: ∃∃α. ∀s. ST s α) = ...

  Note that we cannot annotate the parameter as ∀αs. ST s α since
  the parameter itself is not polymorphic in α. For simplicity, we still
  require type annotations to be closed but of course it is possible to
  extend this with scoped type variables (Peyton Jones and Shields
  2004), where annotations can contain free type variables that are
  bound elsewhere.

  We can formalize partial annotations in the type rules by modifying
  the annotation rule to assume fresh monotypes for the ‘some’
  quantifiers:

  Figure 3. Type annotation propagation

  Moreover, we can remove the FUN rule since we can encode unannoted
  functions λx .e as λ(x ::∃∃α. α).e. Using this encoding, GEN,
  and FUN-ANN, we can derive the following rule for unannoted functions:

            Γ |- λ(x :: τ).e : σ
      FUN*  --------------------------
            Γ |- λx.e : σ

## 5.2 Type annotation propagation

  Another important addition in practice is the propagation of type
  annotations. For example, a programmer might write the following
  definition for poly:

      poly :: (∀α. α → α) → (Int, Bool)
      poly f = (f 1, f True)

  As it stands, this would be rejected by HMF since the parameter f
  itself is not annotated (and used polymorphically). We can remedy
  this situation by propagating the type annotation down through
  lambda and let expressions. Figure 3 defines an algorithm for
  propagating type information, where PJe :: σK propagates the type
  annotation on e. For example, the above expression would be
  transformed into:

      poly :: (∀α. α → α) → (Int, Bool)
      poly (f :: ∀α. α → α) = (f 1, f True) :: (Int, Bool)

  and the definition is now well-typed in HMF. Type propagation can
  be seen as preprocessing step since it is defined as a separate syntactical
  transformation, and can be understood separately from the
  order independent specification of the type rules. We consider this
  an important property since systems that combine type propagation
  with type inference lead to algorithmic formulations of the type
  rules that are fragile and difficult to reason about (Remy 2005). ´

## 5.3 Rigid annotations

  In general, we cannot statically propagate types through application
  nodes (since the expression type can be more polymorphic than the
  propagated type). This is a serious weakness in practice. Consider
  again the definition of ids from the introduction:

      (single :: (∀α. α → α) → ([∀α. α → α])) id

  In a system that mixes type propagation with type inference, like
  boxy type inference (Vytiniotis et al. 2006), we could write instead:

      (single id) :: [∀α. α → α] (rejected in HMF)

  Even though this looks natural and can be implemented for HMF
  too, we will not give in to the siren call of mixing type propagation
  with type inference and stick with a declarative formulation of the
  type rules. Instead, we propose to make type annotations rigid.
  In particular, when a programmer writes a type annotation on an
  argument or the body of a lambda expression, we will take the type
  literally and not instantiate or generalize it further. This mechanism
  allows the programmer to write an annotation on an argument
  instead of a function, and we can write:

      single (id :: ∀α. α → α)

  which has type [∀α. α → α]. We believe that rigid annotations
  are a good compromise to avoid an algorithmic specification of

<!-- page 7 --> 


  Figure 4. System F to HMF translation

  the type system. Moreover, we appreciate the ability to be very
  specific about the type of an expression where rigid annotations
  give precise control over type instantiation. For example, we can
  write a variation of the const function that returns a polymorphic
  function:

      const0 :: ∀α. α → (∀β. β → α) (inferred)
      const0 x = (λy → x ) :: ∃∃α. ∀β. β → α

  Note that with the type annotation propagation of Figure 3 we can
  also write:

      const0 :: ∀α. α → (∀β. β → α)
      const0 x y = x

  Note that rigid annotations are generally useful and are not specific
  to HMF and we believe that expression annotations in any language
  based on Hindley-Milner should be treated rigidly.

  Rigid annotations can be formalized with ease using simple syntactic
  restrictions on the derivations. First we consider an expression
  to be annotated when it either has a direct annotation or if it is
  a let expression with an annotated body. The grammar for annotated
  expressions ea is:

      ea ::= e :: σ | let x = e in ea

  Dually, we define unannotated expressions eu as all other expressions,
  namely:

      eu ::= x | e1 e2 | λx .e | λ(x :: σ).e | let x = e in eu

  We want to treat annotated expressions rigidly and not instantiate
  or generalize their types any further. Therefore, our first adaption
  to the type rules of Figure 2 is to restrict instantiation and generalization
  to unannotated expressions only:

              Γ |- eu : σ1 σ1 v σ2
      INST    ----------------------
              Γ |- eu : σ2

              Γ |- eu : σ α /∈ ftv(Γ)
      GEN     ------------------------
              Γ |- eu : ∀α. σ

  Since instantiation and generalization are now restricted to unannotated
  expressions, we can instantly derive the type [∀α. α → α] for
  the application single (id :: ∀α. α → α) since the minimal weight
  condition of rule APP is now satisfied. At the same time, the application
  (id :: ∀α. α → α) 42 is now rejected – indeed, a correct
  annotation would rather be (id :: ∃∃α. α → α) 42.

  Moreover, we can allow lambda bodies to have a polymorphic
  type as long as the body expression is annotated, and we add an
  extra rule for lambda expressions with annotated bodies:

                      Γ, x : σ1 |- ea : σ2
      FUN-ANN-RIGID   ------------------------------
                      Γ |- λ(x :: σ1).ea : σ1 → σ2

  Note that we don’t need such rule for unannoted functions as FUN*
  can be used with both FUN-ANN and FUN-ANN-RIGID.

      unify :: (σ1, σ2) → S

  where σ1 and σ2 are in normal form

  Figure 5. Unification

## 5.4 Translation of System F to HMF

  HMF extended with rigid type annotations can express any System
  F program. The rigid annotations are required in order to return
  polymorphic values from a function. If we would just consider System
  F programs with prenex types then Plain HMF would suffice
  too. Figure 4 defines a translation function FJeKΓ that translates a
  System F term e under a type environent Γ to a well-typed HMF
  term e. Note that Q denotes the set of quantified types and σ ∈ Q
  implies that σ 6= ρ for any ρ. The expression Γ `F e : σ states that
  the System F term e has type σ under a type environement Γ and is
  standard.

  To translate a System F term to HMF, we keep variables untranslated
  and remove all type abstractions and applications. Parameters
  of a lambda expressions are kept annotated in the translated
  HMF term. If the body has a polymorphic type in the System
  F term, we also annotate the body in the HMF term since HMF
  cannot derive polymorphic types for unannotated lambda bodies.
  Applications are annotated whenever the argument is a quantified
  type.

  There are of course other translations possible, and in many
  cases one can do with fewer annotations in practice. Nevertheless,
  the above translation is straightforward and removes most of the
  annotations that can be inferred automatically.
  Theorem 2 (Embedding of System F):

      If Γ |-F e : σ then Γ |-F[[e]]Γ : σ' where σ' v σ

## 6. Type inference

  The type inference algorithm for HMF is a relatively small extension
  of algorithm W (Damas and Milner 1982) with subsumption
  and unification of quantified types. We first discuss unification and
  subsumption before describing the actual type inference algorithm.

## 6.1 Unification

  Figure 5 describes a unification algorithm between polymorphic
  types. The algorithm is equivalent to standard Robinson unification
  (Robinson 1965) except that type variables can unify with polytypes
  and there is an extra case for unifying quantified types. The
  unification algorithm assumes that the types are in normal form. A
  type σ is in normal form when all quantifiers are bound and or-

<!-- page 8 -->


  Figure 6. Subsumption

  dered with respect to their occurrence in the type. For example,
  ∀αβ. α → β is in normal form, but ∀βα. α → β or ∀α.Int are
  not. Implementation wise, it is easy to keep types in normal form by
  returning the free variables of a type always in order of occurrence.

  Having types in normal form makes it easy to unify quantified
  types. In the last case of unify, we replace the quantifiers of each
  type with fresh skolem constants in order, and unify the resulting
  unquantified types. Afterwards, we check if none of the skolems
  escape through a free variable which would be unsound. For example,
  if β is a free variable, we need to reject the unification of
  ∀α. α → α and ∀α. α → β. This check is done by ensuring that the
  codomain of the substitution does not contain the skolem constant
  c, and the unification fails if c is an element of con(codom(S)))
  (where con(·) returns the skolem constants in the codomain).

  Theorem 3 (Unification is sound): If unify(σ1, σ2) = S then
  Sσ1 = Sσ2.

  Theorem 4 (Unification is complete and most general): If Sσ1 =
  Sσ2 then `unify(σ1, σ2) = S'` where `S = S'' ◦ S'` for `some S''`.

## 6.2 Subsumption

  Figure 6 defines subsumption where subsume(σ1, σ2) returns a
  most general substitution S such that Sσ2 v Sσ1. Informally, it
  instantiates σ2 such that it can unify with the (potentially polymorphic)
  type σ1. It uses the same mechanism that is usually used to
  implement the subsumption relation in type systems based on type
  containment (Odersky and Laufer 1996; Peyton Jones et al. 2007). ¨

  As shown in Figure 6, the algorithm first skolemizes the quantifiers
  of σ1 and instantiates the quantifiers β of σ2 with fresh
  type variables. Afterwards, we check that no skolems escape
  through free variables which would be unsound. For example,
  subsume(∀α. α → α, ∀αβ. α → β) succeeds, but it would be
  wrong to accept subsume(∀α. α → α, ∀α. α → β) where β is a
  free variable. Note that in contrast with unification, we first remove
  the quantifiers β from the domain of the substitution since it is fine
  for those variables to unify with the skolems c.

  Theorem 5 (Subsumption is sound): If subsume(σ1, σ2) = S
  then Sσ2 v Sσ1.

  Theorem 6 (Subsumption is partially complete and most general):
  If Sσ2 v Sσ1 holds and σ1 is not a type variable, then
  subsume(σ1, σ2) = S
  0 where S = S
  00 ◦ S
  0
  for some S
  00
  .
  If σ1 is a type variable, we have that subsume(α, ∀β. ρ) equals
  [α := ρ] for some fresh β. When matching arguments to functions
  with a type of the form ∀α. ... → α → ... this is exactly the
  disambiguating case that prefers predicative instantiation and a
  minimal polymorphic weight, and the reason why subsumption is
  only partially complete.

## 6.3 A type inference algorithm

  Figure 7 defines a type inference algorithm for HMF. Given a type
  environment Γ and expression e, the function infer(Γ, e) returns a

  Figure 7. Type inference for Plain HMF

  Figure 8. Helper functions

  monomorphic substitution θ and type σ such that σ is the principal
  type of e under θΓ.

  In the inference algorithm we use the notation σ ∈ T when
  σ is a monomorphic type, i.e. σ = τ . The expression σ /∈ T is
  used for polymorphic types when there exist no τ such that σ = τ .
  We use the notation θ for monomorphic substitutions, where σ ∈
  codom(θ) implies σ ∈ T , and the notation Θ for polymorphic
  substitutions where σ ∈ codom(Θ) implies σ /∈ T . The function
  split(S) splits any substitution S into two substitutions θ and Θ
  such that S = Θ ◦ θ.

  The rules for variables and let expressions are trivial. In the
  rules for lambda expressions, we first instantiate the result type of
  the body and than generalize over the function type. For unanno-

<!-- page 9 -->

  tated parameters, we can assume a fresh type α in the type environment
  while annotated parameters get their given type.

  The application rule is more involved but still very similar to the
  usual application rule in algorithm W (Damas and Milner 1982). Instead
  of unifying the argument with the parameter type, we use the
  subsume operation since we may need to instantiate the argument
  type. The polymorphic substitution S returned from subsume is
  split in a monomorphic substitution θ3 and a polymorphic substitution
  Θ3, such that S = Θ3 ◦ θ3. Next, we check that no polymorphic
  types escape through free variables in the type environment by
  ensuring that dom(Θ3) 6∩ ftv(θ4Γ). This is necessary since rule
  FUN can only assume monotypes τ for parameters, and without the
  check we would be able to infer polymorphic types for parameters.
  Since the domain of Θ3 does not occur in the type environment, we
  can apply the polymorphic substitution to the result type, and return
  the generalized result together with a monomorphic substitution.

  We can now state our main theorems that type inference for
  (Plain) HMF is sound and complete:

  Theorem 7 (Type inference is sound): If infer(Γ, e) = (θ, σ) then
  θΓ ` e : σ holds.

  Theorem 8 (Type inference is complete and principal): If θΓ ` e :
  σ, then infer(Γ, e) = (θ
  0
  , σ0
  ) where θ ≈ θ
  00 ◦ θ
  0
  and θ
  00σ
  0 v σ.

  Following Jones (1995), we use the notation S1 ≈ S2 to
  indicate that S1α = S2α for all but a finite number of fresh type
  variables. In most cases, we can treat S1 ≈ S2 as S1 = S2 since
  the only differences between substitutions occur at variables which
  are not used elsewhere in the algorithm. We need this mechanism
  because the algorithm introduces fresh variables that do not appear
  in the hypotheses of the rule or other distinct branches of the
  derivation.

## 6.4 Optimizations

  In practice, inference algorithms tend to use direct updateable references
  instead of using an explicit substitution. This works well with
  HMF too, but certain operations on substitutions must be avoided.
  When unifying quantified types in the unify algorithm, the check
  (c ∈ con(codom(S))) can be implemented more effectively when
  using references as (c ∈ con(S(∀α. σ1))∪ con(S(∀β. σ2)) (and
  similarly in subsume).

  In the application case of infer, we both split the substitution
  and there is a check that (dom(Θ3) 6∩ ftv(θ4Γ)) which ensures
  that no poly type escapes into the environment. However, since letbound
  values in the environment always have a generalized type,
  the only free type variables in the environment are introduced by
  lambda-bound parameter types. Therefore, the check can be delayed,
  and done instead when checking lambda expressions. Effectively,
  we remove the split and move the check from the application
  rule to the lambda case:

      infer(Γ, λx .e) =
        assume α and β are fresh
        let (S, ∀β. ρ) = infer((Γ, x : α), e)
        fail if (Sα /∈ T )
        return (S, generalize(SΓ, S(α → ρ)))

  This change makes it directly apparent that only monomorphic
  types are inferred for lambda bound parameters. Of course, it also
  introduces polymorphic substitutions everywhere, but when using
  an updateable reference implementation this happens anyway. Note
  that this technique can actually also be applied in higher-rank
  inference systems (Peyton Jones et al. 2007; Odersky and Laufer ¨
  1996) removing the ‘escaping skolem’ check in subsumption.

## 6.5 Rigid annotations

  It is straightforward to extend the type inference algorithm with
  rigid type annotations, since expressions can be checked syntactically
  if they are annotated or not. In the application case of the
  algorithm specified in Figure 7, we use unify instead of subsume
  whenever the argument expression e2 is annotated, which effectively
  prevents the instantiation of the argument type. Finally, we
  adapt the case for lambda expressions to not instantiate the type of
  an annotated body.

## 6.6 N-ary applications

  Implementing inference that disambiguates over multiple arguments
  using rule APP-N is more involved. First we need to extend
  subsumption to work on multiple arguments at once:

      subsumeN (σ1 ... σn, σ0
      1 ... σ0
      n) =
      let i = if σi ∈ {σ1, ..., σn } ∧ σi ∈ V / then i else 1
      let S = subsume(σi, σ0
      i)
      if n = 1 then return S
      else return S ◦ subsumeN (S(σ1 ... σi−1 σi+1 ... σn),
      S(σ
      0
      1 ... σ0
      i−1 σ
      0
      i+1 ... σ0
      n))

  The function subsumeN applies subsumption to n parameter types
  σ1 ...σn with the supplied argument types `σ1'...σn'`. Due to sharing,
  we can often infer a polymorphic type after matching some arguments,
  as happens for example in revapp id poly where the poly
  argument is matched first. The trick is now to subsume the parameter
  and argument pairs in the right order to disambiguate correctly.
  Since subsumption is unambigious for parameter types that are not
  a type variable (σi ∈ V / ), we first pick these parameter types. Only
  when such parameters are exhausted, we subsume the rest of the
  parameters, where the order does not matter and we arbitrarily pick
  the first. In a previous version of the system, we subsumed in order
  of dependencies between parameter and argument types, but
  one can show that this is unnecessary – if there is any type variable
  shared between parameter and argument types, it must be (lambda)
  bound in the environment, and in that case, we cannot infer a polymorphic
  type regardless of the order of subsumption.

  Secondly, we extend function matching to return as many
  known parameter types as possible, where we pass the number
  of supplied arguments n:

      funmatchN (n, σ1 → ... → σm → σ) =
        where m is the largest possible with 1 6 m 6 n
        return ([ ], σ1 ... σm, σ)
      funmatchN (n, α) =
        assume β1 and β2 are fresh
        return ([α := β1 → β2 ], β1, β2)

  During inference, we now consider all arguments at once, where we
  first infer the type of the function, and then call the helper function
  inferapp with the found type:

      infer(Γ, e e1 ... en) =
        assume n is the largest possible with n > 1
        let (θ1, σ1) = infer(Γ, e)
        let (θ2, σ2) = inferapp(θ1Γ, σ1, e1 ... en)
        return (θ2 ◦ θ1, σ2)

  The inferapp function is defined separately as it calls itself recursively
  for each polymorphic function result until all n arguments
  are consumed:

      inferapp(Γ, ∀α. ρ, e1 ... en) =
        assume α is fresh and n > 1
        let (θ0, σ1 ... σm, σ) = funmatchN (n, ρ)
        let (θ'i, σ'i) = infer(θi−1Γ, ei) for 1 6 i 6 m

<!-- page 10 -->

        let θi = θ'i ◦ θi−1
        let (Θ, θ') = split(subsumeN (θm(σ1 ... σm),
                                      θm(σ1'... σm')))
        let θ = θ' o θm
        fail if not (dom(Θ) 6∩ ftv(θΓ))
        if m < n then return inferapp(θΓ, Θθσ, em+1 ... en)
                else return (θ, generalize(θΓ, Θθσ))

  First, funmatchN is used to consider as many arguments m as possible.
  Note that m is always smaller or equal to n. Next, the types
  of the next m arguments are inferred, and the subsumeN function
  applies subsumption to all the parameter types with the found argument
  types. Afterwards we check again that no polymorphic types
  escape in the environment. Finally, if there are still arguments left
  (as in head ids 1 for example), inferapp is called recursively with
  the remaining arguments and the found result type. Otherwise, the
  generalized result type is returned.

## 7. Related work

  In Section 3 we already discussed MLF and boxy type inference.
  MLF was first described by by Remy and Le Botlan (2004; 2003; ´
  2007; 2007). The extension of MLF with qualified types is described
  in (Leijen and Loh 2005). Leijen later gives a type directed ¨
  translation of MLF to System F and describes Rigid-MLF (Leijen
  2007b), a variant of MLF that does not assign polymorphically
  bounded types to let-bound values but internally still needs the full
  inference algorithm of MLF.

  Vytiniotis et al. (2006) describe boxy type inference which is
  made principal by distinguishing between inferred ‘boxy’ types,
  and checked annotated types. A critique of boxy type inference is
  that its specification has a strong algorithmic flavor which can make
  it fragile under small program transformations (Remy 2005). ´

  To the best of our knowledge, a type inference algorithm for the
  simply typed lambda calculus was first described by Curry and Feys
  (1958). Later, Hindley (1969) introduced the notion of principal
  type, proving that the Curry and Feys algorithm inferred most general
  types. Milner (1978) independently described a similar algorithm,
  but also introduced the important notion of first-order polymorphism
  where let-bound values can have a polymorphic type.
  Damas and Milner (1982) later proved the completeness of Milner’s
  algorithm, extending the type inference system with polymorphic
  references (Damas 1985). Wells (1999) shows that general
  type inference for unannotated System F is undecidable.

  Jones (1997) extends Hindley-Milner with first class polymorphism
  by wrapping polymorphic values into type constructors. This
  is a simple and effective technique that is widely used in Haskell but
  one needs to define a special constructor and operations for every
  polymorphic type. Garrigue and Remy (1999a) use a similar tech- ´
  nique but can use a generic ‘box’ operation to wrap polymorphic
  types. Odersky and Laufer (1996) describe a type system that has ¨
  higher-rank types but no impredicative instantiation. Peyton Jones
  et al. (2007) extend this work with type annotation propagation.
  Dijkstra (2005) extends this further with bidirectional annotation
  propagation to support impredicative instantiation.

## 8. Future work

  We feel that both HMF and MLF present interesting points in the
  design space of type inference for first-class polymorphism. MLF is
  the most powerful requiring only annotations on parameters that are
  used polymorphically, but also introduces more complexity with
  the introduction of polymorphically bounded types. On the other
  end is HMF, which is more pragmatic and uses just System F
  types, but also requires annotations on ambiguous impredicative
  applications. Currently, we are working on a third system, called
  HML, that resides between these design points (Leijen 2008). This
  system is a simplification of MLF that only uses flexible types. The
  addition of flexible quantification leads to a very simple annotation
  rule where only function parameters with a polymorphic type need
  an annotation.

## 9. Conclusion

  HMF is a conservative extention of Hindley-Milner type inference
  that supports first-class polymorphism, is specified with logical
  type rules, and has a simple and effective type inference algorithm
  that infers principal types. Given the relative simplicity combined
  with expressive power, we feel that this system can be a great
  candidate as the basic type system for future languages or even
  Haskell.

## Acknowledgements

  The author would like to thank Dimitrios Vytiniotis and Simon
  Peyton Jones for their feedback on an earlier version of the type
  rules, and to Didier Remy who suggested the extension of HMF to
  N-ary applications. Thanks also to Edsko de Vries for his feedback
  and for implementing a full version of the HMF inference algorithm
  in the Morrow compiler.

## References

  H. Curry and R. Feys. Combinatory Logic, volume 1. NorthHolland, 1958.

  Luis Damas. Type Assignment in Programming Languages. PhD thesis, University of Edinburgh, April 1985. Technical report CST-33-85.

  Luis Damas and Robin Milner. Principal type-schemes for functional programs. In 9th ACM symp. on Principles of Programming Languages (POPL’82), pages 207–212, 1982.

  Atze Dijkstra. Stepping through Haskell. PhD thesis, Universiteit Utrecht, Nov. 2005.

  Jacques Garrigue and Didier Remy. Semi-explicit first-class polymorphism for ML. Journal of Information and Computation, 155:134–169, 1999a
  .
  Jaques Garrigue and Didier Remy. Semi-expicit first-class polymorphism for ML. Journal of Information and Computation, 151:134–169, 1999b.

  J.R. Hindley. The principal type scheme of an object in combinatory logic. Transactions of the American Mathematical Society, 146:29–60, Dec. 1969.

  Mark P. Jones. First-class polymorphism with type inference. In 24th ACM Symposium on Principles of Programming Languages (POPL’97), January 1997.

  Mark P. Jones. Formal properties of the Hindley-Milner type system. Unpublished notes, August 1995.

  Didier Le Botlan. MLF: Une extension de ML avec polymorphisme de second ordre et instanciation implicite. PhD thesis, INRIA Rocquencourt, May 2004. Also in English.

  Didier Le Botlan and Didier Remy. MLF: Raising ML to the power of System-F. In The International Conference on Functional Programming (ICFP’03), pages 27–38, aug 2003.

  Didier Le Botlan and Didier Remy. Recasting MLF. Research Report 6228, INRIA, Rocquencourt, France, June 2007.

  Daan Leijen. HMF: Simple type inference for first-class polymorphism. Extended version with proofs, 2007a. URL http://research.microsoft.com/users/daan/pubs.

<!-- page 11 -->

  Daan Leijen. Flexible types: robust type inference for first-class polymorphism. Technical Report MSR-TR-2008-55, Microsoft Research, March 2008.

  Daan Leijen. A type directed translation from MLF to System F. In The International Conference on Functional Programming (ICFP’07), Oct. 2007b.

  Daan Leijen and Andres Loh. Qualified types for MLF. In ¨ The International Conference on Functional Programming (ICFP’05). ACM Press, Sep. 2005.

  Xavier Leroy and M Mauny. Dynamics in ML. In ACM conference on Functional Programming and Computer Architecture (FPCA’91). Springer-Verlag, 1991. volume 523 of LNCS.

  Robin Milner. A theory of type polymorphism in programming. Journal of Computer and System Sciences, 17:248–375, 1978.

  Martin Odersky and Konstantin Laufer. Putting type annotations to work. In 23th ACM symp. on Principles of Programming Languages (POPL’96), pages 54–67, January 1996.

  Simon Peyton Jones and Mark Shields. Lexically scoped type variables. Draft, March 2004.

  Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich, and Mark Shields. Practical type inference for arbitrary-rank types. Journal of Functional Programming, 17(1):1–82, 2007.

  Benjamin C. Pierce and David N. Turner. Local type inference. In 25th ACM symp. on Principles of Programming Languages (POPL’98), pages 252–265, 1998.

  Didier Remy. Simple, partial type-inference for System-F based on type-containment. In The International Conference on Functional Programming (ICFP’05), September 2005.

  Didier Remy and Boris Yakobowski. A graphical presentation of MLF types with a linear-time unification algorithm. In TLDI’07, pages 27–38, 2007.

  J. A. Robinson. A machine-oriented logic based on the resolution principle. Journal of the ACM, 12(1):23–41, January 1965.

  Dimitrios Vytiniotis, Stephanie Weirich, and Simon Peyton Jones. Boxy types: type inference for higher-rank types and impredicativity. In The International Conference on Functional Programming (ICFP’06), September 2006.

  J.B. Wells. Typability and type checking in System-F are equivalent and undecidable. Ann. Pure Appl. Logic, 98(1–3):111–156, 1999.

<!-- page 12 -->


  Figure 10. Type directed translation to System F.

  Figure 11. System F type rules

## A. A type directed translation to System F

  Instead of directly defining a semantics for HMF expressions, we
  define the semantics in terms of a type directed translation to
  System F. Figure 9 defines the System F transformation terms for
  instantiation. The instantiation rule σ1 v σ2 f states that when
  σ2 is an F-generic instance of σ1, than f is a System F term of type
  σ1 → σ2. We can see f as the System F witness of the instantiation.

  Figure 10 defines the type directed translation to System F. The
  expression Γ ` e : σ e states that an expression e has type σ
  under the environment Γ with an equivalent System F term e. It is
  easy to check that whenever Γ ` e : σ e, than e is a well-typed
  System F term with type σ.


  Figure 12. Syntax directed type rules

  **Theorem 9** (The type directed translation is sound):
  If Γ ` e : σ e then Γ `F e : σ also holds.

  Moreover, the type directed translation is faithful in the sense that
  the type erasure of the System F term is equivalent to the erasure of
  the original HMF term. Specifically, if we remove all types from
  the System F and original HMF term, and replace let-bindings
  (let x = e1 in e2) with applications ((λx .e2) e1), we end up
  with equal lambda terms up to witness applications resulting from
  instantiation. However, as we can see in Figure 9, those witness
  terms always type erase to the identity function and can be removed
  through β-reduction. We can state this formally as:

  **Theorem 10** (The type directed translation is faithful):
  When Γ ` e : σ e, then (e
  ∗ =β e
  let∗
  ).

  where we write `e*` for the type erasure of e and e let* for the type
  and let-binding erasure of e. Therefore, every well-typed term in
  HMF corresponds with an equivalent well-typed System F term.
  Since System F is sound, this implies that HMF is sound too.

## B. Syntax directed rules

  We can also give syntax directed type rules for HMF where all rules
  have a distinct syntactical form in their conclusion. Figure 12 gives
  the syntax directed rules for HMF. The syntax directed rules do
  not contain separate rules for INST and GEN, but instantiate and
  generalize before and after lambda abstractions and applications
  (in the next section we show how we can reduce the number of
  generalizations by parameterizing whether a generalized type is
  required or not).

  The following theorems state that the syntax directed rules are
  sound and complete with respect to the logical type rules:

  **Theorem 11** (The syntax directed rules are sound): If Γ `s e : σ
  holds then we can also derive Γ ` e : σ.

  **Theorem 12** (The syntax directed rules are complete): If Γ ` e : σ
  holds then we can also derive Γ `s e : σ' where σ 0 v σ.

## C. Minimizing generalizations

  Figure 13 defines alternative syntax directed rules that minimize
  the number of generalizations and instantiations. In particular, for
  normal Hindley-Milner programs this reduces the generalizations
  to let bindings only. The expression Γ |-  s e : σ states that e has
  type σ under type environment Γ. Furthermore, the  ‘argument’
  gives the expected form of σ, if  = true than σ is generalized,

<!-- page 13 -->


  Figure 13. Alternative syntax directed type rules that reduce the
  number of generalizations and instantiations.

  otherwise σ is instantiated to a ρ type. The function gen
  (Γ, σ)
  instantiates or generalizes depending on , and most rules call this
  function in their conclusion.

  The VAR
  s
  rule returns gen
  (Γ, σ) which potentially instantiates
  the type. The LET
  s passes true to the inference of e1 so that a
  polymorphic type is derived. The expected form of the body is
  determined by the expected form of the entire let expression (i.e.
  ). In contrast to LET
  s
  , FUN
  s passes false to the body of the lambda
  expression to derive an instantiated ρ type. Rule APP
  s
  is the most
  interesting. It passes false to the function derivation since it requires
  an instantiated function type. Application only needs the argument
  to be generalized if the expected parameter is a polymorphic type,
  and APP
  s passes σ3 ∈ Q for the expected form to the derivation
  of the argument, where we write σ ∈ Q if there exists no ρ such
  that σ = ρ. We still need to instantiate the result in case σ3 ∈ Q
  since the type of e2 could be more general. Note that in the case
  where σ3 ∈ Q/ , we know that σ2 is an unquantified type and that
  no further instantiation can be done, i.e. σ2 = σ3.

## D. Soundness and completeness results
## D.1 General properties

  Theorem 13 (Robustness): If (e1 e2) is well typed, than so is
  (apply e1 e2).

  Proof of Theorem 13: Since e1 e2 is an application, we must have that:
  Γ ` e1 : σ2 → σ Γ ` e2 : σ2 minimal(Jσ2 → σK)
  Γ ` e1 e2 : σ (1)

  for some environment Γ. Moreover, there exists a principal derivation where
  Γ ` e1 : ∀α. σ0
  2 → σ
  0
  , and ∀α. σ0
  2 → σ
  0 v σ2 → σ (2). Assuming that
  α are fresh (3), we can derive:

  Γ ` apply : ∀αβ. (α → β) → α → β
  ∀αβ. (α → β) → α → β v (σ
  0
  2 → σ
  0
  ) → σ
  0
  2 → σ
  0
  Γ ` apply : (σ
  0
  2 → σ
  0
  ) → σ
  0
  2 → σ
  0
  (4)
  Γ ` e1 : ∀α. σ0
  2 → σ
  0 ∀α. σ0
  2 → σ
  0 v σ
  0
  2 → σ
  0
  Γ ` e1 : σ
  0
  2 → σ
  0
  (5)

  and thus

  (4)
  Γ ` apply : (σ
  0
  2 → σ
  0
  ) → σ
  0
  2 → σ
  0
  (5)
  Γ ` e1 : σ
  0
  2 → σ
  0
  minimal(J(σ
  0
  2 → σ
  0
  ) → σ
  0
  2 → σ
  0
  K)
  Γ ` apply e1 : σ
  0
  2 → σ
  0

  Note that to match the required parameter type of apply, we must instantiate
  ∀α. σ0
  2 → σ
  0
  at least to σ
  0
  2 → σ
  0
  , which does not increase the polymorphic
  weight of the type. Furthermore, the most general type of apply
  is minimally instantiated to just those types needed to match the type of
  e1. Therefore, no arbitrary polymorphism is introduced and the minimality
  condition is satisfied. Since α 6∩ ftv(Γ) (by (3)), we can use GEN to derive
  Γ ` apply e1 : ∀α. σ0
  2 → σ
  0
  and by (2), we can use INST again to
  derive Γ ` apply e1 : σ2 → σ. We can now use derivation (1) to derive
  Γ ` (apply e1) e2 : σ. 

## D.2 Syntax-directed type rules

  Proof of Theorem 11: We prove soundness of the syntax directed rules, i.e.
  when Γ `s e : σ then Γ ` e : σ also holds.

  Case x: Immediate by VAR.

  Case let x = e1 in e2: Immediate by induction and LET.

  Case λx.e: By induction Γ, x : τ ` e : σ, and since σ v ρ we can use
  INST to derive Γ, x : τ ` e : ρ and by FUN, Γ ` λx.e : τ → ρ. By
  assumption α 6∩ ftv(Γ) and we can apply GEN multiple times to derive
  Γ ` λx.e : ∀α. τ → ρ.

  Case λ(x :: σ).e: Similar to the previous case.

  Case (e1 e2): By induction Γ ` e1 : σ1 and Γ ` e2 : σ2. Since
  σ1 v σ3 → σ and σ2 v σ3, we can derive Γ ` e1 : σ3 → σ and
  Γ ` e2 : σ3.

  By assumption the Jσ3 → σK is minimal for the syntax directed
  derivations (1). Suppose there exist non-syntax directed derivations for e1
  and e2 where Jσ3 → σK would be lower. By induction over the number of
  applications and by Theorem 12, we would also have an equivalent syntax
  directed derivation which contradicts the assumption (1). Therefore, the
  polymorphic weight of σ3 → σ is minimal for the non-syntax directed
  rules too, and we can use APP to derive Γ ` e1 e2 : σ. Finally, using
  α 6∩ ftv(Γ), we apply GEN multiple times to derive Γ ` e1 e2 : ∀α. σ. 

  -----

  -----

  Proof of Theorem 12: We prove completeness of the syntax directed rules,
  i.e. when Γ ` e : σ holds, we can also derive Γ `s e : σ
  0 where σ
  0 v σ.

  Before proving completeness, we first need to establish that the syntax
  directed rules can always derive fully generalized types. We show that for
  any derivation Γ `s e : σ there also exists a derivation Γ `s e : σ
  0
  such
  that σ
  0 = ∀α. σ for any α /∈ ftv(Γ) (1).

  Case x: Since for any α /∈ ftv(Γ), it must be that α /∈ ftv(σ) since
  (x : σ) ∈ Γ. Therefore σ
  0 = ∀α. σ = σ which is the expected result.

  Case let x = e1 in e2: By induction on the derivation of the body, there
  also exists a derivation Γ, x : σ1 `s e2 : σ
  0
  2
  satisfying σ
  0
  2 = ∀α. σ2 which
  is the expected result.

  Case (λx.e): Since we can derive ∀α. τ → ρ for any α 6∩ ftv(Γ), the
  result is immediate.

  Case (λ(x :: σ).e: Same as the previous case.

  Case (e1 e2): Same as the previous case.
  Now that we proved (1), we can prove completeness by induction over
  the syntax directed rules:

  Case x: Immediate by VARs.

  Case GEN: By induction, Γ `s e : σ1 where σ1 v σ (2). Moreover, by
  (1), there also exists a derivation Γ `s e : σ2 where σ2 = ∀α. σ1 for any
  α /∈ ftv(Γ), and therefore by (2), σ2 v ∀α. σ.

  Case INST: By induction Γ `s e : σ
  0 where σ
  0 v σ1. Since σ1 v σ2, we
  also have σ
  0 v σ2 which is the expected result.

  Case let x = e1 in e2: Immediate by induction and LETs.

  Case λx.e: By induction, we have Γ, x : τ `s e : σ where σ v ρ,
  and we can use FUNs directly to derive Γ `s λx.e : ∀α. τ → ρ, where
  ∀α. τ → ρ v τ → ρ.

  Case λ(x :: σ).e: Similar to the previous case.

  Case (e1 e2): By induction, Γ `s e1 : σ
  0
  1
  holds where σ
  0
  1 v σ2 → σ, and
  Γ `s e2 : σ
  0
  2 where σ
  0
  2 v σ2. By assumption Jσ2 → σK is minimal for the
  type derivations of e1 and e2 (3). Suppose we would be able have syntaxdirected
  derivations for e1 and e2 with a lower weight. By induction on the

<!-- page 14 -->

  number of applications and Theorem 11 this would imply that there also
  exist non-syntax directed derivations with equal weight contradicting the
  assumption (3). Therefore, the weight is minimal under the syntax directed
  rules too and we can use APPs to derive Γ `s e1 e2 : ∀α. σ where
  ∀α. σ v σ. 

## D.3 Substitution properties

  Properties 14

  i. If σ1 v σ2 then Sσ1 v Sσ2 for any substitution S.

  ii. If σ1 = σ2, then Sσ1 = Sσ2 for any substitution S.

  iii. If S = S
  00 ◦ S
  0
  , then JS
  0σK 6 JSσK.

  We write ftv(S) as a shorthand for dom(S)∪ ftv(codom(S)).
  When composing to independent substitutions we write S1 · S2
  where S1 ◦ S2 = S2 ◦ S1. It follows that dom(S1) 6∩ ftv(S2) and
  dom(S2) 6∩ ftv(S1). Note that we can split any substitution S as
  S1 · S2 where dom(S1) 6∩ dom(S2) and dom(S1)∪ dom(S2) =
  dom(S).

## D.4 Unification

  Properties 15

  i. If S = unify(σ1, σ2) then S is idempotent.

  Proof of Theorem 3: (Unification is sound) When unify(σ1, σ2) = S,
  then Sσ1 = Sσ2 holds too.

  Case unify(α, α): Immediate.

  Case unify(α, σ): We have that α /∈ ftv(σ) and S = [α := σ], and
  therefore, Sα = σ = Sσ.

  Case unify(c σ1 ... σn, c σ
  0
  1
  ... σ0
  n): . With S
  0
  i = unify(Siσi, Siσ
  0
  i
  ),
  we have Si+1 = S
  0
  i
  ◦ Si, and by induction Si+1Siσi = Si+1Siσ
  0
  i
  (1).
  By Property 15.i, Si+1 ◦ Si = S
  0
  i
  ◦ Si ◦ Si = S
  0
  i
  ◦ Si = Si+1, and we
  can restate (1) as Si+1σi = Si+1σ
  0
  i
  . Due to Property 14.ii, we also have
  Sn+1σi = Sn+1σ
  0
  i
  . By definition of substitution, Sn+1(c σ1 ... σn) =
  c (Sn+1σ1)...(Sn+1σn) = c (Sn+1σ
  0
  1
  )...(Sn+1σn) = Sn+1(c σ
  0
  1
  ...
  σ
  0
  n).

  Case unify(∀α. σ1, ∀β. σ2): By induction on unify([α := c ]σ1, [β :=
  c ]σ2), we have S[α := c ]σ1 = S[β := c ]σ2 (2). Since unification
  does not introduce new type variables and α and β are fresh, we have
  α /∈ ftv(S) (3) and β /∈ ftv(S) (4). We can treat c as a type variable
  with out loss of generality, and by assumption c ∈/ ftv(codom(S)), we
  also have c ∈/ ftv(S) (5), c ∈/ ftv(Sσ1) and c ∈/ ftv(Sσ2) (6). We can
  now derive:

      S(∀α. σ1) = (3)
      ∀α. Sσ1 = (6), (α-renaming)
      ∀c. [α := c ]Sσ1 = (3), (5)
      ∀c. S[α := c ]σ1 = (2)
      ∀c. S[β := c ]σ2 = (3), (5)
      ∀c. [β := c ]Sσ2 = (6), (α-renaming)
      ∀β. Sσ2 = (4)
      S(∀β. σ2)

  which is the expected result. 

  Proof of Theorem 4: (Unification is complete and principal) We prove
  whenever Sσ1 = Sσ2 then unify(σ1, σ2) = S
  0 where S = S
  00 ◦ S
  0
  for some substitution S
  00. The proof is done by induction over the shape of
  types in normal form.

  Case Sα = Sσ: If α = σ the result is immediate through unify(α, α).
  Otherwise, we have α /∈ ftv(σ) (by an inductive argument on the size of
  σ), and unify(α, σ) returns [α := σ] which is most general.

  Case Sσ = Sα: As the previous case.

  Case S(c σ1 ... σn) = S(c σ
  0
  1
  ... σ0
  n): By definition of substitution, we
  have Sσi = Sσ0
  i
  . We proceed by induction on i. If i = 0 then unification
  succeeds with S1 = [ ] where S = S
  0
  1
  ◦ S1 and therefore S
  0
  1S1σi =
  S
  0
  1Siσ
  0
  i
  . If it holds for i − 1, we have by induction that unify(Siσi, Siσ
  0
  i
  )
  succeeds with S
  0
  i+1 where S = S
  00 ◦ S
  0
  i+1 ◦ Si = S
  00 ◦ Si+1. Therefore
  unification succeeds with Sn+1 and S = S
  00 ◦Sn+1 which is the expected
  result.

  Case S(∀α. σ1) = S(∀β. σ2): We can assume that α /∈ ftv(S) and
  β /∈ ftv(S) (1). We also assume a fresh c such that c ∈/ ftv(S, σ1, σ2) (2).
  Since the types are in normal form and by (1) and (2), we must have
  S[α := c ]σ1 = S[β := c ]σ2. By induction, unify([α := c ]σ1, [β :=
  c ]σ2) = S
  0
  succeeds where S = S
  00 ◦ S
  0
  , and by (2) c ∈/ ftv(S
  0
  ) and
  unification does not fail.

  Case S(c1 σ1 ... σn) = S(c2 σ
  0
  1
  ... σ0
  n): Cannot be equal when c1 6= c2.

  Case S(∀α. σ) = Sρ: Cannot be equal since types are in normal form. 

## D.5 Subsumption

  Proof of Theorem 5: (Subsumption is sound) When subsume(σ1, σ2) =
  S, then Sσ2 v Sσ1 holds too. By definition of subsumption, S =
  S1 − β, where S1 = unify([α := c ]ρ1, ρ2). Therefore, we can write
  S1 as S · Sb (1) where dom(Sb) = β (2). By Theorem 3, we have
  S1[α := c ]ρ1 = S1ρ2 (3) where S1 is most general. Also, we have
  c 6∩ con(codom(S)) (4) (or otherwise subsume fails). Without loss of
  generality, we treat c as type variables, and we have c 6∩ ftv(ρ1) (5), and
  c 6∩ ftv(σ2) (6). We now derive:

      Sσ2 =
      S(∀β. ρ2) v (6), (2)
      S(∀c. Sbρ2) = (4)
      ∀c. SSbρ2 = (1)
      ∀c. S1ρ2 = (3)
      ∀c. S1[α := c ]ρ1 = (1)
      ∀c. SSb[α := c ]ρ1 = (β 6∩ ftv([α := c ]ρ1))
      ∀c. S[α := c ]ρ1 = (4)
      S(∀c. [α := c ]ρ1) = (5), α-renaming
      S(∀α. ρ1) =
      Sσ1

  and therefore Sσ2 v Sσ1. 

  Proof of Theorem 6: (Subsumption is partially complete and principal) We
  prove that when Sσ2 v Sσ1 (1) and when σ1 is not a type variable (2),
  then the algorithm subsume(σ1, σ2) succeeds with S
  0 where S = S
  00 ◦S
  0
  for some S
  00. We assume σ1 = ∀α. ρ1 and σ2 = ∀β. ρ2 for some fresh α
  and β such that α 6∩ β, α 6∩ ftv(σ2) and β 6∩ ftv(σ1) (3). We also have
  (α ∪ β) 6∩ ftv(S) (4) (or otherwise the substitution would capture bound
  variables).

  By definition of generic instance and (3), we have S(∀β. ρ2) v
  S(∀α. [β := σ]ρ2) = S(∀α. ρ1) for some σ. Therefore, by (2)
  and (4), S[β := σ]ρ2 = Sρ1. By Property 14.ii, it also holds that
  [α := c ]S[β := σ]ρ2 = [α := c ]Sρ1 for some fresh c (5), and using
  (4), we can rewrite this as: SSb[α := c ]ρ2 = S[α := c ]ρ1where
  Sb = [β := S[α := c ]σ], and S ◦ Sb = Sb ◦ S (6). Since α 6∩ ftv(ρ1)
  (by (3)), SSbρ2 = SSb[α := c ]ρ1 (7) holds.

  By Theorem 4 and (7), unify([α := c ]ρ1, ρ2) succeeds with a most
  general S
  0 where S ·Sb = S
  00 ◦S
  0
  for some S
  00. We can split S
  0
  as Sr ·S
  0
  b
  where dom(Sr) 6∩ β (8), and S · Sb = S
  00 ◦ (Sr · S
  0
  b
  ). By (6), we have
  dom(S) 6∩ β, and together with (8), this implies S = (S
  00 − β) ◦ Sr (9).
  Finally, by (5), we also have c 6∩ con(S) and by (9) c 6∩ con(Sr) which
  means that subsumption does not fail. Together with (9) this is the expected
  result. 

## D.6 Type inference

  Properties 16

  i. If Γ `s e : σ then θΓ `s e : θσ holds for any θ (since we
  assume closed annotations).

  ii. If infer(Γ, e) = (θ, σ) then θσ = σ.

  iii. If infer(Γ, e) = (θ, σ) then θ is idempotent, i.e. θ ◦ θ = θ.

  iv. If infer(Γ, e) = (θ, σ) then ftv(σ) ⊆ ftv(θΓ).

  Note that in Hindley-Milner, we also have that property that if
  σ1 v σ2 and Γ, x : σ2 `s e : σ, then also Γ, x : σ1 `s e : σ. In
  our case though, this does not hold since instantiation could create
  extra sharing or polymorphic types which would make type inference
  incomplete. Therefore, the environment always contains most

<!-- page 15 -->

  general types for each binding which is ensured by the minimality
  condition on let-bindings.

  We prove soundness and completeness of the type inference algorithm
  (Theorem 7 and Theorem 8) as a direct corollary of the
  soundness and completeness of the syntax directed rules (Theorem
  11, Theorem 12), and the following theorems that state that
  the type inference algorithm is sound and complete with respect to
  the syntax directed rules:

  Theorem 17 (Type inference is sound with respect to the syntax
  directed rules): If infer(Γ, e) = (θ, σ) then θΓ `s e : σ holds.

  Theorem 18 (Type inference is complete and principal with respect
  to the syntax directed rules): If θΓ `s e : σ then infer(Γ, e) =
  (θ
  0
  , σ0
  ) such that θ ≈ θ
  00 ◦ θ
  0
  for some θ
  00, and θ
  00σ
  0 v σ.

  As a corollary we have that every expression has a principal type,
  i.e. for any derivation Γ ` e : σ
  0
  there also exists a derivation
  Γ ` e : σ with a unique most general type σ such that σ v σ
  0
  .
  By Property 1 it follows that every expression also has a type of
  minimal polymorphic weight since JσK 6 Jσ
  0
  K.

  -----

  Proof of Theorem 17: We prove soundness of type inference, i.e. if
  infer(Γ, e) = (θ, σ) then θΓ `s e : σ is derivable, by induction over
  the syntax of e.

  Case x: This implies θ = [ ] and σ = Γ(x) where (x : σ) ∈ dom(Γ). By
  VARs, we can now derive Γ `s x : σ directly.

  Case let x = e1 in e2: This results in (θ2 ◦ θ1, σ2). By induction, we
  know θ1Γ `s e1 : σ1 (1) and θ2θ1(Γ, x : σ1) `s e2 : σ2. Property 16.i
  implies θ2θ1Γ `s e1 : θ2σ1 also holds where θ2σ1 is most general (by
  Theorem 18), and by Property 16.iii we have θ2θ1(Γ, x : θ2σ1) `s e2 : σ2
  too. Now we can use LETs to derive θ2θ1Γ `s let x = e1 in e2 : σ2.

  Case λx.e: We write σ for generalize(θΓ, θ(α → ρ)) (2). By induction,
  we have θ(Γ, x : α) `s e : ∀β. ρ, and by Property 16.i also θ(Γ, x : α) `s
  e : θ(∀β. ρ) (3). We can instantiate θ(∀β. ρ) v θρ (4). Writing α for
  ftv(θ(α → ρ)) − ftv(Γ), we have σ = ∀α. θ(α → ρ) = ∀α. θα → θρ.
  Since α 6∩ ftv(Γ), we can use (3) and (4) with FUNs to derive θΓ `s
  λx.e : σ.

  Case λ(x :: σ).e: As the previous case.

  Case (e1 e2): By induction θ1Γ `s e1 : ∀α. σ1 → σ (5) holds, where α
  is fresh, and θ2θ1Γ `s e2 : σ2 (6). Also, split(subsume(θ2σ1, σ2)) =
  Θ3 ◦ θ3, and by Theorem 5, Θ3θ3σ2 v Θ3θ3θ2σ1 where Θ3 ◦ θ3 is most
  general (7). By Property 16.i, we have θ1σ1 = σ1 by (5), and θ2θ1σ2 =
  σ2 by (6). Using (5) and (6) we know that Θ3θ4σ2 v Θ3θ4σ1 (8)holds.

  The escape check ensures that dom(Θ3) 6∩ ftv(Γ) (9). Combining
  this with Property 16.i and (5), we can derive θ4Γ `s e1 : θ4(∀α. σ1 →
  σ) (10) and θ4Γ `s e2 : θ4σ2 (11). Moreover, by (9) and Property
  16.iv we have that dom(Θ3) 6∩ ftv(θ4∀α. σ1 → σ) and dom(Θ3) 6∩
  ftv(θ4σ2) (12). Therefore, θ4(∀α. σ1 → σ) = Θ3θ4(∀α. σ1 → σ) v
  Θ3θ4σ1 → Θ3θ4σ (13), and by (8), θ4σ2 = Θ3θ4σ2 v Θ3θ4σ1 (14).

  By Theorem 18 we have that ∀α. σ1 → σ and σ2 are most general
  types under a minimal subtitution θ2θ1. Together with (7) we have that
  Θ3 ◦ θ4 is a minimal substitution such that Θ3θ4σ2 v Θ3θ4σ1. Since
  Θ3θ4 is minimal, it introduces the minimal polymorphism necessary to
  match the argument types (by Property 14.iii. Since ∀α. σ1 → σ is most
  general, it now follows that JΘ3θ4(σ1 → σ)K is minimal (15).

  Finally, the result σ
  0
  is defined as generalize(θ4Γ, Θ3θ4σ) and equals
  ∀β. Θ3θ4σ where β = ftv(Θ3θ4σ) − ftv(θ4Γ) which ensures β 6∩
  θ4Γ (16). Using (10), (11), (13), (14), (15), and (16), we can use APPs
  to derive θ4Γ `s e1 e2 : σ
  0 which is the expected result. 

  ----

  Proof of Theorem 18: (Inference is complete and principal) We prove that
  if θΓ `s e : σ, than also infer(Γ, e) = (θ
  0
  , σ0
  ) such that θ ≈ θ
  00 ◦ θ
  0
  for
  some θ
  00, and θ
  00σ
  0 v σ. We proceed by induction on the syntax of e.

  Case x: By assumption x :σ ∈ θΓ and therefore x :σ
  0 ∈ Γ where σ = θσ0
  .
  Now, infer(Γ, x) succeeds with ([ ], σ0
  ) where θ = θ ◦ [ ] and θσ0 v σ.

  Case let x = e1 in e2: By induction infer(Γ, e1) = (θ1, σ0
  1
  ) holds
  where θ ≈ θ
  0
  1
  ◦ θ1 (1) and θ
  0
  1
  σ
  0
  1 v σ1. Actually, since by assumption σ1
  is the most general type derivable for e1, it must be the case that θ
  0σ
  0
  1 =
  σ1 (2). Therefore, we can rewrite the assumption θ(Γ, x : σ1) `s e2 : σ2
  to θ
  0
  1
  (θ1Γ, x :σ
  0
  1
  ) `s e2 : σ2 using (2) and Property 16.i. By induction, we
  now have infer((θ1Γ, x : σ
  0
  1
  ), e2) = (θ2, σ0
  2
  ), where θ
  0
  1 ≈ θ
  0
  2
  ◦ θ2 and
  θ
  0
  2
  σ
  0
  2 v σ2 (3). By (1), we now have θ ≈ θ
  0
  1
  ◦ θ1 = θ
  0
  1
  ◦ θ
  0
  2
  ◦ (θ2 ◦ θ1),
  and by (3) and Property 14.i we have θ
  0
  1
  θ
  0
  2
  σ
  0
  2 v θ
  0
  1
  σ2 = σ2 which is the
  expected result.

  Case λx.e: We have θΓ, x : τ `s e : σ, σ v ρ (4) and α 6∩ ftv(θΓ) (5).
  Let α be a fresh variable and let θ
  0 = [α := τ ] ◦ θ (6) such that the
  derivation for e can be written as θ
  0
  (Γ, x : α) `s e : σ. By induction
  we now have infer((Γ, x : α), e) = (θ1, σ1) where θ
  0 = θ
  0
  1
  ◦ θ1 (7),
  θ
  0
  1
  σ1 v σ (8).

  From (8) and (4), we know that θ
  0
  1
  σ1 v ρ. Therefore, σ1 = ∀β. ρ1
  where θ
  0
  1Sbρ1 = ρ (9) for some Sb = [β := σ] (10), where β are
  fresh (11). Here we assume we instantiate all β and Sb does not have to
  be idempotent. Since θ1σ1 = σ1, we also have θ1ρ1 = ρ1 (12). The result
  type is ∀γ. θ1(α → ρ1) where γ = ftv(θ1(α → ρ1))−ftv(θ1Γ) (13) and
  β ⊆ γ (14) (by (10)). Moreover, taking γ1 = γ − β, we have γ1 ⊆ α (15)
  (by (5)). We can now derive:

      θ
      0
      1
      (∀γ. θ1(α → ρ1)) = (12)
      θ
      0
      1
      (∀γ. θ1α → ρ1) v (10), (14)
      θ
      0
      1
      (∀γ1. θ1α → Sbρ1) v
      ∀γ1. θ0
      1
      θ1α → θ
      0
      1Sbρ1) = (7)
      ∀γ1. θ0α → θ
      0
      1Sbρ1 = (6), (9)
      ∀γ1. τ → ρ v (15)
      ∀α. τ → ρ

  which is the expected result.

  Case λ(x :: σ).e: Similar to the previous case.

  Case (e1 e2): Since θΓ `s e1 : σ1 holds we have by induction that
  infer(Γ, e1) = (θ1, σ0
  1
  ) where θ ≈ θ
  0
  1
  ◦ θ1 (16) and θ
  0
  1
  σ
  0
  1 v σ1 (17). By
  assumption θ
  0
  1
  θ1Γ `s e2 : σ2 implies infer(θ1Γ, e2) = (θ2, σ0
  2
  ) where
  θ
  0
  1 ≈ θ
  0
  2
  ◦ θ2 (18) and θ
  0
  2
  σ
  0
  2 v σ2 (19).

  Also, σ1 v σ3 → σ holds and therefore σ
  0
  1 = ∀α1. σ0
  3 → σ
  0
  (20)
  where α1 is fresh (21). By (17) , we can derive θ
  0
  1
  (∀α1. σ0
  3 → σ
  0
  ) =
  ∀α1. θ0
  1
  (σ
  0
  3 → σ
  0
  ) v Saθ
  0
  1
  (σ
  0
  3 → σ
  0
  ) = σ3 → σ where dom(Sa) ⊆
  α1. Writing S
  0 = Sa ◦ θ
  0
  2
  (22), we have S
  0θ2(σ
  0
  3 → σ
  0
  ) = σ3 → σ (23),
  and S
  0θ2σ
  0 = σ (24). By assumption, we have σ2 v σ3. By (19), we have
  θ
  0
  2
  σ
  0
  2 v σ3, and by (22) and (21), S
  0σ
  0
  2 v σ3. Together with (23) we have
  S
  0σ
  0
  2 v S
  0θ2σ
  0
  3
  (25).

  Given that Jσ3 → σK is minimal, JS
  0θ2(σ
  0
  3 → σ
  0
  )K is minimal too
  (by (23)) (26). Since θ2 is minimal, it follows that S
  0 must introduce the
  minimal amount of polymorphism to fullfil (25). We distinguish now two
  cases depending on whether θ2σ
  0
  3
  is a type variable or not.
  If θ2σ
  0
  3
  is not a type variable, then it folllows by Theorem 6 that
  subsume(θ2σ
  0
  3
  , σ0
  2
  ) = S with a most general S where S
  0 = S
  00 ◦ S
  for some S
  00. If we write S
  00 as Θ00 ◦ θ
  00, we have S
  0 = Θ00 ◦ θ
  00 ◦S. Since
  S is the most general substitution such that Sσ0
  2 v Sθ2σ
  0
  3
  , it must be that
  Θ00 is empty to make the polymorphic weight of S
  0 minimal. Therefore,
  S
  00 = θ
  00 and S
  0 = θ
  00 ◦ S.

  In the other case θ2σ
  0
  3
  is equal to some type variable α. In that case
  subsume(θ2σ
  0
  3
  , σ0
  2
  ) = S where S = [α := ρ] assuming σ
  0
  2 = ∀α. ρ for
  some fresh α. If S
  0
  can be written as S
  00 ◦S, we have S
  0σ
  0
  2 v S
  0α = S
  00ρ
  (by (25)) which implies that S
  00 must be monomorphic substitution θ
  00
  in order to make the polymorphic weight of S
  0 minimal. If we cannot
  write S
  0
  in the form S
  00 ◦ S, it must be by (25) that S
  0
  is a composition
  S
  00 ◦ [α := σ2 ] (where σ2 is a polymorphic type). But in that case the
  weight of S
  0θ
  0
  (σ
  0
  3 → σ
  0
  ) is at least one higher than in the other case
  where S = [α := ρ], which contradicts that S
  0
  introduces minimal
  polymorphism (26).

  As a result, we have in both cases that S
  0
  can be written as θ
  00 ◦ S
  where subsume(θ2σ
  0
  3
  , σ0
  2
  ) = S. We also have split(S) = (Θ3, θ3)
  where S = Θ3 ◦ θ3. Also, we can write Sa as Θa ◦ θa, and we have
  S
  0 = θ
  00 ◦ Θ3 ◦ θ3 = Θa ◦ θa ◦ θ
  0
  2
  , which can be rewritten as (θ
  00 ◦ Θ3) ◦
  θ
  00 ◦ θ3 = Θa ◦ θa ◦ θ
  0
  2
  . This implies that dom(Θ3) ⊆ dom(Θa) ⊆ α
  and therefore dom(Θ3) 6∩ ftv(θ4Γ) (27). Since θ4 = θ3 ◦ θ2 ◦ θ1, we
  have θ ≈ θ
  00 ◦ θ4 (28) by (16) and (18).

  The returned type is ∀γ. Θ3θ4σ
  0 where γ = ftv(Θ3θ4σ
  0
  ) − ftv(Γ),
  and γ ⊆ α. We can derive θ
  00(∀γ. Θ3θ4σ
  0
  ) v ∀γ. θ00Θ3θ4σ
  0 =
  ∀γ. S0θ2θ1σ
  0 = ∀γ. S0θ2σ
  0 = ∀γ. σ v ∀α. σ. Together with (27), this
  is the expected result. 
