# Type Reconstruction for Type Classes 1

  Tobias Nipkow 2 and Christian Prehofer 3

  TU München 4

## Abstract

  We study the type inference problem for a system with type classes as in the functional programming language Haskell.
  Type classes are an extension of ML-style polymorphism with overloading.
  We generalize Milner’s work on polymorphism by introducing a separate context constraining the type variables in a typing judgement.
  This leads to simple type inference systems and algorithms which closely resemble those for ML.
  In particular we present a new unification algorithm which is an extension of syntactic unification with constraint solving.
  The existence of principal types follows from an analysis of this unification algorithm.

## 1 Introduction

  The extension of Hindley/Damas/Milner polymorphism with the notion of type classes in the functional programming language Haskell (HJW92) has attracted much attention.
  Type classes permit the systematic overloading of function names while retaining the advantages of the Hindley/Damas/Milner system: every typable expression has a most general type which can be inferred automatically.
  Although many extensions to Haskell’s type system have already been proposed (and also implemented), we believe that the essence of Haskell’s type inference algorithm has still not been presented in all its simplicity.
  The main purpose of this paper is to give a particularly simple algorithm, a contribution for implementors.
  At the same time we present a correspondingly simple type inference system, a contribution aimed at users of the language.
  Finally we give rigorous proofs of the soundness and completeness of the algorithm with respect to the inference system.
  Although both the algorithm and the inference system resemble their ML-counterparts very closely, the proofs are considerably more involved.

  A type class in Haskell is essentially a set of types (which all happen to provide a certain set of functions).
  The classical example is equality.
  In the pre-standard versions of ML, the equality function = has the polymorphic type `∀α.α → α → bool`,

  ----

  1 This is an extended version of (NP93)

  2 Research supported by ESPRIT BRA 6453, TYPES.

  3 Research supported by the Deutsche Forschungsgemeinschaft (DFG) under grant Br 887/4, Deduktive Programmentwicklung.

  4 Address: Institut für Informatik, Technische Universität München, 80290 München, Germany. Email: {nipkow,prehofer}@informatik.tu-muenchen.de

  ----

<!-- page 2 -->

  where the type variable `α` ranges over all types.
  However, `=` should not be applied to arguments of function type.
  To fix this problem, Standard ML (MTH90) introduces special type variables that range only over types where equality is defined.
  Equality differs from other polymorphic functions not just because of its restricted domain but also because of its mixture of polymorphism and overloading: equality on lists is implemented differently from equality on integers.

  Type classes treat both issues in a systematic way: the type variable α is restricted to elements of a certain type class, say `Eq`, the class of all “equality types”.
  Then for each type `τ` where `=` should be defined, we have to declare that τ is of class Eq by providing an implementation of `=` of type `τ → τ → bool`.

  To express the fact that a type τ is in some class `C` we introduce the judgement `τ : C`. 1
  The idea of viewing Haskell as a three level system of expressions, types and classes, where classes classify types, goes back to Nipkow and Snelting (NS91).
  However, in their system it is impossible to express that a type belongs to more than one class.
  To overcome this difficulty we introduce sorts as finite sets of classes.
  The judgement `τ : {C1, ..., Cn}` is a compact form of the conjunction `τ : C1 ∧ ... ∧ τ : Cn`.
  Alternatively we may think of `{C1, ..., Cn}` as a notation for `C1 ∩ ... ∩ Cn`, the intersection of the types belonging to the classes `C1` to `Cn`.
  This leads to a simple type inference system and algorithm.
  The former resembles that for Mini-ML (CDDK86), the latter is very similar to algorithm I by Milner (Mil78).
  The main difference is that in both cases we also compute a set of constraints of the form `α : {C1, ..., Cn}` where `α` is a type variable.

## 2 Mini-Haskell

  Since the aim of this paper is simplicity, we treat only the most essential features of Haskell relating to type classes.
  The resulting language is basically Mini-ML (CDDK86) plus class and instance declarations, Mini-Haskell for short.
  Its syntax is shown in Figure 1.
  Although the next paragraph provides a brief account of type classes, the reader should consult the Haskell Report (HJW92) or the original paper on type classes (WB89) for motivations and examples.
  Note that we do not follow the concrete names of classes etc. of Haskell in our examples.

  Mini-Haskell extends ML by a restricted form of overloading.
  Ignoring subclasses for a moment, each class declaration introduces a new class `C` and a new overloaded function name `x`.
  Semantically, `C` represents the set of all types which support a function `x`.
  For instance

    class α : Eq where eq : α → α → bool

  introduces the class `Eq` of all those types `τ` which provide a function `eq : τ → τ → bool`.
  A class declaration is like a module interface: it separates declarations from implementations.
  In order to “prove” that a particular type, say `int`, is in `Eq`, a “witness” for the required function eq needs to be provided.
  
  
  ----

  1 If classes are viewed as predicates on types, this leads to the Haskell notation C(τ).

  ----

<!-- page 3 -->

    Type classes        C
    Sorts               S  = {C 1 , ..., C n }
    Type variables      α
    Type constructors   t
    Types               τ  = α | t(τ 1 , ..., τ n )
    Type schemes        σ  = τ | ∀α:S.σ
    Identifiers         x
    Expressions         e  = x
                           | (e0 e1)
                           | λx.e
                           | let x = e0 in e1
    Declarations        d  = class α : C ≤ S where x : σ
                           | inst t : (S 1 , ..., S n )C where x = e
    Programs            p  = d; p | e

  Fig. 1. Syntax of Mini-Haskell types and expressions

  This is the purpose of instance declarations.
  In order to prove `int : Eq` we instantiate `eq` by `eq int`, some existing function of type `int → int → bool`:

    inst int : Eq where eq = eq int

  In general we can instantiate classes not just by ground types but also by type constructors.
  For example we may wish to express that a type `list(τ)` admits equality provided `τ` does:

    inst list : (Eq)Eq where eq = ...

  The declaration `list : (Eq)Eq` expresses that list maps types of class `Eq` to types of class `Eq`.
  The implementation of `eq` on lists is intentionally left blank: due to the absence of pattern matching and recursion in our language, the required code would be a nest of conditionals wrapped up in a fixpoint combinator.

  Classes can be arranged in hierarchies.
  The general class declaration

    class α : C ≤ S where x : σ

  introduces the new class `C` as a subclass of all classes in `S`, which must have been defined already.
  Type `α` is in `C` only if it is in the intersection of all the classes in `S` and provides a function `x` of type `σ`.
  For example the class `Ord` of ordered types can be defined as a subclass of `Eq` which provides an additional function `le`:

    class α : Ord ≤ Eq where le : α → α → bool

  Subclasses are mere syntactic sugar (CHO92).
  In the above example `Ord` could be defined without reference to `Eq` as a completely separate class.
  The only difference is that without subclasses the judgement `τ : Ord` has to be expanded to become `τ : Eq ∧ τ : Ord`, i.e. `τ : {Eq, Ord}`.
  However, it is almost easier to deal with subclasses directly than to eliminate them, as done in (NP93).
  To demonstrate this, and because subclasses are part of Haskell, we have included them in Mini-Haskell.

<!-- page 4 -->
## 2.1 Sorts and Types

  As motivated in the introduction, sorts are finite sets of classes.
  This representation is a key ingredient for the concise treatment of type inference.
  Yet semantically the sort `{C1, ..., Cn}` should be understood as `C1 ∩ ... ∩ Cn`.
  Thus `{C}` and `C` are equivalent, and the empty set `{}` is the sort/set of all types.
  If `S1` is more specialized, i.e. represents fewer types than `S2`, we write `S1 ⪯ S2`.
  Given a partial order `≤` on classes, the induced quasi-order `⪯` on sorts is defined by

    S1 ⪯ S2 ⇔ ∀C2 ∈ S2 .∃C1 ∈ S1.C1 ≤ C2

  It follows directly that `S1 ⊇ S2` implies `S1 ⪯ S2`.
  In the context of a non-trivial ordering `≤` on classes, the reverse implication does not hold: for example `{Ord} ⪯ {Eq}` although `{Ord} /⊇ {Eq}`.
  It is easy to see that any two sorts `S1` and `S2` possess an infimum whose representation is their union `S1 ∪ S2`.

  Because `⪯` is in general only a quasi-order (i.e. it is not antisymmetric), it gives rise to an equivalence

    S1 ≈ S2 ⇔ S1 ⪯ S2 ∧ S2 ⪯ S1.

  Sorts which are equivalent modulo `≈`, for example `{Ord}` and `{Ord, Eq}`, represent the same set of types.
  Although it would be mathematically more elegant to work with equivalence classes `[S]≈`, we prefer to stay closer to an implementation and work with sorts directly.
  Nevertheless it should be kept in mind that an implementation is free to choose an arbitrary representative from an equivalence class `[S]≈`, for example the one with fewest elements.

  Types in Mini-Haskell are simply terms over variables and constructors of fixed arity.
  Note that `→` is just another type constructor, i.e. `τ1 → τ2` is short for `→(τ1, τ2)`.
  The set of free variables in a type scheme is denoted by `FV(σ)`.
  Bound variables in type schemes range only over certain subsets of types: `∀α:S.σ` abbreviates all instances `{α |→ τ}σ` where `τ : S`, a judgment defined formally below.

  In the sequel a list of syntactic objects `s1, ..., sn` is abbreviated by `sn`.
  For instance, `∀(αn : Sn)~ .σ` is equivalent with `∀α1 :S1, ..., αn :Sn .σ`.
  Orderings extend to lists in the componentwise manner: `Sn ⪯ Tn ⇔ ∀i. Si ⪯ Ti`.

## 2.2 Declarations and Programs

  As shown in Figure 1, expressions are λ-terms extended with let-definitions.
  A program is a sequence of declarations followed by an expression.

  A Mini-Haskell class declaration `class α : C ≤ {C1, ..., Cm}` where `x : σ` corresponds to the Haskell declaration `class (C1 α, ..., Cm α) ⇒ Cα where x :: τ`, where `τ` is the body of `σ`.
  Note that σ must contain no free variables except `α`.
  The translation in the opposite direction is more involved because a Haskell class can declare any number of functions.
  This feature is clearly not essential and could, for instance, be modeled by representing a set of functions by a single tuple of functions.
  Strictly speaking, we could have dropped class names altogether since there is a one to one correspondence between class names and the single function declared in that class. 

<!-- page 5 -->

  This would have lead us to the language of Stefan Kaes (Kae88) but would have obscured the connection with Haskell.

  A Mini-Haskell instance declaration `inst t : (S1, ..., Sn)C where x = e` expresses that `t(τ1, ... τn)` is in class `C` provided the `τi` are of sort `Si`.
  It corresponds to the Haskell declaration `inst (con) ⇒ C(t α1 ... αn) where x = e` where `con` is a list consisting of assumptions `C′αi` with `C′ ∈ Si` for all `i = 1 ... n`.

## 2.3 Classifying Types

  Before we embark on type inference, the simpler problem of sort inference has to be settled.
  In ML and many other languages we have the judgement `e : τ`, expressing that `e` is of type `τ`.
  Similarly, we classify types by sorts with the judgement `τ : S`, stating that type `τ` is in sort `S`.
  This judgement depends on

  - the sorts of the type variables in `τ`.

      This is recorded in a sort context `Γ`, which is a total mapping from type variables to sorts such that `Dom(Γ) = {α | Γα /= {}}` is finite.
      Sort contexts can be written as `[α1 : S1, ..., αn : Sn]`.

  - the “functionality” of the type constructors.
  
      The behaviour of type constructors is specified by declarations of the form `t : ((Sn)~)C` which are lifted directly from instance declarations.
      In the sequel `∆` always denotes a set of such declarations.

  - the subclass ordering `≤`.


  The pair `∆,≤` is called a `(type) signature` and is denoted by `Σ`, i.e. `∆, ≤` and `Σ` are used interchangeably.

  Given `Γ` and `Σ` we can infer the sort of a type `τ` using the judgement `Σ,Γ ⊢ τ : S`.
  The rules are shown in Figure 2.
  Remember that the sort `{C}` and the class `C` are equivalent.

  The ordering `⪯` extends easily from sorts to contexts:

    Γ ⪯ Γ′ ⇔ ∀α. Γα ⪯ Γ′ α

  We say that `Γ′` is more general than `Γ`.
  It is easy to show that `⊢` is monotonic w.r.t. this ordering: `Σ,Γ′ ⊢ τ : S` implies `Σ,Γ ⊢ τ : S`.
  In the sequel this fact is often used implicitly.

  Because every two sorts possess an infimum, every type `τ` has a most specific sort `S`, i.e. `Σ, Γ ⊢ τ : S` and if `Σ, Γ ⊢ τ : S′` then `S ⪯ S′`.
  The computation of this most specific sort is straightforward and shall not concern us here because it is not relevant for our purposes.

  Having seen sort inference for Mini-Haskell types we are prepared for our main goal, type inference and type reconstruction for Mini-Haskell programs.

## 3 Type Inference Systems

  In this section we present two type inference systems for Mini-Haskell.
  We start with a set of inference rules which define the types of Mini-Haskell programs and expressions.
  
<!-- page 6 -->

    [i = 1 ... n]
      :
      :
    Σ,Γ ⊢ τ : Ci
    -----------------------------
    Σ,Γ ⊢ τ : {C1, ..., Cn}

    Σ,Γ ⊢ τ : {C1, ..., Cn}
    -----------------------------  i = 1 ... n
    Σ,Γ ⊢ τ : Ci

    Γ(α) = S
    -------------
    Σ,Γ ⊢ α : S

                        [i = 1 ... n]
                              :
                              :
    t : ((Sn)~)C ∈ ∆     Σ,Γ ⊢ τi : Si
    --------------------------------------
    Σ,Γ ⊢ t((τn)~) : C

    Σ,Γ ⊢ τ : C1   C1 ≤ C2
    ---------------------------
    Σ,Γ ⊢ τ : C2

  Fig. 2. The judgement Σ, Γ ⊢ τ : S
  
          ∆,(≤ ∪{(C, D) | D ∈ S})*,Γ,E[x:∀α:C.σ] ⊢ p : σ′
    CLASS ----------------------------------------------------------
          ∆,≤,Γ,E ⊢ (class α : C ≤ S where x : σ; p) : σ′

          ∆ ∪ {t : ((Sn)~)C},≤,Γ,E ⊢ p : σ′
          E(x) = ∀α:C.σ
          Γ[(αn : Sn)~],∆,≤,E ⊢ e : {α |→ t((αn)~)}σ
    INST  -----------------------------------------------------
          ∆,≤,Γ,E ⊢ (inst t : ((Sn)~)C where x = e; p) : σ′

  Fig. 3. The judgement ∆,≤,Γ,E ⊢ p : σ

  Then we proceed to a more restricted, syntax-directed set of rules, which will be the basis for the type inference algorithm.

  As usual in type inference for ML-like languages, an environment is a finite mapping `E = [x1 : σ1, ..., xn : σn]` from identifiers to types.
  The domain of `E` is `Dom(E) = {x1, ..., xn}`.
  `E[x:σ]` is a new map which maps `x` to `σ` and all other `xi` to `σi`.
  The free type variables in `E` are `FV(E) = FV(E(x1)) ∪ ... ∪ FV(E(x n))`.
  If `V` is a set of type variables the restriction of Γ to variables not in `V` is `Γ \ V = [α:Γα | α ∈ Dom(Γ) − V]`.

  **A substitution** is a finite mapping from type variables to types, written as `{α1 |→ τ1, ...}`.
  Substitutions are denoted by `θ` and `δ; {}` is the empty substitution.
  Define `Dom(θ) = {α | θα /= α}`, `Cod(θ) = ∪_{α∈Dom(θ)} FV(θ(α))` and `FV(θ) = Dom(θ) ∪ Cod(θ)`.

  There are two judgements which are defined in Figures 3 and 4: `∆,≤,Γ,E ⊢ p : σ` and `∆,≤,Γ,E ⊢ e : σ` express that program `p` and expression `e` are of type `σ` in the context of `∆`, `≤`, `Γ` and `E`.
  The rules for `∆,≤,Γ,E ⊢ p : σ`, when applied backwards, simply traverse the declarations, building up `∆`, `≤` and `E`.

<!-- page 7 -->

  Class declarations extend `E` and `≤`, instance declarations extend `∆`.
  Notice that it is necessary to take the transitive closure `(≤ ∪ {(C, D) | D ∈ S})*` of `≤` and the new subclass relations in rule CLASS.

  Rule INST also type-checks the instantiation of `x` by `e`, making sure that `e` is of type `{α |→ t((αn)~)}σ`, where `σ` is the generic type of `x` and `{α |→ t((αn)~)}` is a type substitution with new type variables `(αn)~`.

  Note that there are two context conditions for declaration sequences we have chosen not to formalize:

  1. `class α : C ≤ S` must be preceded by a declaration for each superclass in `S`, but not by another declaration `class α : C;`

  2. `inst t : ((Sn)~)C` must be preceded, for each superclass `D` of `C`, by a declaration `inst t : ((Tn)~)D` such that `(Sn)~ ⪯ (Tn)~`, but not by another declaration `inst t(...)C`.


  These conditions are the result of translating the restrictions actually adopted in Haskell (HJW92, 4.3.2) to Mini-Haskell.
  Enforcing them is simple enough and has thus been ignored in this paper.
  Nevertheless we assume in the sequel that all declarations, and hence `∆` and `≤`, meet the above conditions.

    ASM --------------------
        Σ,Γ,E ⊢ x : E(x)

        Σ,Γ,E ⊢ e : ∀α:S.σ   Σ,Γ ⊢ τ : S
    ∀E -----------------------------------
        Σ,Γ,E ⊢ e : {α 7→ τ }σ

        Σ,Γ[α:S],E ⊢ e : σ    α ∈ FV(σ) − FV(E)
    ∀I ----------------------------------------
        Σ,Γ,E ⊢ e : ∀α:S.σ

        Σ,Γ,E ⊢ e1 : τ2 → τ1    Σ,Γ,E ⊢ e2 : τ2
    APP -----------------------------------------
        Σ,Γ,E ⊢ (e1 e2) : τ1

        Σ,Γ,E[x:τ1] ⊢ e : τ2
    ABS ---------------------------
        Σ,Γ,E ⊢ λx.e : τ1 → τ2

        Σ,Γ,E ⊢ e1 : σ1    Σ,Γ,E[x:σ1] ⊢ e2 : σ2
    LET ------------------------------------------
        Σ,Γ,E ⊢ let x = e1 in e2 : σ2

  Fig. 4. The judgement Σ,Γ,E ⊢ e : σ

  The rules for `Σ,Γ,E ⊢ e : σ` extend the classical system of Damas and Milner (DM82) by the notion of sorts, which are represented via `Σ,Γ`, and restricted quantification in type schemes.
  The assumption `α ∈ FV(σ)` in `∀I` is not really essential (for soundness).
  Its practical significance is discussed in Section 8.
  In contrast to the CLASS and INST rules, `Σ` remains fixed.

## 3.1 Syntax-directed Type Inference

  The next step towards a type reconstruction algorithm is a more restricted set of rules.
  The application of these rules is determined by the syntax of the expression whose type is to be computed.

<!-- page 8 -->

  To distinguish the syntax-directed system we use `▷` instead of `⊢` and prime the names of its rules, e.g. `ASM′`.

  Definition 3.1

  The type scheme `σ′ = ∀(αn′ :Sn′)~ .τ′` is a generic instance of `σ = ∀(αm :Sm)~ .τ` under `Σ` and `Γ`, written `Σ,Γ ⊢ σ ⪰ σ′`, iff there exists a substitution `θ` such that

    θτ                   = τ ′ ,
    Dom(θ)               ⊆ {α m },
    Σ, Γ[α n ′ :S n ′ ]  ⊢ θα i : S i  [i = 1 ... m],
    {(α n ′)~} ∩ FV(σ)   = {}.

  With this relation on types we can now define the most general or principal type of an expression.
  We say `E` is closed if `FV(E) = {}`.

  Definition 3.2

  The type scheme `σ` is a principal type of an expression `e` w.r.t. `Σ` and a closed environment `E`, if `Σ,[],E ⊢ e : σ` and for every `σ′` with `Σ,[],E ⊢ e : σ′`, the type scheme `σ'` must be a generic instance of `σ`, i.e. `Σ,[] ⊢ σ ⪰ σ′`.
  For the syntax-directed system, the rules APP and ABS remain unchanged, the quantifier rules are incorporated into ASM and LET, as shown in Figure 5.

         Σ,Γ ⊢ E(x) ⪰ τ
    ASM′ ------------------
         Σ,Γ,E ▷ x : τ

         Σ,Γ[(αk : Sk)~],E ▷ e1 : τ1
         Σ,Γ,E[x:∀(αk :Sk)~ .τ1] ▷ e2 : τ2
    LET′ --------------------------------------
         Σ,Γ,E ▷ let x = e1 in e2 : τ2
         where {(αk)~} = FV(τ1) − FV(E)

  Fig. 5. The judgement Σ,Γ,E ▷ e : σ

  There is a straightforward correspondence between the two systems. The syntax-directed derivations are sound

  Theorem 3.3

  If `Σ,Γ,E ▷ e : τ` then `Σ,Γ,E ⊢ e : τ`.

  and in a certain sense complete w.r.t. the original system:

  Theorem 3.4

  If `Σ,Γ,E ⊢ e : ∀α n : Sn .τ` then `Σ,Γ[αn : Sn], E ▷ e : τ`.

  The proof of the last theorem is standard, as for instance in (CDDK86, App. A.1).

  Theorem 3.4 clarifies in what sense `▷` works differently from `⊢:` by applying the primed rules backwards, the sort constraints for type variables are stored solely in `Γ`, and not in the type scheme of `e`.
  For instance, the `LET′` rule explicitly extends `Γ`.
  The `⪰` operation, used in the `ASM′` rule, may introduce new type variables, whose sorts must be constrained in `Γ`.

<!-- page 9 -->

  The syntax-directed system already has a very operational flavour.
  In order to make the transition from a type inference system to an algorithm we need one more ingredient: unification.

## 4 Unification of Types with Sort Constraints

  This section deals with unification in the presence of sort constraints in the form of contexts.
  This problem can in principle be reduced to order-sorted unification, as done in (NS91) w.r.t. `⪯`.
  However, we have refrained from doing so because it is contrary to our quest for simplicity: involving order-sorted unification makes the algorithm appear more complicated than it actually is.
  In addition, the standard theory of order-sorted unification would need to be reformulated anyway: it assumes that variables are tagged with their sort, rather than using contexts.

  For the remainder of this paper we assume a fixed signature `Σ = (∆, ≤)`.
  This is simply a notational device which avoids excessive parameterization.

  Since sort information is maintained in contexts, we frequently work with pairs of contexts and substitutions.
  A substitution `θ` obeys the sort constraints of `Γ` in the context of `Γ′`, written `Γ′ ⊢ θ : Γ`, iff `Σ, Γ′ ⊢ θα : Γα` for all `α`.
  Because `Σ,Γ′ ⊢ θα : Γα` is trivially fulfilled if `Γα = {}` it suffices to require`Σ,Γ′ ⊢ θα : Γα` for all `α ∈ Dom(Γ)`.
  For instance, let `Eq` and list be defined as in the examples in Section 2.
  Then we have `[β:Eq] ⊢ {α |→ list(β)} : [α:Eq]`.

  We define an ordering on context-substitution pairs:

    (Γ,θ) ≥ (Γ′,θ′) ⇔ ∃δ. δθ = θ′ ∧ Γ′ ⊢ δ : Γ

  where `δθ` is defined as the composition: `(δθ)(s) = δ(θ(s))`.

  The set of unifiers of `τ1` and `τ2` w.r.t. `Γ`, written `U(Γ, τ1 = τ2)`, consists of the following context-substitution pairs:

    U(Γ, τ1 = τ2) = {(Γ′,θ) | θτ1 = θτ2 ∧ Γ′ ⊢ θ : Γ}

  A unifier `(Γ0 , θ0) ∈ U(Γ, τ1 = τ2)` is most general if `(Γ0, θ0) ≥ (Γ1, θ1)` for all `(Γ1, θ1) ∈ U(Γ, τ1 = τ2)`.
  We say that unification modulo `Σ` is unitary if for all `Γ` and `τ1 = τ2` the set `U(Γ, τ1 = τ2)` is empty or contains a most general unifier.

  A signature `Σ` is called coregular if for all type constructors `t` and all classes `C` the set

    D(t, C) = {(Sn)~ | ∃D ≤ C. (t : ((Sn)~)D) ∈ ∆}

  is either empty or contains a greatest element w.r.t. `⪯`.
  If `Σ` is coregular let `Dom(t,C)` return the greatest element of `D(t,C)` or fail if `D(t,C)` is empty.
  For instance, `Dom(list, Eq) = Eq` but `Dom(list, Ord)` fails.

  Sorted unification can be expressed as unsorted unification plus constraint solving.
  Given a coregular signature `Σ`, this has the following simple form:

    unify(Γ,τ1 = τ2) =
      let θ  = mgu(τ1 = τ2)
          Γc = Constrain(θ,Γ)
      in (Γc ∪ (Γ \ Dom(θ)),θ)

<!-- page 10 -->

  where

  - `mgu` computes an unsorted `mgu` (in particular we assume that `θ` is idempotent and that `Dom(θ) ∪ Cod(θ) ⊆ FV(τ1 = τ2))` or fails if none exists,

  - the union of two sort contexts is defined by

         Γ1 ∪ Γ2 = [α : Γ1α ∪ Γ2α | α ∈ Dom(Γ1) ∪ Dom(Γ2)]

  - `Constrain(θ,Γ)` computes the most general context `Γc` such that `Γc ⊢ θ : Γ`:
  
        Constrain(θ,Γ) = ∪_{α∈Dom(θ)} constrain(θα,Γα)

  - `constrain(τ,S)` computes the most general context `Γ` such that `Σ,Γ ⊢ τ : S`:

        constrain(α,S)        = [α:S]
        constrain(t((τn)~), S)  = ∪_{C∈S} constrains((τn)~, Dom(t,C))
        constrains((τn)~,(Sn)~) = ∪_{i=1...n} constrain(τi, Si)
        

  Thus unify fails if mgu fails or if some `Dom(t,C)` used in `Constrain` does not exist.
  By induction on the first argument of constrain it can be shown that
  
    constrain(t((τn)~,S) = ∪_{C∈S} constrains((τn)~, Dom(t,C))
  

  which provides an alternative definition of constrain which is also useful in the proofs below.
  To see how constrain works, assume `Eq` and list again as in the examples in Section 2.
  Then `constrain(list(β), Eq) = constrains(β, Dom(list, Eq)) = [β:Eq]`.

  Soundness and completeness of Constrain are captured by the following lemmas which assume coregularity of `Σ` and are proved by induction on the structure of `τ`:

  Lemma 4.1

  If `constrain(τ,S)` is defined then `Σ`, `constrain(τ,S) ⊢ τ : S`.

  Lemma 4.2

  If `constrain(θτ,S)` is defined then `constrain(τ,S)` is defined as well and furthermore `constrain(θτ,S) ⊢ θ : constrain(τ,S)` holds.

  Lemma 4.3

  If `Σ,Γ ⊢ τ : S` then `constrain(τ,S)` is defined and more general than `Γ`.

  Finally, the main theorems:

  Theorem 4.4

  If `Σ` is coregular, unify computes a most general unifier.

  Proof To show soundness, let `unify(Γ,τ1 = τ2)` terminate with result `(Γ0,θ0)`.
  It follows directly that `θ0 τ1 = θ0 τ2`.
  It remains to be seen that `Γ0 ⊢ θ0 α : Γα` for all `α`.
  If `α ∈/ Dom(θ0)`, then `Γα ⊆ Γ0 α` and the claim follows trivially.
  If `α ∈ Dom(θ0)` then `Γ0 ⪯ Γc = Constrain(θ0,Γ) ⪯ constrain(θ0 α,Γα)` and the claim follows from Lemma 4.1.

<!-- page 11 -->

  To show completeness let `(Γ1,θ1) ∈ U(Γ, τ1 = τ2)`, i.e. `θ1 τ1 = θ1 τ2` and `Γ1 ⊢ θ1 : Γ`.
  Since `τ1` and `τ2` have an unsorted unifier `θ1`, `mgu(τ1 = τ2)` is defined and yields a substitution `θ0` such that `θ1 = δθ0` for some `δ`.
  Definedness of `unify(Γ,τ1 = τ2)` also requires definedness of `constrain(θ0 α, Γα)`: since `Γ1 ⊢ θ1 α : Γα`, Lemma 4.3 implies definedness of `constrain(θ1 α, Γα)` and Lemma 4.2 yields definedness of `constrain(θ0 α, Γα)`.
  Thus `unify(Γ,τ1 = τ2)` terminates with a result `(Γ0,θ0)`.

  It remains to be shown that `Γ1 ⊢ δ : Γ0`.
  If `β ∈ Dom(θ0)` then `Γ0 β = {}` and hence `Γ1 ⊢ δβ : Γ0 β` holds trivially.
  Now assume `β ∈/ Dom(θ0)`.
  Thus `Γ0 β = Γc β ∪ Γβ`.
  From `Γ1 ⊢ θ1 : Γ` it follows that `Γ1 ⊢ δβ : Γβ`.
  Proving `Γ1 ⊢ δ : Γc` is more involved.
  From Lemma 4.2 it follows that `constrain(θ1 α, Γα) ⊢ δ : constrain(θ0 α, Γα)` for any `α`.
  Since `Γ1 ⊢ θ1 α : Γα`, Lemma 4.3 implies `Γ1 ⪯ constrain(θ1 α, Γα)` and thus by monotonicity `Γ1 ⊢ δ : constrain(θ0 α, Γα)`.
  This in turn easily yields `Γ1 ⊢ δ : Constrain(θ0, Γ)`, i.e. `Γ1 ⊢ δ : Γc`. □

  Theorem 4.5

  Unification modulo `Σ` is unitary iff `Σ` is coregular.

  Proof The “if” direction is a consequence of Theorem 4.4.
  For the “only if” direction let `Σ` not be coregular.
  Thus there are classes `C`, `D ≤ E` and declarations `t : ((Sn)~)C` and `t : ((Tn)~)D, (Sn)~ ⪯/ (Tn)~`, and `(Tn)~ ⪯/ (Sn)~`, such that there is no third declaration `t : ((Un)~)E′`, `E′ ⪯ E`, and `(Sn)~`, `(Tn)~ ⪯ (Un)~`.
  Hence the unification problem `([β:E], t((αn)~)=β)` does not have a most general unifier.
  Two maximal ones are `([(αn : Sn)~],θ)` and `([(αn:Tn)~], θ)` where `θ = {β → t((αn)~)}`. □

  Thus we have a precise characterization of those signatures where principal types exist.

  It remains to be seen if Mini-Haskell’s CLASS and INST declarations yield coregular signatures.
  In fact they do if restricted by the unformalized context conditions set out in Section 3.
  The latter context conditions imply that every `∆` and `≤` derived from valid class and instance declarations has the following strong property:
  `D(t,C)` is either the singleton `{(Sn)~}`, where `t((Sn)~)C` is the unique declaration for `t` with result `C`, or empty, if there is no such declaration.
  Therefore `Dom(t,C)` can be computed using `∆` alone, without reference to `≤`.
  This leads to the observation that type unification, and hence, as we shall see in the next section, type inference, can ignore the subclass hierarchy completely.

  It should be pointed out that ignoring the subclass hierarchy means giving up a degree of freedom afforded by the equivalence `≈` on sorts defined in Section 2.
  For example `unify([α : {Eq}, β : {Ord}], α = β)` returns `([α : {Eq, Ord}], {β |→ α})`.
  Taking `≈` into account, we could just as well return `([α : {Ord}], {β |→ α})`.
  In order to show that the subsequent developments do not depend on which of these unifiers is computed, we assume in the sequel that `unify` is an arbitrary function which, provided `Σ` is coregular, returns a most general unifier: if `U(Γ, τ1 = τ2) /= {}` then

  - `unify(Γ, τ1 = τ2) ∈ U(Γ, τ1 = τ2)` and
  - `(Γ′,θ) ≤ unify(Γ, τ1 = τ2)` for all `(Γ′,θ) ∈ U(Γ, τ1 = τ2)`.


  This implies a number of simple properties:

<!-- page 12 -->

  Fact 4.6

  If `unify(Γ,τ1 = τ2) = (Γ′,θ)` then

  - `θ` is a most general unifier of `τ1` and `τ2`,
  - `Dom(Γ′) ∪ FV(θ) ⊆ Dom(Γ) ∪ FV(τ1) ∪ FV(τ2)`,
  - `Dom(Γ′) ∩ Dom(θ) = {}`.


  The second fact states that `unify` does not introduce new variables, and the last expresses that `Γ′` does not constrain variables instantiated by `θ`.
  It is easy to see that the `Γ′` is determined only up to `≈`.
  Hence the unification algorithm could always ensure that `Γ′` is “minimized” by removing redundant elements from each sort.

  Finally one may wonder if the fact that coregularity is strictly weaker than Haskell’s context conditions means the latter could be relaxed.
  We believe that there are no non-trivial relaxations but do not want to enlarge on this subject because it requires going beyond the type system to take semantics and pragmatics into account.

## 5 Algorithm W

  The syntax-directed rule system in Figure 5 is non-deterministic, since rule ASM′ can choose any instance of the type of `x`.
  To obtain a deterministic algorithm, we refine the syntax directed system such that it keeps types as general as possible.
  The result is algorithm W in Figure 6. In this section we assume that `Σ` is coregular — otherwise unify is not well-defined.

    W(V,Γ,E,e) = case e of
                     x ⇒ let ∀(αn : Sn)~ .τ = E(x)
                                          βi ∈/ V [i = 1 ... n]
                          in (V′ ∪ {βn}, Γ[(βn :Sn)~], {}, {(αn |→ βn)~}τ)
                  λx.e ⇒ let            α ∈/ V
                              (V′,Γ′,θ′,τ) = W(V ∪ {α}, Γ, E[x:α], e)
                          in (V′,Γ′,θ′,α → τ)
               (e1 e2) ⇒ let (V1,Γ1,θ1,τ1) = W(V,Γ,E,e1)
                              (V2,Γ2,θ2,τ2) = W(V1,Γ1,θ1 E,e2)
                                          α ∈/ V2
                                    (Γ′,θ′) = unify(Γ2, θ2θ1τ1 = θ2τ2 → α)
                          in (V2 ∪ {α},Γ′,θ′θ2θ1,α)

      let x = e1 in e2 ⇒ let (V1,Γ1,θ1,τ1) = W(V, Γ, E, e 1 )
                                       {αn} = FV(θ1 τ1) − FV(θ1 E)
                              (V2,Γ2,θ2,τ2) = W(V1, Γ1 \ {αn},
                                                (θ1 E)[x : ∀(αn :Γ1 αn)~ .θ1 τ1],e2)
                          in (V2,Γ2,θ2θ1,τ2)

  Fig. 6. Algorithm W

  Algorithm W follows the same pattern as Milner’s original algorithm of the same name (Mil78): the type of an expression `e` is computed by traversing `e` in a top-down manner.
  `W(V,Γ,E,e)` returns a quadruple `(V′,Γ′,θ,τ)`, where `θτ` is the type of `e` in the context of `Γ′` and `θE`. 

<!-- page 13 -->

  The top level call is `W({},[],E,e)`, where `E` is closed.
  Observe the different let-constructs: the one on the left hand side is in the object language, the ones on the right are part of the type inference algorithm.

  The parameter `V` contains all “used” variables, i.e. variables that occur in `Γ` or in `E`.
  Thus a type variable `α ∈/ V` is a “new” variable.
  For our algorithm to be truly functional, a linear ordering on variables may be used, such that the “next” new variable `α ∈/ V` can be computed deterministically.
  We will assume in general that `W` is invoked with `V`, `Γ` and `E` such that `FV(E) ∪ Dom(Γ) ⊆ V`.

  Algorithm W is not meant to be implemented directly but merely serves as a mathematically tractable stepping stone towards an efficient implementation.
  Its principal weakness is the fact that substitutions are computed from scratch and composed later on.
  This problem is addressed and solved with algorithm I in the next section.
  In contrast to substitutions, contexts are computed incrementally, i.e. the result context `Γ′` is an extension of the input context `Γ`.

  A formal analysis of `W` requires some more notation.
  For an environment `E` and a substitution `θ`, define `θE = [x : θ(E(x)) | x ∈ Dom(E)]`.
  Two substitutions are equal on a set of variables `W`, written as `θ = W θ′`, if `θα = θ′ α` for all `α ∈ W`.
  The restriction of a substitution to a set of variables `W` is defined as `θ |W α = θα` if `α ∈ W` and `θ |W α = α` otherwise.
  Given a list of syntactic objects `(Cn)~` we write `FV((Cn)~)` instead of `FV(C1) ∪ ... ∪ FV(Cn)`.

  We first show that the algorithm is invariant under α-conversion.
  The free variables of an expression e, i.e. `FV(e)`, and the application of a substitution to `e` are defined as usually in λ-calculus.

  Lemma 5.1

  If `W(V,Γ,E[x:τ],e) = (V′,Γ′,θ′,τ′)` and `y ∈/ Dom(E)` then `W(V,Γ,E[y:τ], {x |→ y}e) = (V′,Γ′,θ′,τ′)`.
  Proof by induction on `e`. □

  With this lemma, we can easily show the desired theorem for α-conversion.

  Theorem 5.2

  Let `e` be `λx.e2` or `let y = e1 in e2`.
  If `W(V,Γ,E,e)` is defined, `y ∈/ Dom(E)`, and `y ∈/ FV(e)`, then `W(V,Γ,E,e′) = W(V,Γ,E,e)`, where `e′` is `λy.{x |→ y} e2` or `let y = e1 in {x |→ y} e2` respectively.

  Proof by induction, using Lemma 5.1. □

  The following correctness and completeness results for `W` do not depend on the particular unification algorithm, as discussed towards the end of Section 4.

  Theorem 5.3 (Correctness of W)

  If `W(V,Γ,E,e) = (V′,Γ′,θ,τ)` then `Σ,Γ′, θE ▷ e : θτ`.

  Before we can prove the correctness theorem, we need to supply a series of lemmas.
  The following lemma shows the basic relations between the variables of the objects used by `W`.
  The first item states that all used variables are recorded in `V′`.
  Next, all new variables occuring in the computed objects are in `V′` but not in `V`, i.e. there is no “reuse” of names.

<!-- page 14 -->

  The third item states that if some type variables of the computed type are not new, they must have been in the environment `E`.
  The last item requires the computed context to be free of assumptions about old variables (which are in `Dom(θ′)`), i.e. no “litter”.

  Lemma 5.4

  Assume `W(V,Γ,E,e) = (V′,Γ′,θ′,τ)` and `FV(E) ∪ Dom(Γ) ⊆ V`.
  Then

  1. `V ⊆ V′` and `Dom(Γ′) ∪ FV(θ′,E,τ) ⊆ V′`
  2. `(Dom(Γ′) ∪ FV(θ′,τ)) − (Dom(Γ) ∪ FV(E)) ⊆ V′ − V`.
  3. `FV(θ′,τ) ∩ V ⊆ FV(E)`
  4. `Dom(Γ′) ∩ Dom(θ′) = {}`


  **Proof** The first claim follows easily since all new variables are recorded in `V′` and since the unification algorithm does not introduce new variables (see Fact 4.6).
  For the same reason and since all variables in `Dom(Γ′) ∪ FV(θ′,τ)` are either new or in `Dom(Γ) ∪ FV(E)`,  `FV(E) ∪ Dom(Γ) ⊆ V`.

  The remaining two items are shown by induction on the term structure:

  `x` : trivial since all `βi` are new variables, i.e. `βi ∈/ V`.

  `λx.e` : by induction hypothesis and since `α` is a new variable.

  `(e1 e2)` : We first show

    (FV(θ′) ∪ {α}) ∩ V ⊆ FV(E).

  Since the unification algorithm does not introduce new variables, it follows that `FV(θ′) ⊆ FV(θ2θ1,τ1,τ2) ∪ {α}`.
  Because `α ∈/ V`, it suffices to show

    FV(θ2θ1,τ2,τ1) ∩ V ⊆ FV(E).  (1)

  The induction hypothesis for `e2` yields `FV(θ2,τ2) ∩ V1 ⊆ FV(θ1E)`.
  Using `FV(θ1E) ⊆ FV(E,θ1)` and `V ⊆ V1` we obtain `FV(θ2,τ2) ∩ V ⊆ F V(E,θ1)`.
  By induction hypothesis for `e1`, i.e. `FV(θ1,τ1)∩V ⊆ FV(E)`, we easily get (1).
  Next we show that `Dom(Γ′) ∩ Dom(θ′θ2θ1) = {}`.
  We obtain `Dom(Γ1) ∩ Dom(θ1) = {}` from the induction hypothesis.
  Then from `Dom(θ1) ⊆ V1` and `Dom(θ1) ∩ FV(θ1E) = {}` (idempotence of `θ1`) we obtain `Dom(Γ2) ∩ Dom(θ1) = {}`, as `Dom(Γ2) − (Dom(Γ1) ∪ FV(θ1E)) ⊆ V2 − V1` (item 2 of Lemma 5.4).
  Next, from `Dom(Γ2) ∩ Dom(θ2) = {}` (induction hypothesis) and since `Dom(θ2θ1) = Dom(θ1) ∪ Dom(θ2)`, `Dom(Γ2) ∩ Dom(θ2θ1) = {}` follows.
  Then `Dom(Γ′) ∩ Dom(θ′θ2θ1) = {}` follows from the properties of the unification algorithm, i.e. it may not constrain variables from `Dom(θ′)` (see Fact 4.6).

  `let x = e1 in e2` : We first show

    FV(θ2θ1,τ2) ∩ V ⊆ FV(E).

  The induction hypothesis for `e2` yields

    FV(θ2,τ2) ∩ V1 ⊆ FV(θ1 E[x : ∀(αn :Γ1 αn)~ .θ1τ1])

  Since `FV(∀(αn :Γ1 αn)~ .θ1 τ1) ⊆ FV(θ1E)`, we get

    FV(θ2,τ2) ∩ V1 ⊆ FV(θ1E).

<!-- page 15 -->

  Then the rest of the proof proceeds as for `(e1 e2)`.
  The proof of `Dom(Γ2) ∩ Dom(θ2θ1) = {}` also works as in the `(e1 e2)` case;
  the only difference (apart from the additional `θ′`) is that we have `Dom(Γ2) ∩ {(αn)~} = {}`, which only simplifies the proof. □

  The next lemma shows that the relation `Γ ⊢ θ : Γ′` enjoys a kind of transitivity property w.r.t. substitutions.

  Lemma 5.5

  If `Γ2 ⊢ θ2 : Γ1` and `Γ1 ⊢ θ1 : Γ` then `Γ2 ⊢ θ2 θ1 : Γ`

  **Proof** We have to show `∀α ∈ Dom(Γ).Γ2 ⊢ θ2θ1α : Γα`.
  Consider the derivation of `Γ1 ⊢ θ1 α : Γα` (by premise).
  It is easy to construct a derivation of `Γ2 ⊢ θ2 θ1 α : Γα`, since `∀β ∈ FV(θ1 α).Γ2 ⊢ θ2 β : Γ1 β` and `Γ1 ⊢ β : Γβ` (as `θ1 β = β` follows from the idempotence of `θ1`). □

  The fact that `W` specializes contexts is shown in the next result.

  Lemma 5.6

  If `W(V,Γ,E,e) = (V′,Γ′,θ′,τ)` then `Γ′ ⊢ θ′ : Γ`.

  Proof by induction on the structure of `e`:

  `x` : `Γ[βn : Sn] ⊢ {} : Γ` trivial.

  `λx.e` : Since the induction hypothesis holds for any `E`, including `E[x:α],Γ′ ⊢ θ′ : Γ` follows directly.

  `(e1 e2)`: By induction hypothesis we get `Γ1 ⊢ θ1 : Γ` and `Γ2 ⊢ θ2 : Γ1` and by transitivity (Lemma 5.5) and by correctness of the unification algorithm we get `Γ′ ⊢ θ′θ2θ1 : Γ`.

  `let x = e1 in e2` : By induction hypothesis we get

    Γ1 ⊢ θ1 : Γ           (2)
    Γ2 ⊢ θ2 : Γ1 \ {αn}   (3)

  Now we show

    Γ1 \ {αn} ⊢ θ1 : Γ    (4)

  That is, we have to show `Γ1 \ {(αn)~} ⊢ θ1β : Γβ` for all `β ∈ Dom(Γ)`.
  First we prove `FV(θ1 (Dom(Γ))) ∩ {(αn)~} = {}`.
  From Lemma 5.4, item 3, it follows that `FV(θ1τ1) ∩ Dom(Γ) ⊆ FV(E)`.
  Idempotence of `θ1` yields `FV(θ1τ1) ∩ FV(θ1(Dom(Γ))) ⊆ FV(θ1E)`.
  Simple set theory yields `FV(θ1(Dom(Γ))) ∩ (FV(θ1τ1) − FV(θ1E)) = {}` as claimed above.
  Now (4) follows.
  Combining (4) and (3) by transitivity (Lemma 5.5) yields `Γ2 ⊢ θ2θ1 : Γ` □

  The next lemma states that `▷` is preserved under instantiation assuming a context that obeys the constraints.

  Lemma 5.7

  If `Σ,Γ,E ▷ e : τ` and `Γ′ ⊢ θ′ : Γ`, then `Σ,Γ′,θ′E ▷ e : θ′ τ`.

<!-- page 16 -->

  Proof simple by adding proofs of the form `Γ′ ⊢ θ′ α : Γα` in the proof tree of `Σ,Γ,E ▷ e : τ` to obtain a proof of `Σ,Γ′,θ′E ▷ e : θ′ τ`. □

  At last we are able to prove the correctness theorem:

  Proof of Theorem 5.3 by induction on the structure of `e`.
  We have the following cases:

  `x` : Correctness follows easily from

         Σ, Γ[(βn : Sn)~] ⊢ E(x) ⪰ {(αn |→ βn)~}τ
    ASM′ ーーーーーーーーーーーーーーーーーーーーーーーーーー
         Σ, Γ[(βn : Sn)~],E ▷ x : {(αn |→ βn)~}τ

  `λx.e`: By induction hypothesis we get `Σ,Γ′,(θ′E)[x:θ′α] ▷ e : θτ`.
  Then ABS applies:

        Σ,Γ,(θ′E)[x:θ′α] ▷ e : θ′ τ′
    ABS -----------------------------
        Σ,Γ,θ′E ▷ λx.e : θ′α → θτ′

  `(e1 e2)`: We get

    Σ,Γ1,θ1E ▷ e1 : θ1 τ1
    Σ,Γ2,θ2θ1E ▷ e2 : θ2 τ2

  from the induction hypotheses for `e1` and `e2`.
  The correctness of the unification algorithm yields `Γ′ ⊢ θ′ : Γ2` and then with `Γ2 ⊢ θ2 : Γ1` (from Lemma 5.6) and Lemma 5.5 we obtain `Γ′ ⊢ θ′θ2 : Γ1`.

  From Lemma 5.7 we now get the two premises for the APP rule, since `θ′θ2θ1τ1 = θ′θ2τ2 → θ′ α`.
  Furthermore, `θ2θ1α = α`, since `α` is a new variable (i.e. `α ∈/ V2` and `Dom(θ2) ∪ Dom(θ1) ⊆ V2` by Lemma 5.4).

        Σ,Γ′,θ′θ2θ1E ▷ e1 : θ′θ2θ1τ1
        Σ,Γ′,θ′θ2θ1E ▷ e2 : θ′θ2τ2
    APP ------------------------------
        Σ,Γ′,θ′θ2θ1E ▷ (e1 e2) : θ′α

  `let x = e1 in e2` : Using `Γ′1 = Γ1 \ {(αn)~}`, `(Sn = Γ1 αn)~`, and `E′ = E[x : ∀(αn :Sn)~ .θ1τ1]` the induction hypotheses are

    Σ,Γ′1[(αn : Sn)~],θ1E ▷ e1 : θ1τ1 (5)
    Σ,Γ2,θ2θ1E′ ▷ e2 : θ2τ2 (6)

  Notice that `FV(θ1E′) ∩ {(αn)~} = {}`.
  To apply LET′, we show

    Σ, Γ2[(αn : Sn)~], θ2θ1E ▷ e1 : θ2θ1τ1  (7)

  As we get `Γ2 ⊢ θ2 : Γ′1` from Lemma 5.6 and `FV(θ2) ∩ {(αn)~} = {}` from Lemma 5.4 (recall that `{(αn)~} ⊆ V1` and `{(αn)~} ∩ FV(θ1 E′) = {}`), we obtain

    Γ2[(αn : Sn)~] ⊢ θ2 : Γ′1[(αn : Sn)~].

  Then (7) follows from Lemma 5.7 and (5).

  As `Dom(θ1) ∩ FV(τ2) = {}` is a consequence of Lemma 5.4 (as above, `Dom(θ1) ⊆ V1` and `Dom(θ1) ∩ FV(θ1 E′) = {}` as `θ1` is idempotent), we obtain

    Σ,Γ2,θ2θ1E′ ▷ e2 : θ2θ1τ2  (8)

<!-- page 17 -->

  Now LET′ applies to

    (7)   (8)   {(αn)~} = FV(θ1τ1) − FV(θ1E)
    --------------------------------------------------------
    Γ2,Σ,θ2θ1E ▷ let x = e1 in e2 : θ2θ1τ2

  □

  The following lemma is crucial for establishing the principal type theorem.

  Lemma 5.8 (Completeness of W)

  If `Σ,Γ*,θ* E ▷ e : τ*`, `Dom(Γ) ∪ FV(E) ⊆ V`, and `Γ* ⊢ θ* : Γ` then there exists a substitution `δ` such that

    W(V,Γ,E,e)    = (V1,Γ1,θ1,τ1),
    θ* E          = δθ1E,
    τ*            = δθ1τ1,
    Γ*            ⊢ δ : Γ1.

  Proof by induction on the structure of `e`.
  We assume w.l.o.g. a derivation for `Σ,Γ*,θ* E ▷ e : τ*` that has no variable overlap with the new variables `V1 − V` used by algorithm W.

  `x`: We have

          Σ,Γ* ⊢ θ* E(x) ⪰ τ*
    ASM′ ---------------------
          Σ,Γ*,θ* E ▷ x : τ*

  Observe that we can write `θE(x) = θ* ∀(αn :Sn)~ .τ` as `∀(αn :Sn)~ .θ̂* τ`, where `θ̂* = θ*| (Dom(θ*) − {(αn)~})`, possibly by renaming some `(αn)~`.
  Assuming `Σ, Γ* ⊢ θ* E(x) ⪰ τ*`, let `θ` be the corresponding substitution as in Definition 3.1 with `Dom(θ) ⊆ {(αn)~}`, and `τ* = θθ̂* τ`. Let `δ = θ* ∪ {(βn |→ θαn)~}`.
  As `(βn)~` are new variables, `θ* E = δE` holds.
  Next, `τ* = δ({(αn |→ βn)~}τ) = θθ̂* τ` follows easily.
  Finally, `Γ* ⊢ δ : Γ[(βn : Sn)~]` follows from `Γ* ⊢ θ* : Γ` (by premise) and from `Σ,Γ* ⊢ θ′ αi : Si`, `[i = 1, ..., n]` (see Definition 3.1).

  `λx.e`: The derivation ends with

        Σ,Γ*,(θ* E)[x:τ1*] ▷ e : τ2*
    ABS -----------------------------------------
        Σ,Γ*,θ* E ▷ λx.e : τ1* → τ2*

  As the algorithm is invariant under α-conversion (Lemma 5.2), we can safely assume that `x ∈/ Dom(E)`.
  To apply the induction hypotheses, we define `θ0 = θ* ∪ {α |→ τ1*}`.
  Then `Σ, Γ*, θ0 (E[x:α]) ▷ e : τ2*` and `Γ* ⊢ θ0 : Γ` are easy to verify.
  By induction hypothesis there exists `δ1` such that

    W(V ∪ {α}, Γ, E[x:α], e) =  (V′,Γ′,θ′,τ′),              (9)
    (θ0 E)[x:τ1*]             = δ1 θ′ E[x:α],               (10)
    τ2*                       = δ1 θ′ τ′,                   (11)
    Γ*                        ⊢ δ1 : Γ′                     (12)

  and hence `W(V,Γ,E,λx.e) = (V′,Γ′,θ′,α → τ′)`.
  Now `θ0 E = δ1 θ′ E` follows from (10).
  Furthermore, from (10) we obtain `τ1* = δ1 θ′ α` and hence `τ1* → τ2* = δ1 θ′ (α → τ′)` from (11).

<!-- page 18 -->

  `(e1 e2)` : We assume

        Σ,Γ*,θ* E ▷ e1 : τ2* → τ1*    Σ,Γ*,θ* E ▷ e2 : τ2*
    APP -----------------------------------------------------
        Σ,Γ*,θ* E ▷ (e1 e2) : τ1*

  Applying the induction hypothesis to `e1` yields `δ1` such that

       W(V,Γ,E,e) =  (V1,Γ1,θ1,τ1), (13)
             θ* E =  δ1θ1 E,        (14)
        τ2* → τ1* =  δ1θ1 τ1,       (15)
               Γ* ⊢  δ1 : Γ1.       (16)

  The induction hypothesis with `e2` with `Γ1`, where `Γ* ⊢ δ1 : Γ1`, and `θ1 E` yields `δ2` such that

  W(V1,Γ1,θ1E,e2) = (V2,Γ2,θ2,τ2), (17)  
             θ* E = δ2θ2θ1E,       (18)
              τ2* = δ2θ2τ2,        (19)
               Γ* ⊢ δ2 : Γ2.       (20)
  
  Let `δ*` be defined as
  
            | δ1 β    if β ∈ FV(θ1τ1) − Cod(θ2)
    δ * β = | τ1*     if β = α
            | δ2 β    otherwise


  We show that `δ*` is a unifier of `θ2θ1τ1 = θ2τ2 → α`.
  Notice that `FV(θ2,τ2) ∩ FV(θ1τ1) ⊆ FV(θ1 E)` follows from Lemma 5.4 and that the two substitutions `δ2θ2` and `δ1` coincide on `FV(θ1 E)`: combining (14) with (18) yields

    δ2 θ2 = FV(θ1 E) δ1  (21)

  This overlap simplifies the following proofs by case analysis, in which the case `β = α` is immaterial.
  First, to show `δ* θ2 τ2 = δ2 θ2 τ2 = τ2*`, assume `β ∈ FV(τ2)`.
  Then in case `β ∈/ FV(θ2)`, `δ* τ2 = δ2 τ2` follows from (21) if `β ∈ FV(θ1 τ1)` as `FV(θ2,τ2) ∩ FV(θ1τ1) ⊆ FV(θ1 E)`, and is trivial otherwise.
  If `β ∈ FV(θ2)`, then `δ*θ2τ2 = δ2θ2τ2` is trivial.

  To show `δ*θ2θ1τ1 = δ1θ1τ1`, assume `β ∈ FV(θ1τ1)`.
  If `β ∈ Dom(θ2)`, (21) gives the desired result as `Dom(θ2) ∩ FV(θ1τ1) ⊆ FV(θ1E)`.
  In case `β ∈/ Dom(θ2)`, `δ*θ1τ1 = δ1τ1` follows easily.
  Hence `δ*` is the desired unifier:

    δ* θ2θ1τ1 = τ2* → τ1* = δ* (θ2τ2 → α).

  We obtain `Γ* ⊢ δ* : Γ2` from (16) and (20) by case analysis: we have to show `Σ,Γ* ⊢ δ* β : Γ2 β` for all `β ∈ Dom(Γ2)`.
  First recall that `Dom(Γ2) ∩ Dom(θ2) = {}` by Lemma 5.4.
  If `β ∈ FV(θ1τ1) − Cod(θ2)`, then we have another case distinction: if `β ∈ Dom(θ2)`, then `Γ2 β = {}`, otherwise `θ2 β = β` and from `Γ2 ⊢ θ2 : Γ1` (Lemma 5.6), we have `Γ1 β ⊆ Γ2 β` and the claim follows from (16).
  The case `β = α` is trivial because `Γ2 α = {}`.
  The remaining case, `Σ,Γ* ⊢ δ2θ2 β : Γ2 β`, follows easily from (20) since `Dom(Γ2) ∩ Dom(θ2) = {}`.

<!-- page 19 -->

  Then by completeness of unification, `θ′` is a most general unifier computed in unify, and there exists `δ′` such that `δ* = δ′θ′`.
  Hence we get

    W(V,Γ,E,(e1 e2)) =  (V2 ∪ α,Γ′,θ′θ2θ1,α),
               θ* E = δ′θ′θ2θ1E,
                 τ* = δ′θ′θ2τ2,
                 Γ* ⊢ δ′ : Γ′,

  where the last statement follows from the completeness of unification.
  `let x = e1 in e2`: We assume `Γ* ⊢ θ* : Γ` and, by LET′,

    Σ,Γ*[(αk* : Sk*)~],θ* E ▷ e1 : τ1*
    Σ,Γ*,(θ* E)[x:∀(αk* : Sk*)~ .τ1*] ▷ e2 : τ2*
    ----------------------------------------------
    Σ,Γ*,θ* E ▷ let x = e1 in e2 : τ2*

  where `{(αk*)~} = FV(τ1*) − FV(θ* E)`.
  As the algorithm is invariant under α-conversion (Lemma 5.2), we can safely assume that `x ∈/ Dom(E)`.

  As `{(αk*)~} ∩ FV(θ* E) = {}` we can w.l.o.g. rename `(αk*)~` in the premises of the above rule (not in `Γ′`) in order to assume that `{(αk*)~} ∩ Dom(Γ*) = {}`.
  Formally, this can be done by Lemma 5.5.
  Then we can apply the induction hypothesis to `e1` with `Γ* [(αk* : Sk*)~] ⊢ θ* : Γ` and obtain `δ1` such that

         W(V,Γ,E,e1) = (V1,Γ1,θ1,τ1),            (22)
                θ* E = δ1θ1E,                    (23)
                 τ1* = δ1θ1τ1,                   (24)
    Γ*[(αk* : Sk*)~] ⊢  δ1 : Γ′1 [(αn : Sn)~],   (25)

  where `Γ′1 = Γ1 \ {(αn)~}` and `(Sn)~ = (Γ1 αn)~`.

  From `{(αn)~} = FV(θ1τ1) − FV(θ1 E)` we infer `FV({(δ1αn)~}) = FV(δ1θ1τ1) − FV(δ1θ1E) = {(αk*)~}`.
  Hence `Γ* ⊢ δ1 : Γ′1` follows from (25).
  Notice that `δ1 ∀(αn :Sn)~ .θ1τ1 = ∀(αn :Sn)~ .δ̂1θ1τ1`, where `δ̂1 = δ1|(Dom(δ1)−{(αn)~})`, follows from the assumption that new variables used by W do not occur in the chosen derivation, i.e. in `Cod(δ1)`.
  Then we obtain `Σ,Γ* ⊢ δ1 ∀(αn :Sn)~ .θ1 τ1 ⪰ ∀(αk* :Sk*)~ .τ*` by using `δ1 | {(αn)~}` as the substitution in Definition 3.1 and because `Σ,Γ* [(αk* :Sk*)~] ⊢ δ1αi : Si`, `[i = 1 ... n]` follows from (25).

  Now the problem is that the induction hypothesis cannot be applied directly to `e2` with `Γ* ⊢ δ1 : Γ′1` and `θ1 E[...]`, since in general `(θ* E)[x:∀(αk* :Sk*)~ .τ1* ] /= (δ1 θ1 E)[x:δ1 ∀(αn :Sn)~ .θ1 τ1]`.
  Thus we have to find a different basis in order to apply the induction hypothesis for `e2`.

  From

    Σ,Γ* (θ* E)[x:∀(αk* :Sk*)~ .τ1*] ▷ e2 : τ2*  (26)

  we can infer

    Σ,Γ*,(θ* E)[x:δ1 ∀(αn :Sn)~ .θ1 τ1] ▷ e2 : τ2*,

  since at each application of ASM′ to `x` in the proof of (26), we can use the more general `[x:δ1 ∀(αn :Sn)~ .θ1 τ1]` instead of `[x:∀(αk* :Sk*)~ .τ1*]`.

  Then the induction hypothesis applies to e 2 with `Γ* ⊢ δ1 : Γ′1` and the environment `θ1 E[x:∀αn :Sn .θ1 τ1]`.
  
<!-- page 20 -->

  We get `δ2` such that

                  W(V1,Γ′1,E,e2) = (V2,Γ2,θ2,τ2),  (27)
    (θ* E)[x:δ1 ∀αn : Sn .θ1τ1] = δ2θ2 (θ1 E[x:∀αn : Sn.θ1τ1]), (28)
                             τ2* = δ2θ2τ2, (29)
                              Γ* ⊢ δ2 : Γ2. (30)
  
  Hence `W(V,Γ,E,let x = e1 in e2) = (V2,Γ2,θ2,τ2)`.
  We obtain `τ2* = δ2θ2θ1τ2` from `τ2* = δ2θ2τ2` and `Dom(θ1) ∩ FV(τ2) = {}` (as in Theorem 5.3).
  It only remains to show `θ* E = δ2θ2θ1E`, which is a consequence of (28), as `x ∈/Dom(E)`. □

  Now we can finally show the desired principal type theorem.

  Theorem 5.9

  If `e` has type `σ0` under a closed environment `E`, i.e. `Σ,[],E ⊢ e : σ0` and `FV(E) ⊆ V0`, then `W(V0,[],{},E,e) = (V,Γ,θ,τ)` and `∀(αn : Γαn)~ .θτ` is a principal type of `e` w.r.t. `Σ` and `E`, where `{(αn)~} = FV(θτ)`.

  **Proof** Assume some typing `Σ,[],E ⊢ e : ∀(αm':Sm')~ .τ'`.
  We infer `Σ,[αm′ :Sm′], E ⊢ e` :
  `τ'` by `∀E` and then obtain a syntax-directed derivation `Σ,[αm' :Sm'],E ▷ e : τ′` by Theorem 3.4.
  Then Lemma 5.8 applies with `Γ* = [αm' : Sm']` and `θ* = {}`.
  We thus get `δ` such that

    E   = δE,
    τ′  = δθτ,
    Γ*  ⊢  δ : Γ.

  Then `∀(αn : Γαn)~ .θτ` is a principal type of `e` w.r.t. `E`, since `∀(αn :Γαn)~ .θτ ⪰ ∀(αm' :Sm')~ τ'` follows from `τ' = δθτ`, `{(αn)~} = FV(θτ)`, and `Γ* ⊢ δ : Γ`. □

## 6 Algorithm I

  As in the original work by Milner (Mil78), we now present a more efficient refinment of algorithm W.
  Compared to W, algorithm I 2 takes an extra argument, the substitution computed so far.
  This substitution is extended incrementally instead of computing new subtitutions and composing them later.

  The equivalence of W and I is an easy matter.
  A renaming is an injective substitution that maps variables to variables only.

  Theorem 6.1 (Equivalence of W and I)

  Assume `θ0` is an idempotent substitution such that `θ0E0 = E`.
  If `W(V,Γ,E,e) = (V′,Γ′,θ′,τ′)` then `I(V,Γ,θ0,E0,e) = (V′′,Γ′′,θ′′,τ′′)` and there exists a renaming `σ` such that `V′′ = σV′`, `∀α.Γ′′α = Γ′(σα)`, `θ′′τ′′ = σθ′τ′` and `θ′′E = σθ′E`.

  Proof by simple induction on the structure of `e`. □

  ----

  2 Although the typography in (Mil78) is ambiguous, Milner has confirmed by email that he intended it to be I, not J : it is an imperative implementation of W.
  Milner’s I is imperative because he maintains a single global copy of `θ` which is updated by side-effects.
  In a purely functional style this requires an additional argument and result.

  ----

<!-- page 21 -->

    I(V,Γ,θ,E,e) = case e of
      x    ⇒ let ∀(αn : Sn)~ .τ  =  E(x)
                              βi  ∈/  V [i = 1 ... n]
              in (V ∪ {(βn)~},Γ[(βn : Sn)~],θ,{(αn |→ βn)~}τ)
      λx.e ⇒ let        α ∈/ V
              (V′,Γ′,θ′,τ) = I(V ∪ {α},Γ,θ,E[x:α],e)
              in (V′,Γ′,θ′,α → τ)
      (e1 e2) ⇒ let (V1,Γ1,θ1,τ1) = I(V,Γ,θ,E,e1)
                     (V2,Γ2,θ2,τ2) = I(V1,Γ1,θ1,E,e2)
                                 α ∈/ V2
                           (Γ′,θ′) = unify(Γ2,θ2τ1 = θ2τ2 → α)
                in (V2 ∪ {α},Γ′,θ′θ2,α)
      let x = e1 in e2 ⇒ let (V1,Γ1,θ1,τ1) = I(Γ,θ,E,e1)
                                    {(αn)~} = FV(θ1τ1) − FV(θ1E)
                          in I(V1,Γ1 \ {(αn)~},θ1,E[x : ∀(αn :Γ1 αn)~ .θ1τ1],e2)

  Fig. 7. Algorithm I

## 7 Related Work

  The structure of algorithms W and I is very close to that of Milner’s algorithms of the same name (Mil78).
  Apart from the fact that our version of I is purely applicative (hence we carry the substitution and the set of used variables around explicitly), the main difference is that we also have to maintain a set of constraints `Γ`.
  In fact, this is the only real difference to Milner’s algorithms.

  Probably the first combination of ML-style polymorphism and parametric overloading (as opposed to finite overloading as in Hope (BMS80)) was presented by Kaes (Kae88).
  His language is in fact very close to our Mini-Haskell, except that he does not introduce classes explicitly.
  More importantly, he does not use contexts to record information about type variables but tags type variables directly.

  The original version of type classes as presented by Wadler and Blott (WB89) was significantly more powerful than what went into Haskell, the reason being that the original system was undecidable, as shown later by Volpano and Smith (VS91).
  The relationship to Haskell proper is discussed in Section 2.

  Nipkow and Snelting (NS91) realized that type inference for type classes can be formulated as an extension of ordinary ML-style type inference with order-sorted unification, i.e. simply by changing the algebra of types and the corresponding unification algorithm.
  Although this was an interesting theoretical insight, it only lead to a simple algorithm for a restricted version of Haskell where each type variable is constrained by exactly one class.
  In addition it was not possible to identify ambiguous typings like `Σ,[α:C],E ⊢ e : int` because there was no notion of contexts and type variables were tagged with their sort.
  Both problems have been eliminated in the present paper.

  An interesting extension of Haskell using the notion of “qualified types” was designed and implemented by Mark Jones (Jon92b).
  The main difference is that he allows arbitrary predicates `P(τ1,...,τn)` over types as opposed to our membership constraints `α : S`.

<!-- page 22 -->

  On the other hand he does not solve constraints of the form `τ : S` to obtain atomic constraints of the form `α : S′` as is done in our function constrain.
  Instead he accumulates the unsolved constraints.

  Independently of our own work Chen, Hudak and Odersky (CHO92) developed an extension of type classes using similar techniques and arriving at a similar type reconstruction algorithm.
  Since their type system is more general, they use different and more involved formalisms, in particular for unification.
  In contrast, we reduce unification to its essence by splitting it into standard unification plus constraint solving.
  This enables us to give a sufficient and necessary criterion for unitary unification, which is required for principal types.
  As discussed in Section 4, the restrictions in Haskell guarantee unitary unification.

  Kaes (Kae92) presents an extension of Hindley/Milner polymorphism with overloading, subtypes and recursive types.
  Due to the overall complexity of the resulting system, the simplicity of the pure system for overloading is lost.

  The pragmatics of implementing type classes are discussed by Peterson and Jones (PJ83).
  In particular they give hints on how to implement a truly imperative version of algorithm I using mutable variables.
  This is of significant importance because a naive functional implementation of algorithm I, in particular one representing substitutions as association lists, performs quite poorly.

## 8 Ambiguity

  We would like to conclude this paper with a discussion of the ambiguity problem which affects most type systems with overloading.
  It is caused by the fact that although a program may have a unique type, its semantics is not well-defined.
  According to our rules, the program

    class α : C where f : α → int;
    class α : D where c : α;
    (f c)

  has type `int` in any context containing an assumption `α : {C, D}`.
  Yet the program has no semantics because there are no instances of `f` and `c` at all.
  If there were multiple instances of both `C` and `D`, it would be impossible to determine which one to use in the expression `(f c)`.

  Motivated by such examples, a typing `Σ,Γ,E ⊢ e : σ` is usually defined to be ambiguous if there is a type variable in `Γ` which does not occur free in `σ` or `E`.

  Ideally one would like to have that every well-typed expression has a well-defined semantics.
  However, ambiguous terms may have more than one semantics, as the above example suggests.
  Fortunately, Blott (Blo92) and Jones (Jon92a) have shown that in type systems closely related to the one studied in this paper, the semantics of unambiguous terms is indeed well-defined.

  As we have not provided a semantics for our language, we have not introduced ambiguity formally.
  Nevertheless there is one place in our inference system where we anticipate a particular treatment of ambiguity.
  In rule `∀I`, the proviso `α ∈ FV(σ)` is intended to propagate ambiguity problems: with this restriction, the expression `let x = (f c) in 5` (preceded by classes `C` and `D` as declared above) has type int only in a context containing an assumption `α : {C, D}`.


<!-- page 23 -->

  If the proviso is dropped, the expression also has type `int` in the empty context, thus disguising the local ambiguity.
  The reason is that `x` can be given the ambiguous type `∀α:{C, D}.int`, but since `x` does not occur in 5, this does not matter.
  Although in a lazy language `x` need not be evaluated and hence the semantics of the whole `let` is indeed unambiguous, we would argue that for pragmatic reasons it is advisable to flag ambiguities whenever they arise.

  From this discussion it is obvious that a semantics and a coherence proof for the type system w.r.t. a semantics are urgently needed.

  **Acknowledgements.** The authors wish to thank the anonymous referees for their critical reading and their helpful comments.

## References

  Stephen Blott. An approach to overloading with polymorphism. PhD thesis, Dept. of Computing Science, University of Glasgow, 1992.

  Rod Burstall, Dave MacQueen, and Don Sannella. Hope: an experimental applicative language. In Proc. 1980 LISP Conference, pages 136–143, 1980.

  Dominique Clément, Joëlle Despeyroux, Thierry Despeyroux, and Gilles Kahn. A simple applicative language: Mini-ML. In Proc. ACM Conf. Lisp and Functional Programming, pages 13–27, 1986.

  Kung Chen, Paul Hudak, and Martin Odersky. Parametric type classes. In Proc. ACM Conf. on LISP and Functional Programming, pages 170–181. ACM Press, June 1992.

  Luis Damas and Robin Milner. Principal type schemes for functional programs. In Proc. 9th ACM Symp. Principles of Programming Languages, pages 207–212, 1982.

  Paul Hudak, Simon Peyton Jones, and Philip Wadler. Report on the programming language Haskell: A non-strict, purely functional language. ACM SIGPLAN Notices, 27(5), May 1992. Version 1.2.

  Mark P. Jones. Qualified types: Theory and practice. D.Phil. Thesis, Programming Research Group, Oxford University Computing Laboratory, July 1992.

  Mark P. Jones. A theory of qualified types. In Bernd Krieg-Brückner, editor, Proc. European Symposium on Programming, pages 287–306. LNCS 582, 1992.

  Stefan Kaes. Parametric overloading in polymorphic programming languages. In Proc. 2nd European Symposium on Programming, pages 131–144. LNCS 300, 1988.

  Stefan Kaes. Type inference in the presence of overloading, subtyping and recursive types. In Proc. ACM Conf. LISP and Functional Programming, pages 193–204. ACM Press, June 1992.

  Robin Milner. A theory of type polymorphism in programming. J. Comp. Sys. Sci., 17:348–375, 1978.

  Robin Milner, Mads Tofte, and Robert Harper. The Definition of Standard ML. MIT Press, 1990.

  Tobias Nipkow and Christian Prehofer. Type checking type classes. In Proc. 20th ACM Symp. Principles of Programming Languages, pages 409–418. ACM Press, 1993.

  Tobias Nipkow and Gregor Snelting. Type classes and overloading resolution via order-sorted unification. In Proc. 5th ACM Conf. Functional Programming Languages and Computer Architecture, pages 1–14. LNCS 523, 1991.

  John Peterson and Mark Jones. Implementing type classes. In Proc. SIGPLAN ’93 Symp. Programming Language Design and Implementation, pages 227–236. ACM Press, 1983.

<!-- page 24 -->

  Dennis M. Volpano and Geoffrey S. Smith. On the complexity of ML typability with overloading. In Proc. 5th ACM Conf. Functional Programming Languages and Computer Architecture, pages 15–28. LNCS 523, 1991.

  Philip Wadler and Stephen Blott. How to make ad-hoc polymorphism less ad hoc. In Proc. 16th ACM Symp. Principles of Programming Languages, pages 60–76, 1989.
