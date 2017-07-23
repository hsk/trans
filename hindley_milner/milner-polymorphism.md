# A Theory of Type Polymorphism in Programming

    ROBIN MILNER

    Computer Science Department, University of Edinburgh, Edinburgh, Scotland

    Received October 10, 1977; revised April 19, 1978
    <!-- Edinburgh エジンバラ(イギリス北部、スコットランドの東の都市、スコットランドには西にグラスゴーがある。スコットランドはグラスゴーとエジンバラの２つの大きな都市がある) -->

  ----

  0022-0000/78/0173-0348$02.00/0
  Copyright 8 1978 by Academic Press, Inc.
  All rights of reproduction in any form reserved.

  ----

  The aim of this work is largely a practical one.
  A widely employed style of programming, particularly in structure-processing languages which impose no discipline of types, entails defining procedures which work well on objects of a wide variety.
  We present a formal type discipline for such polymorphic procedures in the context of a simple programming language, and a compile time type-checking algorithm W which enforces the discipline.
  A Semantic Soundness Theorem (based on a formal semantics for the language) states that well-type programs cannot “go wrong” and a Syntactic Soundness Theorem states that if W accepts a program then it is well typed.
  We also discuss extending these results to richer languages; a type-checking algorithm based on W is in fact already implemented and working, for the metalanguage ML in the Edinburgh LCF system.
  <!-- aim 目標 largely 主に practical 実用 widely 広く employed 採用された particularly 特に impose 課す descipline 規律 entails 必然的に variety バラエティー enforce 強制する -->

## 1. INTRODUCTION

  The aim of this work is largely a practical one.
  A widely employed style of programming, particularly in structure-processing languages which impose no discipline of types (LISP is a perfect example), entails defining procedures which work well on objects of a wide variety (e.g., on lists of atoms, integers, or lists).
  Such flexibility is almost essential in this style of programming; unfortunately one often pays a price for it in the time taken to find rather inscrutable bugs-anyone who mistakenly applies CDR to an atom in LISP, and finds himself absurdly adding a property list to an integer, will know the symptoms.
  On the other hand a type discipline such as that of ALGOL 68 [22] which precludes the flexibility mentioned above, also precludes the programming style which we are talking about.
  ALGOL 60 was more flexible-in that it required procedure parameters to be specified only as “procedure” (rather than say “integer to realprocedure”)-but the flexibility was not uniform, and not sufficient.
  <!-- unfortunately 残念ながら rather むしろ inscrutable 不可解な absurdly 不当に
  symptom 症状, 徴候, 症候, 兆候, 病徴, 下地 preclude 排除する mention 言及 above 上の also さらに、また sufficient 十分 -->

  An early discussion of such flexibility can be found in Strachey [19], who was probably the first to call it polymorphism.
  In fact he qualified it as “parametric” polymorphism, in contrast to what he called “adhoc” polymorphism.
  An example of the latter is the use of “-|” to denote both integer and real addition (in fact it may be further extended to denote complex addition, vector addition, etc.); this use of an identifier at several distinct types is often now called “overloading,” and we are not doncerned with it in this paper.
  <!-- early 早い probably おそらく denote 意味する -->

  <!-- 349 2/28 -->

  In this paper then, we present and justify one method of gaining type flexibility, but also retaining a discipline which ensures robust programs.
  We have evidence that this work is not just a theoretical exercise; the polymorphic type discipline which we discuss here has been incorporated in the LCF metalanguage ML [2, 3], and has been in use for nearly 2 years.
  The compile-time type checker for this language has proved to be a valuable filter which traps a significant proportion of programming errors.
  <!-- justify 正当化する gaining 獲得する but also だけでなく retaining 保持する ensures 確実に robust programs 堅牢なプログラム
  theoretical exercise 理論的な運動 incorporated 組み込まれた significant proportion かなりの割合 -->

  The main body of the present paper is concerned with a technical account-both semantic and syntactic-of our discipline of types in the context of a simple illustrative language, but at this point it is helpful to characterize the approach informally.
  We outline its predominant features.
  <!-- concerned 心配している illustrative 説明的な helpful 役に立つ informally 非公式に predominant 支配的 concerning について  -->

  First, everything concerning types is done at compile time; once the type checker (part of the compiler) has accepted a program or program phrase, code may be generated which assumes that no objects carry their types at run-time.
  This is widely accepted as yielding efficient object code, though it does impose constraints on the use of types compared with, for example, the approach in EL1 [21].
  <!--assume 想定する yield 産出、生成 -->

  Second, many nontrivial programs can avoid mentioning types entirely, since they be inferred from context.
  (In ML however, as in other languages, the user may-indeed often should-define his own types together with operations over these types.
  Recent languages which allow the user to define his own types in this manner are CLU [8], ALPHARD [23] and Euclid [6]).
  Although it can be argued convincingly that to demand type specification for declared variables, including the formal parameters of procedures, leads to more intelligible problems, it is also convenient-particularly in on-line programming-to be able to leave out these specifications.
  In any case, the type checker which we present is quite simple and could not be made much simpler even if the types of variables were always specified in declarations.
  <!-- entirely 完全に may-indeed 本当にかもしれない Although しかし argued 主張した
  convincingly 説得力のある demand 要求 intelligible 分かりやすい convenient 便利 quite かなり-->

  Third, polymorphism plays a leading role.
  For example, a procedure is assigned a polymorphic type (which we abbreviate to polytype) in general; only when the types of its arguments and result can be uniquely determined from the context is it monomorphic (i.e., assigned a monotype).
  Gries and Gehani [4], among others, have made a convincing case for controlled polymorphic programming (in contrast with the typeless programming in LISP or in SNOBOL); for them however, and also for Tennent [20], the presence of type variables or identifiers is needed to specify polymorphic types.
  For us, the polymorphism present in a program is a natural outgrowth of the primitive polymorphic operators which appear to exist in every programming language; such operators are assignment, function application, pairing and tupling, and list-processing operators.
  It is principally the type constraints on these operators, and in the declaration and use of variables, which determine for us the types of a program phrase and its subphrases.

  We do not discuss in this paper-except briefly at the end-either coercions or the “overloading” of identifiers.
  Our view is that these concepts, and also run-time type manipulation, are somewhat orthogonal to a compile-time polymorphic type discipline, and may (to some extent) be incorporated without invalidating it.

  In Section 2 we illustrate our type discipline by examples in a fragment of ML.
  This fragment should be self-explanatory, but an outline of ML is given in [3] and a full description appears in [2].
  These illustrations should serve to make the point that we are able to handle useful languages.
  The remainder of the paper justifies the discipline using a very simple applicative language, Exp.
  The justification factors into two parts.

  <!-- 350 3/28 -->

  In Section 3 we define the notion of well typing (correct type assignment) and prove the Semantic Soundness Theorem, which says that a well-typed program is semantically free of type violation.
  If we were to give an operational definition of the language, this would imply that, for example, an integer is never added to a truth value or applied to an argument, and consequently need not carry its type around for run-time checking.
  In Section 4 we present a well-type algorithm W and prove the Syntactic Soundness Theorem, which states that W, if it succeeds, produces a well typing of a program.
  We also give a more efficient algorithm I, which simulates W.

  The types in Exp are just the hierarchy of purely functional types over a set of basic types.
  That is, the polymorphism in Exp is the natural outgrowth of a single primitive polymorphic operator, function application, together with variable binding.
  To add other primitive polymorphic operators, such as pairing and list-processing operators (as in ML), together with types built from basic ones `☓` (Cartesian Product), list (list-forming), and `+` (disjoint sum) in addition to (function type), presents no extra difficulty in the two soundness theorems.
  Indeed, adding an assignment operator is also easy as far as the Syntactic Soundness Theorem is concerned, but the Semantic Soundness Theorem is harder to extend in this case, due to the extra semantic complication of a memory or store which holds the current values of assignable variables.
  We discuss this further in Section 5.

  Our work is a step towards solving the problem expressed by Morris [10] in his thesis as follows:
  “to design a language and a type system in which a programmer may define functions whose parameters may have different types for different calls of the function.”
  We recommend Chapter 4 of this thesis as a lucid introduction to the problem.
  Although Morris does not discuss the semantics of types formally, or give a polymorphic type system, he describes how a valid type assignment may be found for a term of the λ-calculus by solving a set of simultaneous linear equations; we take this idea further in the next section.

  After doing this work we became aware of Hindley’s [5] method for deriving the “principal type scheme” (which is what we call a polymorphic type) for a term in combinatory logic.
  Hindley appears to have been the first to notice that the Unification Algorithm of Robinson [14] is appropriate to this problem.
  Our work can be regarded as an extension of Hindley’s method to programming languages with local declarations, and as a semantic justification of the method.

  In summary, we present a polymorphic type discipline which is syntactically well understood and justified for a currently used programming language with imperative features, and is also semantically explained for a nontrivial, though nonimperative, sublanguage.

## 2. ILLUSTRATIONS OF THE TYPE DISCIPLINE

  We illustrate our notion of polymorphism by means of some simple examples.
  They are written in a fragment of ML which we hope is self-explanatory; this fragment is indeed no more than Landins ISWIM [7], and we refer the reader to Burge’s book [1] in  which he uses this style of programming almost exactly.

  <!-- 351 4/28 -->

  We use no imperative constructs here (assignments or jumps).
  The constructs

    let x = e in e’,
    let f (x1, ..., xn) = e in e’

  are used to give `x` the value of `e`, and to givef the value of the abstraction `λ(x1, ..., xn) .e`, throughout `e’`.
  For recursive functions `letrec` is used in place of `let`, and when the part in `e’` is omitted we have a declaration.

  The fully determined types (i.e., the monotypes) of ML are built from a set of basic types (`int`, `bool`, etc.) by the binary infixed operators `☓` (Cartesian product), `+` (disjoint sum) and `➙` (function type), and the unary postfixed operator list.
  Polymorphic types (polytypes) are obtained by admitting type variables, which here are represented by `α`, `β`, `γ` ... .
  We represent arbitrary types by `ρ`, `σ`, `τ`.
  For this section we leave the meaning of types to the reader’s intuition; it is made precise in the next section.

  EXAMPLE 1. Mapping a function over a list.

    letrec map (f, m) = if null (m) then nil
                       else cons (f (hd (m) ), map (f, tl (m) )).

  Intuitively, the function map so declared takes a function from things of one sort to things of another sort, and a list of things of the first sort, and produces a list of things of the second sort. So we say that map has type

    ((α ➙ β) ☓ α list) + β list,

  where `α`, `β` are type variables.

  How is this type determined from the bare declaration of map ?
  First, the generic types (we discuss “generic” later) of the identifiers occurring free in the declaration are given by

    null : α list ➙ bool,
     nil : α list,
      hd : α list ➙ α,
      tl : α list ➙ a list,
    cons : (α ☓ α list) ➙ α list,

  that is, they are polymorphic, because their types contain one or more type variables, and our rule is: To every occurrence of such an identifier is assigned a type which is a substitution instance (substituting types for type variables) of its generic type.

  Now each of these identifiers occurs just once in the declaration, so if we denote by `σid` the type assigned to an identifier `id` we must have for some types `τ1, ..., τ5`,

    σnull = τ1 list ➙ bool,
    σnil = τ2 list,
      σhd = τ3 list ➙ τ3
      σtl = τ4 list ➙ τ4 list,
    σcons = (τ5 ☓ τ5 list) ➙ τ5 list.

  <!-- 352 5/28 -->

  The other identifiers (`map`, `f`, `m`) each occur more than once, and our rules demand that each occurrence is assigned the same type. The rules also demand that the following equations are satisfied for some types `ρ1, ρ2 ,...`,

    σmap = σf ☓ σm ➙ ρ1,
    σnull = σm ➙ bool,
      σhd = σm ➙ ρ2,
      σtl = σm ➙ ρ3,
      σf = ρ2 ➙ ρ4,
    σmap = σf ☓ ρ3 ➙ ρ5,
    σcons = ρ4 ☓ ρ5 ➙ ρ6
      ρ1 = ρnil = ρ6.

  The first of these conditions relates the type of a function to those of its formal parameters;
  each of the other conditions arises from some subterm which is a function application, except the last, which is because a conditional expression has the same type as its two arms, and because the definiens and definiendum of a declaration have the same type.

  Now these equations may be solved for the variables `ρi`, `τi`, and `σid` ;
  Morris [10] discusses the solution of such equations.
  Indeed, the situation is entirely appropriate for the use of the Unification Algorithm of Robinson [14]; our well-typing algorithm is based upon this algorithm, and (since in this case nothing more than unification is needed) we may conclude from Robinson’s work that the most general type of map is obtained, i.e., any other type σmap which satisfies the equations must be a substitution instance of the type obtained.
  In fact, the solution of the above equations is

    σmap = (γ ➙ δ) ☓ γ list ➙ δ list,

  where `γ`, `δ` are any distinct type variables.
  So this is the generic type of `map`, that is, to any occurrence of map within the scope of this declaration must be assigned some substitution instance of this type.

  These instances need not be the same.
  Suppose that `tok` is a basic type (a token being a sequence of characters) and that we have available the identifiers (with their types)

  and

      tokl: tok list    (a variable),
    length: tok ➙ int,
    sqroot: int + real,     two obvious functions.

  Then in the expression

    map(sqroot, map(length, tokl))

  the two occurrence of `map` will have types

    ((tok ➙ int) ☓ tok list) ➙ int list,
    ((id ➙ real) ☓ int list) ➙ real list.

  <!-- 353 6/28 -->

  Similarly, if `null`, for example, had occurred twice in the definition of `map`, its types could have been different instances of

    α list ➙ bool

  but our rules demand that different occurrences of a formal parameter (f, for example), or of an identifier (map) being recursively defined, shall have the same type.

  In passing, note that the occurrences of map mentioned above can be regarded as uses of two separately declared (and monomorphic!) map functions, which differ only in that different types are explicitly provided for their arguments and results.
  As Gries and Gehani remark, the compiler could be given the task of generating these distinct declarations-or more accurately (since the programmer need not see the replication or even be aware of it), the task of generating different code for the body of the map function for use at distinct types.
  This would indeed be necessary in the above example if, for efficiency, token lists were implemented differently from integer lists (and the primitive polymorphic functions `hd`, `tl` etc., were correspondingly different).
  We are concerned with a conceptual framework in which these `map` functions may all be regarded semantically as the same object; then the implementor is left with the freedom to implement as few or many variants as he wishes.

  It is clear from our example that the rules of typing need to be carefully framed.
  We leave this task until the next section, but here we look at one more example to illustrate what happens when let or letrec is used locally.

  EXAMPLE 2. Tagging. Suppose we want a function tagpair, such that tagpair `(a)` is a function under which

    (b,c) |➙ ((a, b), (a, c)).

  Of course, we can easily write

    let tagpair = λ(b, c) . ((a, b), (a, c)).

  Now we can explain, without setting up equations, how our well-typing algorithm tackles this declaration.
  It first assigns “unknown” types (i.e., type variables) `α`, `β`, and `γ` to `a`, `b`, and `c`.
  Then `((a, b), (a, c))` acquires type `(α ☓ β) ☓ (α ☓ γ)`, and the λ-expression acquires `β ☓ γ ➙ (α ☓ γ) ☓ (α ☓ γ)`; finally tagpair acuires

    α ➙ (β ☓ γ ➙ (α ☓ β) ☓ (α ☓ γ))     (*)

  (no type equations have placed any constraint upon the types of `a`, `b`, and `c`).

  But consider another way of defining tagpair, using the (infixed) function

    #: (α ➙ β) ☓ (γ ➙ δ) ➙ ((α ☓ γ) ➙ (β ☓ δ))

  such that `(f # g)(a, c) = (f(a), g(c))`, and the pairing function

    pair: α ➙ (β ➙ (α ☓ β))

  <!-- 354 7/28 -->

  such that `pair(a)(b) = (a, b)`.
  We could write

    let tagpair write = λa . (let tag = pair(a) in tag # tag)

  We might then expect the well-typing algorithm to proceed as follows.
  First, `α` is assigned to `a`.
  Then, using the generic type of `pair`, `pair(a)` acquires `δ ➙ α ☓ δ`.
  This is then used as the local generic type of tag, and the two occurrences of `tag` in `tag # tag` are assigned `β ➙ α1 ☓ β`, `γ ➙ α2 ☓ γ`, respectively.
  The occurrence of `#` is assigned an instance of its generic type (again using new type variables) and the type equation for function application will cause the type

    β ☓ γ ➙ (α1 ☓ β) ☓ (α2 ☓ γ)

  to be assigned to tag `#` tag, and to the body of the λ-expression, so that tagpair acquires the type

    α ➙ (β ☓ γ ➙ (α1 ☓ β) ☓ (α2 ☓ γ)).  (**)

  Now comparing `(*)` with `(**)`, something has gone wrong; the second type is too general.
  The problem is that tag and its generic type depend upon the λ-bound variable a and its type `α`, and we do not allow different bound occurrences of a λ-bound variable to have different type.
  Indeed, as far as type is concerned, we should get the same as if tagpair were defined in yet a third way, by

    let tagpair = λa . (pair(a) # pair(a))

  and the reader may be able to obtain (by setting up some equations as in Example 1) the expected type `(*)` in this case.

  The solution is fortunately straightforward.
  We decree that in instantiating the type of a variable bound by let or by letrec.
  only those type variables which do not occur in the types of enclosing λ-bindings (or formal parameter bindings) may be instantiated.
  We call such instantiable variables (in a generic type) generic type variables.

  Now in the second definition of tagpair, the locally defined tag acquired a generic type `δ ➙ α ☓ δ`, in which `δ` is generic but `α` is not.
  Thus α should not have been instantiated in assigning types to the occurrences of tag, and then `(**)` would have been identical with `(*)`.
  This example may appear a little contrived; indeed, our experience has been that almost always either all or none of the type variables of a generic type are generic.
  But there seem to be no simple syntactic constraints which would eliminate the exceptions, nor does it seem desirable to do so.

  From the examples it becomes clear that the rules for typing variables bound, respectively, by let (or letrec) and by λ are going to be different.
  Thus, although our semantics for the two expressions

    let x = e in e’;    (λx . e’)e

  may be (and are) equivalent, it may be possible to assign types correctly to the former but not to the latter.
  An example is the pair

    let I = λx . x in I(I);    (λI . I(l))(λx. x).

  <!-- 355 8/28 -->

  A (partial) intuition for this is that a λ-abstraction may often occur without an argument; the second expression above contains a special (and rather unnecessary) use of abstraction, in that an explicit argument-`(λx . x)`-is present.
  Since the let construct (when translated) involves this restricted use of an abstraction, it is not unnatural that its rule for type assignment is less constrained.
  A compiler could, of course, treat all explicit occurrences of `(λx . e’) e` in the less constrained manner.

  The treatment of types in the interaction between λ-bindings (i.e., formal parameter bindings) and let bindings is really the core of our approach.
  It gives a consistent treatment of the nonglobal declaration of a procedure which may contain, as a free variable, a formal parameter of an enclosing procedure.
  This appears to be one of the more crucial difficulties with polymorphism, and therefore we feel justified in presenting our analysis in terms of a simple language (Exp) which excludes as much as possible that is irrelevant to the difficulty.

  The reader may still feel that our rules are arbitrarily chosen and only partly supported by intuition.
  We certainly do not claim that the rules are the only possible ones, but the results given later demonstrate that they are semantically justified.
  In fact, we show that a program which admits a correct type assignment cannot fail at run-time due to a type error-or more precisely, that type constraints encoded in its semantics are always satisfied.
  It follows from this that compile-time type checking (i.e., the attempt to discover a correct type assignment) obviates the need to carry types at run-time, with an obvious gain in the efficiency of implementation.

  This is of course a principal aim in compile-time type checking; another is the early detection of programming errors (many of which result in ill-typed programs).
  Our achievement is to extend type checking to embrace polymorphism.
  Moreover the type-checking algorithm in its final form (Algorithm I in Section 3) is remarkably simple, even though the proof of the Syntactic Soundness Theorem, which states that-if it succeeds-it produces a correct type assignment, is rather tedious.

  We would like to give an independent characterization of the class of programs which can be well typed in our system, but we have no idea how to do this.
  However, we can give some pointers.
  At the suggestion of a referee we looked at Burge [l, Chapt. 3] concerning general functions for processing data structures.
  All of the functions there (with the exception of Section 3.11 which we did not examine) acquired the expected types from the ML type checker after they had been modified in two respects.
  First, Burge leaves implicit the coercion functions between a disjoint sum type and its summand types; we needed to make these explicit (this point was mentioned in our Introduction).
  Second, we used the ML abstract type construct (see Section 5 for an example) to formulate the recursive type definitions used by Burge.
  In this construct, the isomorphism between a defined abstract type and its representation is made explicit and must be used explicitly.
  To see the need for this requirement consider the case of an α-stream, which is defined to be a function which yields a pair consisting of an `α` and an `α-stream`.
  The type equation

    α-stream = - - - ➙ (α ☓ α-stream)

  cannot be solved by unification (unless we allow infinite type expressions).

  <!-- 356 9/28 -->

  But by treating the equation as an isomorphism, and using two functions to convert back and forth between an abstract type and its representation, this difficulty is removed.
  We claim that this solution is in keeping with the notion of abstract type (see [8], for example).

  On the negative side, there are certainly useful expressions which we cannot well type, though we are not clear how burdensome it is to do without them.
  An obvious example is Curry’s `Y` combinator.

    Y = λf.(λx.f(x(x)))(λx.f(x(x)))

  since self-application is ill typed for us.
  But letrec avoids the need for `Y`.
  More practically, consider

    let F(f) = λ(a, b). (f(a),f(b))

  which-it may be argued-should accept as argument a function which is polymorphic enough to operate upon a and b of different type.
  For example,

    F(reverse)(x, y)

  produces a pair of reversed lists of different type if `x` and `y` are lists of different type.
  Our system rejects such a use of `F` (since it requires a and b to have the same type), but admits

    let reversepair = λ(x, y) . (reverse(x), reverse(y))

  or any other specialization of the function argument of `F`.

  We feel that this example illustrates the main limitation of our system, but that we may have kept as much flexibility as is possible without the use of explicit type parameters.
  When these are introduced, the problem arises of the type of types; Reynolds [12] has made some progress in solving this problem, but we were anxious to see how much could be done while avoiding it.

## 3. A SIMPLE APPLICATIVE LANGUAGE AND ITS TYPES

## 3.1. The Language Exp

  Let x range over identifiers, that is

    x ∈ Id.

  Then the expression language Exp is generated by the following grammar:

    e ::= x | (e e’) | if e then e’ else e” |
          λx.e | fix x.e | let x = e in e’.

  Here `(e e')` means application, `fix x . e` stands for the least fixed point of `λx . e`, and the last clause binds `x` to the value of `e` throughout `e’`.
  We often use `d`, `e`, `f`-with primes and suffixes-to range over `Exp`.
  Constants are omitted; we can imagine instead some standard bindings for certain identifiers in an initial environment.

  <!-- 357 10/28 -->

  We give an ordinary denotational semantics for `Exp`, in which we include a value “wrong,” which corresponds to the detection of a failure at run-time.
  In this small language, the only failures are the occurrence of a non-Boolean value as the condition of a conditional, and the occurrence of a nonfunctional value as the operator of an application.

  Our semantic domains may be taken to be complete partial orders (cpos); a cpo `D` (see [9]) is a partially ordered set such that (a) there exist a minimum element, `⊥D`, (b) every directed subset of `D` has a least upper bound in `D`.
  Take as given a set `{Bi}` of basic domains, with `B0 = T`, the three element truth value domain

    true    false
       \     /
         ⊥T

  and we define recursively

    V = B0 + B1 + ... + F + W    (disjoint sum of domains, with
                                  ⊥V adjoined as minimum element),
    F = V ➙ V                    (continuous functions from V to V),
    W = {・}                      (error).

  The solution (up to isomorphism) of such a set of domain equations is assured by Scott [15].
  Although he worked with complete lattices, the solution also exists in cpos (see Plotkin [11]).

  The semantic function is `ε ∈ Exp ➙ Env ➙ V`, where `Env = Id ➙ V`, the domain of environments.
  We use `η` to range over `Env`.
  In defining `ε`, and later, we use some familiar notation of Scott and Strachey [16], illustrated by these examples (where `D` is some summand of `V`):

    (i) If `d ∈ D`, then `d` in `V` is the image of `d` under the injection of `D` into `V`.

    (ii) If `v ∈ V`, then

      v ⁅ D == true     if v = d in V for some d ∈ D,
            == ⊥T      if v = ⊥V,
            == false    otherwise.

    (iii) If `v ∈ V`, then

      v | D == d      if v = d in V, for some d ∈ D,
            == ⊥D    otherwise.

  <!-- 358 11/28 -->

  The environment `η’ = η{v/x}` is identical with `η` except that `η’(x) = v`.
  The value `・` in `V (・ ∈ W)` is written “wrong.”
  We require the conditional function `COND ∈ T ➙ V ➙ V ➙ V`, where `COND ∈ T ➙ V ➙ V` is written `t ➙ v`, `v’` and takes the value

    v     if t == true,
    v'    if t == false,
    ⊥V   if t == ⊥T.
 
## 3.2. Semantic Equations for Exp

  In these equations, the open brackets `[| |]` indicate syntactic arguments.


    ε[|x|]η = η[|x|]
    ε[|e1 e2|]η = v1 ⁅ F ➙ (v2 ⁅ W ➙ wrong, (v1 | F)v2),
                     wrong
                  where vi is ε[|ei|]η            (i = 1,2).
    ε[|if e1 then e2 else e3|]η = v1 ⁅ B0 ➙ (v1 | B0 ➙ v2, v3), wrong
                          where vi is ε[|ei|]η    (i = 1,2,3)
    ε[|λx.e|]η = (λv・ε[|e|]η {v/x}) in V
    ε[|fix x.e|]η = Y(λv.ε[|e|]η{v/x})
    ε[|let x = e1 in e2|]η = v1 ⁅ W ➙ wrong, ε[|e2|]η{v1/x}
                         where v1 = ε[|e1|]η.

#### Notes.

  (i) `Y` is the least fixed-point operation.
  In many languages the `e` in `fix f ・ e` would be restricted to be an abstraction `λy . e’`, and then

    let f = fix f . (λy . e’)

  might receive the syntax

    let rec f(y) = e’

  (ii) It is easy to see that `“let x = e1 in e2”` has the same meaning under `ε` as
  `“(λx . e2)e1“`.
  But part of our aim is a type discipline which admits certain expressions in the first form and yet rejects their translations into the second form; this is because λ-abstractions may in general occur without an explicit operand, and need more careful treatment.

  (iii) The semantics for`(e1 e2)` corresponds to call-by-value, since the test `“v2 ⁅ W”` ensures that the meaning of `(e1 e2)` is `⊥V` if the meaning of `e2` is `⊥V`.
  The omission of this test gives a call-by-name semantics (a similar test may be omitted in the semantics of the let construct), and the Semantic Soundness Theorem goes through equally in this case.

  <!-- 359 12/28 -->

## 3.3. Discussion of Types

  We now proceed, in outline, as follows.
  We define a new class of expressions which we shall call types; then we say what is meant by a value possessing a type.
  Some values have many types, and some have no type at all.
  In fact “wrong” has no type.
  But if a functional value has a type, then as long as it is applied to the right kind (type) of argument it will produce the right kind (type) of result-which cannot be “wrong”!

  Now we wish to be able to show that-roughly speaking-an Exp expression evaluates (in an appropriate environment) to a value which has a type, and so cannot be wrong.
  In fact, we can give a sufficient syntactic condition that an expression has this robust quality; the condition is just that the expression has a “well-typing” with respect to the environment, which means that we can assign types to it and all its subexpressions in a way which satisfies certain laws.

  So there are two main tasks, once the laws of type assignment are given.
  The first-to show that an expression (or program) with a legal type assignment cannot “go wrong”-- is tackled in this section; surprisingly enough, it is the easier task (at least for an applicative language).
  The second task is to discooer a legal type assignment, given a program with incomplete type information.
  This task is often called type checking. Of course, this term can also mean just verifying that a given type assignment is legal; in a practical situation we probably require something between the two, since one cannot expect a programmer to attach a type to every subexpression.
  In Section 4 we look at the class of legal type assignments for a given program (the class is infinite in general, since we admit polymorphism), and we give an algorithm which, if it succeeds, produces a legal type assignment.
  We conjecture that if the latter exists then the algorithm finds one which is most general, in the sense that any other legal type assignment is a substitution instance of it (substituting types for type variables).

## 3.4. Types and their Semantics

  The syntax of types is as follows.

  (1) `ι0`, `ι1`,... are (basic) types; one for each `Bi`.

  (2) There is a denumerable set of type variables, which are types.
  We use `α`, `β`, `γ`,... to range over type variables.

  (3) If `ρ` and `σ` are types, so is `p ➙ σ`.

  A monotype is a type containing no type variables.
  We use `μ`, `ν`, `π`,... to range over monotypes.
  We use the word polytype when we wish to imply that a type may, or does, contain a variable.

  We first give the semantics of monotypes; that is, we give the conditions under which a value `v ∈ V` possesses a monotype `μ`, which we write `v : μ`.

  (i) `v : ιi` iff `v = ⊥V` or `v ⁅ Bi`

  (ii) `v: μ ➙ ν` iff either `v = ⊥V`, or `v ⁅ F and (v | F) u : ν` whenever `u : μ`. 

  <!-- 360 13/28 -->

  It is clear then that many values have no type.
  Examples are

    wrong, (λv ∈ V . wrong) in V,
    (λv ∈ V . v ⁅ B0 ➙ (v | B0, ➙ x in V, y in V), wrong) in V
      (where x ∈ B1, y ∈ B2, for example).

  But in the last example if `y ∈ B1`, instead, then the function has type `ι0 ➙ ι1` (and no other).

  Some values have many types; the identity function

    (λv ∈ V . v) in V

  for example has type `μ ➙ μ` for every `μ`.
  And of course `⊥V` has every type (it is the only value which has every type).

  This notion of type is derived from Scott [17].
  In fact, it is what Scott calls functionality (after Curry), and is distinct from the notion of a retract.
  If we temporarily identify a type with the set of values which possess it, then it is easy to show that types are downward closed and directed complete, that is

  (i) `∀v, v’ ∈ V. (v : μ and v’ ⊑ v) ⇒ v’ : μ`,

  (ii) For each directed subset `X` of `V`, `(∀v ∈ X . v : μ) ⇒ ∐ X : μ`.

  Retracts share the second property, but not the first.
  Recently Shamir and Wadge [18] have defined a type to be any set with these two properties, and they investigate the consequences of identifying a value `v` with the type `{v’ | v’ ⊑ v}`.

  The semantics of polytypes is as follows.
  First, we use `ρ ⩽ σ` to mean that ρ may be obtained from `σ` by substituting types for type variables ( `⩽` is clearly reflexive and transitive).
  For example,

    μ ➙ ⩽ α ➙ α ⩽ β ➙ β ⩽ α ➙ β

  but

    α ➙ β /⩽ β ➙ β    (unless α = β).

  Then we define

    v : σ iff ∀μ ⩽ σ.v : μ.

  For example,

    (λv . v) in V : α ➙ α.

  Polytypes thereby also stand for subsets of `V`, and these are also directed complete.
  The reader may like to think of each type variable in a polytype as universally quantified at the outermost; for example,

    α ➙ α “means” Vα.α ➙ α,

  where the bound α ranges over monotypes.
  In fact, it is because `α` here ranges over monotypes (not all types) and because we do not admit expressions like

    (∀α . α ➙ α) ➙ (∀α . α ➙ α) 


  as types-though we can see they “mean” if the bound variables are taken to range over monotypes-that we avoid the difficulties (and also some of the interest) of Reynolds [12] in his richer notion of type.

  <!-- 361 14/28 -->

  We need the following simple properties, which are immediate from our definitions.

#### PROPOSITION 1. If `v : σ` and `τ ⩽ σ` then `v : τ`.

#### PROPOSITION 2. If `v : σ ➙ τ` and `v’ : σ`, then `(v | F)v’ : τ`.

  In each case, a property of monotypes is lifted to polytypes.

## 3.5. Type Assignments

  To prepare the ground for the theorem that well-typed expressions cannot “go wrong,” we need to define what is meant by typing an expression.
  We need first some notion of a type environment to give types to the free variables in an expression.

  A prefix `p` is a finite sequence whose members have the form `let x`, `fix x`, or `λx`, where `x` is a variable.
  A prefixed expression `(pe)` has the form `p` | `e`, where every variable free in `e` occurs in a member of `p`.
  We separate the members of a prefix by a period `(.)`.

  Every pe has sub-pe’s given by the following,
  together with transitive reflexive closure:

    (i)   p | x has no sub-pe’s except itself,
    (ii)  p | (e e’) has sub-pe’s p | e and p | e’,
    (iii) p | (if e then e’ else e”) has sub-pe’s p | e, p | e’ and p | e”,
    (iv)  p | (λx . e) has sub-pe p . λx | e,
    (v)   P | (fix x . e) has sub-pe p . fix x | e,
    (vi)  p | (let x = e in e’) has sub-pe’s p | e and p . let x | e’.

  For example, `λy | (let f = λx . (xy) in (fy))` has sub-pe's (besides itself)

    λy | λx . (xy),      λy . λx | (xy),   λy . λx | x,      λy . λx | y,
    λy . let f | (fy),   λy . let f | f,   λy . let f | y

  so a sub-pe is just a subexpression prefixed with all the variable bindings which enclose it.
  Note that λx.(x,v) in the above is not enclosed by let f-it is not in the scope of this binding.

  We say that a member let x or fix x or λx of p is active in p just if no x occurs to the right of it in p.

  Now a typing of a pe p | e is an assignment of a type to each element of p, and to each subexpression and each λx, fix x, or let x in e, with the constraint that in a subexpression (let x = e’ in e”) the same type is assigned to let x and e’.
  Thus one typing of the illustrated pe (it is nearly, but not quite, a well typing in the sense later defined) is as follows:


    λy_α | let f_((α➙β)➙β) = (λx_(α➙β) (x_(α➙β) y_α)_β)_((α➙β)➙β) in (f_((α➙γ)➙γ)y_α)_γ)_γ .

  We denote a typing of p | e by p~ | e~, or by p~ | eσ~ when we want to indicate the type σ assigned to e itself. 

  <!-- 362 15/28 -->

  In any p~ | e~, and any binding let xσ in either p~ or e~, a type variable in σ which does not occur in (the type of) any enclosing λyτ or fix yτ, binding is called a generic type variable for the binding let xσ.
  In the example above, β is generic, but α is not, for the binding let f_((α➙β)➙β).
  Intuitively, the generic type variables in a binding let xσ represent degrees of freedom for the types at which x may be used; they represent the local polymorphism of x.
  Notice that if no λ or fix bindings enclose let xσ , then all the type variables in σ are generic.
  Ageneric instance of σ is an instance of σ in which only generic type variables are instantiated.

  For technical reasons we require that generic type variables occur in a controlled manner.
  We say that p~ | d~ is standard if for every typed sub-pe p’~ | d’~ (with induced typing) the generic type variables of each member let xσ of p’~ occur nowhere else in p’~ | d'~.
  Thus in particular, if let xρ = eρ~ in eσ'~ is a subexpression of d~, with induced typing, then the generic type variables in ρ may not occur in eσ'~ (though they must of course occur in eρ~).

  We now define the notion of a well-typed (wt) pe as follows:

    (i)   p~ | xτ, is wt iff it is standard, and either
      (a) λxτ, or fix xτ, is active in p~, or
      (b) let xσ, is active in p~, and τ is a generic instance of σ.
    (ii)  p~ | (eρ~ eσ'~)_τ is wt iff p~ | e~ and p~ | e'~ are both wt, and ρ = σ ➙ τ.
    (iii) p~ | (if eρ~ then eρ'~ else eρ”~)_τ' is wt iff p~ | e~, p~ | e’~ and p~ | e”~ are all wt, ρ = ι0 and σ = τ = τ’.
    (iv)  p~ | (λxρ . eρ~)_τ is wt iff p~.λxρ | e~ is wt and　τ = ρ ➙ σ.
    (v)   p~ | (fix xρ eρ~)_τ is wt iff p~.fix xρ | e~ is wt and ρ = σ = τ.
    (vi)  p~ | (let xρ = eρ~ in eσ'~)_τ is wt iff p~ | e~ and p~ . let xρ | e'~ are both wt, and σ = τ.

  Although this recursive definition is useful for some proofs, an alternative characterization of wt is sometimes useful.
  The proof of the next proposition is fairly straightforward, and we omit it.
  Note that a wt p~ | d~ is necessarily standard, by an easy structural induction.

#### PROPOSITION 3. p~ | d~ a is wt iff the following conditions hold:

  (A) It is standard.

  (B) For every (bound) occurrence xσ, the corresponding binding occurmtce is either λxσ, OY fix xσ, or let xσ , where σ is a generic instance of τ.

  (C) The following conditions hold for all subexpressions (with induced typing) of d~

      (eρ~ eσ'~)_τ                       ρ = a ➙ τ,
      (if eρ~ then eσ'~ else eτ"~)_τ'    ρ = τ0 and σ = τ = τ’,
      (λxρ . eσ~)_τ                      τ = ρ ➙ σ,
      (fix xρ . eσ~)_τ                   ρ = σ = τ,
      (let xρ = eρ~ in eσ'~)_τ           σ = τ. 

  <!-- 363 16/28 -->

  The typing which we illustrated above therefore fails to be wt for only one reason:
  The subexpression `(f_((α➙γ)➙γ) yα)_γ` violates the first of conditions (C) in Proposition 3.
  Consider another example.
  The following (with p~ empty) is a well typing:

    let I_(α➙α) = (λxα.xα)_(α➙α) in
      (I_((ι➙ι)➙(ι➙ι)) I_(ι➙ι))_(ι➙ι) .

  Note that α is generic in the type α ➙ α of the declaration of I, so may be instantiated (possibly differently) in the types of bound occurrences of I.

  To illustrate the need to instantiate only generic type variables, for variables declared by let, notice first that in λxα.xβ, we must have α = β, by condition (B) of Proposition 3.
  Indeed, we can argue intuitively for this as follows: if we declare

    let I = λx . x in ...,

  then we wish to have that any expression (Ie) in the scope of this declaration receives the same type as the subexpression e.
  But now suppose we write (with assigned types)

    let I_(α➙β) = (λxα . (let yα = xα in yβ)_β)_(α➙β) in ...

  then-since this is semantically equivalent to the simpler declaration-we should again demand that α = β.
  But this is imposed in our definition of well typing, just because α is not generic for the binding let yα, so may not be instantiated in a bound occurrence of y.

## 3.6. Substitutions

  A substitution `S` is a map from type variables to types.
  `S` may be extended in a natural way to yield a map from types to types, from typed pe’s to typed pe’s, etc.
  We say that S involves a type variable α if either Sα ≠ α, or for some β ≠ α, α ∈ Sβ.
  (α ∈ τ means α occurs in τ.)
  We need substitutions extensively in the second part of this paper, but for the present we need only one property relating substitutions and wt.

#### PROPOSITION　4.

  If S involves no generic variables of a wt p~ | d~, then S(p~ | d~) is also a wt.

#### Proof.

  We use Proposition 3. First, observe that the assumption on S yields that the generic variables for each binding in S(p~ | d~) are exactly those for the corresponding binding in p~ | d~.
  Since Sβ contains no generic variable when β is not generic, S(p~ | d~) is standard.

  Second, if xσ, is bound by λxσ or fix xσ in p~ | d~, then xsσ is bound by λxsσ or fix xsσ in S(p~ | d~).
  If xσ is bound by let xτ, and σ = [ρ1/α1 ,..., αn /αn]_τ, where αi are the generic variables of τ, then in S(p~ | d~) xsσ is bound by let xsτ , and `Sσ = [Sρ1/α1 ,..., Sρn/αn](Sτ)` is a generic instance of Sτ.

  Third, conditions (C) of Proposition 3 are easily verified for S(p~ | d~), using identities like S(u ➙ τ) = Sσ ➙ Sτ. ■

  <!-- 364 17/28 -->

## 3.7. Well-Typed Expressions Do Not Go Wrong

  First we need a simple relation between semantic environments η and our type environments-which are typed prefixed p~.
  We say

    η respects p~ iff, whenever let xρ or λxρ or fix xρ, is active in p~, η[|x|] : ρ.

#### THEOREM 1 (Semantic Soundness). If η respects p~ and p~ | dτ~ is well typed then ε[|d|]η : τ.

#### Proof.

  A fairly simple structural induction. Take the six cases for dτ~.

    (i) xτ.
    Then either λxτ or fix xτ is active in p~, and η[|x|] : τ, so ε[|x|]η : τ, or let xσ is active in p~, and η[|x|] : σ; but then T ⩽ σ, so ε[|x|]η = η[|x|] : τ, by Proposition I.

    (ii) `(e_(σ➙τ) eσ~)_τ`.
     Then p~ | e_(σ➙τ)~ is wt, so ε[|x|]η : σ ➙ τ, and similarly ε[|e'|]η : σ.
    Then from the semantic equation (remembering that wrong has no type) and by Proposition 2 we get ε[|d|]η : τ.

    (iii) (if eι0~ then eσ'~ else eσ"~). Straightforward; the only extra detail needed here is that ⊥V has every type.

    (iv) (λxρ . eσ~)_(ρ➙σ). Then p~ . λxρ | eσ~, is wt. Now we require (λv . ε[|e|]η{v/x}) in V
 : ρ ➙ σ.
    Denote this function by f in V. The inverse of Proposition 2 does not hold, that is, to show f in V : ρ ➙ σ it is not sufficient (though it is necessary) that whenever v:ρ, fv : σ.
    What is required is that for every η ➙ ν ⩽ ρ ➙ σ, f in V : μ ➙ ν.


  Suppose then that μ ➙ ν ⩽ ρ ➙ σ. Then there is a substitution S, involving only the type variables in ρ and σ, such that μ ➙ ν = S(ρ ➙ σ).
  Then, since none of these type variables is generic in p~ ・ λxρ | eσ~, it follows that S(p~) ・ λxμ | S(e~)_ν” is wt by Proposition 4.
  Moreover η respects S(p~) (since by Proposition 1 whenever η[|x|] : σ’ and τ’ ⩽ σ’, η[|x|] : τ’) so for any v : μ we also have η{v/x} respects S(p~) . λxμ .

  It then follows by induction that ε[|e|]η{v/x} : ν, so we have shown that v : μ implies fv : ν, and this yields f in V : μ ➙ ν as required.

  (v) (fix xρ . eρ~)_ρ . Then p~ . fix xρ | eρ~ is wt.
  Now we require that v : ρ, where

    v = Y(λv' . ε[|e|] η{v’/x}).

  Now v0 = ∐i, vi, where v0 = ⊥V, v_(i+1) = ε[|e|] η{vi/x}, and by the directed completeness of types we only have to show vi : ρ for each i.

  Clearly v0 : ρ.
  Assume vi : ρ.
  Since η respect p~, we have that η{vi/x} respects p~ ・ fix xρ , so by the main induction hypothesis v_(i+1) : ρ also, and we are done.

  (vi) (let x = eρ~ in eσ'~)_σ .
  Then p~ | eρ~ is wt, so we immediately have v : ρ, where v = ε[|e|]η. We require ε[|e'|]η(v/x) : σ.

  Now p~ ・ let xρ | eσ'~ is also wt, and because v : ρ we have that η{σ/x} respects p~ ・ let xρ ; the rest follows by the induction hypothesis. ■

  <!-- 365 18/28 -->

  As a corollary, under the conditions of the theorem we have

    ε[|d|]η ≠ wrong,

  since wrong has no type.

## 4. A WELL-TYPING ALGORITHM AND ITS CORRECTNESS

## 4.1. The Algorithm W

  In this section we tackle the question of finding a well typing for a prefixed expression.
  We present an algorithm W for this.
  We would like to prove that W is both syntactically sound and (in some sense) complete.
  By syntactic soundness, we mean that whenever W succeeds it produces a wt; by completeness, we mean that whenever a wt exists, W succeeds in finding one which is (in some sense) at least as general.

  Although W is probably complete, it is difficult to find a simple proof.
  So we concentrate on soundness, and then comment on implementation of W and on extending it to deal with richer languages.
  Since a type-checking algorithm which simulates W has been working successfully for nearly 2 years in the context of the LCF metalanguage ML [2], we have evidence for its usefulness and even-to some extent-for its completeness.

  W is based on the unification algorithm of Robinson [14].
  Indeed, the only feature of well typing which does not fall directly within the framework of unification is the condition that τ should be a generic instance of σ whenever xτ is bound by let xσ .
  The completeness (in some sense) of W should follow from the second part of the following proposition concerning unification, but we need only the first half for our proof that W is sound.

#### PROPOSITION 5 (Robinson).

  There is an algorithm U, taking a pair of expressions (over some alphabet of variables) and yielding a substitution, such that for any pair of expressions σ and τ

  (A) If U(σ, τ) succeeds, yielding U, then U unifies σ and τ (i.e., Uσ = Uτ).

  (B) If R unifies σ and τ, then U(σ,τ) succeeds yielding a U such that for some substitution S, R = SU.

  Moreover, U involves only variables in σ and τ.

  To find a well typing of a complete program f, we would expect to supply also a typed prefix p~, containing only let bindings, giving the types of values bound to predefined identifiers.
  We would then expect W to yield f~ such that p~ | f~ is a wt.

  To state (and prove) W recursively however, prefixes containing all types of binding occur, and W in general needs to modify the nongeneric type variables in the prefix to meet constraints imposed on the program.
  We therefore make W return also a substitution T, indicating the necessary transformation. 

  <!-- 366 19/28 -->

  To be precise, we show that if W(p~, f) succeeds and returns (T, f~), then (T p~) | f~ is a wt.

  We first state W.
  At certain points W requires type variables which have not previously occurred; such new type variables are denoted by β or βi.
  W(p~, f) is defined by induction on the structure of f; the algorithm is expressed in a purely applicative programming style, in contrast with the more efficient algorithm I presented later, which is expressed more in the style of imperative programming.

#### Algorithm W

  `W(p~,f) = (T,f~)`, where

  (i) If f is x, then:

    if λxσ, or fix xσ is active in p~ then
      T = I, f~ = xσ;
    if let xσ is active in p~ then
      T = I, f~ = xτ
    where τ = [Bi/αi] σ, αi are the generic variables of σ,
    and βi are new variables.

  (ii) If f is (de), then:

    let (R, dρ~) = W(p~,d), and (S, eσ~) = W(R p~, e);
    let U = U(Sρ, σ ➙ β), β new;
    then T = USR, and f~ = U(((S d~)e~)_β).

  (iii) If f is (if d tha e else e’), then:

    let (R, dρ~) = W(p~, d) and U0 = U(ρ, ι0);
    let (S, eσ~) = W(U0 R p~, e), and (S’, e_(σ')'~) = W(S U0 R p~, e’);
    let U = U(S' σ, σ’);
    then T = U S’ S U0 R, and
         f = U((if S' S U0 d~ then S' e~ else e’~)_σ).

  (iv) If f is (λx . d), then:

    let (R, d~) = W(p~ . λxβ, d), where β is new;
    then T = R, and f~ = (λxRβ . dρ~)_(Rβ➙ρ) .

  (v) If f is (fix x . d), then:

    let (R, dρ~) = W(p~ . fix xβ, d), β new;
    let U = U(R　β, ρ);
    then T = U R, and f~ = (fix xURβ . U d~)_(URβ) .

  (vi) If f is (let x = d in e), then:

    let (R, dρ~) = W(p~, d);
    let (S, eσ~) = W(R p~ . let xρ, e);
    then T = S R, and f~ = (let xSρ = S d~ in e~)_σ . ■

  <!-- 367 20/28 -->

## 4.2. The Soundness of W

  To show that W is sound it is convenient to have a few simple definitions.
  If A is a type, a typed prefix, or a typed pe then

    Vars(A) ≝ {α | α ∈ A, α a type variable}.

  If A is a typed prefix or a typed pe then

     Gen(A) ≝ {α | α ∈ A, α a generic type variable).
    Spec(A) ≝ Vars(A) - Gen(A).

  If S is a substitution, then

    Inv(S) ≝ {α | S involves α}
           = {α | ∃β. S β ≠ β and α ∈ {β} ∪ Vars(S β)}.

  We need the following simple properties, whose proof we omit:

#### PROPOSITION 6.

    (A) Inv(RS) ⊆ Inv(R) ∪ Inv(S)
    (B) Vars(Sτ) ⊆ Vars(T) ∪ Inv(S).

#### THEOREM 2 (Syntactic Soundness).

  Let `p~` be a standard prefix, and `p | f a (closed) pe`.
  Then, if W(p~,f) = (T,fτ~),

    (A) T p~ | f~ is wt,

    (B) Inv(T) ⊆ Spec(p~) ∪ New,

  and

    (C) Vars(τ) ⊆ Spec(p~) ∪ New,

  where New is the set of new type variables used by W.

#### Proof.

  By induction on the structure of f, using the recursive definition of wt.
  We omit the cases of conditional and fix expressions, since nothing new arises there, and we treat the easier of the other cases first

  (i) f is X. Then T = I, so (B) is immediate.
  If λxσ, or fix xσ is active in p~ then f~ = xσ and (A), (C) are immediate.

  If let xσ is active, then f~ = xτ, where τ = [βi/αi]σ, {αi} generic in σ, New = {βi}.
  Then T p~ | f~ = p~ | xτ is standard and (A), (C) follow easily.

  (iv) f is (λx . d). Let (R, dρ~) = W(p~ . λxβ, d), using new variables New1, say. 

  <!-- 368 21/28 -->

  By induction, R(p~ ・ λxβ) | dρ~, is wt, so for (A) f~ = R p~ | (λxRβ . d~)_(Rβ➙ρ) is wt (defn of wt).
  Also by induction,

    Inv(R) |
    Var(ρ) | ⊆ Spec(p~ ・ λxβ) ∪ New,
             = Spec(p) ∪ {β} ∪ New,
             = Spec(i) ∪ New     (since New = New1 ∪ {p})

  and (B) follows since T = R. For (C)

    Vars(R β ➙ ρ) ⊆ Inv(R) ∪ {@ U Vars(p)    by Proposition 6,
                  ⊆ Spec(p) ∪ New
  as required.

  (vi) f is (let x = d in e).
  Then let (R, dρ~) = W(p~, d), using new variables New1 , say.
  Then by induction

    R p~ | d~ is wt                                    (1)

    Inv(R)  |
    Vars(ρ) | ⊆ Spec(p~) ∪ New1                       (2)

  Now from (2)

    Spec(R p~) ⊆ Inv(R)   ∪ Spec(p~)
               ⊆ Spec(p~) ∪ New1                      (3)

  and from (1) by standardness

    Gen(R p~ | d~) ∩ Spec(R p~) = ∅.                   (4)

  We also have that Gen(R p~) = Gen(p~), which is disjoint from Vars(ρ) by (2), hence R p~ ・ let xρ, is a standard prefix.

  So let S, eσ~ = W(R p~ ・ let xρ, e) using new variables New2.
  Then by induction

    S(R p~ ・ let xρ) | e~ is wt                         (5)

    Inv(S)  |
    Vars(σ) | ⊆ Spec(R p~ ・ let xρ) ∪ New2             (6)

  But Spec(R p~ ・ let xρ) = Spec(R p~), so putting (6) and (4) together yields (since New2 are new variables)

    Inv(S) n Gen(R p~ | d~) = ∅

  and it follows by Proposition 4 that S(R p~ | d~) is wt, and using (5) we have by definition of wt that 

    S R p~ | (let xSρ = S d~ in e~)_σ

  is wt; but this is just T p~ | f~, so we have proved (A).

  <!-- 369 22/28 -->

  For (B), we have

    Inv(T) ⊆ Inv(S) ∪ Inv(R), by Proposition 6,
           ⊆ Spec(p~) ∪ New1 ∪ New2, using (6), (3), and (2)

  and for (C), by similar reasoning,

    Vars(σ) ⊆ Spec(p~) ∪ New1 ∪ New2 .

  This is all we need, since New = New1 ∪ New2 in this case.

  (ii) f is (de).
  Let (R, dρ~) = W(p~, d), using new variables New1 , and (S, eσ~) = W(R p~, e), using new variables New2 ; then by similar reasoning to case (vi) we find that

    S R p~ | S(dρ~) is wt                            (7)
    S R p~ | eσ~    is wt                            (8)

    Inv(S R)          |
    Vars(Sρ), Vars(σ) | ⊆ Spec(p~) ∪ New1 ∪ New2.  (9)

  Now if U = U(Sp, σ ➙ β), where β is new, we have by Proposition 5 that

    U Sρ = Uσ ➙ Uβ,                                  (10)
    Inv(U) ⊆ Vars(Sρ) ∪ Vars(σ) ∪ {β}.             (11)

  It follows that U involves no generic variables of the wt’s (7) and (8), and that

    U S R p~ | U(((S d~) e~)_β)

  is wt; but this is just T p~ | f~, so we have proved (A). For (B), we have first that

    New = New1 ∪ New2 ∪ {β},

  so

    Inv(T) ⊆ Inv(U) ∪ Inv(SR),        by Proposition 6,
           ⊆ Spec(p~) ∪ New,          from (9) and (11),

  and for (C),

    Vars(τ) = Vars(U β)
            ⊆ Inv(U) ∪ {β},           by Proposition 6,
            ⊆ Spec(p~) ∪ New,         again from (9) and (11). ■

## 4.3. Implementation of W; a Simplified Algorithm I

  As it stands, W is hardly an efficient algorithm; substitutions are applied too often.
  It was formulated to aid the proof of soundness.
  We now present a simpler algorithm I which simulates W in a precise sense. 

  <!-- 370 23/28 -->

  I differs from W in two ways.
  First, we adopt an idea familiar in the literature on resolution-based theorem-proving systems, in which substitutions are composed, but only applied when it is essential to do so.
  Second, we take advantage of the fact that what is often needed in practice from a well-typing algorithm is not the whole type assignment f, but only the type assigned to f itself.

  In fact `I` builds only one substitution, called `E`, which is idempotent-that is, `EE = E`
  -which is to say that if `β ∈ Eα`, then `Eβ = β`.
  This substitution is held in a program variable (called `E`) global to `I`, and `I` works by transforming `E`.
  In place of the unification function `U`, `I` calls a unification procedure `UNIFY` which delivers no result but sideeffects the variable `E`.
  We assume that `U` and `UNIFY` are related as follows: of `E` and `E’` are the values of `E` before and after the command

    UNIFY(σ, τ)

  and if

    S(Eσ, Eτ) = U

  then

    E’ = U E.

  Thus, applying UNIFY to types σ and τ in the presence of E corresponds to applying U to types Eσ and Eτ; σ and τ may be thought of as implicit types standing for the explicit types which would be gained by applying the “explicating” substitution E (the idempotency of E means that further explication is unnecessary).
  The effect of UNIFY (if successful) is to generate the new explicating substitution E’.
  I will similarly handle an implicit typed prefix p~, which can be explicated when necessary by applying E.
  We assume that I has local variables ρ, σ, and σ’, whose values are implicit types, and generates its result in a fourth variable τ.

  Assuming an initial idempotent `E`, `I` is given a typed prefix `p~` and an expression `f` such that `E p~` is standard and `p | f` is a `pe` (i.e., all free identifiers in `f` are bound in `p`).
  Here is the algorithm:

#### Algorithm I

    I(p~, f) = τ, where

  (i) If f is x, then:

    If λxσ or fix xσ is active in p~, τ := σ.
    If let xσ is active in p~, τ := [βi/αi]Eσ, where αi are the generic type variables of let xEσ in E p~, and βi are new variables.

  (ii) If f is (de) then:

    ρ := I(p~, d); σ := I(p~, e);
    UNIFY (ρ, σ ➙ β); (β new)
    τ := β

  <!-- 371 24/28 -->

  (iii) If f is (if d then e else e’), then:

    ρ := I(p~, d); UNIFY (ρ, ι0);
    u := I(p~, e); σ’ := I(p~, e’);
    UNIFY(σ, σ’); τ := σ

  (iv) If f is (λx . d), then:

    ρ := I(p~ ・ λxβ, d); (β new)
    τ := β ➙ ρ

  (v) If f is (fix x ・ d), then:

    ρ := I(p~ . fix xβ, d); (β new)
    UNIFY(β, ρ); τ := β

  (vi) If f is (let x == d in e) then:

    ρ := I(p~, d); σ := I(p~ . let xρ, e);
    τ := σ.      ■

  What is the simulation relation between I and W?
  It is simply expressed by the following proposition, whose proof we omit, since it is an easy structural induction involving few of the subtleties which we encountered in the soundness theorem.

#### PROPOSITION 7. Let p | f be a pe, E be idempotent, and E p~; be standard.
  Then I(p~, f) succeeds (producing τ’ and a new value E’ for E) iff W(E p~, f) succeeds (producing T and fT), and moreover if both succeed

    (A) E’ = TE,
    (B) E’T’ = τ.

  Thus the type produced by I, when explicated, is exactly that ascribed to f by W.

  In practice, E may be efficiently represented by a table INST of variable-type pairs, representing those variables which have hitherto been instantiated.
  The effect of UNIFY is merely to add some entries to INST, representing instantiation of previously uninstantiated variables.
  The substitution E represented by INST is given recursively as follows

        E(ι) = ι      (basic types),
        E(α) = E(ρ)   if (α, ρ) ∈ INST for some ρ,
             = α      otherwise,
    E(σ ➙ τ) = Eσ ➙ Eτ.

  In fact, in the extended version of I implemented for ML, which is written in LISP, INST itself is represented by a property (in the LISP sense) INSTANCE of type variables.
  Each type variable α has ρ as its INSTANCE property value, if (α, ρ) ∈ INST for some ρ; otherwise the property value is NIL. 

  <!-- 372 25/28 -->

## 5. TYPES IN EXTENDED LANGUAGES

  We now consider some extensions of our language, and how our results may be strengthened to apply to them.

#### (1) As we said in the introduction, the addition of extra (primitive) type operators such as ☓ (Cartesian product), + (disjoint sum) and list (list forming), causes no difficulty.
  Together with ➙, these are the primitive type operators in the language ML.
  For ☓ one has the standard polymorphic functions

    pair: α ➙ β ➙ (α ☓ β)    (one could add the syntax (e, e’) for pair (e)(e’)),
    fst: α ☓ β ➙ α,
    snd: α ☓ β ➙ β.

  For +, one has

    inl: α ➙ α + β,    inr: β ➙ α + β    (left and right injections),
    outl: α + β ➙ α,   outr: α + β ➙ β   (left and right projections),
    isl: α + β ➙ bool, isr: α + β ➙ bool (left and right discriminators)

  with natural interpretations.
  For list, one has the standard list-processing functions mentioned in Section 2.
  Notice that all members of a list must have the same type.

  With appropriate adjustment to the semantic domains, the Semantic Soundness Theorem extends naturally; the Syntactic Soundness Theorem goes through virtually without change.

#### (2) Next, we consider assignable variables and assignments.
  One way (used in ML) of adding these is to allow the assignment expression form “x := e” (whose value is the value of e), and the expression form “letref x = e in e’” to declare an assignable variable, initialized to the value of e.
  The first effect of these additions is a major change in the semantic domains, since now expressions may have side effects.
  Although we believe that a Semantic Soundness Theorem may be proved, it appears to be a cumbersome task.
  The reason for the difficulty is illustrated by considering

    λx.(y := x)

  which is an identity function with a side effect.
  To say that it has type α ➙ α, meaning that whenever it is given an argument of type μ it gives a result of type μ, takes no account of the side effect on y.
  What is required is a more careful definition (in terms of the semantic domains) of what such functional types mean, which takes side effects into account.

  By contrast, it is easy enough to give well-typing rules for the two new kinds of expression.
  We would extend the definition of wt by the following clauses:

  (i) (c) If letref xτ, is active in a standard prefix p~, then p~ | xτ, is wt. (Thus, all letref-bound occurrences of x must have the same type). 

  <!-- 373 26/28 -->

  (vii) p~ | (xρ := eσ~)_τ is wt iff p~ | e~ is wt, and letref xρ is active in p~, and ρ = σ = τ.

  (viii) p~ | (letref xρ = eρ~ in eσ'~)_τ is wt iff p~ | e~ and p~.letref xρ | e’~ are wt, and σ = τ.

  It is a routine matter to extend the algorithm W and the Syntactic Soundness Theorem to handle these clauses.
  But returning again to semantic considerations, there is a problem concerned with nonlocal assignments within λ-abstractions (procedures).
  Consider

    let g = (letref y = nil in λz . y := cons(z, y))

  which sets up y as a hidden (own) variable for the function g.
  With our rules, g obtains generic type α ➙ α list.
  Remembering that the value of an assignment is the value assigned, each call of g augments the hidden y, returning the augmented list as value.
  Now since α is generic, the expressions

    g(2),    g(true)

  are both admissible within the scope of g, with types int list and boo1 list, respectively.
  But if they are evaluated in this order, the value of the second expression is a list whose two members are true and 2; it is not a list of Boolean values!

  There are at least two possible solutions to this dilemma.
  One is to decree that in such a situation α is not a generic type variable.
  Another, which we adopt in ML, is to forbid nonlocal assignments to polymorphic assignable variables within λ-abstractions (procedures).
  Polymorphic assignable variables are still useful, for example, in programming iterations (while statements) which themselves can be given simple wt rules.

  I believe that the second solution, even slightly relaxed, admits a Semantic Soundness Theorem.
  The details, however, are unattractive, and I have been discouraged (particularly after a useful discussion with John Reynolds) from attempting to complete the proof.
  What is rather needed is a language design which pays more respect to side effects; one approach might be to modify PASCAL by requiring that all variables assigned in a procedure be listed as output parameters of the procedure.
  But how to combine this with the rather useful properties of own variables is, as far as I know, an open problem in language design, and a good solution would be a valuable step forward.
  For a recent promising attempt to control side effects see Reynolds [13].

#### (3) To complete the list of nontrivial extensions which we have included in ML, consider the declaration (possibly recursive) of a new type operator in terms of old ones.
  Such a declaration may have nonglobal scope.
  If it is also accompanied by the declaration of a set of functions over the new type operator, and the explicit definition of the type operator is only available in defining the set of functions (not within the whole scope of the new type operator), one has a version of what is currently called abstract type.
  In ML, we would define the class of binary trees whose tips are labeled by objects of arbitrary type as follows, using the type variable α to stand for the type of tip labels:

    absrectype  α bitree = α ➙ (α bitree ☓ α bitree)
          with  sons(t) = ...
           and  maketree(t, t’) = ...
           and  tiptree(a) = ... 

  <!-- 374 27/28 -->

  in which only the omitted defining expressions are given access to the representation of bitrees.
  The defined functions are polymorphic, with generic types

    α bitree ➙ (α bitree)^2,   (α bitree)^2 ➙ α bitree,   α ➙ α bitree.

  For full details of this construct, see [2]; we owe the construct partly to Lockwood Morris and to discussion with Jerry Schwarz.
  In this case the wt rules are also rather easy; we have not checked syntactic and semantic soundness, but suspect that there should be no great difficulty.

#### (4) Two features which contribute a kind of polymorphism have been completely ignored so far.
  The first is coercions. The expression x := 42, where x is a real assignable variable, is ill typed for us.
  However, there is no barrier to having the type checker report such instances of ill typing and allowing the compiler to receive the report and insert a coercion to rectify it.

  The second feature is to allow certain procedures-either standard or user defined-to possess more than one type.
  We may wish “+” to possess int^2 ➙ int and real^2 ➙ real, without of course possessing α^2 ➙ α (which is the least general polytype having the two given types as instances).
  While we have not investigated the question, there appears to be a good possibility of superposing this feature upon our discipline.

## 6. CONCLUSION

  We have presented a discipline of polymorphic programming which appears to be practically useful, and have given a rather simple type-checking algorithm.
  In a restricted language we have shown that this algorithm can be proved correct (the proof was factored into two Soundness Theorems).
  Though much work remains to be done, we hope to have made the point that the practice of type checking can and should by supported by semantic theory and proof.

## ACKNOWLEDGMENTS

  I am indebted to the referees for their apposite comments on the first version of this paper, which.drew my attention to some inaccuracies and also led me to a clearer exposition in several
  places.
  My thanks also go to Dorothy McKie for her careful preparation of two versions of the paper.

## REFERENCES

  1. W. H. BURGE, “Recursive Programming Techniques,” Addison-Wesley, Reading, Mass., 1975.
  2. M. GORDON, R. MILNER, AND C. WADSWORTH, “Edinburgh LCF,” CSR-11-77, Computer Science Dept., Edinburgh University, 1977.
  3. M. GORDON, R. MILNER, L. MORRIS, M. NEWEY, AND C. WADSWORTH, A metalanguage for interactive proof in LCF, in “Proc. 5th Annual ACM SIGACT-SIGPLAN Symposium on Principles of Programming Languages, Tucson, Arizona, 1978.” 

  <!-- 375 28/28 -->

  4. D. GRIES AND N. GEHANI, Some ideas on data type in high-level languages, Comm. ACM. 20 (1977), 414-420.
  5. R. HINDLEY, The principal type-scheme of an object in combinatory logic, %zx~. Amer. &ikth. Sot. 146 (1969), 29-60.
  6. B. W. LAMPSON, J. J. HORNING, R. L. LONDON, J. G. MITCHELL, AND G. L. POPEK, Report on the programming language Euclid, SIGPLAN Notices (ACM) 12, 2 (1977).
  7. P. J. LANDIN, The next 700 programming languages, Comm. ACM 9 (1966), 157-164.
  8. B. H. LISKOV AND S. ZrLLEs,Programming with abstract data types, in “Proc. of ACM SIGPLAN conference on Very High Level Languages,” SIGPLAN Notices (ACM) 9 (1974), 50-59.
  9. R. MILNER, “Models of LCF,” Mathematical Centre, Amsterdam, Tracts, Vol. 82, pp. 49-63, 1976.
  10. J. H. MORRIS, “Lambda-Calculus Models of Programming Languages,” Ph.D. Thesis, M.4CTR-57, MIT, 1968.
  11. G. PLOTKIN, A power-domain construction, SIAM J. Comput. 5 (1976), 452-487.
  12. J. C. REYNOLDS, “Towards a Theory of Type Structure,” Systems and Inform. Sci., Syracuse University, 1974.
  13. J. C. REYNOLDS, Syntactic control of interference, in “Proc. 5th ACM Symposium on Principles of Programming Languages, Tucson, Arizona, 1978,” pp. 39-46.
  14. J. A. ROBINSON, A machine-oriented logic based on the resolution principle, J. Assoc. Comput. Much. 12 (1965), 23-41.
  15. D. SCOTT, Lattice theoretic models for various type-free calculi, in “Proc. 4th International Congress for Logic, Methodology and Philosophy of Science, Bucharest, Rumania, 1972.”
  16. D. SCOTT AND C. STRACHEY, Towards a mathematical semantics for computer languages, in “Proc. Symposium on Computers and Automata,” Vol. 21, Microwave Res. Inst. Symposia Series, Polytech. Inst. of Brooklyn, 1971.
  17. D. SCOTT, Data types as lattices, SIAM j. Comput. 5 (1976), 522-587.
  18. A. SHAMIR AND W. WADGE, Data types as objects, in “Proc. 4th ICALP Conference, Turku, Finland, 1977.”
  19. C. STRACHEY, Fundamental concepts in programming languages, Notes for the International Summer School in Computer Programming, Copenhagen, 1967.
  20. R. D. TENNENT, On a new approach to representation-independent data classes, Acta Inform. 8 (1977), 315-324.
  21. B. WEGBREIT, The treatment of types in ELl, Comm. ACM 17 (1974), 251-264.
  22. A. VAN WIJNGAARDEN ET AL., Revised report on the algorithmic language ALGOL 68, Acta Informutica 5 (1975), l-236.
  23. R. A. WULF, R. L. LONDON, AND M. SHAW, “Abstraction and Verification in ALPHARD: Introduction to Language and Methodology,” ISIIRR-76-46, Univ. of California, 1976.
