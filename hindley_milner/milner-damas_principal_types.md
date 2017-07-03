# Principal type-schemes for functional programs∗

  Luis Damas† and Robin Milner

  First published in POPL ’82: Proceedings of the 9th ACM SIGPLAN-SIGACT

  symposium on Principles of programming languages, ACM, pp. 207–212

Permission to copy without fee all or part of this material is granted provided that the copies are not made or distributed for direct commercial advantage, the ACM copyright notice and the title of its publication and date appear, and notice is given that copying is by permission of the Association for Computing Machinery.
To copy otherwise, or to republish, requires a fee and/or specific permission.

© 1982 ACM 0-89791-065-6/82/001/0207 $00.75

---

∗ Re-keyed 12 October 2010 by Ian Grant iang@pobox.com

† The work of this author is supported by the Portuguese Instituto Nacional de Investigacao Cientifica 1

---

## 1 Introduction

This paper is concerned with the polymorphic type discipline of ML, which is a general
purpose functional programming language, although it was first introduced as a
metalanguage (whence its name) for constructing proofs in the LCF proof system.[4]
The type discipline was studied in [5] where it was shown to be semantically sound,
in a sense made precise below, but where one important question was left open:
does the type-checking algorithm — or more precisely the type assignment algorithm
(since types are assigned by the compiler, and need not be mentioned by the programmer)
— find the most general type possible for every expression and declaration?
Here we answer the question in the affirmative, for the purely applicative part
of ML. It follows immediately that it is decidable whether a program is well-typed, in
contrast with the elegant and slightly more permissive type discipline of Coppo. [1]
After several years of successful use of the language, both in LCF and other research,
and in teaching to undergraduates, it has become important to answer these questions
— particularly because the combination of flexibility (due to polymorphism),
robustness (due to semantic soundness) and detection of errors at compile time has
proved to be one of the strongest aspects of ML.

The discipline can be well illustrated by a small example. Let us define in ML the
function map, which maps a given function over a given list — that is

  map f [x1; ...; xn] = [f(x1),...,f(xn)]

The required declaration is

  letrec map f s = if null s then nil
                   else cons(f(hd s)) (map f (tl s))

The type checker will deduce a type-scheme for map from existing type-schemes for
null, nil, cons, hd and tl; the term type-scheme is appropriate since all these objects
are polymorphic. In fact from

  null : ∀α(αlist → bool)
  nil  : ∀α(αlist)
  cons : ∀α(α → (αlist → αlist))
  hd   : ∀α(αlist → α)
  tl   : ∀α(αlist → αlist)

will be deduced

  map : ∀α∀β((α → β) → (αlist → βlist)).

Types are built from type constants (bool...) and type variables (α,β,...) using type
operators (such as infixed → for functions and postfixed list for lists); a type-scheme

is a type with (possibly) quantification of type variables at the outermost.
Thus, the main result of this paper is that the type-scheme deduced for such a declaration
(and more generally, for any ML expression) is a principal type-scheme, i.e.
that any other type-scheme for the declaration is a generic instance of it. This is a
generalisation of Hindley’s result for Combinatory Logic [3].

ML may be contrasted with Algol 68, in which there is no polymorphism, and with
Russell [2], in which parametric types appear explicitly as arguments to polymorphic
functions. The generic types of Ada may be compared with type-schemes. For simplicity,
our definitions and results here are formulated for a skeletal language, since
their extension to ML is a routine matter. For example recursion is omitted since it
can be introduced by simply adding the polymorphic fixed-point operator

  fix : ∀α((α → α) → α)

and likewise for conditional expressions.

## 2 The language

Assuming a set Id of identifiers x the language Exp of expressions e is given by the
syntax

  e ::= x | e e' | λx.e | let x = e in e'

(where parentheses may be used to avoid ambiguity). Only the last clause extends the
ń-calculus. Indeed for type checking purposes every let expression could be eliminated
(by replacing x by e everywhere in e'), except for the important consideration
that in on-line use of ML declarations

  let x = e

are allowed, whose scope (e') is the remainder of the on-line session. As illustrated
in the introduction, it must be possible to assign type-schemes to the identifiers thus
declared.

Note that types are absent from the language Exp. Assuming a set of type variables α
and of primitive types ι, the syntax of types τ and of type-schemes σ is given by

  τ ::= α | ι | τ → τ
  σ ::= τ | ∀ασ

A type-scheme ∀α1...∀αnτ (which we may write ∀α1...αnτ) has generic type variables
α1...αn. A monotype µ is a type containing no type variables.

## 3 Type instantiation

If S is a substitution of types for type variables, often written [τ1/α1,...,τn/αn] or
[τi/αi], and σ is a type-scheme, then Sσ is the type-scheme obtained by replacing
each free occurrence of αi in σ by τi, renaming the generic variables of σ if necessary.
Then Sσ is called an instance of σ; the notions of substitution and instance extend
naturally to larger syntactic constructs containing type-schemes.

By contrast a type-scheme σ = ∀α1...αmτ has a generic instance σ' = ∀β1...βnτ'
if τ' = [τi/αi]τ for some types τ1,...,τm and the βj are not free in σ. In this case
we shall write σ > σ'. Note that instantiation acts on free variables, while generic
instantiation acts on bound variables. It follows that σ > σ' implies Sσ > Sσ'.

## 4 Semantics

The semantic domain V for Exp is a complete partial order satisfying the following
equations up to isomorphism, where Bi is a cpo corresponding to primitive type ιi:

  V = B0 + B1 + ··· + F +W (disjoint sum)
  F = V → V (function space)
  W = {·} (error element)

To each monotype µ corresponds a subset V , as detailed in [5]; if v ∈ V is in the
subset for µ we write v :µ. Further we write v :τ if v :µ for every monotype instance
µ of τ, and we write v :σ if v :τ for every τ which is a generic instance of σ.

Now let Env = Id → V be the domain of environments η. The semantic function
" : Exp → Env → V is given in [5]. Using it, we wish to attach meaning to assertions
of the form

  A |= e :σ

where e ∈ Exp and A is a set of assumptions of the form x :σ, x ∈ Id. If the assertion
is closed, i.e. if A and σ contain no free type variables, then the sentence is said to
hold iff, for every environment η, whenever η[[x]] :σ' for each member x :σ' of A,
it follows that "[[e]]η:σ. Further, an assertion holds iff all its closed instances hold.
Thus, to verify the assertion

  x :α, f :∀β(β → β) |= (f x):α

it is enough to verify it for every monotype µ in place of α. This example illustrates
that free type variables in an assertion are implicitly quantified over the whole
assertion, while explicit quantification in a type scheme has restricted scope.

The remainder of this paper proceeds as follows. First we present an inference system
for inferring valid assertions. Next we present an algorithm W for computing a typescheme
for any expression, under assumptions A. We then show that W is sound,
in the sense that any type-scheme it derives is derivable in the inference system. Finally
we show that W is complete, in the sense that [any] derivable type-scheme is an
instance of that computed by W .

## 5 Type inference

From now on we shall assume that A contains at most one assumption about each
identifier x. Ax stands for removing any assumption about x from A.

For assumptions A, expressions e and type-scheme σ we write

  A |- e :σ

if this instance may be derived from the following inference rules:

  TAUT: (x :σ ∈ A)
  A |- x :σ
  A |- e :σ
  INST: (σ > σ0
  )
  A |- e :σ
  0
  A |- e :σ
  GEN: (α not free in A)
  A |- e :∀ασ
  A |- e :τ
  0 → τ A |- e
  0
  :τ
  0
  COMB:
  A |- (e e0
  ):τ
  Ax ∪ {x :τ
  0
  } |- e :τ
  ABS:
  A |- (λx.e):τ
  0 → τ
  A |- e :σ Ax ∪ {x :σ} |- e
  0
  :τ
  LET:
  A |- (let x = e in e
  0
  ) :τ

The following example of a derivation is organised as a tree, in which each node
follows from those immediately above it by an inference rule.

  TAUT:
  x :α |- x :α
  ABS: |- (λx.x):α → α
  GEN: †
  |- (λx.x):∀α(α → α)
  TAUT:
  i :∀α(α → α) |- i :∀α(α → α)
  INST:
  i :∀α(α → α) |- i :(α → α) → (α → α)
  TAUT:
  i :∀α(α → α) |- i :∀α(α → α)
  INST:
  i :∀α(α → α) |- i :α → α
  COMB: ‡
  i :∀α(α → α) |- i i :α → α

  †
  |- (λx.x):∀α(α → α)
  ‡
  i :∀α(α → α) |- i i :α → α
  LET: |- (let i = (λx.x)in i i) :α → α

The following proposition, stating the semantic soundness of inference, can be proved
by induction on e.

Proposition 1 (Soundness of inference). If A |- e :σ then A |= e :σ.

We will also require later the following two properties of the inference system.

Proposition 2. If S is a substitution and A |- e :σ then SA |- e : Sσ. Moreover if there is
a derivation of A |- e :σ of height n then there is also a derivation of SA |- e : Sσ of height
less [than] or equal to n.

Proof. By induction on n. □

Lemma 1. If σ > σ' and Ax ∪ {x :σ'} |- e :σ' then also Ax ∪ {x :σ} |- e :σ'.

Proof. We construct a derivation of Ax ∪ {x :σ} |- e :σ' from that of Ax ∪ {x :σ'} |-
e :σ' by substituting each use of TAUT for x :σ' with x :σ, followed by an INST step to
derive x :σ'. Note that GEN steps remain valid since if α occurs free in σ then it also
occurs free in σ'.

## 6 The type assignment algorithm W

The type inference system itself does not provide an easy method for finding, given
A and e, a type-scheme σ such that A |- e :σ. We now present an algorithm W for
this purpose. In fact, W goes a step further. Given A and e, if W succeeds it finds
a substitution S and a type τ, which are most general in a sense to be made precise
below, such that

  SA |- e :τ.

To define W we require the unification algorithm of Robinson [6].

Proposition 3 (Robinson). There is an algorithm U which, given a pair of types, either
returns a substitution V or fails; further

(i) If U(τ,τ') returns V , then V unifies τ and τ', i.e. V τ = τ'.

(ii) If S unifies τ and τ' then U(τ,τ') returns some V and there is another substitution
R such that S = RV .

Moreover, V involves only variables in τ and τ'.
We also need to define the closure of a type τ with respect to assumptions A;

  A~ (τ) = ∀α1,...,αnτ where α1,...,αn

are the type variables occurring free in τ but not in A.

### Algorithm W .

W (A,e) = (S,τ) where 1

(i) If e is x and there is an assumption x :∀α1,...,αnτ' in A then S = Id2 and
τ = [βi/αi]τ' where the βi s are new.

(ii) If e is e1 e2 then letW (A,e2) = (S1,τ2) and W (S1 A,e2) = (S2,τ2) and U(S2 τ1,τ2 →
β) = V where β is new; then S = V S2 S1 and τ = V β.

(iii) If e is λx.e1 then let β be a new type variable and W (Ax ∪{x :β},e1) = (S1,τ1);
then S = S1 and τ = S1β → τ1.

(iv) If e is let x = e1 in e2 then letW (A,e1) = (S1,τ2) and W (S1 Ax ∪ {x : S1 A(τ1)},e2) =
(S2,τ2); then S = S2 S1 and τ = τ2 .

NOTE: When any of the conditions above is not met W fails. □

The following proposition proves that W meets our requirements.

Proposition 4 (Soundness of W ). If W (A,e) succeeds with (S,τ) then there is a derivation
of SA |- e :τ .

Proof. By induction on e using proposition 2.

It follows that there is also a derivation of SA |- e : SA(τ). We refer to SA(τ) as a
type-scheme computed by W for e under A.

## 7 Completeness of W

Given A and e, we will call σP a principal type-scheme of e under assumptions A iff

(i) A |- e :σP

(ii) Any other σ for which A |- e :σ is a generic instance of σP.

Our main result, restricted to the simple case where A contains no free type variables,
may be stated as follows:

  If A |- e :σ for some σ, then W computes a principal type scheme for e
  under A.

This is a direct corollary of the following general theorem which is a stronger result
suited to inductive proof:

Theorem (Completeness of W ). Given A and e, let A' be an instance of A and σ a
type-scheme such that

  A' |- e :σ

----

1 [There are obvious typographic errors in parts (ii) and (iv) which are in the original publication.
I have left the correction of these as an easy exercise for the reader.]

2 [Of course this is the identity (empty) substitution, not the set Id of identifiers.]

----

then

(i) W (A,e) succeeds.

(ii) If W (A,e) = (S,τ) then, for some substitution R,

  A' = RSA and RSA(τ) > σ.

In fact, from the theorem one also derives as corollaries that it is decidable whether
e has any type at all under the assumptions A, and that, if so, it has a principal typescheme
under A.

The detailed proofs of results in this paper, and related results, will appear in the first
author’s forthcoming Ph.D. Thesis.

## References

  [1] M. Coppo. An extended polymorphic type system for applicative languages. In Lecture Notes in Computer Science, volume 88, pages 192–204. Springer, 1980.

  [2] A. Demers and J. Donahue. Report on the programming language russell. Technical Report TR-79-371, Computer Science Department, Cornell University, 1979.

  [3] R. Hindley. The principal type-scheme of an object in combinatory logic. Transactions of the AMS, 146:29–60, 1969.

  [4] R. Milner M. Gordon and C. Wadsworth. Edinburgh LCF. In Lecture Notes in Computer Science, volume 78. Springer, 1979.

  [5] R. Milner. A theory of type polymorphism in programming. JCSS, 17(3):348–375, 1978.

  [6] J.A. Robinson. A machine-oriented logic based on the resolution principle. Journal of the ACM, 12(1):23–41, 1965.
