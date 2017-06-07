# A Second Look at Overloading

  Martin Odersky

  Universitat Karlsruhe <a name="rd"></a>[†](#d)

  (odersky@ira.uka.de)

  Philip Wadler

  University of Glasgow <a name="rdd"></a>[‡](#dd)

  (wadler@dcs.glasgow.ac.uk)

  Martin Wehr

  Universitat Karlsruhe
  (wehr@ira.uka.de)

## Abstract

  We study a minimal extension of the Hindley/Milner system that supports overloading and polymorphic records.
  We show that the type system is sound with respect to a standard untyped compositional semantics.
  We also show that every typable term in this system has a principal type and give an algorithm to reconstruct that type.

## 1 Introduction

  Arithmetic, equality, showing a value as a string: three operations guaranteed to give a language designer nightmares.
  Usually they are dealt with by some form of overloading; but which form is best?

  Even if we limit our attention to languages based on the highly successful Hindley/Milner type system, we find many differing treatments of overloading.
  The same language may treat different operators differently; different languages may treat the same operator differently; and the same language may treat the same operator differently over time.
  For instance, in Miranda arithmetic is defined only on a single numeric type; equality is a polymorphic function defined at all types, including abstract types where it breaks the abstraction barrier; and the show function may be defined by the user for new types.
  In the first version of SML equality was simply overloaded at all monomorphic types;
  while the second version introduced special equality type variables.

  Type classes were introduced into Haskell in order to provide a uniform framework for overloading [WB89].
  It must have been an idea whose time had come, as it was independently described by Kaes [Kae88].
  Since then type classes have attracted considerable attention, with many refinements and variants being described [NS91, NP93, HHPW94, Aug93, PJ93, Jon92b, CHO92, Jon93].
  They have also attracted some criticism [App93].

  In our view, one of the most serious criticisms of type classes is that a program cannot be assigned a meaning independent of its types.

  ---

  <a name="*"></a>[*](#r*) In Proc. FPCA'95 Conf. on Functional Programming Languages and Computer Architecture

  <a name="d"></a>[†](#rd)
  Institut fur Programmstrukturen, 76128 Karlsruhe, Germany.

  <a name="dd"></a>[‡](#rdd)
  Department of Computing Science, University of Glasgow, Glasgow G12 8QQ,

  ---

  A consequence of this is that two of the most celebrated properties of the Hindley/Milner type system are not satisfied in the presence of type classes: there is no semantic soundness result, and the principal types result holds only in a weak form.

  The semantic soundness result shows a correspondence between the typed static semantics of program and its untyped dynamic semantics.
  It is summarised by Milner's catchphrase `well typed programs cannot go wrong'.
  One cannot even formulate such a result for type classes, as no untyped dynamic semantics exists.

  The principal type result shows that every typable program has a single most general type.
  This is also true for type classes.
  However, much of the utility of this result arises from another property of the Hindley/Milner system:
  every typeable program remains typeable if all type declarations are removed from it, so type declarations are never required.
  This fails for type classes: some programs are inherently ambiguous, and require type declarations for disambiguation.
  Put another way: under Hindley/Milner, a program is untypeable only if it may have no meaning;
  under type classes, a program may be untypeable because it has too many meanings.

  The absence of these properties is not merely the lack of a technical nicety: they arise because the meaning of a program cannot be understood separately from its type.
  This reduces the range of ways of understanding programs available to a programmer, and reduces the range of ways of implementing programs available to a compiler.

### Restricting type classes

  By a simple restriction to type classes, we may ensure that a program possesses a meaning that can be determined independently of its type.

  Recall that a type class limits a type variable, say a, to range over only those types on which an overloaded operator is defined; the overloaded operator may have any type involving a.
  Here are some examples, representing in simpli ed form parts of the Haskell standard prelude.

    class (Num a) where
      (+) :: a -> a -> a
      (*) :: a -> a -> a
      neg :: a -> a
      fromInteger :: Integer -> a

    class (Eq a) where
      (==) :: (Eq a) => a -> a -> Bool

    class (Text a) where
      show :: a -> String
      showList :: [a] -> String
      read :: String -> a

  For instance, the first of these states that type `a` belongs to class `Num` only when there are operators `(+)`, `(*)`, `neg`, and `fromInteger` of the specified types defined for `a`.

  The restriction is as follows: for a type class over a type variable `a`, each overloaded operator must have a type of the form `a -> t`, where `t` may itself involve `a`.
  In the above, `(+)`, `(*)`, `neg`, `(==)`, and `show` satisfy this restriction, while `fromInteger`, `showList`, and `read` do not.

  Remarkably, this simple restriction enables one to construct an untyped dynamic semantics, and ensures that no ambiguity can arise: hence type soundness and the strong form of principal types do hold.
  The resulting system is still powerful enough to handle the overloading of arithmetic, equality, and showing a value as a string, but not powerful enough to handle the overloading of numerical constants or reading a string as a value.
  The latter are perhaps less essential than the former: neither Miranda nor SML support overloading of the latter sort, and Kaes considered only this restricted form of overloading in his original paper [Kae88].

  As an example of the value of this restriction, consider the phrase `[] == []`.
  In Haskell, this phrase as it stands is ambiguous, and hence meaningless:
  one must disambiguate by specifying the type of the list elements.
  This is because the meaning of the program is given by the translation `eqList eqElt [] []`,
  where `eqList` is equality on lists, and `eqElt` is equality over on the list elements.

  In our restricted system, we are guaranteed that the phrase `[] == []` has a meaning independent of types;
  and that all valid translations yield this meaning.
  The implementor has a choice: overloading may be implemented by run-time branching, corresponding to the untyped dynamic semantics of Section 3, or by compile-time translation, corresponding to the typed static semantics of Section 4.
  In the latter case, a valid translation of the program is `eqList undef [] []`,
  where `undef` is the function that is everywhere undefined;
  this is because coherence guarantees that if the program doesn't force a translation, then any translation will do.
  For unrestricted Haskell the compiler writer must choose a translation, because there is no dynamic semantics, and must choose `eqElt` rather that `undef`, because there is no suitable coherence result.

  Thus, our restriction of type classes ensures additional useful properties that hold.
  These additional properties in turn make it possible for us to consider a generalisation of type classes.

#### Generalising type classes

  Type classes constrain type variables to range over types at which certain overloaded operators are defined.
  This appears to be closely related to bounded polymorphism, which constrains type variables to range over types that are subtypes of a given type [CW85, BTCGS91].
  Indeed, one can use type classes to mimic bounded polymorphism for the usual subtyping relation on records [Pet94].
  But, annoyingly, this mimicry works only for monomorphic records; type classes are not quite powerful enough to handle polymorphic records.

  For instance, one would expect the operations `xcoord` and `ycoord` to apply to any record type that contains those fields,
  for instance it should apply both to a type `Point` containing just those two fields, and to a type `CPoint` that contains both those fields plus a colour.
  Here is how one can mimic such records in Haskell.

    class (Pointed a) where
      xcoord :: a -> Float
      ycoord :: a -> Float

    data Point = MkPoint Float Float
    data CPoint = MkCPoint Float Float Colour

    instance Pointed Point where
      xcoord (MkPoint x y) = x
      ycoord (MkPoint x y) = y

    instance Pointed CPoint where
      xcoord (MkCPoint x y c) = x
      ycoord (MkCPoint x y c) = y

    distance :: (Pointed a) => a -> Float
    distance p = sqrt (sqr (xcoord p) + sqr (ycoord p))

  Function `distance` computes the distance of a point from the origin.
  The type signature is optional, as it may be inferred given only the class declaration and the function body.

  Note, alas, that this mimicry depends on each field of the record having a monomorphic type that can appear in the class declaration.
  The polymorphic equivalent of the above would be to have operations first and second that return the corresponding components of either a pair or a triple, where these may have any type rather than being restricted to `Float`.
  But there is no way to do this in Haskell.

  The source of this problem is class declarations. For

    xcoord :: Point -> Float
    xcoord :: CPoint -> Float

  can arise as instantiations of the class declaration

    xcoord :: a -> Float .

  But for first the instances

    first :: (a,b) -> a
    first :: (a,b,c) -> a

  have no corresponding class declaration.

  We solve this problem by getting rid of class declarations.
  Instead of declaring that a group of operators belong to a class and specifying a type declaration, we only specify that an operator is overloaded and give no type declaration.

  Here is the previous example in our new notation.

    over xcoord
    over ycoord

    data Point = MkPoint Float Float
    data CPoint = MkCPoint Float Float Colour

    inst xcoord :: Point -> Float
         xcoord (MkPoint x y) = x

    inst ycoord :: Point -> Float
         ycoord (MkPoint x y) = y

    inst xcoord :: CPoint -> Float
         xcoord (MkCPoint x y c) = x

    inst ycoord :: CPoint -> Float
         ycoord (MkCPoint x y c) = y

    distance :: (xcoord,ycoord::a->Float) => a -> Float
    distance p = sqrt (sqr (xcoord p) + sqr (ycoord p))

  Again, the type declaration for `distance` may be inferred from its body
  (ignoring, for simplicity, the overloading of `sqrt`, `sqr`, and `+`).

  Furthermore, it is now possible to overload `first` and `second` on polymorphic pairs and triples.

    over first
    over second
    over third

    inst first :: (a,b) -> a
         first (x,y) = x

    inst second :: (a,b) -> b
         second (x,y) = x

    inst first :: (a,b,c) -> a
         first (x,y,z) = x

    inst second :: (a,b,c) -> b
         second (x,y,z) = y

    inst third :: (a,b,c) -> c
         third (x,y,z) = c

    demo :: (first::a->b,second::a->c) => a -> (c,b)
    demo r = (second r, first r)

  Function `demo` takes a pair or triple and returns its `second` and `first` components, in that order.
  Again, its type can be inferred.

  In short, eliminating class declarations makes type classes powerful enough to model bounded polymorphism.

  Eliminating class declarations means one need no longer decide in advance which operations belong together in a class.
  In many situations, this will be a positive advantage.
  For instance, if we're dealing with pairs we only want `first` and `second` grouped together, but if we're dealing with triples we'll want `third` as well.
  As a further example, consider the diculties that the Haskell designers had deciding how to group numeric operators into classes.
  This design is still argued: should + and * be in a `ring` class?
  The problem is exacerbated because there is no mechanism in Haskell whereby a user may break a given class into smaller classes.

  On the other hand, eliminating class declarations means that inferred types become more verbose: the type of every overloaded operator must be mentioned.
  Records provide some relief here, since they allow us to group related operations together, using a common overloaded identifier for them all.
  This is explained in more detail in Section 5.

#### Contributions of this work

  We combine the above restrictions and generalisations of type classes to define System O, a type system for overloading with the following properties.

  - System O possesses an untyped dynamic semantics, and satisfies a corresponding type soundness theorem.
  - System O has a strong principal types property.
  It is never necessary to add type declarations to disambiguate a program.
  - As with type classes, there is a standard dictionary transform which takes well-typed programs in System O into equivalent well-typed programs in the Hindley/Milner system.
  - System O is powerful enough to model a limited form of F-bounded polymorphism over records, including polymorphic records.

  ----

  We believe that this makes System O an interesting alternative to type classes.

## Related work.

  Overloading in polymorphic programming languages has first been studied by Kaes [Kae88] and Wadler and Blott [WB89].
  Similar concepts can be found in earlier work in symbolic algebra [JT81].
  This paper is very much in the tradition of Kaes in that overloading is restricted to functions.
  It can be seen as a simplification of his system that gets rid of all syntactic declarations of predicates or type classes.
  We extend the scope of his work by a proof of type soundness and the relationship to record typing.

  Much of the later work on overloading is driven by the design and implementation of Haskell's type classes, e.g. Nipkow et al.
  [NS91, NP93] on type reconstruction, Augustsson [Aug93] and Peterson and Jones [PJ93] on implementations, and Hall, Hammond, Peyton Jones and Wadler [HHPW94] on the formal definition of type classes in Haskell.
  We have already compared our system to that of Haskell.

  Other generalisations of Haskell type classes have been proposed.
  Wadler and Blott, and Jones, consider type classes with multiple type variables [WB89, Jon92b].
  Chen, Hudak and Odersky's parametric type classes [CHO92] also have multiple type variables, but a functional dependence is imposed between a primary class variable and dependent parameters.
  Parametric type classes can model container classes and records.
  Constructor classes generalize type classes to type constructors [Jon93].
  Constructor classes are very good at modeling containers with operations that mediate between similar containers with different element types.
  We consider it an important problem to determine whether our type system can be generalized to type constructors.

  All systems discussed so far implement an open world approach, where even empty classes, which do not have any instances at all, are considered legal.
  This approach works well in a system with separate compilation, where the type checker does not have complete knowledge of instance declarations.
  By contrast, the closed world approach of e.g.
  [Rou90, Smi91 , Kae92] rules out empty type schemes.
  Duggan and Ophel [DO94] support both approaches by distinguishing between open and closed kinds.
  Volpano [Vol93] has argued that many previously known open world systems are unsound.
  Volpano's negative results arise because he works with an untyped dynamic semantics for programs with type classes.
  We have argued here that this is not permissible for Haskell-like programs.
  Also, by proving type soundness with respect to the untyped dynamic semantics of System O, we show that Volpano's critique does not apply to open world systems in general.

  An alternative treatment of overloading regards it as a special case of dynamic typing, using a typecase construct to discriminate between overloaded variants [DRW95, HM95].
  A semantics along these lines was studied by Thatte [Tha94].
  Thatte's semantics maps programs to an explicitly typed polymorphic language similar to XML [MH88].
  Type classes denote sets of recursive types in this language.
  By contrast, our semantics maps to an untyped language where types and type schemes denote ideals.

### Outline

  The rest of this paper is organized as follows.
  Section 2 presents syntax and typing rules of System O.
  Section 3 develops a compositional semantics and proves a type soundness theorem.
  Section 4 discusses the dictionary passing transform.
  Section 5 presents an encoding of a polymorphic record calculus.
  Section 6 discusses type reconstruction and the principal type property.
  Section 7 concludes.

## 2 Type System

  We base our discussion on a simple functional language with overloaded identifiers.

#### syntax

  Figure 1 gives the syntax of terms and types.

    Unique variables       u ∈ U
    Overloaded variables   o ∈ O
    Constructors           k ∈ K = ∪{K_D | D ∈ D}
    Variables              x = u | o | k
    Terms                  e = x | λu.e | e e' | let u = e in e'
    Programs               p = e | inst o : στ = e in p

    Type variables         α ∈ A
    Datatype constructors  D ∈ D
    Type constructors      T ∈ T = D ∪ {->}
    Types                  τ = α | τ -> τ' | D τ1 ... τn        where n = arity(D)
    Type schemes           σ = τ | ∀α.πα => σ
    Constraints on α       πα= o1 : α -> τ1, ..., on : α -> τn  (n ≧ 0, with o1, ..., on distinct)
    Typotheses             Γ = x1 : σ1, ..., xn : σn            (n ≧ 0)

  Figure 1: Abstract syntax of System O.

  We split the variable alphabet into subalphabets `U` for unique variables, ranged over by `u`, `O` for overloaded variables, ranged over by `o`, and `K` for data constructors, ranged over by `k`.
  The letter `x` ranges over both unique and overloaded variables as well as constructors.
  We assume that every non-overloaded variable `u` is bound at most once in a program.

  The syntax of terms is identical to the language Exp in [<a name="rMil78"></a>[Mil78](#Mil78)].
  A program consists of a sequence of instance declarations and a term.
  An instance declaration (`inst o:στ = e in p`) overloads the meaning of the identifier `o` with the function given by `e` on all arguments that are constructed from the type constructor `T`.

  A type `τ` is a type variable, a function type, or a datatype.
  Datatypes are constructed from datatype constructors `D`.
  For simplicity, we assume that all value constructors and selectors of a datatype `Dτ1...τn` are prede ned, with bindings in some fixed initial typothesis `Γ0`.
  With user-defined type declarations, we would simply collect in `Γ0` all selectors and constructors actually declared in a given program.
  Let `K_D` be the set of all value constructors that yield a value in `D τ1, ..., τn` for some types `τ1, ..., τn`.

  We assume that there exists a bottom datatype `⫫ ∈ D` with `K_⫫ = ∅`.
  Note that this type is present in Miranda, where it is written `()`, but is absent in Haskell, where `()` has a value constructor, also written `()`.
  We let `T` range over datatype constructors as well as the function type constructor `(->)`, writing `(->) τ τ'` as a synonym for `τ -> τ'` .

  A type scheme `σ` consists of a type `τ` and quantifiers for some of the type variables in `τ`.
  Unlike with Hindley/Milner polymorphism, a quantified variable `α` comes with a constraint `πα`, which is a (possibly empty) set of bindings `o : α -> τ`.
  An overloaded variable `o` can appear at most once in a constraint.
  Constraints restrict the instance types of a type scheme by requiring that overloaded identifiers are defined at given types.
  The Hindley/Milner type scheme `∀α.σ` is regarded as syntactic sugar for `∀α.()⇒σ`.

  ----

#### typing rules

  Figure 2 defines the typing rules of System O.

    Γ ⊢ x : σ (x : σ ∈ Γ)              (TAUT)

    Γ, πα ⊢ e : σ     (α ∉ tv(Γ))
    ------------------------------------ (∀I)
    Γ ⊢ e : ∀α.πα => σ

    Γ, u : τ ⊢ e : τ'
    ------------------------------------ (->I)
    Γ ⊢ λu.e : τ -> τ'

    Γ ⊢ e : σ     Γ, u : σ ⊢ e' : τ
    ------------------------------------ (LET)
    Γ ⊢ let u = e in e' : τ

    Γ ⊢ x1 : σ1 ... Γ ⊢ xn : σn
    ------------------------------------ (SET)
    Γ ⊢ x1 : σ1, ..., xn : σn

    Γ ⊢ e : ∀α.πα => σ   Γ ⊢ [τ/α] πα
    ------------------------------------ (∀E)
    Γ ⊢ e : [τ/α] σ

    Γ ⊢ e : τ' -> τ      Γ ⊢ e' : τ'
    ------------------------------------ (->E)
    Γ ⊢ e e' : τ

    (o : σ_{T'} ∈ Γ => T ≠ T')
    Γ ⊢ e : στ    Γ, o : στ ⊢ p : σ'
    ------------------------------------ (INST)
    Γ ⊢ inst o : στ = e in p : σ'

  Figure 2: Typing rules for System O.

  The type system is identical to the original Hindley/Milner system, as presented in in [<a name="rDM82"></a>[DM82](#DM82)], except for two modifications.

  - In rule (∀I), the constraint πα on the introduced bound variable ff is traded between typothesis and type scheme.
  Rule (∀E) has as a premise an instantiation of the eliminated constraint.
  Constraints are derived using rule (SET).
  Note that this makes rules (∀I) and (∀E) symmetric to rules (->I) and (->E).

  - There is an additional rule (INST) for instance declarations.
  The rule is similar to (LET), except that the overloaded variable `o` has an explicit type scheme `στ` and it is required that the type constructor `T` is different in each instantiation of a variable `o`.


  We let `στ` range over closed type schemes that have `T` as outermost argument type constructor:

    στ  = T α1 ... αn -> τ    (tv(τ) ⊆ {α1, ..., αn})
        | ∀α.πα => στ'       (tv(πα) ⊆ tv(στ')) .

  The explicit declaration of `στ` in rule (INST) is necessary to ensure that principal types always exist.
  Without it, one might declare an instance declaration such as

    inst o = λx.x in p

  where the type constructor on which `o` is overloaded cannot be determined uniquely.

  The syntactic restrictions on type schemes `στ` enforce three properties:
  First, overloaded instances must work uniformly for all arguments of a given type constructor.
  Second the argument type must determine the result type uniquely.
  Finally, all constraints must apply to component types of the argument.
  The restrictions are necessary to ensure termination of the type reconstruction algorithm.
  An example is given in Section 6.

  The syntactic restrictions on type schemes `στ` also explain why the overloaded variables of a constraint `πα` must be pairwise different.
  A monomorphic argument to an overloaded function completely determines the instance type of that function.
  Hence, for any argument type `τ` and overloaded variable `o`, there can be only one instance type of `o` on arguments of type `τ`.
  By embodying this rule in the form of type variable constraints we enforce it at the earliest possible time.

## Example 2.1

  The following program fragment gives instance declarations for the equality function `(==)`.
  We adapt our notation to Haskell's conventions, writing `::` instead of `:`
  in a typing; writing `(o::a->t1)=>t2` instead of `∀α:(o : a -> τ1) ⇒ τ2`
  and writing `inst o :: s; o = e` instead of `inst o : σ = e`.

    inst (==) :: Int -> Int -> Bool
         (==) = primEqInt

    listEq :: ((==)::a->a->Bool) => [a]->[a]->Bool
    listEq [] []         = True
    listEq (x:xs) (y:ys) = x == y && listEq xs ys

    inst (==) :: ((==):: a->a->Bool) => [a]->[a]->Bool
         (==) = listEq

  Note that using `(==)` directly in the second instance declaration would not work, since instance declarations are not recursive.
  An extension of System O to recursive instance declaration would be worthwhile but is omitted here for simplicity.

### Example 2.2

The following example demonstrates an object-oriented style of programming, and shows where we are more expressive than Haskell's type classes.
  We write instances of a polymorphic class `Set`, with a member test and operations to compute the union, intersection, and difference of two sets.
  In Haskell, only sets of a fixed element type could be expressed.
  The example uses the record extension of Section 5;
  look there for an explanation of record syntax.

    type Set a sa
      = (union, inters, diff :: sa -> sa,
         member :: a -> Bool)
    inst set :: ((==)::a->a->Bool) => [a] -> Set a [a]
        set xs =
          (union  = \ys -> xs ++ ys,
           inters = \ys -> [y | y <- ys | y `elem` xs],
           diff   = \ys -> xs \\ ys,
           member = \y  -> y `elem` xs)

    inst set :: ((==),(<):: a->a->Bool)
                          => Tree a -> Set a (Tree a)
        set = ...

        -- m Here are some functions that work with sets.

    union :: (set: sa -> Set a sa) => sa -> sa -> sa
    union xs ys = #union (set xs) ys

    diff :: (set: sa -> Set a sa) => sa -> sa -> sa
    diff xs ys = #diff (set xs) ys

    simdiff :: (set: sa -> Set a sa) => sa -> sa -> sa
    simdiff xs ys = union (diff xs ys) (dif ys xs)

## 3 Semantics

  We now give a compositional semantics of System O and show that typings are sound with respect it.
  The semantics specifies lazy evaluation of functions, except for overloaded functions, which are strict in their first argument.
  Alternatively, we could have assumed strict evaluation uniformly for all functions, with little change in our definitions and no change in our results.

  The meaning of a term is a value in the `CPO V`, where `V` is the least solution of the equation

    V = W_⊥ + V -> V + Σ(k ∈ K) (k V1 ... V_{arity(k)})_⊥.

  Here, `(+)` and `Σ` denote coalesced sums <a name="r1"></a>[1](#1) and `V -> V` is the continuous function space.
  The value `W` denotes a type error - it is often pronounced "wrong".
  We will show that the meaning of a well-typed program is always different from "wrong".

  The meaning function `〚・〛` on terms is given in Figure 3.
  It takes as arguments a term and an environment `η` and yields an element of `V`.
  The environment `η` maps unique variables to arbitrary elements of `V`, and it maps overloaded variables to strict functions:

    η : U -> V ∪ O -> (V o-> V).

  The notation `η[x := v]` stands for extension of the environment `η` by the binding of `x` to `v`.

  Note that our semantics is more "lazy" in detecting wrong terms than Milner's semantics [<a name="rMil78"></a>[Mil78](#Mil78)].
  Milner's semantics always maps a function application `f W` to `W` whereas in our semantics `f W = W` only if `f` is strict.
  Our semantics correspond better to the dynamic type checking which would in practice be performed when an argument is evaluated.
  We anticipate no change in our results if Milner's stricter error checking is adopted.

  We now give a meaning to types.
  We start with types that do not contain type variables, also called monotypes.
  We use `μ` to range over monotypes.

  ----

   <a name="1"></a>[1](#r1) Injection and pro jection functions for sums will generally be left implicit to avoid clutter.

  ----

    〚x〛                    η = η (x)

    〚λu.e〛                 η = λv.〚e〛 η [u := v]

    〚k M1 ... Mn〛          η = k(〚M1〛 η) ... (〚Mn〛 η),
                                 where n = arity(k)

    〚e e'〛                 η = if 〚e〛 η ∈ V -> V then (〚e〛 η)(〚e'〛 η)
                                 else W

    〚let u = e in e'〛      η = 〚e'〛 η [u := 〚e〛 η]

    〚inst o : στ = e in p〛 η =
            if 〚e〛 η ∈ V -> V then
                〚p〛 η [o := extend(T,〚e〛 η,η(o))]
            else W
    where
      extend((->), f, g) =
        λv.if v ∈ V -> V then f(v) else g(v)
      extend (D, f, g) =
        λv.if ∃k ∈ K_D.v ∈ k {V...V | arity(k)} then f(v) else g(v).

  Figure 3: Semantics of terms.

  ----

  Following [<a name="rMil78"></a>[Mil78](#Mil78)] and [<a name="rMPS86"></a>[MPS86](#MPS86)], we let monotypes denote ideals.
  For our purposes, an ideal `I` is a set of values in `V` which does not contain `W`, is downward-closed and is limit-closed.
  That is, `y ∈ I` whenever `y ≦ x` and `x ∈ I` , and `∐ X ∈ I` whenever `x ∈ I` for all elements `x` of the directed set `X`.

  The meaning function `〚・〛`  takes a monotype `μ` to an ideal.
  It is defined as follows.

    〚D μ1 ... μm〛 =
      {⊥} ∪ ∪{k 〚μ1'〛 ... 〚μn'〛
                 | Γ0 ⊢ k : μ1' -> ... -> μn' -> D μ1 ... μm}
    〚μ1 -> μ2〛 =
      {f ∈ V -> V | v ∈ 〚μ1〛 => f v ∈ 〚μ2〛}.

  **Proposition 3.1** Let `μ` be a monotype. Then `〚μ〛` is an ideal.

  Proof: A straightforward induction on the structure of `μ`. □

  When trying to extend the meaning function to type schemes we encounter the diculty that instances of a constrained type scheme `∀α.πα=>σ` depend on the overloaded instances in the environment.
  This is accounted for by indexing the meaning function for type schemes with an environment.

  **Definition.** A monotype `μ` is a semantic instance of a type scheme `σ` in an environment `η`, written `η ⊨ μ ≼ σ`, iff this can be derived from the two rules below.

  (a) `η ⊨ μ ≼ μ`.

  (b) `η ⊨ μ ≼ (∀α.πα => σ)` if there is a monotype `μ'` such that `η ⊨ μ ≼ [μ'/α] σ` and `η(o) ∈ 〚 [μ'/α] τ〛`, for all `o : τ ∈ πα`.

  **Definition.** The meaning `〚σ〛_η` of a closed type scheme  `σ` is given by

    〚σ〛 η = ∩{〚μ〛 | η ⊨ μ ≼ σ}.

  **Definition.** `η ⊨ e1 : σ1, ..., en : σn` iff `〚ei〛 η ∈ 〚σi〛 η` , for `i = 1, ..., n`.

  The meaning of type schemes is compatible with the meaning of types:

  **Proposition 3.2** Let `μ` be a monotype, and let `η` be an environment.　Then `〚μ〛 η = 〚μ〛`.

  Proof: Direct from the definitions of `〚σ〛 η` and `≼`. □

  We now show that type schemes denote ideals.
  The proof needs two facts about the bottom type `⫫`.

  **Lemma 3.3** Let η be an environment.

  (a) `η ⊨ o : ⫫ -> μ`, for any variable `o`, monotype `μ`.

  (b) Let `σ = ∀α1 :πα1 => ... ∀αn :παn => τ` be a type scheme.
  Then `η ⊨ [⫫/α1, ..., ⫫/αn] τ ≼ σ`.

  Proof: (a) Assume  `v∈ 〚⫫〛` .
  Since `⫫` does not have any constructors, `〚⫫〛 = {⊥}`, hence  `v = ⊥`.
  Since `η(o)` is a strict function, `η(o)v = ⊥`, which is an element of every monotype.

  (b) Follows from the definition of `≼` and (a). □

  **Proposition 3.4** Let `σ` be a type scheme and let `η` be an environment. Then `〚σ〛 η` is an ideal.

  Proof:
  The closure properties are shown by straightforward inductions on the structure of `σ`.
  It remains to be shown that `W ∉ 〚σ〛`.
  By Lemma 3.3(b) there is a monotype `μ` such that `η ⊨ μ ≼ σ`.
  Hence, `〚σ〛 η ⊆ 〚μ〛`.
  But `〚μ〛` is an ideal and therefore does not contain `W`. □

  Proposition 3.4 expresses an important property of our semantics: every type scheme is an ideal, even if it contains a type variable constraint `o : α -> τ` , where `o` does not have any explicitly declared instances at all.
  Consequently, there is no need to rule out such a type scheme statically.
  This corresponds to Haskell's "open world" approach to type-checking, as opposed to the "closed world" approach of e.g.
[<a name="rSmi91"></a>[Smi91](#Smi91)].
  Interestingly, the only thing that distinguishes those two approaches in the semantics of type schemes is the absence or presence of the bottom type `⫫`  .

  We now show that System O is sound, i.e. that syntactic type judgements `Γ ⊢ p : σ` are reected by semantic type judgements `Γ ⊨ p : σ`.

  **Definition.** Let `e` be a term, let `Γ` be a closed typothesis, and let `σ` be a closed type scheme.
  Then `Γ ⊨ e : σ` iff, for all environments `η`, `η ⊨ Γ` implies `η ⊨ e : σ`.

  As a first step, we prove a soundness theorem for terms.
  This needs an auxiliary lemma, whose proof is straightforward.

  **Lemma 3.5** If `η ⊨ e : σ` and `η ⊨ μ ≼ σ` then  `η ⊨ e : μ`.

  **Theorem 3.6** (Type Soundness for Terms)
  Let `Γ ⊢ e : σ` be a valid typing judgement and let `S` be a substitution such that `SΓ` and `Sσ` are closed. Then `SΓ ⊨ e : Sσ`.

  Proof:
  Assume `Γ ⊢ e : σ` and `η ⊨ SΓ`.
  We do an induction on the derivation of `Γ ⊢ e : σ`.
  We only show cases (∀I), (∀E), whose corresponding inference rules differ from the Hindley/Milner system.
  The proofs of the other rules are similar to the treatment in [<a name="rMil78"></a>[Mil78](#Mil78)].

    Γ ⊢ u : σ ≻ u          (u : σ ∈ Γ)                 (TAUT)
    Γ ⊢ k : σ ≻ u          (k : σ ∈ Γ)                 (TAUT)
    Γ ⊢ o : σ ≻ u_{o,σ}    (o : σ ∈ Γ)                 (TAUT)

    Γ, o1 : τ1, ..., on : τn ⊢ e : σ ≻ e*    α ∉ tv(Γ)
    ---------------------------------------------------- (∀I)
    Γ ⊢ e : ∀α.(o1 : τ1, ..., on : τn) => σ
      ≻ λu_{o1,τ1}....λu_{on,τn}.e*

    Γ ⊢ e : ∀α.(o1 : τ1 ,..., on : τn) => σ ≻ e*
    Γ ⊢ oi : [τ/α] τi ≻ ei*       (i = 1, ..., n)
    ---------------------------------------------------- (∀E)
    Γ ⊢ e: [τ/α] σ
      ≻ e* e1* ... en*

    Γ, u : τ ⊢ e : τ' ≻ e*
    ------------------------------------------------- (->I)
    Γ ⊢ λu.e : τ -> τ
      ≻ λu.e*

    Γ ⊢ e1 : τ' -> τ ≻ e1*       Γ ⊢ e2 : τ' ≻ e2* 
    ------------------------------------------------- (->E)
    Γ ⊢ e1 e2 : τ
      ≻ e1* e2*

    Γ ⊢ e1 : σ ≻ e1*     Γ, u : σ ⊢ e2 : τ ≻ e2* 
    ------------------------------------------------- (LET)
    Γ ⊢ let u = e1 in e2 : τ
      ≻ let u = e1* in e2* : τ

    o : στ' ∈ Γ => T ≠ T'
    Γ ⊢ e : στ ≻ e       Γ, o : στ ⊢ p : σ' ≻ p*
    ------------------------------------------------ (INST)
    Γ ⊢ inst o : στ = e in p : σ'
      ≻ let u_{o,στ} = e* in p*

  Figure 4: The dictionary passing transform

  Case (∀I): Then the last step in the derivation is for some `α`, `πα`, `σ'` with `σ = ∀α.πα => σ'`.

    Γ, πα ⊢ e : σ'   α ∉ tv(Γ)
    --------------------------------
    Γ ⊢ e : ∀α.πα => σ'


  We have to show that `e ∈ 〚μ〛`, for all `μ` such that `η ⊨ η ≼ ∀α.S πα => S σ'`.
  Pick an arbitrary such `μ`.
  By definition of `(≼)`, there exists `a μ'` such that  `η ⊨ [μ'/α] (S πα)` and `η ⊨ μ ≼ [μ'/α] (Sσ')`.
  Let `S' = [μ'/α] o S`. Then `η ⊨ S'Γ` and `η ⊨ S'(Γ,πα)`.
  Since `α ∉ tv(Γ)`, `η ⊨ S'Γ` and therefore `η ⊨ S'(Γ, πα)`.
  Then by the induction hypothesis, `η ⊨ e : S' σ'`.
  It follows with Lemma 3.5 that `η ⊨ e : μ`.


  Case (∀E): Then the last step in the derivation is

    Γ ⊢ e : ∀α.πα => σ'    Γ ⊢ [τ / α] πα
    ------------------------------------------
    Γ ⊢ e : [τ / α] σ'

  for some `α`, `πα`, `σ'`, `τ` with `σ = [τ/α]σ'`.

  We have to show that `e ∈ 〚μ〛`, for all `μ` such that `η ⊨ μ ≼ [Sτ/α] Sσ'`.
  Pick an arbitrary such `μ`. By the induction hypothesis, `η ⊨ e : ∀α:Sπα => Sσ'` and `η ⊨ [Sτ/α] (Sπα)` .
  It follows with the definition of `≼` that `η ⊨ μ ≼ ∀α:Sπα => Sσ'`.
  Then by Lemma 3.5, `η ⊨ e : μ`. □

  We now extend the type soundness theorem to whole programs that can contain instance declarations.

  **Theorem 3.7** (Type Soundness for Programs)
  Let `Γ ⊢ p : σ` be a valid closed typing judgement. Then `Γ ⊨ p : σ`.

  Proof:
  By induction on the structure of `p`.
  If `p` is a term, the result follows from Theorem 3.6.
  Otherwise `p` is an instance declaration at top-level. Then the last step in the derivation of `Γ ⊢ p : σ`

      o : στ' ∈ Γ => T ≠ T'
      Γ ⊢ e : στ    Γ, o : στ ⊢ p : σ
      -----------------------------------
      Γ ⊢ inst o : στ = e in p' : σ

  for some type scheme `στ`.
  We have to show that `η ⊨ inst o : στ = e in p' : σ`:
  By Theorem 3.6, `η ⊨ e : στ` , which implies that `〚e〛 η` is a function.
  Therefore, `〚p〛 η = 〚p'〛 η [o := f]` where `f = extend(T,〚e〛 η,η(o))`.

  Our next step is to show that `f∈ 〚στ〛 η`.
  Let `μ` be such that `η ⊨ μ ≼ στ`. Then `μ = T μ1, ..., μn -> μ'`, for some monotypes `μ1, ..., μn, μ'`.

  Now assume that `v ∈ 〚Tμ1, ..., μn〛`.
  If `v = ⊥` then `f v = ⊥ ∈ 〚μ'〛`.
  Otherwise, by the definition of `extend`, `f v = 〚e〛 η v` and `〚e〛 η v ∈ 〚μ'〛`.
  In both cases `f v ∈ 〚μ'〛`.
  Since `v ∈ 〚Tμ1, ..., μn〛` was arbitrary, we have `f ∈ 〚μ〛`.
  Since `μ` was arbitrary, this implies `f ∈ [στ] η`

  It follows that `η [o := f] ⊨ o : στ`.
  Furthermore, since `η ⊨ Γ`, and `Γ` contains by the premise of rule (INST) no binding `o : στ`, we have that `η [o := f] ⊨ Γ`.
  Taken together, `η [o := 0 f] ⊨ Γ,o : στ` .
  By the induction hypothesis, `η [o := f] ⊨ p' : σ` , which implies the proposition. □

  A corollary of this theorem supports the slogan that "well typed programs do not go wrong".

**Corollary 3.8** Let
`Γ ⊢ p : σ` be a valid closed typing judgement
and let `η` be an environment. If `η ⊨ Γ` then `〚p〛 η ≠ W` .

  Proof: Immediate from Theorem 3.7 and Proposition 3.4. □

## 4 Translation

  This section studies the "dictionary passing" transform from System O to the Hindley/Milner system.
  Its central idea is to convert a term of type `∀α.πα => τ` to a function that takes as arguments implementations of the overloaded variables in `πα`.
  These arguments are also called "dictionaries".

  The target language of the translation is the Hindley/Milner system, which is obtained from System O by eliminating overloaded variables `o`, instance declarations, and constraints `πα` in type schemes.
  The translation of terms is given in Figure 4.
  It is formulated as a function of type derivations, where we augment type judgements with an additional component `e*` that defines the translation of a term or program `p`, e.g. `Γ ⊢ p : σ ≻ p*`.
  To ensure the coherence of the translation, we assume that the overloaded identifiers `oi` in a type variable constraint `{o1 : α -> τ1, ..., on : α -> τn}` are always ordered lexicographically.

  Types and type schemes are translated as follows.

                            τ* = τ
                 (∀α.e => σ)* = ∀α.σ*
    (∀α.o : α -> τ, πα => σ)* = ∀α.(α -> τ) -> (∀πα => σ)*

  The last clause violates our type syntax in that a type scheme can be generated as the result part of an arrow.
  This is compensated by defining

    τ -> ∀α.σ def= ∀α.τ -> σ.

  Bindings and typotheses are translated as follows.

                 (u : σ)* = u : σ* 
                 (o : σ)* = u_{o,σ} : σ*.
    o1 : σ1, ..., on : σn = (o1 : σ1)*, ..., (on : σn)*.

  This translates an overloaded variable `o` to a new unique variable `u_{o,σ}` , whose identity depends on both the name `o` and its type scheme, `σ`.

  Each derivation rule `Γ ⊢ p : σ`  in System O corresponds to a derivation of translated typotheses, terms and type schemes in the Hindley/Milner system.
  One therefore has:

  **Proposition 4.1** If `Γ ⊢ p : σ ≻ p*` is valid then `Γ* ⊢ p* : σ*` is valid in the Hindley/Milner system

  We believe that the translation preserves semantics in the following sense.

  **Conjecture** Let `p` be a program, `μ` be a monotype, and let `η` be an environment.
  Let `Γ` be a typothesis which does not contain overloaded variables.
  If `Γ ⊢ p : μ ≻ p*` and `η ⊨ Γ` then `〚p〛 η = 〚p*〛 η`.

  Although the above claim seems clearly correct, its formal proof is not trivial.
  Note that coherence of the translation would follow immediately from the above conjecture.
  Coherence, again, is a property that appears obvious but is notoriously tricky to demonstrate [<a name="rBlo91"></a>[Blo91](#Blo91), <a name="rJon92a"></a>[Jon92a](#Jon92a)], so it is perhaps not surprising that the above conjecture shares this property.

## 5 Relationship with Record Typing

  In this section we study an extension of our type system with a simple polymorphic record calculus similar to Ohori's [<a name="rOho92"></a>[Oho92](#Oho92)].
  Figure 5 details the extended calculus.
  We add to System O

  - record types `{l1:τ1、...、ln:τn}`,
  - record expressions `{l1 = e1、...、ln = en}`, and
  - selector functions `#l`.

  It would be easy to add record updates, as in the work of Ohori, but more dicult to handle record extension, as in the work of Wand [Wan87] or Remy [Rem89].
  Jones [Jon92a] has shown how to embed Remy's system of extensible records by extending unification to an AC theory for records and using (multi-parameter) type classes for stating the absence of fields in a record.
  Both updates and extensions are however omitted here for simplicity.

  Leaving open for the moment the type of selector functions, the system presented so far corresponds roughly to the way records are defined in Standard ML.
  Selectors are treated in Standard ML as overloaded functions.
  As with all overloaded functions, the type of the argument of a selector has to be known statically;
  if it isn't, an overloading resolution error results.

  Our record extension also treats selectors as overloaded functions but uses the overloading concept of System O.
  The most general type scheme of a selector `#l` is

    ∀β.∀α.(α ≦ {l : β}) => α -> β.

  This says that `#l` can be applied to records that have a field `l : τ`, in which case it will yield a value of type `τ`.
  The type scheme uses a subtype constraint `α ≦ ρ`.
  Subtype constraints are validated using the subtyping rules in Figure 5.
  In all other respects, they behave just like overloading constraints `o : α -> τ`.

  Example 5.1 The following program is typable in System O (where the typing of `max` is added for convenience).

      let max : ∀β.((<) : β -> β -> bool) =>
                  ∀α. (α ≦ {key : β}) => α -> α -> α
              = λx.λy. if #key x < #key y then y else x
      in
          max {key = 1, data = a} {key = 2, data = b}

  In Standard ML, the same program would not be typable since neither the argument type of the selector `#key` nor the argument type of the overloaded function `(<)` are statically known.

  Note that the bound variable in a subtype constraint can also appear in the constraining record type, as in

    ∀α.(α ≦ {l : α -> bool}) => [α]

  Hence, we have a limited form of F-bounded polymorphism [CCH+ 89] - limited since our calculus lacks the subsumption and contravariance rules often associated with bounded polymorphism [CW85].
  It remains to be seen how suitable our system is for modeling object-oriented programming.
  Some recent developments in object-oriented programming languages seem to go in the same direction, by restricting subtyping to abstract classes [SOM93].

  We now show that the record extension adds nothing essentially new to our language. We do this by presenting an encoding from System O with records to plain System O.
  The source of the encoding is a program with records, where we assume that the labels `l1, ..., ln` of all record expressions `{l1 = e1, ..., ln = en}` in the source program are sorted lexicographically (if they are not, just rearrange fields).
  The details of the encoding are as follows.

  1. Every record-field label `l` in a program is represented by
  an overloaded variable, which is also called `l`.

  2. For every record expression `{l1 = e1, ..., ln = en}` in a program, we add a fresh `n-ary` datatype `R_{l1...ln}` with a constructor of the same name and selectors as given by the declaration

      data R_{l1...ln} α1 ...αn = R_{l1...ln} α1 ...αn.

  3. For every datatype `R_{l1...ln}` created in Step 2 and every label `li(i = 1, ...,n)`, we add an instance declaration

        inst li : ∀_{α1...αn}.R_{l1...ln} α1 ...αn -> αi
              = λ (R_{l1...ln} x1 ... xn) :xi

    (where the pattern notation in the formal parameter is used for convenience).

  4. A record expression `{l1 = e1, ..., ln = en}` now translates to `R_{l1...ln} e1 ...en`.
  5. A selector function `#l` translates to `l`.
  6. A record type `{l1 : τ1, ..., ln : τn}` is translated to `Rl1 ... lnτ1...τn`.

  ---

    Additional Syntax
        Field labels        l  ∈ L
        Terms               e  = ... | #l | {l1 = e1, ..., ln = en}  (n ≧ 0)
        Record types        ρ  = {l1 : τ1, ..., ln : τn}             (n ≧ 0, with l1, ..., ln distinct)
        Types               τ  = ... | ρ
        Constraints on α    πα = ... | α ≦ ρ
        Typotheses          Γ  = ... | α ≦ ρ

    Subtyping Rules

        Γ, α ≦ ρ ⊢ α ≦ ρ                                            (Taut)

        Γ ⊢   {l1 : τ1, ..., ln : τn, ln+1 : τn+1, ..., ln+k : τn+k}
             ≦ {l1 : τ1, ..., ln : τn}                                (Rec)

    Additional Typing Rules

        Γ ⊢ e1 : τ1   ...   Γ ⊢ en : τn
        --------------------------------------------------------------({} I)
        Γ ⊢ {l1 = e1, ..., ln = en} : {l1 : τ1, ..., ln : τn}

        Γ ⊢ #l : ∀β: ∀α ≦ {l : β}.α -> β                            ({} E)

  Figure 5: Extension with record types.

  7. A subtype constraint `α ≦ {l1 : τ1, ..., ln : τn}` becomes an overloading constraint `l1 : α -> τ1, ..., ln : α -> τn`:

  Let `e†`, `σ†`, or `Γ†` be the result of applying this translation to a term `e`, a type scheme `σ`, or a typothesis `Γ`.
  Then one has:

  **Proposition 5.2** `Γ ⊢ e : τ` iff `Γ† ⊢ e† : τ†`.

  Proposition 5.2 enables us to extend the type soundness and principal type properties of System O to its record extension without having to validate them again.
  It also points to an implementation scheme for records, given an implementation scheme for overloaded identifiers.

  **Example 5.3** The program of Example 5.1 translates to

    inst data : ∀α∀β:R_{data,key} α β -> α
              = λR_{data,key} x y. x in
    inst key  : ∀α∀β:R_{data,key} α β -> β
              = λR_{data,key} x y. y in
    let max   : ∀β.((<) : β -> β -> bool) =>
                ∀α.(key : α -> β)=> α -> α -> α
              = λx.λy.if key x < key y then y else x
    in
          max (R_{data,key} 1 a) (R_{data,key} 2 b)

  Records can help to contain the number of overloaded identifiers in type signatures.
  The idea is to put related operations in a record which is constructed with a single overloaded identifier.
  The next example expresses shows how to model a simplified `Num` class in this way.
  In the Haskell-like syntax we use parentheses `(...)` instead of braces `{...}` for records.

    type Num a = (plus :: a -> a -> a,
                  minus:: a -> a -> a,
                  neg  :: a -> a)
    over num
    inst num :: Int -> Num Int
        num = ...

    (+), (-) :: (num :: a -> Num a) => a -> a -> a
    neg      :: (num :: a -> Num a) => a -> a
    (+) x y = #plus  (num x) x y
    (-) x y = #minus (num x) x y
    neg x   = #neg   (num x) x

  Note the similarity to dictionary passing.
  One shortcoming of this scheme with respect to Haskell's class declarations concerns subclassing.
  For instance, we could not pass a variable of type `(num :: a -> Num a) => a` to a function of type

    (num :: a -> (plus  :: a -> a -> Bool,
                  minus :: a -> a -> Bool)) => a -> b

  Even without introducing full subtyping on records it may be helpful to supplement our system with some way for dealing with this common case.
  Further experience will be required to determine this.

## 6 Type Reconstruction

  Figures 6 and 7 present type reconstruction and unification algorithm for System O.
  Compared to Milner's algorithm W [Mil78] there are two extensions.

  - The case of binding a type variable in the unification algorithm is extended.
    To bind a type variable `α` to a type `τ` the constraints of `Γα` have to be satisfied.
    The function `mkinst` ensures that type `τ` statisfies the constraints `Γα`.

  - The function `tp` is extended with a branch for instance declarations `inst o : στ = e in p`.
     In this case it must be checked that the inferred type `στ'` for the overloading term `e` is less general then the given type `στ`.

  We now state soundness and completeness results for the algorithms `unify` and `tp`.
  The proofs of these results are along the lines of [<a name="rChe94"></a>[Che94](#Che94)]; they are omitted here.

  We use the following abbreviations:

    Γα = {o : α -> τ | o : α -> τ ∈ Γ}
    ΓA = ∪_{α ∈ A} Γα

  where `A` is a set of type variables.

  **Definition**.
  A configuration is a pair of a typothesis `Γ` and a substitution `S` such that, for all `α ∈ dom(S)`, `Γα = ∅`.

    unify : (τ,τ) -> (Γ,S) -> (Γ,S)
    unify (τ1,τ2) (Γ,S) = case (S τ1,S τ2) of
      (α,α) =>
        (Γ,S)
      (α,τ),(τ,α) where α ∉ tv(τ) =>
        foldr mkinst (Γ \ Γα,[τ / α] o S) Γα
      (T τ1s,T τ2s) =>
        foldr unify (Γ,S) (zip(τ1s,τ2s))

    mkinst : (o : α -> τ) -> (Γ,S) -> (Γ,S)
    mkinst (o : α -> τ) (Γ,S) = case S α of
      β =>
        if ∃o : β -> τ' ∈ Γ
        then unify (τ,τ') (Γ,S)
        else (Γ ∪ {o : β -> [β / α] τ},S)
      T τs =>
        case {newinst(στ,Γ,S) | o : στ ∈ Γ} of
          {(τ1,Γ1,S1)} => unify (α -> τ,τ1) (Γ1,S1)

  Figure 6: Algorithm for constrained unification

  **Definition.** The following defines a preorder `≼` on substitutions and configurations and a preorder `≼Γ` on type schemes.
  If `X≼Y` we say that `Y` is more general than `X`.

  - `S' ≼ S` iff there is a substitution `R` such that `S' = R o S`.
  - `(Γ',S') ≼ (Γ,S)` iff `S' ≼ S`, `S'Γ' ⊢ S'Γdom(S')` and `Γ' ⊇ Γ \ Γdom(S')`.
  - `σ' ≼Γ σ` iff, for all `u ∉ dom(Γ)` , `Γ ⊢ u : σ` implies `Γ ⊢ u : σ'`.

  **Definition.** A constrained unification problem is a pair of tuples `(τ1,τ2)(Γ,S)` where `τ1,τ2` are types and `(Γ,S)` is a configuration.

  A configuration `(Γ',S')` is called a unifying configuration for `(τ1,τ2)(Γ,S)` iff `(Γ',S') ≼ (Γ,S)` and `S' τ1 = S' τ2`.

  The unifying configuration `(Γ',S')` is most general iff `(Γ'',S'') ≼ (Γ',S')`, for every other unifying configuration `(Γ'',S'')`.

  **Definition.** A typing problem is a triple `(p,Γ,S)` where `(Γ,S)` is a configuration and `p` is a term or program with `fv(p) ⊆ dom(Γ)`.

  A typing solution of a typing problem `(p,Γ,S)` is a triple `(σ,Γ',S')` where `(Γ',S') ≼ (Γ,S)` and `S'Γ' ⊢  p : S'σ`.

  The typing solution `(σ'',Γ',S')` is most general iff for every other typing solution `(σ'',Γ'',S'')` it holds `(Γ'',S'') ≼ (Γ',S')` and `S'' σ'' ≼S''Γ'' S'' σ`.

  **Theorem 6.1** Let `(τ1,τ2)(Γ,S)` be a constrained unification problem

  (a) If `unify(τ1,τ2)(Γ,S) = (Γ',S')` then `(Γ',S')` is a most general unifying configuration for `(τ1,τ2)(Γ,S)`.

  (b) If `unify(τ1,τ2)(Γ,S)` fails then there exists no unifying configuration for `(τ1,τ2)(Γ,S)`.

  **Theorem 6.2** Let `(p,Γ,S)` be a typing problem.

  (a) If `tp (p,Γ,S) = (σ,Γ',S')` then `(σ,Γ',S')` is a most generalsolution of `(p,Γ,S)`.

  (b) If `tp (p,Γ,S)` fails, then `(p,Γ,S)` has no solution.

  As a corollary of Theorem 6.2, we get that every typable program has a principal type, which is found by `tp`.

  **Corollary 6.3** (Principal Types) Let `(p,Γ, id)` be a typing problem such that `tv(Γ) = ∅`.

  (a) Assume `gen (tp (p,Γ,id)) = (σ',Γ',S)`  and `let σ = Sσ'`. Then

      Γ ⊢ p : σ                   and
      Γ ⊢ p : σ'' => σ'' ≼Γ σ,  for all type schemes σ''.

  (b) If `tp (p,Γ,id)`  fails then there is no type scheme `σ` such that `Γ ⊢ p : σ`.

  The termination of `unif` and `mkinst` critically depends on the form of overloaded type schemes `στ` :

    στ = T α1 ... αn -> τ   (tv(τ) ⊆ {α1, ..., αn})
       | ∀α.πα => στ'      (tv(πα) ⊆ tv(στ')).

  We show with an example why `στ` needs to be parametric in the arguments of `T`. Consider the following program, where  `k ∈ KT` .

    p = let (;) x y = y in
        inst o : ∀α.o : α -> α => T(Tα) -> α
            = λk (k x).o x
        in λx.λy.λf. o x; o y; f (k y); f x

  Then computation of `tp (p,∅,id)` leads to a call `tp (f x,Γ,S)` with `x : α, y : β, f : Tβ -> δ ∈ Γ`.
  This leads in turn to a call `unify (α,Tβ)(Γ,S)` where the following assumptions hold:

  - `στ = ∀α.o : α -> α => T(Tα) -> α`
  - `Γ ⊇ {o : α -> α,o : β -> β,o : στ}`,
  - `S` is a substitution with  `α,β ∉ dom (S)`.

  ----

  Unfolding `unify` gives `mkinst (o : α -> α)(Γ \ Γα, S')` where `S' = [Tβ/α] o S`, which leads in turn to the following two calls:

  1. `newinst(στ, Γ \ Γα, S') = (T (T γ) -> γ, Γ', S')` where `Γ' ⊇ {o : β -> β, o : γ -> γ, o : στ}` and `γ` is a fresh type variable, and

  2. `unify (α -> α, T (T γ) -> γ)(Γ', S')`.

  ----

  Since `S' α = Tβ`, unfolding of (2) results in an attempt to unify `Tβ` and `T (Tγ))`, which leads to the call `unify (β, Tγ)(Γ,S)`.
  This is equivalent to the original call `unify (α, Tβ)(Γ,S)` modulo renaming of `α,β` to `β,γ`.
  Hence, `unify` would loop in this situation.

  The need for the other restrictions on `στ` are shown by similar constructions.
  It remains to be seen whether a more general system is feasible that lifts these restrictions, e.g. by extending unification to regular trees [Kae92].

## 7 Conclusion

  We have shown that a rather modest extension to the Hindley/Milner system is enough to support both overloading and polymorphic records with a limited form of F-bounded polymorphism.
  The resulting system stays firmly in the tradition of ML typing, with type soundness and principal type properties completely analogous to the Hindley/Milner system.

    newinst     : (σ,Γ,S)  ->  (τ,Γ,S)
    newinst(∀α.πα => σ,Γ,S)
                = let β a new type variable
                  in  newinst
                  ([β/α] σ,Γ ∪ [β/α] πα,S)
    newinst(τ,Γ,S)
                = (τ,Γ,S)

    skolemize   : (σ,Γ,S) -> (τ,Γ,S)
    skolemize (∀α.πα => σ,Γ,S)
                = let T a new 0-ary type constructor
                  in  skolemize
                      ([T/α] σ,Γ ∪ [T/α] πα,S)
    skolemize (τ,Γ,S)
                = (τ,Γ,S)

    gen         : (τ,Γ,S) -> (σ,Γ,S)
    gen (σ,Γ,S) = if ∃ α.α ∈ tv(Sσ) \ tv(S (Γ\Γα))
                  then gen (∀α.Γα => σ,Γ\Γα,S)
                  else (σ,Γ,S)

    tp          : (p,Γ,S) -> (τ,Γ,S)
    tp (u,Γ,S)  = if u : σ ∈ Γ
                  then newinst (σ,Γ,S)

    tp (o,Γ,S)  = newinst (∀β ∀α: (o : α -> β) => α -> β,Γ,S)

    tp (λu.e,Γ,S)
                = let α a new type variable
                    (τ,Γ1,S1) = tp (e, Γ ∪ {u : α},S)
                  in (α -> τ,Γ1,S1)

    tp (e e',Γ,S)
                = let (τ1,Γ1,S1) = tp (e,Γ,S)
                      (τ2,Γ2,S2) = tp (e,Γ1,S1)
                      α a new type variable
                      (Γ3,S3) = unify (τ1,τ2 -> α) (Γ2,S2)
                  in (α,Γ3,S3)

    tp (let u = e in e',Γ,S)
                = let (σ,Γ1,S1) = gen (tp (e,Γ,S))
                  in tp (e,Γ1 ∪ {u : σ},S1)

    tp (inst o : στ = e in p,Γ,S)
                = let (στ',Γ1,S1) = gen (tp (e,Γ,S))
                      (τ2,Γ2,S2) = skolemize (στ,Γ1,S1)
                      (τ3,Γ3,S3) = newinst (στ',Γ2,S2)
                  in if ∀o : στ' ∈ Γ . T ≠ T' ∧
                        unify (τ2,τ3)(Γ3,S3) defined then
                          tp (p, Γ1 ∪ {o : στ},S1)

  Figure 7: Type reconstruction algorithm for System O

  The encoding of a polymorphic record calculus in System O indicates that there might be some deeper relationships between F-bounded polymorphism and overloading.
  This is also suggested by the similarities between the dictionary transform for type classes and the Penn translation for bounded polymorphism [BTCGS91].
  A study of these relationships remains a topic for future work.

## Acknowledgments

  We are grateful to Kung Chen and John Maraist for valuable comments on previous drafts of this paper.
  The section on records was motivated in part by a discussion led by Simon Peyton Jones, Mark Jones and others on the Haskell mailing list.
  Many other discussions with numerous participants have also contributed to this work.

# References

  [<a name="App93"></a>[App93](#rApp93)] Andrew W. Appel. A critique of standard ML. Journal of Functional Programming, 3(4), 1993.

  [<a name="Aug93"></a>[Aug93](#rAug93)] Lennart Augustsson. Implementing Haskell overloading. In Proc. ACM Conf. on Functional Programming Languages and Computer Architecture, pages 65-73, June 1993.

  [<a name="Blo91"></a>[Blo91](#rBlo91)] Stephen Blott. An Approach to Overloading with Polymorphism. PhD thesis, Department of Computer Science, University of Glasgow, Sept 1991.

  [<a name="BTCGS91"></a>[BTCGS91](#rBTCGS91)] Val Breazu-Tannen, Thierry Coquand, Carl A. Gunter, and Andre Scedrov. Inheritance as implicit coercion. Information and Computation, 93:172-221, 1991.

  [<a name="CCH+89"></a>[CCH+89](#rCCH+89)] Peter Canning, William Cook, Walter Hill, Walter Olthoff, and John C. Mitchell. F-bounded polymorphism for object-oriented programming. In Functional Programming Lan-guages and Computer Architecture, pages 273-280, September 1989.

  [<a name="Che94"></a>[Che94](#rChe94)] Kung Chen. A Parametric Extension of Haskell's Type Classes. PhD thesis, Yale University, New Haven, Connecticut, December 1994. YALEU/DCS/RR-1057.

  [<a name="CHO92"></a>[CHO92](#rCHO92)] Kung Chen, Paul Hudak, and Martin Odersky. Parametric type classes. In Proc. ACM Conf. on Lisp and Functional Programming, pages 170-181, June 1992.

  [<a name="CW85"></a>[CW85](#rCW85)] Luca Cardelli and Peter Wegner. On under-standing types, data abstraction, and polymorphism. Computing Surveys, 17(4):471-522, De- cember 1985.

  [<a name="DM82"></a>[DM82](#rDM82)] Luis Damas and Robin Milner. Principal type schemes for functional programs. In Proc. 9th ACM Symposium on Principles of Program-ming Languages, January 1982.

  [<a name="DO94"></a>[DO94](#rDO94)] Dominic Duggan and John Ophel. Kinded parametric overloading. Technical Report CS-94-35, University of Waterloo, September 1994.

  [<a name="DRW95"></a>[DRW95](#rDRW95)] Catherine Dubois, Francois Rouaix, and Pierre Weis. Extensional polymorphism. In Proc. 22nd ACM Symposium on Principles of Pro-gramming Languages, pages 118-129, January 1995.

  [<a name="HHPW94"></a>[HHPW94](#rHHPW94)] Cordelia Hall, Kevin Hammond, Simon Pey-ton Jones, and Philip Wadler. Type classes in Haskell. In Proc. 5th European Symposium on Programming, pages 241-256, 1994. Springer LNCS 788.

  [<a name="HM95"></a>[HM95](#rHM95)] Robert Harper and Greg Morrisett. Compiling polymorphism using intensional type analysis. In Proc. 22nd ACM Symposium on Principles of Programming Languages, pages 130-141, January 1995.

  [<a name="Jon92a"></a>[Jon92a](#rJon92a)] Mark P. Jones. Qualified Types: Theory and Practice. D.phil. thesis, Oxford University, September 1992.

  [<a name="Jon92b"></a>[Jon92b](#rJon92b)] Mark P. Jones. A theory of qualified types. In Proc. 4th European Symposium on Programming, pages 287-306, February 1992. Springer LNCS 582.

  [<a name="Jon93"></a>[Jon93](#rJon93)] Mark P. Jones. A system of constructor classes: Overloading and implicit higher-order polymorphism. In Proc. ACM Conf. on Functional Programming Languages and Computer Architecture, pages 52-61, June 1993.

  [<a name="JT81"></a>[JT81](#rJT81)] R.D. Jenks and B.M. Trager. A language for computational algebra. In Proc. ACM Symposium on Symbolic and Algebraic Manipulation, pages 22-29, 1981.

  [<a name="Kae88"></a>[Kae88](#rKae88)] Stefan Kaes. Parametric overloading. In Proc. 2nd European Symposium on Programming. Springer-Verlag, 1988. Springer LNCS 300.

  [<a name="Kae92"></a>[Kae92](#rKae92)] Stefan Kaes. Type inference in the presence of overloading, subtyping, and recursive types. In Proc. ACM Conf. on Lisp and Functional Programming, pages 193-204, June 1992.

  [<a name="MH88"></a>[MH88](#rMH88)] John C. Mitchell and Robert Harper. The essence of ML. In Conference Record of the Fifteenth Annual ACM Symposium on Principles of Programming Languages, pages 28-46. ACM, ACM Press, January 1988.

  [<a name="Mil78"></a>[Mil78](#rMil78)] Robin Milner. A theory of type polymorphism in programming. Journal of Computer and System Sciences, 17:348-375, Dec 1978.

  [<a name="MPS86"></a>[MPS86](#rMPS86)] D. MacQueen, G. Plotkin, and R. Sethi. An ideal model for recursive polymorphic types. Information and Control, 71:95-130, 1986.

  [<a name="NP93"></a>[NP93](#rNP93)] Tobias Nipkow and Christian Prehofer. Type checking type classes. In Proc. 20th ACM Symposium on Principles of Programming Languages, pages 409-418, 1993.

  [<a name="NS91"></a>[NS91](#rNS91)] Tobias Nipkow and Gregor Snelting. Type classes and overloading resolution via order-sorted unification. In Proc. ACM Conf. on Functional Programming Languages and Computer Architecture, pages 1-14, August 1991. Springer LNCS 523.

  [<a name="Oho92"></a>[Oho92](#rOho92)] Atsushi Ohori. A compilation method for ML-style polymorphic record calculi. In Proc. 19th ACM Symposium on Principles of Programming Languages, pages 154-165, January 1992.

  [<a name="Pet94"></a>[Pet94](#rPet94)] John Peterson. Structures in Yale Haskell. draft paper, 1994.

  [<a name="PJ93"></a>[PJ93](#rPJ93)] John Peterson and Mark Jones. Implementing type classes. In Proc. ACM Conf. on Programming Language Design and Implementation, pages 227-236, June 1993. SIGPLAN Notices 28(6).

  [<a name="Rem89"></a>[Rem89](#rRem89)] D. Remy. Typechecking records and variants in a natural extension of ML. In Proc. 16th ACM Symposium on Principles of Programming Languages, pages 77-88. ACM, January 1989.

  [<a name="Rou90"></a>[Rou90](#rRou90)] François Rouaix. Safe run-time overloading. In Proc. 17th ACM Symposium on Principles of Programming Languages, pages 355-366, January 1990.

  [<a name="Smi91"></a>[Smi91](#rSmi91)] Geoffrey S. Smith. Polymorphic type inference for languages with overloading and subtyping. PhD thesis, Cornell University, Ithaca, NY, August 1991.

  [<a name="SOM93"></a>[SOM93](#rSOM93)] Clemens Szyperski, Stephen Omohundro, and Stephan Murer. Engineering a programming language: The type and class system of Sather. In Programming Languages and System Architectures, pages 208-227. Springer Verlag, Lecture Notes in Computer Science 782, November 1993.

  [<a name="Tha94"></a>[Tha94](#rTha94)] Satish R. Thatte. Semantics of type classes revisited. In Proc. Conference on Lisp and Functional Programming, pages 208-219, 1994.

  [<a name="Vol93"></a>[Vol93](#rVol93)] Dennis Volpano. A critique of type systems for global overloading. Computer Science Technical Report NPSCS-94-006, Naval Postgraduate School, October 1993.

  [<a name="Wan87"></a>[Wan87](#rWan87)] Mitchell Wand. Complete type inference for simple objects. In Proc.IEEE Symposium on Logic in Computer Science, pages 37-44, June 1987.

  [<a name="WB89"></a>[WB89](#rWB89)] Philip Wadler and Stephen Blott. How to make ad-hoc polymorphism less ad-hoc. In Proc. 16th ACM Symposium on Principles of Programming Languages, pages 60-76, January 1989.


