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
  In the first version of SML equality was simply overloaded at all monomorphic types; while the second version introduced special equality type variables.

  Type classes were introduced into Haskell in order to provide a uniform framework for overloading [WB89].
  It must have been an idea whose time had come, as it was independently described by Kaes [Kae88].
  Since then type classes have attracted considerable attention, with many refinements and variants being described [NS91, NP93, HHPW94, Aug93, PJ93, Jon92b, CHO92, Jon93].
  They have also attracted some criticism [App93].

  In our view, one of the most serious criticisms of type classes is that a program cannot be assigned a meaning independent of its types.


  ---

  In Proc. FPCA'95 Conf. on Functional Programming Languages and Computer Architecture

 Institut fur Programmstrukturen, Universitat Karlsruhe, 76128 Karlsruhe, Germany; e-mail:odersky,wehr@ira.uka.de y

Department of Computing Science, University of Glasgow, Glasgow G12 8QQ, Scotland; e-mail: wadler@dcs.gla.ac.uk

  ---

  A consequence of this is that two of the most celebrated properties of the Hindley/Milner type system are not satisfied in the presence of type classes: there is no semantic soundness result, and the principal types result holds only in a weak form.

  The semantic soundness result shows a correspondence between the typed static semantics of program and its untyped dynamic semantics.
  It is summarised by Milner's catchphrase `well typed programs cannot go wrong'.
  One cannot even formulate such a result for type classes, as no untyped dynamic semantics exists.

  The principal type result shows that every typable program has a single most general type.
  This is also true for type classes.
  However, much of the utility of this result arises from another property of the Hindley/Milner system: every typeable program remains typeable if all type declarations are removed from it, so type declarations are never required.
  This fails for type classes: some programs are inherently ambiguous, and require type declarations for disambiguation.
  Put another way: under Hindley/Milner, a program is untypeable only if it may have no meaning; under type classes, a program may be untypeable because it has too many meanings.

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

  For instance, the first of these states that type a belongs to class Num only when there are operators (+), (*), neg, and fromInteger of the specified types defined for a.

  The restriction is as follows: for a type class over a type variable a, each overloaded operator must have a type of the form a -> t, where t may itself involve a.
  In the above, (+), (*), neg, (==), and show satisfy this restriction, while fromInteger, showList, and read do not.

  Remarkably, this simple restriction enables one to construct an untyped dynamic semantics, and ensures that no ambiguity can arise: hence type soundness and the strong form of principal types do hold.
  The resulting system is still powerful enough to handle the overloading of arithmetic, equality, and showing a value as a string, but not powerful enough to handle the overloading of numerical constants or reading a string as a value.
  The latter are perhaps less essential than the former: neither Miranda nor SML support overloading of the latter sort, and Kaes considered only this restricted form of overloading in his original paper [Kae88].

  As an example of the value of this restriction, consider the phrase [] == [].
  In Haskell, this phrase as it stands is ambiguous, and hence meaningless: one must disambiguate by specifying the type of the list elements.
  This is because the meaning of the program is given by the translation eqList eqElt [] [], where eqList is equality on lists, and eqElt is equality over on the list elements.

  In our restricted system, we are guaranteed that the phrase [] == [] has a meaning independent of types; and that all valid translations yield this meaning.
  The implementor has a choice: overloading may be implemented by run-time branching, corresponding to the untyped dynamic semantics of Section 3, or by compile-time translation, corresponding to the typed static semantics of Section 4.
  In the latter case, a valid translation of the program is eqList undef [] [], where undef is the function that is everywhere undefined; this is because coherence guarantees that if the program doesn't force a translation, then any translation will do.
  For unrestricted Haskell the compiler writer must choose a translation, because there is no dynamic semantics, and must choose eqElt rather that undef, because there is no suitable coherence result.

  Thus, our restriction of type classes ensures additional useful properties that hold.
  These additional properties in turn make it possible for us to consider a generalisation of type classes.

### Generalising type classes

  Type classes constrain type variables to range over types at which certain overloaded operators are defined.
  This appears to be closely related to bounded polymorphism, which constrains type variables to range over types that are subtypes of a given type [CW85, BTCGS91].
  Indeed, one can use type classes to mimic bounded polymorphism for the usual subtyping relation on records [Pet94].
  But, annoyingly, this mimicry works only for monomorphic records; type classes are not quite powerful enough to handle polymorphic records.

  For instance, one would expect the operations xcoord and ycoord to apply to any record type that contains those fields, for instance it should apply both to a type Point containing just those two fields, and to a type CPoint that contains both those fields plus a colour.
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

  Function distance computes the distance of a point from the origin.
  The type signature is optional, as it may be inferred given only the class declaration and the function body.

  Note, alas, that this mimicry depends on each field of the record having a monomorphic type that can appear in the class declaration.
  The polymorphic equivalent of the above would be to have operations first and second that return the corresponding components of either a pair or a triple, where these may have any type rather than being restricted to Float.
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

  Again, the type declaration for distance may be inferred from its body (ignoring, for simplicity, the overloading of
  sqrt, sqr, and +).

  Furthermore, it is now possible to overload first and second on polymorphic pairs and triples.

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

  Function demo takes a pair or triple and returns its second and first components, in that order.
  Again, its type can be inferred.

  In short, eliminating class declarations makes type classes powerful enough to model bounded polymorphism.

  Eliminating class declarations means one need no longer decide in advance which operations belong together in a class.
  In many situations, this will be a positive advantage.
  For instance, if we're dealing with pairs we only want first and second grouped together, but if we're dealing with triples we'll want third as well.
  As a further example, consider the diculties that the Haskell designers had deciding how to group numeric operators into classes.
  This design is still argued: should + and * be in a `ring' class? The problem is exacerbated because there is no mechanism in Haskell whereby a user may break a given class into smaller classes.

  On the other hand, eliminating class declarations means that inferred types become more verbose: the type of every overloaded operator must be mentioned.
  Records provide some relief here, since they allow us to group related operations together, using a common overloaded identifier for them all.
  This is explained in more detail in Section 5.

### Contributions of this work

  We combine the above restrictions and generalisations of type classes to define System O, a type system for overloading with the following properties.

  - System O possesses an untyped dynamic semantics, and satisfies a corresponding type soundness theorem.
  - System O has a strong principal types property.
  It is never necessary to add type declarations to disambiguate a program.
  - As with type classes, there is a standard dictionary transform which takes well-typed programs in System O into equivalent well-typed programs in the Hindley/Milner system.
  - System O is powerful enough to model a limited form of F-bounded polymorphism over records, including polymorphic records.


  We believe that this makes System O an interesting alternative to type classes.

## Related work.

  Overloading in polymorphic programming languages has first been studied by Kaes [Kae88] and Wadler and Blott [WB89].
  Similar concepts can be found in earlier work in symbolic algebra [JT81].
  This paper is very much in the tradition of Kaes in that overloading is restricted to functions.
  It can be seen as a simplification of his system that gets rid of all syntactic declarations of predicates or type classes.
  We extend the scope of his work by a proof of type soundness and the relationship to record typing.

  Much of the later work on overloading is driven by the design and implementation of Haskell's type classes, e.g.
  Nipkow et al.
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

We base our discussion on a simple functional language with overloaded identifiers.
Figure 1 gives the syntax of terms and types.
We split the variable alphabet into subalphabets U for unique variables, ranged over by u, O for overloaded variables, ranged over by o, and K for data constructors, ranged over by k.
The letter x ranges over both unique and overloaded variables as well as constructors.
We assume that every non-overloaded variable u is bound at most once in a program.

The syntax of terms is identical to the language Exp in [Mil78 ].
A program consists of a sequence of instance declarations and a term.
An instance declaration (inst o : T = e in p) overloads the meaning of the identifier o with the function given by e on all arguments that are constructed from the type constructor T .

A type  is a type variable, a function type, or a datatype.
Datatypes are constructed from datatype constructors D.
For simplicity, we assume that all value constructors and selectors of a datatype D 1 ::: n are prede ned, with bindings in some fixed initial typothesis 0 .
With user-defined type declarations, we would simply collect in 0 all selectors and constructors actually declared in a given program.
Let KD be the set of all value constructors that yield a value in D 1; ::::; n for some types 1; ::::; n.
We assume that there exists a bottom datatype ? 2 D with K? = ;.
Note that this type is present in Miranda, where it is written (), but is absent in Haskell, where () has a value constructor, also written ().
We let T range over datatype constructors as well as the function type constructor (!), writing (!)   0 as a synonym for  !  0 .

A type scheme  consists of a type  and quantifiers for some of the type variables in  .
Unlike with Hindley/Milner polymorphism, a quantified variable ff comes with a constraint ff, which is a (possibly empty) set of bindings o : ff !  .
An overloaded variable o can appear at most once in a constraint.
Constraints restrict the instance types of a type scheme by requiring that overloaded identifiers are defined at given types.
The Hindley/Milner type scheme 8ff: is regarded as syntactic sugar for 8ff:() ) .
Figure 2 defines the typing rules of System O.
The type system is identical to the original Hindley/Milner system, as presented in in [DM82], except for two modifications.

- In rule (8I), the constraint ff on the introduced bound variable ff is traded between typothesis and type scheme.
Rule (8E) has as a premise an instantiation of the eliminated constraint.
Constraints are derived using rule (SET).
Note that this makes rules (8I) and (8E) symmetric to rules (!I) and (!E).

- There is an additional rule (INST) for instance declarations.
The rule is similar to (LET), except that the overloaded variable o has an explicit type scheme T and it is required that the type constructor T is different in each instantiation of a variable o.


We let T range over closed type schemes that have T as outermost argument type constructor:

T = T ff1 ::: ffn !  (tv( )  fff1; : : : ; ffng) j 8ff:ff ) 0 T (tv(ff)  tv(0 T )):

The explicit declaration of T in rule (INST) is necessary to ensure that principal types always exist.
Without it, one might declare an instance declaration such as

    inst o = x:x in p

where the type constructor on which o is overloaded cannot be determined uniquely.

The syntactic restrictions on type schemes T enforce three properties: First, overloaded instances must work uniformly for all arguments of a given type constructor.
Second the argument type must determine the result type uniquely.
Finally, all constraints must apply to component types of the argument.
The restrictions are necessary to ensure termination of the type reconstruction algorithm.
An example is given in Section 6.

The syntactic restrictions on type schemes T also explain why the overloaded variables of a constraint ff must be pairwise different.
A monomorphic argument to an overloaded function completely determines the instance type of that function.
Hence, for any argument type  and overloaded variable o, there can be only one instance type of o on arguments of type  .
By embodying this rule in the form of type variable constraints we enforce it at the earliest possible time.

Example 2.1

The following program fragment gives instance declarations for the equality function (==).
We adapt our notation to Haskell's conventions, writing :: instead of : in a typing; writing (o::a->t1)=>t2 instead of 8ff:(o : a ! 1) ) 2; and writing inst o :: s; o = e instead of inst o :  = e.


inst (==) :: Int -> Int -> Bool (==) = primEqInt listEq :: ((==)::a->a->Bool) => [a]->[a]->Bool listEq [] [] = True listEq (x:xs) (y:ys) = x == y && listEq xs ys inst (==) :: ((==):: a->a->Bool) => [a]->[a]->Bool (==) = listEq 


Note that using (==) directly in the second instance declaration would not work, since instance declarations are not recursive.
An extension of System O to recursive instance declaration would be worthwhile but is omitted here for simplicity.


Example 2.2

The following example demonstrates an object-oriented style of programming, and shows where we are more expressive than Haskell's type classes.
We write instances of a polymorphic class Set, with a member test and operations to compute the union, intersection, and difference of two sets.
In Haskell, only sets of a fixed element type could be expressed.
The example uses the record extension of Section 5; look there for an explanation of record syntax.

type Set a sa = (union, inters, diff :: sa -> sa, member :: a -> Bool |} inst set :: ((==)::a->a->Bool) => [a] -> Set a [a] set xs = (union = \ys -> xs ++ ys, inters = \ys -> [y | y <- ys | y `elem` xs], diff = \ys -> xs \\ ys, member = \y -> y `elem` xs) inst set :: ((==),(<):: a->a->Bool) => Tree a -> Set a (Tree a) set = ...

m Here are some functions that work with sets.

union :: (set: sa -> Set a sa) => sa -> sa -> sa union xs ys = #union (set xs) ys diff :: (set: sa -> Set a sa) => sa -> sa -> sa diff xs ys = #diff (set xs) ys simdiff :: (set: sa -> Set a sa) => sa -> sa -> sa simdiff xs ys = union (diff xs ys) (diff ys xs)

## 3 Semantics

We now give a compositional semantics of System O and show that typings are sound with respect it.
The semantics specifies lazy evaluation of functions, except for overloaded functions, which are strict in their first argument.
Alternatively, we could have assumed strict evaluation uniformly for all functions, with little change in our definitions and no change in our results.
The meaning of a term is a value in the CPO V, where V is the least solution of the equation

V = W? + V ! V + X k2K (k V1 ::: Varity(k))?:

Here, (+) and P denote coalesced sums 1 and V ! V is the continuous function space.
The value W denotes a type error { it is often pronounced "wrong".
We will show that the meaning of a well-typed program is always different from "wrong".

The meaning function [[]] on terms is given in Figure 3.
It takes as arguments a term and an environment  and yields an element of V.The environment  maps unique variables to arbitrary elements of V, and it maps overloaded variables to strict functions:

 : U ! V [ O ! (V ! V):

The notation [x := v] stands for extension of the environment  by the binding of x to v.

Note that our semantics is more \lazy" in detecting wrong terms than Milner's semantics [Mil78 ].
Milner's semantics always maps a function application f W to W whereas in our semantics f W = W only if f is strict.
Our semantics correspond better to the dynamic type checking which would in practice be performed when an argument is evaluated.
We anticipate no change in our results if Milner's stricter error checking is adopted.

We now give a meaning to types.
We start with types that do not contain type variables, also called monotypes.
We use μ to range over monotypes.
Following [Mil78 ] and

----
1 Injection and pro jection functions for sums will generally be left implicit to avoid clutter.
----

Figure 3: Semantics of terms.

[MPS86], we let monotypes denote ideals.
For our purposes, an ideal I is a set of values in V which does not contain W, is downward-closed and is limit-closed.
That is, y 2 I whenever y  x and x 2 I , and F X 2 I whenever x 2 I for all elements x of the directed set X.

The meaning function [[]] takes a monotype μ to an ideal.
It is defined as follows.

Proposition 3.1 Let μ be a monotype. Then [[μ]] is an ideal.

Proof: A straightforward induction on the structure of μ. □

When trying to extend the meaning function to type schemes we encounter the diculty that instances of a constrained type scheme 8ff:ff )  depend on the overloaded instances in the environment.
This is accounted for by indexing the meaning function for type schemes with an environment.

Definition.

A monotype μ is a semantic instance of a type scheme  in an environment , written  j= μ μ , iff this can be derived from the two rules below.

(a)  j= μ μ μ.

(b)  j= μ μ (8ff:ff ) )
if there is a monotype μ0 such that  j= μ μ [μ0 =ff] and (o) 2 [[[μ0 =ff] ]], for all o :  2 ff.

Definition.

The meaning [[]] of a closed type scheme  is given by

    [[]] = \ f[[μ]] j  j= μ μ g:
    
Definition.

 j= e1 : 1; : : : ; en : n iff [[ei]] 2 [[i]], for i = 1; : : : ; n.

The meaning of type schemes is compatible with the meaning of types:

Proposition 3.2 Let μ be a monotype, and let  be an environment.　Then [[μ]] = [[μ]].

Proof: Direct from the definitions of [[]] and μ. □

We now show that type schemes denote ideals.
The proof needs two facts about the bottom type ? .

Lemma 3.3 Let  be an environment.
(a)  j= o : ?? ! μ, for any variable o, monotype μ.
(b) Let  = 8ff1 :ff 1 ) : : : 8ffn:ff n )  be a type scheme.
Then  j= [? =ff1; : : : ; ? =ffn] μ .

Proof: (a) Assume v 2 [[? ]].
Since ? does not have any constructors, [[? ]] = f?g, hence v = ?.
Since (o) is a strict function, (o)v = ?, which is an element of every monotype.
(b) Follows from the definition of μ and (a). □

Proposition 3.4 Let  be a type scheme and let  be an environment.
Then [[]] is an ideal.
Proof: The closure properties are shown by straightforward inductions on the structure of .
It remains to be shown that W 62 [[]].
By Lemma 3.3(b) there is a monotype μ such that  j= μ μ .
Hence, [[]]  [[μ]].
But [[μ]] is an ideal and therefore does not contain W.
2 Proposition 3.4 expresses an important property of our semantics: every type scheme is an ideal, even if it contains a type variable constraint o : ff !  , where o does not have any explicitly declared instances at all.
Consequently, there is no need to rule out such a type scheme statically.
This corresponds to Haskell's \open world" approach to type-checking, as opposed to the \closed world" approach of e.g.
[Smi91].
Interestingly, the only thing that distinguishes those two approaches in the semantics of type schemes is the absence or presence of the bottom type ? .

We now show that System O is sound, i.e.
that syntactic type judgements ` p :  are reected by semantic type judgements j= p : .

Definition.

Let e be a term, let be a closed typothesis, and let  be a closed type scheme.
Then j= e :  iff, for all environments ,  j= implies  j= e : .

As a first step, we prove a soundness theorem for terms.
This needs an auxiliary lemma, whose proof is straightforward.

Lemma 3.5 If  j= e :  and  j= μ μ  then  j= e : μ.

Theorem 3.6 (Type Soundness for Terms) Let ` e :  be a valid typing judgement and let S be a substitution such that S and S are closed.
Then S j= e : S.

Proof: Assume ` e :  and  j= S.
We do an induction on the derivation of ` e : .
We only show cases (8I), (8E), whose corresponding inference rules differ from the Hindley/Milner system.
The proofs of the other rules are similar to the treatment in [Mil78 ].

Case (8I): Then the last step in the derivation is


Figure 4: The dictionary passing transform

for some ff, ff, 0
with  = 8ff:ff ) 0
.
We have to show
that e 2 [[μ]], for all μ such that  j= μ μ 8ff:Sff ) S0
.
Pick an arbitrary such μ.
By definition of (μ), there exists
a μ0
such that  j= [μ0
=ff](Sff) and  j= μ μ [μ0
=ff](S0
).
Let S0
= [μ0
=ff]  S.
Then  j= S0
ff and  j= μ μ S0
0
.
Since ff 62 tv(),
 j= S0

and therefore  j= S0
(;
ff). Then
by the induction hypothesis,  j= e : S0
0
. It follows with
Lemma 3.5 that  j= e : μ.


Case (8E): Then the last step in the derivation is

` e : 8ff:ff ) 0

` [ =ff]ff

` e : [ =ff]0
for some ff, ff, 0
,  with  = [ =ff]0
. We have to show
that e 2 [[μ]], for all μ such that  j= μ μ [S =ff]S0
. Pick
an arbitrary such μ. By the induction hypothesis,  j= e :
8ff:Sff ) S0
and  j= [S =ff](Sff). It follows with the
definition of μ that  j= μ μ 8ff:Sff ) S0
. Then by
Lemma 3.5,  j= e : μ. 2
We now extend the type soundness theorem to whole programs
that can contain instance declarations.
Theorem 3.7 (Type Soundness for Programs)
Let
` p :  be a valid closed typing judgement. Then

j= p : .
Proof: By induction on the structure of p. If p is a term, the
result follows from Theorem 3.6. Otherwise p is an instance
declaration at top-level. Then the last step in the derivation
of
` p :  is
o : T 0 2
) T 6= T 0

` e : T ;
o : T ` p : 

` inst o : T = e in p
0
: 
for some type scheme T . We have to show that  j= inst o :
T = e in p
0
: . By Theorem 3.6,  j= e : T , which implies
that [[e]] is a function. Therefore, [[p]] = [[p
0
]][o := f ]
where f = extend(T ; [[e]]; (o)).
Our next step is to show that f 2 [[T ]]. Let μ be
such that  j= μ μ T . Then μ = T μ1 ; : : : ; μn ! μ0
,
for some monotypes μ1 ; : : : ; μn; μ0
. Now assume that v 2
[[T μ1; : : : ; μn]]. If v = ? then f v = ? 2 [[μ0
]]. Otherwise, by
the definition of extend, f v = [[e]] v, and [[e]] v 2 [[μ0
]]. In
both cases f v 2 [[μ0
]]. Since v 2 [[T μ1; : : : ; μn]] was arbitrary,
we have f 2 [[μ]]. Since μ was arbitrary, this implies f 2
[[T ]]
It follows that [o := f ] j= o : T . Furthermore, since
 j= ,
and
contains by the premise of rule (INST) no
binding o : T , we have that [o := f ] j= .
Taken together,
[o := f ] j= ;
o : T . By the induction hypothesis, [o :=
f ] j= p
0
: , which implies the proposition. 2
A corollary of this theorem supports the slogan that \well
typed programs do not go wrong".
Corollary 3.8 Let
` p :  be a valid closed typing judgement
and let  be an environment. If  j=
then [[p]] 6= W.
Proof: Immediate from Theorem 3.7 and Proposition 3.4. 2

## 4 Translation

This section studies the \dictionary passing" transform from System O to the Hindley/Milner system.
Its central idea is to convert a term of type 8ff:ff )  to a function that takes as arguments implementations of the overloaded variables in ff.
These arguments are also called \dictionaries".
The target language of the translation is the Hindley/Milner system, which is obtained from System O by eliminating overloaded variables o, instance declarations, and constraints ff in type schemes.
The translation of terms is given in Figure 4.
It is formulated as a function of type derivations, where we augment type judgements with an additional component e

that defines the translation of a term
or program p, e.g.
` p :   p

. To ensure the coherence of
the translation, we assume that the overloaded identifiers oi
in a type variable constraint fo1 : ff ! 1 ; : : : ; on : ff ! ng
are always ordered lexicographically.
Types and type schemes are translated as follows.


= 
(8ff: ) )

= 8ff:
(8ff:o : ff !  ; ff ) )

= 8ff:(ff !  ) ! (8ff ) )

The last clause violates our type syntax in that a type
scheme can be generated as the result part of an arrow.
7
This is compensated by defining
 ! 8ff:
def
= 8ff: ! :
Bindings and typotheses are translated as follows.
(u : )

= u : 
(o : )

= uo; : 
:
o1 : 1 ; : : : ; on : n = (o1 : 1)

; : : : ; (on : n)

:
This translates an overloaded variable o to a new unique
variable uo; , whose identity depends on both the name o
and its type scheme, .
Each derivation rule
` p :  in System O corresponds
to a derivation of translated typotheses, terms and type
schemes in the Hindley/Milner system. One therefore has:
Proposition 4.1 If
` p :   p

is valid then
` p

: 
is valid in the Hindley/Milner system
We believe that the translation preserves semantics in
the following sense.
Conjecture Let p be a program, μ be a monotype, and let
 be an environment. Let
be a typothesis which does not
contain overloaded variables. If
` p : μ  p

and  j=
then [[p]] = [[p

]].
Although the above claim seems clearly correct, its formal
proof is not trivial. Note that coherence of the translation
would follow immediately from the above conjecture. Coherence,
again, is a property that appears obvious but is
notoriously tricky to demonstrate [Blo91, Jon92a], so it is
perhaps not surprising that the above conjecture shares this
property.
5 Relationship with Record Typing
In this section we study an extension of our type system
with a simple polymorphic record calculus similar to Ohori's
[Oho92]. Figure 5 details the extended calculus. We add to
System O
 record types fl1 : 1; : : : ; ln : ng,
 record expressions fl1 = e1 ; :::; ln = eng, and
 selector functions #l.
It would be easy to add record updates, as in the work
of Ohori, but more dicult to handle record extension, as
in the work of Wand [Wan87] or Remy [Rem89]. Jones
[Jon92a] has shown how to embed Remy's system of extensible
records by extending unification to an AC theory for
records and using (multi-parameter) type classes for stating
the absence of fields in a record. Both updates and extensions
are however omitted here for simplicity.
Leaving open for the moment the type of selector functions,
the system presented so far corresponds roughly to
the way records are defined in Standard ML. Selectors are
treated in Standard ML as overloaded functions. As with
all overloaded functions, the type of the argument of a selector
has to be known statically; if it isn't, an overloading
resolution error results.
Our record extension also treats selectors as overloaded
functions but uses the overloading concept of System O. The
most general type scheme of a selector #l is
8fi :8ff:(ff  fl : fig) ) ff ! fi :
This says that #l can be applied to records that have a field
l :  , in which case it will yield a value of type  . The
type scheme uses a subtype constraint ff  . Subtype constraints
are validated using the subtyping rules in Figure 5.
In all other respects, they behave just like overloading constraints
o : ff !  .
Example 5.1 The following program is typable in System
O (where the typing of max is added for convenience).
let max : 8fi :((<) : fi ! fi ! bool) )
8ff:(ff  fkey : fig) ) ff ! ff ! ff
= x:y:if #key x < #key y then y else x
in
max fkey = 1; data = ag fkey = 2; data = bg
In Standard ML, the same program would not be typable
since neither the argument type of the selector #key nor the
argument type of the overloaded function (<) are statically
known.
Note that the bound variable in a subtype constraint can
also appear in the constraining record type, as in
8ff:(ff  fl : ff ! boolg) ) [ff]
Hence, we have a limited form of F-bounded polymorphism
[CCH+
89] | limited since our calculus lacks the subsumption
and contravariance rules often associated with bounded
polymorphism [CW85]. It remains to be seen how suitable
our system is for modeling object-oriented programming.
Some recent developments in object-oriented programming
languages seem to go in the same direction, by restricting
subtyping to abstract classes [SOM93].
We now show that the record extension adds nothing
essentially new to our language. We do this by presenting
an encoding from System O with records to plain System O.
The source of the encoding is a program with records, where
we assume that the labels l1 ; : : : ; ln of all record expressions
fl1 = e1; :::; ln = eng in the source program are sorted lexicographically
(if they are not, just rearrange fields). The
details of the encoding are as follows.
1. Every record-field label l in a program is represented by
an overloaded variable, which is also called l.
2. For every record expression fl1 = e1; :::; ln = eng in
a program, we add a fresh n-ary datatype Rl1 :::ln with a
constructor of the same name and selectors as given by the
declaration
data Rl1:::ln ff1 ::: ffn = Rl1:::ln ff1 ::: ffn:
3. For every datatype Rl1:::ln created in Step 2 and every
label li (i = 1; :::; n), we add an instance declaration
inst li : 8ff1:::ffn:Rl1:::ln ff1 ::: ffn ! ffi
= (Rl1 :::ln x1 ::: xn):xi
(where the pattern notation in the formal parameter is used
for convenience).
4. A record expression fl1 = e1; :::; ln = eng now translates
to Rl1:::ln e1 ::: en.
5. A selector function #l translates to l.
6. A record type fl1 : 1; :::; ln : ng is translated to
Rl1:::ln 1 ::: n.
8
Additional Syntax
Field labels l 2 L
Terms e = : : : j #l j fl1 = e1; : : : ; ln = eng (n  0)
Record types  = fl1 : 1; : : : ; ln : ng (n  0, with l1 ; : : : ; ln distinct)
Types  = : : : j 
Constraints on ff ff = : : : j ff  
Typotheses
= : : : j ff  
Subtyping Rules
(Taut) ;
ff   ` ff  
` fl1 : 1; : : : ; ln : n; ln+1 : n+1; : : : ; ln+k : n+k g (Rec)
 fl1 : 1; : : : ; ln : ng
Additional Typing Rules
(f gI)
` e1 : 1 : : :
` en : n

` fl1 = e1; : : : ; ln = eng : fl1 : 1; : : : ; ln : ng

` #l : 8fi :8ff  fl : fig:ff ! fi (f gE)
Figure 5: Extension with record types.
7. A subtype constraint ff  fl1 : 1; :::; ln : ng becomes an
overloading constraint l1 : ff ! 1; : : : ; ln : ff ! n:
Let e
y
, y
, or y
be the result of applying this translation
to a term e, a type scheme , or a typothesis .
Then one
has:
Proposition 5.2
` e :  iff y
` e
y
: 
y
.
Proposition 5.2 enables us to extend the type soundness and
principal type properties of System O to its record extension
without having to validate them again. It also points to an
implementation scheme for records, given an implementation
scheme for overloaded identifiers.
Example 5.3 The program of Example 5.1 translates to
inst data : 8ff8fi :Rdata;key ff fi ! ff
= Rdata;key x y: x in
inst key : 8ff8fi :Rdata;key ff fi ! fi
= Rdata;key x y: y in
let max : 8fi :((<) : fi ! fi ! bool) )
8ff:(key : ff ! fi) ) ff ! ff ! ff
= x:y:if key x < key y then y else x
in
max (Rdata;key 1 a) (Rdata;key 2 b)

Records can help to contain the number of overloaded identifiers in type signatures.
The idea is to put related operations in a record which is constructed with a single overloaded identifier.
The next example expresses shows how to model a simplified Num class in this way.
In the Haskell-like syntax we use parentheses (...) instead of braces {...} for records.

type Num a = (plus :: a -> a -> a,
minus:: a -> a -> a,
neg :: a -> a)
over num
inst num :: Int -> Num Int
num = ...
(+), (-) :: (num :: a -> Num a) => a -> a -> a
neg :: (num :: a -> Num a) => a -> a
(+) x y = #plus (num x) x y
(-) x y = #minus (num x) x y
neg x = #neg (num x) x

Note the similarity to dictionary passing.
One shortcoming of this scheme with respect to Haskell's class declarations concerns subclassing.
For instance, we could not pass a variable of type (num :: a -> Num a) => a to a function of type

    (num :: a -> (plus :: a -> a -> Bool,
    minus :: a -> a -> Bool)) => a -> b

Even without introducing full subtyping on records it may be helpful to supplement our system with some way for dealing with this common case.
Further experience will be required to determine this.

## 6 Type Reconstruction

Figures 6 and 7 present type reconstruction and unification algorithm for System O.
Compared to Milner's algorithm W [Mil78] there are two extensions.

- The case of binding a type variable in the unification algorithm is extended.
To bind a type variable ff to a type  the constraints of  have to be satisfied.
The function mkinst ensures that type  statisfies the constraints .
- The function tp is extended with a branch for instance declarations inst o : T = e in p.
In this case it must be checked that the inferred type 0 T for the overloading term e is less general then the given type T .
We now state soundness and completeness results for the algorithms unif y and tp.
The proofs of these results are along the lines of [Che94]; they are omitted here.
We use the following abbreviations:

 = fo : ff !  j o : ff !  2 g
A
= [ff2A

where A is a set of type variables.
Definition. A configuration is a pair of a typothesis
and
a substitution S such that, for all ff 2 dom(S),
 = ;.
9
unif y : ( ;  ) ! (;
S) ! (;
S)
unif y (1; 2) (;
S) = case (S1; S2 ) of
(ff; ff) )
(;
S)
(ff;  ); ( ; ff) where ff 62 tv( ) )
f oldr mkinst (n
; [ =ff]  S)

(T  1; T  2) )
f oldr unif y (;
S) (zip ( 1;  2))
mkinst : (o : ff !  ) ! (;
S) ! (;
S)
mkinst (o : ff !  ) (;
S) = case Sff of
fi )
if 9o : fi ! 
0
2
then unif y ( ;  0
) (;
S)
else (
[ fo : fi ! [fi=ff] g; S)
T  )
case fnewinst (T ; ;
S) j o : T 2 g
of
f(1 ; 1;
S1 )g ) unif y (ff !  ; 1) (1;
S1 )
Figure 6: Algorithm for constrained unification
Definition. The following defines a preorder μ on substitutions
and configurations and a preorder μ
on type schemes.
If X μ Y we say that Y is more general than X.
 S0
μ S iff there is a substitution R such that S0
= RS.
 (0
; S0
) μ (;
S) iff S0
μ S, S0
0
` S0
dom(S0
) and
0

n dom(S0
) .
 0
μ
 iff, for all u 62 dom(),

` u :  implies

` u : 0
.
Definition. A constrained unification problem is a pair of
tuples (1; 2)(;
S) where 1; 2 are types and (;
S) is a
configuration.
A configuration (0
; S0
) is called a unifying configuration
for (1; 2)(;
S) iff (0
; S0
) μ (;
S) and S0
1 = S0
2 .
The unifying configuration (0
; S0
) is most general iff
(00
; S00 ) μ (0
; S0
), for every other unifying configuration
(00
; S00 ).
Definition. A typing problem is a triple (p; ;
S) where
(;
S) is a configuration and p is a term or program with
fv(p)  dom().
A typing solution of a typing problem (p; ;
S) is a triple
(; 0
; S0
) where (0
; S0
) μ (;
S) and S0
0
` p : S0
.
The typing solution (; 0
; S0
) is most general iff for every
other typing solution (00
; 00
; S00 ) it holds (00
; S00) μ (0
; S0
)
and S0000 μS0000
S00.
Theorem 6.1 Let (1; 2 )(;
S) be a constrained unification
problem
(a) If unif y(1; 2 )(;
S) = (0
; S0
) then (0
; S0
) is a most
general unifying configuration for (1; 2 )(;
S).
(b) If unif y(1 ; 2)(;
S) fails then there exists no unifying
configuration for (1; 2)(;
S).
Theorem 6.2 Let (p; ;
S) be a typing problem.
(a) If tp (p; ;
S) = (; 0
; S0
) then (; 0
; S0
) is a most general
solution of (p; ;
S).
(b) If tp (p; ;
S) fails, then (p; ;
S) has no solution.
As a corollary of Theorem 6.2, we get that every typable
program has a principal type, which is found by tp.
Corollary 6.3 (Principal Types) Let (p; ;
id) be a typing
problem such that tv()
= ;.
(a) Assume gen (tp (p; ;
id)) = (0
; 0
; S) and let  =
S0
. Then

` p :  and

` p : 00 ) 00 μ
; for all type schemes 00
.
(b) If tp (p; ;
id) fails then there is no type scheme  such
that
` p : .
The termination of unif y and mkinst critically depends on
the form of overloaded type schemes T :
T = T ff1 ::: ffn !  (tv( )  fff1; : : : ; ffng)
j 8ff:ff ) 0
T (tv(ff)  tv(0
T )):
We show with an example why T needs to be parametric
in the arguments of T . Consider the following program,
where k 2 KT .
p = let (;) x y = y in
inst o : 8ff:o : ff ! ff ) T (T ff) ! ff
= k(k x):o x
in x:y:f : o x ; o y ; f (k y) ; f x
Then computation of tp(p; ;; id) leads to a call tp(f x; ;
S)
with x : ff; y : fi ; f : T fi !  2 .
This leads in turn to a call
unif y(ff; T fi)(;
S) where the following assumptions hold:
 T = 8ff:o : ff ! ff ) T (T ff) ! ff

 fo : ff ! ff; o : fi ! fi ; o : T g,
 S is a substitution with ff; fi 62 dom(S).
Unfolding unify gives mkinst(o : ff ! ff)(
n
; S0
) where
S0
= [T fi=ff]  S, which leads in turn to the following two
calls:
1. newinst(T ;
n
; S0
) = (T (T ) !  ; 0
; S0
)
where 0
 fo : fi ! fi ; o :  !  ; o : T g and  is a
fresh type variable, and
2. unif y(ff ! ff; T (T ) ! )(0
; S0
).
Since S0
ff = T fi, unfolding of (2) results in an attempt
to unify T fi and T (T )), which leads to the call
unif y(fi ; T )(0
; S0
). This is equivalent to the original call
unif y(ff; T fi)(;
S) modulo renaming of ff; fi to fi ; .

Hence, unif y would loop in this situation.
The need for the other restrictions on T are shown by similar constructions.
It remains to be seen whether a more general system is feasible that lifts these restrictions, e.g.
by extending unification to regular trees [Kae92].

## 7 Conclusion

We have shown that a rather modest extension to the Hindley/Milner system is enough to support both overloading and polymorphic records with a limited form of F-bounded polymorphism.
The resulting system stays firmly in the tradition of ML typing, with type soundness and principal type properties completely analogous to the Hindley/Milner system.

Figure 7: Type reconstruction algorithm for System O

The encoding of a polymorphic record calculus in System O indicates that there might be some deeper relationships between F-bounded polymorphism and overloading.
This is also suggested by the similarities between the dictionary transform for type classes and the Penn translation for bounded polymorphism [BTCGS91].
A study of these relationships remains a topic for future work.
Acknowledgments We are grateful to Kung Chen and John Maraist for valuable comments on previous drafts of this paper.
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

