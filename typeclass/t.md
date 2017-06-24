# Parametric Type Classes
(Extended Abstract)
Kung Chen, Paul Hudak, Martin Odersky
Yale University, Department of Computer Science,
Box 2158 Yale Station, New Haven, CT 06520
ACM Conf. on LISP and Functional Programming, June 1992

## Abstract

We propose a generalization to Haskell's type classes where
a class can have type parameters besides the placeholder
variable. We show that this generalization is essential to
represent container classes with overloaded data constructor
and selector operations. We also show that the resulting
type system has principal types and present unification and
type reconstruction algorithms.

## 1 Introduction

Haskell's type classes provide a structured way to introduce
overloaded functions, and are perhaps the most innovative
(and somewhat controversial) aspect of the language design
HJW91]. Type classes permit the definition of overloaded
operators in a rigorous and (fairly) general manner that inte-
grates well with the underlying Hindley-Milner type system.
As a result, operators that are monomorphic in other typed
languages can be given a more general type. Examples in-
clude the numeric operators, reading and writing of arbi-
trary datatypes, and comparison operators such as equality,
ordering, etc.

Haskell's type classes have proven to be quite useful. Most
notably missing, however, are overloaded functions for data
selection and construction. Such overloaded functions are
quite useful, but the current Haskell type system is not ex-
pressive enough to support them (of course, no other lan-
guage that we know if is capable of supporting them in a
type-safe way either).

## A Motivating Example

As a simple example, consider the concept of a sequence :
a linearly ordered collection of elements, all of the same
type. There are at least two reasonable implementations of
sequences, linked lists and vectors. There is an eciency

----

This research was supported by DARPA through ONR contracts
N00014-90-C-0024 and N00014-91-J-4043.

----

tradeooff in choosing one of these representations: lists sup-
port the ecient addition of new elements, whereas vectors
support ecient random (including parallel) access. Cur-
rently the choice between representations is made at the pro-
gramming language level. Most functional languages pro-
vide lists as the \core" data structure (often with special
syntax to support them), relegating arrays to somewhat of
a second-class status. Other languages, such as Sisal and
Nial, reverse this choice and provide special syntax for ar-
rays instead of lists (this often reects their bias toward
parallel and/or scientific computation).

Of course, it is possible to design a language which places
equal emphasis on both \container structures". However,
a naive approach faces the problem that every function on
sequences has to be implemented twice, once for lists and
once for arrays. The obvious cure for this name-space pol-
lution and duplicated code is overloading . In our context,
that means specifying the notion of a sequence as a type
class with (at least) lists and vectors as instance types. Us-
ing Haskell-like notation, this would amount to the following
declarations:

    class Sequence a s
    where cons :: a -> s -> s
          nth  :: s -> Int -> a
          len  :: s -> Int

    instance Sequence a (List a)
    where cons = (:)
          nth  = (!)
          len  = (#)

    instance Sequence a (Vector a)
    where cons = vecCons
          nth  = vecNth
          len  = vecLen

This defines the overloaded constructor cons , overloaded in-
dexing selector nth , and a length function len . (Note the
resemblance to a \container class" in object-oriented pro-
gramming.)

The only problem with this code is that it is not valid
Haskell, since Haskell's type classes are permitted to con-
strain only one type, thus ruling out a declaration such as
"class Sequence a s". In essence, this restriction forces
overloaded constructors and selectors to be monomorphic
(which makes them fairly useless).

-----

page 1

-----

Even if this restriction did not exist, there is another prob-
lem with the current type class mechanism, which can be
demonstrated through the typing of len :
Sequence a s => s -> Int
Even if multi-argument type classes were allowed, this qual-
ified type would still not be valid Haskell since it is am-
biguous : Type variable a occurs in the context ( Sequence a
s ), but not in the type-part proper ( s->Int ). Ambiguous
types need to be rejected, since they have several, possibly
conicting, implementations.
A related, but harder, problem arises if we extend our exam-
ple to include an overloaded map function. Having such a
function is attractive, since together with join and filter ,
it allows us to generalize (i.e. overload) the notion of a \list
comprehension" to include all instances of Sequence , not
just lists. In Section 7 we elaborate on this, extending it
further to comprehensions for arbitrary instances of class
monad , such as bags and lists. This seems quite natural
since, after all, the domain of sets is where the \compre-
hension" notation came from. However, a problem becomes
evident as soon as we attempt to give a type for map .
map: (Sequence a sa, Sequence b sb)
=> (a -> b) -> sa -> sb.
This type is too general, since it would admit also imple-
mentations that take one sequence type (e.g. a list) and
return another (e.g. a vector). Generality is costly in this
context since it again leads to ambiguity. For instance, the
function composition (map f . map g) would be ambigu-
ous the type of map g , which does not appear in the type
of the enclosing expression, can be either a list or a vector.
What is needed is some way to specify that map returns the
same kind of sequence as its argument, but with a possibly
diofferent element type. A nice way to notate this type would
be:
map: Sequence (s a) => (a -> b) -> s a -> s b
where s is a variable which ranges over type constructors
instead of types. To accommodate this, Sequence should
now be viewed as a type constructor class instead of a type
class. However, because the instance relationships are now
expressed at the functor-level, there is the danger (as has
been conjectured in Lil91]) that second order unification
is needed to reconstruct types, thus rendering the system
undecidable.
Our Contributions
To solve these problems, we introduce the notion of para-
metric type classes as a significant generalization of Haskell's
type classes. Our contributions can be summarized as fol-
lows:

1. Parametric type classes can have type arguments in
addition to the constrained type variable, and thus
are able to express classes such as Sequence defined
earlier.

2. Through a simple encoding scheme, we show that para-
metric type classes are able to capture the notion of
\type constructor variables," thus permitting the def-
inition of overloaded operators such as map .

3. Parametric type classes are a conservative extension of
Haskell's type system: If all classes are parameterless,
the two systems are equivalent.

4. We prove that our system is decidable, and provide an
eoffective type inference algorithm.

5. As a concrete demonstration of the power and practi-
cality of the system, we formulate classes monad and
monad0 that allow us to generalize the concept of list
comprehensions to monads. This is done using the
standard translation rules for list comprehensions no
special syntax is needed.

## Related Work

Wadler and Blott WB89] introduced type classes and pre-
sented an extension of the Hindley-Milner type system that
incorporates them. They proposed a new form of type,
called a predicated type , to specify the types of overloaded
functions. A quite similar notion was used under the name
of category in the Scratchpad II system for symbolic compu-
tation JT81]. Also related are Kaes' work on parametric
overloading Kae88], F-bounded
polymorphism in object-
oriented programming CCH + 89], and Rou90]. The type
class idea was quickly taken up in the design of Haskell. Its
theoretical foundation, however, took some time to develop.
The initial approach of WB89] encoded Haskell's source-
level syntax in a type system that was more powerful than
Haskell itself, since it could accommodate classes over multi-
ple types. This increased expressiveness can, however, lead
to undecidability, as has been investigated by Volpano and
Smith VS91]. Indeed, the system published in WB89] is
apparently undecidable.
The source-level syntax of Haskell, on the other hand, has
a sucient number of static constraints to guarantee de-
cidability. This was shown in NS91], where Nipkow and
Snelting modeled type classes in a three-level system of val-
ues, types, and partially ordered sorts. In their system,
classes correspond to sorts and types are sorted accord-
ing the class hierarchy. Order-sorted unification MGS89]
is used to resolve overloading in type reconstruction. The
use of an order-sorted approach is mathematically elegant,
yet we argue that the ordering relation between classes is a
syntactic mechanism and thus not necessary for developing
a type system for type classes. Furthermore, it is not obvi-
ous how to extend their system to incorporate our proposed
extensions.
Work was also done to extend the type class concept to pred-
icates over multiple types. Volpano and Smith VS91] looked
into modifications of the original system in WB89] to ensure
decidability of type reconstruction and to get a sharper no-
tion of well-typed expressions. Jones Jon91, Jon92b] gave
a general framework for qualified types . His use of predicate
sets is at first sight quite similar to our context-constrained
instance theory. The main diofference between the two ap-
proaches lies in our use of normal forms (Jones does not ad-
dress this issue) and our distinction between constrained and
dependent variables. This distinction allows us to solve the

-----

page 2

-----

ambiguity problems previously encountered in definitions of
container classes.
The rest of this paper is organized as follows: Section 2 in-
troduces parametric type classes. Section 3 presents them
formally, in a non-deterministic type system. Section 4
presents an equivalent syntax-directed system that bridges
the gap between the non-deterministic system and a type
reconstruction algorithm. Section 5 discusses type recon-
struction and unification. Section 6 explains when a type
scheme is ambiguous. Section 7 applies our system in defin-
ing monads as parametric classes. Section 8 conclues.

## 2 Parametric Type Classes

A parametric type class is a class that has type parameters in
addition to the placeholder variable which is always present
in a class declaration. To distinguish between placeholder
and type parameters, we write the placeholder in front of
the class, separated by an infix (::). For instance:

    class t :: Eq where
    class s :: Sequence a where

The first definition introduces a class without parameters
in Haskell this would be written class Eq t . The second
definition defines a type class Sequence with one parameter
this cannot be expressed in standard Haskell. The infix (::)
notation is also used in instance declarations and contexts.
The two instance declarations of Sequence presented in the
last section would now be written:
inst List a
:: Sequence a where ...
inst Vector a :: Sequence a where ...
In an instance declaration, of form T :: Sequence a , say,
the type T must not be a variable. Furthermore, if two types
T1 and T2 are both declared to be instances of Sequence ,
then their top-level type constructors must be diofferent. Thus,
the instance declarations given above are both valid. On the
other hand,
inst a :: Sequence (List a)
would violate the first restriction, and
inst List Int :: Sequence Int
inst List Char :: Sequence Char
would violate the second restriction. Eoffectively, these re-
strictions ensure that in a proof of an instance relationship
every step is determined by the class name and the type
in placeholder position. The class parameter types, on the
other hand, depend on the placeholder type.
One consequence of these restrictions is that there is at most
one way to deduce that a type is an instance of a class. This
is necessary to guarantee coherence . It is not sucient, since
types might be ambiguous see Section 6 for a discussion.
Another consequence is that sets of instance predicates are
now subject to a consistency criterion: If we have both T ::
Sequence a and T :: Sequence b then we must have a =
b . That is, a = b is a logical consequence of the two instance
predicates and the restrictions on instance declarations. The
type reconstruction algorithm enforces consistency in this
situation by unifying a and b .
Enforcing consistency early helps in keeping types small.
Otherwise, we could get many superuous instance con-
straints in types. As an example, consider the composition
(tl . tl) , where tl is typed (s :: Sequence a) => s
-> s . Without the consistency requirement, the most gen-
eral type for the composition would be (s :: Sequence
a, s :: Sequence b) => s -> s . Composing tl n times
would yield a type with n Sequence constraints, all but one
being superuous.
3 The Type System of Parametric Classes
This section presents our type system formally. We first
define the abstract syntax of classes and types in the context
of a small example language. We then explain formally what
it means for a type to be an instance of a class. Based on
these definitions, we define a non-deterministic type system
with the same six rules as in DM82], but with parametric
type classes added. We claim that, in spite of its added
generality, the system is actually simpler than previously
published type systems for standard Haskell.
For lack of space, we refer the reader to COH92] for detailed
proofs of the results presented in this and the following sec-
tions.
Syntax
The example language is a variant of Mini-Haskell NS91],
augmented with parameterized type classes. Its abstract
syntax and types are shown in Figure 1. A parametric type
class in this syntax has the form c  , where c is a class
constructor, corresponding to a class in Haskell, and  is
a type. Classes with several parameters are encoded using
tuple types, e.g. c ( fi  ). Parameterless classes are encoded
using the unit type, e.g. Eq (). The instance relationship
between a type and a type class is denoted by an infix (::)
the predicate  :: c  reads  is an instance of c  .
One simplification with respect to standard Haskell concerns
the absence of a hierarchy on classes. The subclass/superclass
relationship is instead modeled by class sets ;. Consider for
instance the class Eq () of equality types in Haskell and its
subclass Ord () of ordered types. We can always represent
Ord () as a set of two classes, f Eq () fi Ord () g , where Ord
contains only operations ( <fi  ), which are defined in Ord
but not in Eq . Translating all classes in a program in this
way, we end up with sets over a at domain of classes. This
shows that we can without loss of generality disregard class
hierarchy in the abstract syntax.
0
0
0
0

### Instance Theories

In this section, we make precise when a type  is an instance
of a class set ;, a fact which we will express  ::;. Clearly,
the instance relation depends on the instance declarations
Ds in a program. We let these declarations generate a theory
whose sentences are instance judgments of the form C ``  

-----

page 3

-----


    ::Type variables
    
    Type constructors 
    Class constructors c
    Types
     ::= () j   j  j  1 fi  2 j  1 !  2
    Type schemes
     ::= 8  ::; : j 
    Type classes
    ::= c 
    Class sets
    ; ::= f c 1  1 fi :::fi c n  n g
    Contexts
    C ::= f  1 ::; 1 fi : : : fi  n ::; n g
    ( n  0 fi c i pairwise disjoint)
    ( n  0)
    e ::= x j e e j 
    x : e j let x = e in e
    p ::= class  :: where x :  in p
    j inst C )  :: where x = e in p
    j e
    Expressions
    Programs
    0
    0

Figure 1: Abstract Syntax of Mini-Haskell+

    C ``  ::
    C `` C
    C ``  ::
    0
    (  :: f : : :
    : : : g
    ( inst C )  ::
    0
    C ``  :: 1 : : : C ``  :: n
    C ``  :: f 1 fi :::fi n g
    2 C )
    2 Ds )
    ( n  0)
    C ``  1 ::; 1 : : : C ``  n ::; n
    C `` f  1 ::; 1 fi : : : fi  n ::; n g
    ( n  0)

Figure 2: Inference Rules for Entailment.

 An instance judgment is true in the theory ioff it can be
deduced using the inference rules in Figure 2.

## Context

In these rules the context C is a set of instance assumptions
 :: ; (all  's in C are disjoint). Where convenient, we will
also regard a context as a finite mapping from type variables
to class sets, i.e. C  = ; ioff  ::; 2 C . Thus the domain of
C , dom ( C ) , is defined as the set of type variables  such
that  ::; 2 C . As type classes can now contain parameters,
we define the region of a context C ,

    reg(C) = ∪(α ∈ dom(C)) fv(C)

and the closure of C over a set of type variables, Δ, written
C*(Δ), as the least fixpoint of the equation

    C*(Δ) = Δ ∪ C(C* (Δ)).

We say C 1 is contained in C 2 , written C 1  C 2 , if dom ( C 1 ) 
dom ( C 2 ) and C 1   C 2  for each  2 dom ( C 1 ). We write
C 1 ] C 2 for the disjoint union of two contexts and C n for
restriction of a context C to all type variables in its domain
other than  . A context C is called closed if C ( dom ( C )) =
dom ( C ), or, equivalently, reg ( C )  dom ( C ). A context C
is called acyclic if all the type variables  ,  2 dom ( C ),

can be topologically sorted according to the order:  <  if
 2 fv ( C  ). We shall restrict our discussion to only closed
acyclic contexts in the remainder of the paper.
Constrained Substitution
In the following, we will apply variable substitutions not
only to types, but also to (sets of) classes and (sets of) in-
stance predicates. On all of these, substitution is defined
pointwise, i.e. it is a homomorphism on sets, class construc-
tor application and (::). Since a context is a special form
of an instance predicate set, substitutions can be applied to
contexts. However, the result of such a substitution is in
general not a context, as the left hand side  of an instance
predicate  ::; can be mapped to a non-variable type. Our
typing rules, on the other hand, require contexts instead of
general predicate sets. Thus, we need a means to find a
context that is a conservative approximation to a predicate
set. We use the following definitions:
Denition. A constrained substitution is a pair ( S fi C ) where
S is a substitution and C is a context such that C = SC .
Denition. A constrained substitution ( S fi C ) preserves a
constrained substitution ( S 0 fi C 0 ) if there is a substitution R
such that S = R  S 0 and C `` RC 0 . We write in this case
( S fi C )  ( S 0 fi C 0 ).It is easy to show that  is a preorder.
Denition. A constrained substitution ( S fi C ) is most gen-
eral among those constrained substitutions that satisfy some
requirement R if ( S fi C ) satisfies R , and, for any ( S fi C )
that satisfies R , ( S fi C )  ( S fi C ).
0
0
0
0
Denition. A constrained substitution ( S fi C ) is a normal-
izer of an instance predicate set P if C `` SP .
To ensure the principal type property of our type system
with parametric classes, we have to place the following re-
quirements on the entailment relation `` :
off monotonicity: for any contexts C and C , if C  C
then C `` C .
off transitivity under substitution: for any substitu-
tion S , contexts C and C , predicate set P , if C `` SC
and C `` P then C `` SP .
off most general normalizers: If a predicate set P has
0
0
0
0
0
this system and the standard Hindley/Milner system is that
the bound variable in a type scheme 8  ::; : can be instan-
tiated to a type  only if we know from the context that
 :: ; (rule 8; elim ). The second diofference concerns rule
( 8; intro ), where the instance predicate on the generalized
variable  is \discharged" from the context and moved into
the type scheme 8  ::; : .
The rules in Figure 4 extend this system from expressions
to programs. In rule ( class ), the overloaded identifier x
is added to the assumption set. Rule ( inst ) expresses a
compatibility requirement between an overloaded identifier
and its instance expressions. These rules have to be taken
in conjunction with the requirements ( a ), ( b ) on instance
declarations listed in the last subsection. We say a pro-
gram p = Ds e has type scheme  , ioff Ds satisfies these
requirements and generates an entailment relation `` , and
A 0 fi fg ` p :  , for some given closed initial assumption set
A 0 .
0
a normalizer then it has a most general normalizer.
From the viewpoint of type reconstruction, the first two re-
quirements are needed to ensure that once established en-
tailments are not falsified by later substitutions or additions
to contexts. They follow directly from the inference rules in
Figure 2. The last requirement ensures that there is a most
general solution to an entailment constraint. To establish
existence of most general normalizers, we have to place two
restrictions on the instance declarations in a program:
(a) There is no instance declaration of the form
inst C )  :: c :
(b) For every pair of type and class constructor ( fi c ),
there is at most one instance declaration of the form
inst C )   :: c : Furthermore,  must be the unit
type, or a possible empty tuple of distinct type vari-
ables and both dom ( C ) and fv (  ) are contained in
fv (  ).
0
0
0
Restriction (a) is part of current Haskell, and restriction (b)
is a direct generalization of current Haskell's restriction to
incorporate parametric type classes.
Theorem 3.1 If the instance declarations Ds of a program
satisfy the restrictions ( a ) and ( b ), then `` admits most
general normalizers.
Typing Rules
Given an entailment relation `` between contexts and in-
stance predicates, we now formalize a theory of typing judg-
ments. Typing judgments are of the form A fi C ` e :  ,
where A is an assumption set of type predicates x :  (all
x disjoint), C is a context, and e is an expression or a pro-
gram. A typing judgment A fi C ` e :  holds in the theory
ioff it can be deduced using the inference rules in Figures 3
and 4.
The rules in Figure 3 form a non-deterministic type sys-
tem for expressions, along the lines of of the standard Hind-
ley/Milner system DM82]. One notable diofference between
The Instance Relation and Principal Type Schemes
A useful fact about Hindley/Milner type system is that when
an expression e has a type, there is a principal type scheme
which captures the set of all other types derivable for e
thruogh the notion of generic instances . The remainder of
this section introduces the definitions of generic instance and
principal type schemes in our system.
Denition. A type scheme  = 8  j :: ; j : is a generic
instance of a type scheme  = 8  i ::; i : under a context C ,
if there exists a substitution S on f  i g , such that S  =  ,
 j is not free in  , and C ] f  j :: ; j g `` S  i :: S ; i . We
write in this case,   C  .
The definiton of  C is an extension of the ordering relation
0
0
0
0
0
0
0
0
0
defined in DM82]. The only new requirement on instance
entailment is needed for the extension with parametric type
classes. It is easy to see that  C defines a preorder on the
set of type schemes.
The following property is a direct consequence of the defini-
tion.
Lemma 3.2 If   C  and C  C then   C 0  .
0
0
0
The next lemma shows that the ordering on type schemes is
preserved by constrained substitutions.
Lemma 3.3 If   C  and C `` SC then S   C 0 S  .
0
0
0
With the definiton of ordering on type schemes, we can de-
fine the notion of principal type schemes in our system.
Denition. Given A , C , and e , we call  a principal type
scheme for e under A and C ioff A fi C ` e :  , and for every
 , if A fi C ` e :  then   C  .
We shall develop an algorithm to compute principal type
schemes in the following sections.
0
0
0
4 A Deterministic Type Inference System
We present a deterministic type inference system in this sec-
tion. Compared to the typing rules in Section 3, the rules( x :  2 A )
A fi C ` x : 
( var )
A fi C ` e : 8  ::; :
C ``  ::;
A fi C ` e :   7!  ] 
( 8; elim )
( 8; intro ) A fi C : ::; ` e : 
A fi C ` e : 8  ::; :
( 
 ; elim ) A fi C ` e :  ! 
A fi C ` e : 
A fi C ` e e : 
( 
 ; intro ) A : x :  fi C ` e : 
A fi C ` 
 x : e :  ! 
(  62 fv A 
 reg C )
0
0
0
0
0
0
A fi C ` e : 
A : x : fi C ` e : 
A fi C ` let x = e in e : 
0
( let )
0
Figure 3: Typing Rules for Expressions
A : x : 8 fv  8  :: f g :fi C ` p : 
A fi C ` class  :: where x :  in p : 
( class )
A fi C ` x : 8  :: f g :
A fi C ` e :  7!  ] 
A fi C ` p : 
A fi C ` inst C )  :: where x = e in p : 
0
( inst )
0
0
Figure 4: Typing Rules for Declarations
here are so formulated that the typing derivation for a given
term e is uniquely detrmined by the syntactic structure of
e , and hence are better suited to use in a type inference
algorithm. We show that the system is equivalent to the
previous one in terms of expressiveness and, in addition,
has all the nice properties toward the construction of a type
reconstruction algorithm.
Deterministic Typing Rules
The typings rules for the deterministic system are given in
Figure 5. The rules 8; intro and 8; elim have been removed
and typing judgements are now of the form A fi C ` e : 
where  ranges over the set of type expressions as opposed to
type schemes in the typing judgements of Section 3. Other
major diofferences are that rule ( var ) instantiates a type
scheme to a type according to the definition of generic in-
stance and rule ( let ) use the generalization function, gen ,
to introduce type schemes.
The function gen takes as arguments a type scheme, an as-
sumption set, and a context, and returns a generalized type
scheme and a discharged context. It is defined by
0
0
0
gen ( fi A fi C ) =
if 9  2 dom ( C ) n ( fv A 
 reg C ) then
gen ( 8  :: C :fi A fi C n )
else ( fi C )
In other words, instance assumptions in the given context,
except those constraining type variables in the assumption
set, are discharged and moved to form a more general type
scheme in an order so that type variables are properly quan-
tified.
Equivalence of the two Systems
We now present a number of useful properties of the deter-
ministic type system. They are useful not only in estab-
lishing the congruence of the two type systems, but also in
investigating the relation between the type system and the
type reconstruction algorithm.
Lemma 4.1 (Substitution lemma) If A fi C ` e :  and
C `` SC then SA fi C ` e : S  .
0
0
0
0
This result assures us that typing derivations are preserved
under constrained substitution.
The next two lemmas express a form of monotonicity of
typing derivations with respect to the context and the as-
sumption set.
Lemma 4.2 If A fi C ` e :  and C  C then A fi C `
0
0
0
0
e :  .
Lemma 4.3 If A : x : fi C ` e :  and
A : x :  fi C ` e :  .
0
0
0

 C
 0
then
Now we can show that the deterministic system ` is equiv-
alent to the non-deterministic system ` in the following
sense.
0
Theorem 4.4 If A fi C ` e :  then A fi C ` e :  .
0( var )
0
0
( 
 ; elim )
0
( 
 ; intro )
0
( let )
0
A fi C ` x :  ( x :  2 A fi   C  )
A fi C ` e :  ! 
A fi C ` e : 
A fi C ` e e : 
A : x :  fi C ` e : 
A fi C ` 
 x : e :  ! 
A fi C ` e : 
A : x : fi C ` e : 
A fi C ` let x = e in e : 
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
( fi C ) = gen (  fi A fi C ) fi C  C
00
0
0
0
00
Figure 5: Determinstic Typing Rules for Expressions
Theorem 4.5 If A fi C ` e :  then there is a context C ,
and a type  such that C  C , A fi C ` e :  and   C 
0
0
0
0
0
where (  fi C ) = gen ( fi A fi C ).
0
00
0
5 Unication and Type Reconstruction
This section discusses type reconstruction. As usual, type
reconstruction relies on unification, and we will first work
out what kind of unification is needed for parametric type
classes. We then go on to present a type reconstruction
algorithm, and state its soundness and completeness with
respect to the inference rules given in Section 3 using those
rules in the last section and the equivalence result estab-
lished therein. As a corollary of these results, we obtain
a prinicpal type scheme property of our system analogous
to the one in DM82]. The type reconstruction algorithm
has been implemented in the Yale Haskell compiler. Its size
and complexity compare favorably to the type reconstruc-
tion parts of our prior Haskell compiler.
Context-Preserving Unication
Type reconstruction usually relies on unification to compute
most general types. One consequence of rule ( 8; elim ) is that
the well-known syntactic unification algorithm of Robinson
Rob65] cannot be used since not every substitution of vari-
ables to types satisfies the given instance constraints. Nip-
kow and Snelting have shown that order-sorted unification
can be used for reconstructing of types in Haskell NS91], but
it is not clear how to extend their result to parametric type
classes. We show in this section that algorithm mgu , shown
in Figure 6, yields the most general context-preserving uni-
fier of two types.
Function mgu takes two types and returns a transformer on
constrained substitutions. The application mgu  1  2 ( S 0 fi C 0 )
returns a most general constrained substitution that unifies
the types  1 and  2 and preserves ( S 0 fi C 0 ), if such a substitu-
tion exists. The algorithm is similar to the one of Robinson,
except for the case mgu   ( S 0 fi C 0 ), where  may be sub-
stituted to  only if  can be shown to be an instance of C 0  .
This constraint translates to an application of the subsidary
function mgn to  and C  . The call mgn  ; ( S 0 fi C 0 ) com-
putes a most general normalizer of C 0 
 f  :: ; g , provided
one exists.
Theorem 5.1 Given a constrained substitution ( S 0 fi C 0 ) and
types  1 ,  2 , if there is a ( S 0 fi C 0 )-preserving unifier of  1 and
 2 then mgu  1  2 ( S 0 fi C 0 ) returns a most general such uni-
fier. If there is no such unifier then mgu  1  2 ( S 0 fi C 0 ) fails
in a finite number of steps.
Type Reconstruction
An algorithm for type reconstruction is shown in Figure 7. 1
Function tp takes as arguments an expression, an assump-
tion set, and an initial constrained substitution, and returns
a type and a final constrained substitution. The function is
straightforwardly extended to programs. The remainder of
this section establishes the correspondence between tp and
the type system of Section 4 and, thereby, that of Section 3.
We need the following lemmas to establish the soundness
and completeness of our algorithm. We begin by showing
that tp is indeed a constrained substitution transformer.
Lemma 5.2 Let ( S fi C ) be a constrained substitution and
( fi S fi C ) = tp ( e fi A fi S fi C ), then ( S fi C ) is a constrained
substitution.
Hence we will omit the requirement of constrained substitu-
tion from now on.
0
0
0
0
Lemma 5.3 If tp ( e fi A fi S fi C ) = ( fi S fi C ) then ( S fi C ) 
0
0
0
0
( S fi C ).
This result can be established by a straightforward induc-
tion except in the let ; case. Recall the typing rule ( let )
presented in Section 4. There are two contexts used in the
antecedent part of that rule : one for deriving the type of the
let-definition and one for the type of the let-body. But only
the second one appears in the conclusion part and it is those
instance assumptions contained in the first one that are gen-
eralized by the gen function. While in tp , we maintain a
single context and pass it through the whole algorithm. If
we were to use the gen function in the let ; case in tp we
would overgeneralize those instance assumptions generated
in the previous stages and passed to tp as part of the initial
context.
0
1 This is actually a simpli
cation of the real algorithm becuase we
can get a cyclic context after the call to uni
cation function and thus
violate our restriction on contexts. So what is missing here is a clique-
detection algorithm, which is simply a variant of occur checking. We
omit it here for simplicity.mgu :  !  ! S fi C ! S fi C
mgn :  ! ; ! S fi C ! S fi C
mgu  1  2 ( S fi C )
=
mgu  
=
mgu   ( S fi C ) j  62 fv (  ) =
=
mgu   ( S fi C )
mgu () ()
=
mgu     ( S fi C )
=
mgu (  1 fi  2 ) (  1 fi  2 )
=
mgu (  1 !  2 ) (  1 !  2 )
=
mgn  fg
=
mgn  f g ( S fi C )
=
mgn  (; 1 
 ; 2 )
=
mgn  c  ( S fi C )
=
mgu ( S  1 ) ( S  2 ) ( S fi C )
id S C
mgn  ( C  ) (  7!  ]  S fi   7!  ] C n )
mgu   ( S fi C )
id S C
mgu   ( S fi C )
( mgu  1  1 )  ( mgu  2  2 )
( mgu  1  1 )  ( mgu  2  2 )
id S C
mgn ( S  ) ( S ) ( S fi C )
( mgn  ; 1 )  ( mgn  ; 2 )
if 9  : ( c  2 C  ) then mgu   ( S fi C )
else ( S fi C   7! C  
 f c  g ])
mgn   c  ( S fi C ) j 9 inst C )   ~ :: c  ~ 2 Ds
= let S = match  ~ 
( S fi C ) = mgu  ( S  ~ ) ( S fi C )
f  1 ::; 1 fi : : : fi  n ::; n g = S C
in ( mgn  1 ; 1 ( : : : ( mgn  n ; n ( S fi C ))))
(and similarly for ! , fi , ())
0
0
fi
0
0
0
0
fi
0
0
0
0
0
0
0
0
0
0
0 0
fi
0
0
0
0
0
0
0
0
0
0
0
00
00
0
0
0
0
00
00
Figure 6: Unification and Normalization Algorithms
Corollary 5.7 (Completeness of tp ) Suppose that S A fi C `
e :  and ( S fi C )  ( S 0 fi C 0 ). Then tp ( e fi A fi S 0 fi C 0 ) succeeds
To avoid such overgeneralization, we need to confine the do-
main of generalization to only those instance assumptions
generated while reconstructing the type of the let-definition.
We define a new generalization function, tpgen , which, com-
pared to gen , takes an extra context parameter, C , whose
instance assumptions will be excluded from generalization.
Then in the algorithm, when doing generalization, we pass
the initial context to tpgen as the second context argument
to restrict the domain of generalization. Thus only those
newly generated instance assumptions will be generalized.
Now we can proceed to state the soundness of our algorithm.
0
0
0
0
0
0
Corollary 5.8 Suppose that tp ( e fi A fi S 0 fi C 0 ) = ( fi S fi C ) and
gen ( fi SA fi C ) = ( fi C ). Then  is a principal type scheme
for e under SA and C .
0
Theorem 5.4 If tp ( e fi A fi S fi C ) = ( fi S fi C ) then S A fi C `
0
0
0
with ( fi S fi C ), and there is a substitution R such that
S A = RSA fi and   C 0 R 
where ( fi C ~ ) = gen ( fi SA fi C ).
As a corollary, we have the following result for principal type
schemes.
0
0
0
0
0
e :  .
Together with Theorem 4.4, we have the following soundness
result.
6 Ambiguity Revisited
As we have seen in the introduction, parametric type classes
Corollary 5.5 (Soundness of tp ) If tp ( e fi A fi S fi C ) = ( fi S fi C ) share with standard type classes the problem that type
schemes might be ambiguous.
then S A fi C ` e :  .
0
0
0
0
Ultimately, we will state the principal typing result.
Theorem 5.6 Suppose that S A fi C ` e :  and ( S fi C ) 
0
0
0
0
0
0
( S 0 fi C 0 ). Then tp ( e fi A fi S 0 fi C 0 ) succeeds with ( fi S fi C ), and
there is a substitution R such that
S A = RSA fi C `` RC fi and  = R :
0
0
0
Together with Theorem 4.5, we have the completeness re-
sult.
Denition. Given a type scheme  = 8  i ::; i : , let C  =
f  i ::; i g be the generic context of  .
Denition. A generic type variable  in a type scheme  =
8  i :: ; i : is (weakly) ambiguous if (1) C   6 =  , and (2)
 62 C  ( fv  ).

Ambiguous type variables pose an implementation prob-
lem. The usual approach to implement overloading poly-
morphism is to pass extra dictionary arguments for every
type class in the context of a function signature. Since thetp ( x fi A fi S fi C )
tp ( e 1 e 2 fi A fi S fi C )
= inst ( S ( Ax ) fi S fi C )
= let (  1 fi S 1 fi C 1 ) = tp ( e 1 fi A fi S fi C )
(  2 fi S 2 fi C 2 ) = tp ( e 2 fi A fi S 1 fi C 1 )
 a fresh type variable
( S 3 fi C 3 ) = mgu  1 (  2 !  ) ( S 2 fi C 2 : :: fg )
in ( S 3 fi S 3 fi C 3 )
tp ( 
 x : e fi A fi S fi C )
= let  a fresh type variable
(  1 fi S 1 fi C 1 ) = tp ( e 1 fi A : x : fi S fi C : :: fg )
in ( S 1  !  1 fi S 1 fi C 1 )
tp ( let x = e 1 in e 2 fi A fi S fi C ) = let (  1 fi S 1 fi C 1 ) = tp ( e 1 fi A fi S fi C )
( fi C 2 ) = tpgen (  1 fi S 1 A fi C 1 fi C )
in tp ( e 2 fi A : x : fi S 1 fi C 2 )
where
inst ( 8  ::; :fi S fi C )
inst ( fi S fi C )
tpgen ( fi A fi C fi C )
0
= let  a fresh type variable
in inst (  7!  ] fi S fi C : ::;)
= ( fi S fi C )
= if 9  2 dom ( C ) n ( fv ( A ) 
 reg ( C ) 
 dom ( C )) then
tpgen ( 8  :: C :fi A fi C n fi C )
else ( fi C )
0
0
Figure 7: Type Reconstruction Algorithm
constraints on ambiguous variables are non-empty (1), dic-
tionaries need to be passed. But since the ambiguous vari-
able does not occur free in the type (2), it is never instanti-
ated, hence we do not know which dictionaries to pass. Seen
from another perspective, any dictionary of an appropriate
instance type would do, but we have a problem of coher-
ence: There are several implementations of an expression
with possibly diofferent semantics Jon92a].
The problem is avoided by requiring that the programmer
disambiguate expressions if needed, by using explicit type
signatures. Conceptually, the ambiguity check takes place
after type reconstruction would it be part of type recon-
struction then the principal type property would be lost. In
a way, the ambiguity problem shows that sometimes recon-
structed types are too general. Every ambiguous type has
a substitution instance which is unambiguous (just instan-
tiate ambiguous variables). The trouble is that there is not
always a most general, unambiguous type.
Compared to multi-argument type classes, our type system
often produces types with less ambiguity. Consider:
len :: (sa :: Sequence a) => sa -> Int
Seen as a multi-argument type class, a would be ambiguous,
since it occurs in a predicate but not in the type itself. Seen
as a parametric type class, however, a is not ambiguous: Al-
though it does not occur in the type, it both unconstrained
and dependent on sa through (sa :: Sequence a) . Hence
both (1) and (2) fail.
Ambiguity problems can be further reduced by making use
of the following observation: Because of restriction ( b ) in
Section 3, the top-level type constructor of a type uniquely
determines the dictionary that needs to be passed. Hence,
if two types have the same top-level type constructor (but
possibly diofferent type arguments), their dictionaries share
the same data constructor (but have possibly diofferent pa-
rameters). We can recognize equality of top-level type con-
structors statically, using the following technique:
We introduce a special \root" class TC , with one type pa-
rameter but no operations. Every type is an instance of TC
by virtue of the following instance declaration (which can be
thought of being implicitely generated for every type   ).
inst   :: TC (  ())
Eoffectively, TC is used to \isolate" the top-level type con-
structor of a type. That is, if two types are related by a TC
constraint, we know that they have the same top-level type
constructor. The two types are then called similar :
Denition. Given a context C , let similarity in C , (  C ),
be the smallest transitive and symmetric relation such that
C ``  1 :: TC  2 implies  1  C  2 .
TC is treated like every other type class during type re-
construction. It is treated specially in the ambiguity check,
allowing us to strengthen the ambiguity criterion:
Denition. A generic type variable  in a type scheme  is
strongly ambiguous if  is weakly ambiguous in  , and, for
every type  ,   C  implies that  is a strongly ambiguous
type variable in  .
The TC technique enables us to type map precisely 2
map : 8 a : 8 b : 8 t :
8 sa :: f Sequence a fi TC t g :
8 sb :: f Sequence b fi TC t g : ( a ! b ) ! sa ! sb
2 Previously, it has been conjectured that this required second-
order uni
cation.This states that sa and sb are instance types of Sequence
with element types a and b , and that sa and sb share the
same type constructor.
The knowledge that sa and sb have the same type construc-
tor is initially on the meta-level, derived from the form of the
compiler-generated instance declarations. We can formalize
it in the type system as follows:
Denition. A type scheme  = 8  i :: ; i : is in reduced
0
form if none of the ; i contains a class TC (   ), for arbitrary
constructor  and type  . We use  R for type schemes in
reduced form.
Denition. Two type schemes  1 ,  2 are equivalent under
a context C ,  1 ' C  2 , ioff for all reduced type schemes
 R ,
 R
 C  1 ,
 R
 C  2 :
We extend the definition of generic instance to include equiv-
alence: A type scheme  1 is a generic instance of a type
scheme  2 under a context C if there is a type scheme 
s.t.  1 ' C  , and   C  2 according to the definition of
 C in Section 3. This stronger notion of generic instance is
important to check user-defined type signatures.
Example: After substituting List a for sa , the type signa-
ture of map would become:
8 sb :: f Sequence b fi TC ( List ()) g : ( a ! b ) ! List a ! sb
The usual definition of map for lists, on the other hand,
would have type:
( a ! b ) ! List a ! List b
Equivalence is necessesary to verify that the first type is an
instance of the second.
To keep contexts short, we will use in the next section the
similarity relation (  ) directly, instead of its definition in
terms of TC .
0
0
0
7 From Monads to Lists
In this section, we show how to use parametric type classes
to generalize many of the operations and concepts which
were previously restricted to lists. As sketched in the in-
troduction, a first step overloads operations that are com-
mon to all implementations of sequence. Some important
operations can even be applied in the more general Moand
contextWad90] hence it makes sense to have \Monad" and
\Monad with zero" as superclasses of \Sequence". The fol-
lowing enumeration shows on which levels in the hierarchy
some familiar list operations are defined.
Monad: unit, join, map , monad comprehensions.
Monad0: nil, filter , comprehensions with filters.
Sequence: cons, hd, tl, reverse, foldl, foldr, (++) .
The use of monads in functional programming was explored
in Wad90, Wad91] for a motivation of the concept we refer
the reader to the examples given there. The point we want
to explore here is how to express monads (and their special-
izations) in the type system of a programing language such
that we can abstract from their concrete implementations.
We show how the monad operations can be overloaded, us-
ing parametric type classes. This is useful since it allows to
define functions over arbitrary Monads, to reuse the same
names for operations on diofferent monads, and to generalize
list comprehensions without changing their present syntax.
We formulate class Monad as follows:
class ma :: Monad a where
unit :: a -> ma
bind :: (mb :: Monad b, ma ~ mb)
=> ma -> (a -> mb) -> mb
map :: (mb :: Monad b, ma ~ mb)
=> (a -> b) -> ma -> mb
join :: (mma :: Monad ma, mma ~ ma)
=> mma -> ma
-- Default definitions:
map f xs = xs `bind (unit . f)
join xss = xss `bind` id
bind xs f = join (map f xs)
This introduces two equivalent formulations of a monad, one
in terms of unit and bind , the other in terms of unit , map
and join . The default definitions in the class express one
formulation in terms of the other hence instances can al-
ternatively define bind or map and join . To qualify for a
monad, an instance has to satisfy three laws, which are not
enforced by the type system. bind must be associative, with
unit as left and right unit:
(m `bind` f) `bind` g = m `bind` \x -> f x `bind` g
\x -> unit x `bind` f = f
m `bind` unit
= m
Lists form a monad, as witnessed by the following instance
declaration, and a check that monad laws hold:
inst List a :: Monad a where
unit x
= x]
map f ] xs
= ]
map f (x:xs)
= f x : map f xs
= ]
join ]
join (xs::xss) = xs ++ join xss
Another example of a monad are \reply"-types, as witnessed
by:
data Maybe a = Some a | None
inst Maybe a :: Monad a where
unit x
= Some x
bind (Some x) f
= f x
bind None f
= None
As a consequence, code can now be written that works on
lists as well as on reply types or any other monad instance.
In particular, we can use the list comprehension notationin each case, by applying the standard translation to unit ,
map and join :
 t ]
= ^ unit t
 t j g 1 fi g 2 ] = ^ join   t j g 2 ] j g 1 ]
 t j x  e ] = ^ map ( 
 x : t ) e
Here, t and e are terms, x is a variable, and g 1 and g 2 are
generators x  e .
Monad0 is a subclass of Monad . It adds a zero monad, nil ,
and a filter function.
class (ma :: Monad a) => ma :: Monad0 a where
nil
:: ma
filter :: (a -> Bool) -> ma -> ma
Monads with zero are the most general type class on which
list comprehensions with filters can be defined. The stan-
dard translation functions are ( p is a filter, i.e. a Boolean
term):
]
= ^ nil
 t j p ] = ^ filter p ( unit t )
Lists and reply types both have zeros, as witnessed by:
instance List a :: Monad0 a where
nil
= ]
filter p ]
= ]
filter p (x:xs)
= if p x then
x : filter p xs
else filter p xs
instance Maybe a : Monad0 a where
nil
= None
filter p None
= None
filter p (Some x) = if p x then Some x
else None
As an example of programming with Monads we discuss
abstract parsers, adapting and extending an example from
Wad90]. A parser is a function that maps a sequence of in-
put symbols to some output, or to a failure value, if no legal
parse exists. If a parse exists, then it will consist of the un-
used portion of the input stream, plus some application de-
pendent result value, such as a parse tree. If the parser uses
backtracking, there might exist several such parses, whereas
if it is determinstic, there will be zero or one. We construct
in the following a library for determinstic parsers. Such
parsers all have type signature:
data Parser a = P (String -> Maybe (a, String))
The constructor tag P is necessary because of the restriction
that instances may only be formed of datatypes. Parsers
form themselves a monad with zero, as witnessed by the
following instance declarations.
inst Parser a :: Monad a where
unit x
= P (\i -> (x, i)])
map f (P p) = P (\i ->
(f x, i') | (x, i') <- p i])
join (P pp) = P (\i ->
(x, i'') | (P p, i') <- pp i,
(x, i'') <- p i'])
inst Parser a :: Monad0 a where
nil
= P (\i -> ])
filter b (P p)
= P (\i ->
(x, i') | (x, i') <- p i,
b x])
Note that we have overloaded the comprehension notation.
The monad comprehensions in the previous two instance
declarations work on option types, not lists.
We need two primitive parsers and one more parser combi-
nator:
sym
sym :: Parser Char
= P p
where p Nil
= ]
p (Cons c cs) = (c, cs)]
lookahead
lookahead :: Parser Char
= P p
where p Nil = ]
p cs = (hd cs, cs)]
(|||)
:: Parser a -> Parser a -> Parser a
P p ||| P q = P (\i -> case p i of
None
=> q i
| Some x => Some x)
A deterministic parser for lambda terms can then be written
as follows:
data Term =
|
|
|
Lambda Term Term
Apply Term Term
Id Char
Error
term
term :: Parser Term
= Lambda x y | '\' <- sym,
x <- ident, y <- term]
||| y | x <- aterm, y <- aterms x]
aterm
aterm :: Parser Term
= x | '(' <- sym, x <- aterm']
||| ident
aterm'
aterm' :: Parser Term
= x | x <- term, ')' <- sym]
||| Error]
aterms
:: Term -> Parser Term
aterms x = z | c <- lookahead,
'a' <= c && c <= 'z' || c = '(',
y <- aterm,
z <- aterms (Apply x y)]
||| x]
ident
ident
:: Parser Term
= Id c | c <- sym, 'a' <= c && c <= 'z']
||| Error]
The defined parser is determinstic it never backtracks.
Therefore, parse failure has to be treated diofferently accord-
ing to whether it occurs at the start of a production, or inthe middle. If failure occurs at the start of a production,
it signals that another alternative should be tried. Failure
in the middle of a production signals a syntax error that is
reported by returning an Error node.
Note that most of the productions are expressed in terms of
monad comprehensions. This time, comprehensions refer to
parsers instead of option types or lists. Unlike in Wad90],
monad comprehensions need not be labelled with the monad
they refer to we rely instead on the type system for disam-
biguation (including programmer defined typings if ambigu-
ities arise otherwise). The monad style gives us a exible
interface between parsing and abstract tree generation. The
resulting parser resembles an attribute grammar with both
synthesized and inherited attributes (see the definition of
aterm ).
8 Conclusion
We have proposed a generalization of Haskell's type classes
to support container classes with overloaded data construc-
tors and selectors. The underlying type system is an ex-
tension of the Hindley/Milner system with parametric type
classes. This extension preserves two important properties
of the original system, namely decidable typability and prin-
cipal types. Its type scheme uses bounded quantification
whose introduction and elimination depend on a separate
context-constrained instance theory. The decoupling of the
instance theory from the type inference system makes our
system more modular than previous work. We believe that
the gained modularity can also be a great aid to implemen-
tors.
A point we have not discussed so far is how to implement
parametric type classes at run-time. Essentially, a trans-
lation scheme into Haskell along the lines of WB89] can
be employed. Additional parameters for type classes trans-
late then into parameters for run-time dictionaries. Such a
translation can provide a (transformational) semantics for
parametric type classes. Whether it can also provide a good
run-time model is debatable. Existing implementations that
are based on this translation scheme have been criticized for
their run-time performance. We argue that, in principle,
the run-time performance of a program with type classes
should not be any worse than the performance of a program
written in an object-oriented language. Moreover, similar
optimization techniques can be used CU90].
References
CCH + 89] P. Canning, W. Cook, W. Hill, W. Oltho, and
J. Mitchell. F-bounded polymorphism for object-
oriented programming. In Proc. ACM Conf. Func-
tional Programming Languages and Computer Archi-
tecture , pages 273{280, 1989.
COH92] Kung Chen, Martin Odersky, and Paul Hudak. Type
inference for parametric type classes. Technical Re-
port YALEU/DCS/RR-900, Dept. of Computer Sci-
ence, Yale University, New Haven, Conn., May 1992.
CU90] Craig Chambers and David Ungar. Iterative type
analysis and extended message splitting: Optimizing
dynamically-typedobject-orientedprograms. In Proc.
SIGPLAN '90 Conf. on Programming Language De-
sign and Implementation , White Plains, New York,
June 1990.
DM82] L. Damas and R. Milner. Principal type schemes
for functional programs. In Proc. 9th ACM Symp.
on Principles of Programming Languages , pages 207{
212, Jan. 1982.
HJW91] Paul Hudak, Simon Peyton Jones, and Philip L.
Wadler. Report on the programming language
Haskell: a non-strict, purely functional language, ver-
sion 1.1. Technical Report YALEU/DCS/RR-777,
Dept. of Computer Science, Yale University, New
Haven, Conn., August 1991.
Jon91] Mark P. Jones. Type inference for qualied types.
Technical Report PRG-TR-10-91, Oxford University
Computing Laboratory, Oxford, UK, 1991.
Jon92a] Mark P. Jones. Coherence for qualied types. Private
communication, March 1992.
Jon92b] Mark P. Jones. A theory of qualied types. In
B. Krieg-Bruckner, editor, Proc. European Sysposium
on Programming , pages 287{306. Springer Verlag,
Feburary 1992. LNCS 582.
JT81] R.D. Jenks and B.M. Trager. A language for computa-
tional algebra. In Proc. ACM Symposium on Symbolic
and Algebraic Manipulation , pages 22{29, 1981.
Kae88] S. Kaes. Parametric overloading in polymorphic pro-
gramming languages. In H. Ganzinger, editor, Proc.
2nd European Symosium on Programming, Lecture
Notes in Computer Science, Vol. 300 , pages 131{144,
Nancy, France, March 1988. Springer-Verlag.
Lil91] Mark D. Lilibridge. A generalization of type classes.
distributed to Haskell mailing list, June 1991.
MGS89] J. Meseguer, J. Goguen, and G. Smolka. Order-
sorted unication. J. Symbolic Computation , 8:383{
413, 1989.
NS91] T. Nipkow and G. Snelting. Type classes and over-
loading resolution via order-sorted unication. In
J. Hughes, editor, Proc. Conf. on Functional Pro-
gramming and Computer Architecture , pages 15{28.
Springer-Verlag, 1991. LNCS 523.
Rob65] J. Robinson. A machine-oriented logic based on
the resolution principle. J. Assoc. Comput. Mach. ,
12(1):23{41, 1965.
Rou90] Francois Rouaix. Safe run-time overloading. In Sev-
enteenth Annual ACM Symp. on Principles of Pro-
gramming Languages , pages 355{366, San Franscico,
CA, January 1990.
VS91] Dennis M. Volpano and Geoery S. Smith. On the
complexity of ML typability and overloading. In
J. Hughes, editor, Proceedings of Functional Pro-
gramming and Computer Architecture , pages 15{28.
Springer-Verlag, 1991. LNCS 523.
Wad90] P. Wadler. Comprehending monads. In Proc. ACM
Conf. on LISP and Functional Programming , pages
61{78, June 1990.
Wad91] P. Wadler. Continuing monads, August 1991. Tutorial
Notes at FPCA'91.
WB89] Phil Wadler and Stephen Blott. How to make ad-
hoc polymorphism less ad hoc. In Sixteenth An-
nual ACM Symp. on Principles of Programming Lan-
guages , pages 60{76. ACM, 1989.
