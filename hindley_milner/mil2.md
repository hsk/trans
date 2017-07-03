  JOURNAL OF COMPUTER AND SYSTEM SCIENCES 17, 348-375 (1978)
  A Theory of Type Polymorphism in Programming
  ROBIN MILNER
  Computer Science Department, Vm+ersity of Edinburgh, Edinburgh, Scotland
  Received October 10, 1977; revised April 19, 1978
  The aim of this work is largely a practical one. A widely employed style of programming,
  particularly in structure-processing languages which impose no discipline of types,
  entails defining procedures which work well on objects of a wide variety. We present a
  formal type discipline for such polymorphic procedures in the context of a simple programming
  language, and a compile time type-checking algorithm w which enforces the
  discipline. A Semantic Soundness Theorem (based on a formal semantics for the language)
  states that well-type programs cannot “go wrong” and a Syntactic Soundness Theorem
  states that if fl accepts a program then it is well typed. We also discuss extending these
  results to richer languages; a type-checking algorithm based on w is in fact already
  implemented and working, for the metalanguage ML in the Edinburgh LCF system,
  1. INTRODUCTION
  The aim of this work is largely a practical one. A widely employed style of programming,
  particularly in structure-processing languages which impose no discipline of types
  (LISP is a perfect example), entails defining procedures which work well on objects of
  a wide variety (e.g., on lists of atoms, integers, or lists). Such flexibility is almost essential
  in this style of programming; unfortunately one often pays a price for it in the time taken
  to find rather inscrutable bugs-anyone who mistakenly applies CDR to an atom in
  LISP, and finds himself absurdly adding a property list to an integer, will know the
  symptoms. On the other hand a type discipline such as that of ALGOL 68 [22] which
  precludes the flexibility mentioned above, also precludes the programming style which
  we are talking about. ALGOL 60 was more flexible-in that it required procedure
  parameters to be specified only as “procedure” (rather than say “integer to realprocedure”)
  -but the flexibility was not uniform, and not sufficient.
  An early discussion of such flexibility can be found in Strachey [19], who was probably
  the first to call it polymorphism. In fact he qualified it as “parametric” polymorphism,
  in contrast to what he called “adhoc” polymorphism. An example of the latter is the use
  of “+” to denote both integer and real addition (in fact it may be further extended to
  denote complex addition, vector addition, etc.); this use of an identifier at several distinct
  types is often now called “overloading,” and we are not doncerned with it in this paper.
  In this paper then, we present and justify one method of gaining type flexibility, but
  also retaining a discipline which ensures robust programs. We have evidence that this
  348
  0022-0000/78/0173-0348$02.00/0
  Copyright 8 1978 by Academic Press, Inc.
  All rights of reproduction in any form reserved. 
  TYPE POLYMORPHISM 349
  work is not just a theoretical exercise; the polymorphic type discipline which we discuss
  here has been incorporated in the LCF metalanguage ML [2, 31, and has been in use for
  nearly 2 years. The compile-time type checker for this language has proved to be a
  valuable filter which traps a significant proportion of programming errors.
  The main body of the present paper is concerned with a technical account-both
  semantic and syntactic-of our discipline of types in the context of a simple illustrative
  language, but at this point it is helpful to characterize the approach informally. We
  outline its predominant features.
  First, everything concerning types is done at compile time; once the type checker
  (part of the compiler) has accepted a program or program phrase, code may be generated
  which assumes that no objects carry their types at run-time. This is widely accepted as
  yielding efficient object code, though it does impose constraints on the use of types
  compared with, for example, the approach in EL1 [21].
  Second, many nontrivial programs can avoid mentioning types entirely, since they be
  inferred from context. (In ML however, as in other languages, the user may-indeed
  often should-define his own types together with operations over these types. Recent
  languages which allow the user to define his own types in this manner are CLU [8],
  ALPHARD [23] and Euclid [6]). Although it can be argued convincingly that to demand
  type specification for declared variables, including the formal parameters of procedures,
  leads to more intelligible problems, it is also convenient-particularly in on-line programming-to
  be able to leave out these specifications. In any case, the type checker which
  we present is quite simple and could not be made much simpler even if the types of
  variables were always specified in declarations.
  Third, polymorphism plays a leading role. For example, a procedure is assigned a
  polymorphic type (which we abbreviate to polytype) in general; only when the types of
  its arguments and result can be uniquely determined from the context is it monomorphic
  (i.e., assigned a monotype). Gries and Gehani [4], among others, have made a convincing
  case for controlled polymorphic programming (in contrast with the typeless programming
  in LISP or in SNOBOL); for them however, and also for Tennent [2B], the presence of
  type variables or identifiers is needed to specify polymorphic types. For us, the polymorphism
  present in a program is a natural outgrowth of the primitive polymorphic
  operators which appear to exist in every programming language; such operators are
  assignment, function application, pairing and tupling, and list-processing operators.
  It is principally the type constraints on these operators, and in the declaration and use
  of variables, which determine for us the types of a program phrase and its subphrases.
  We do not discuss in this paper-except briefly at the end-either coercions or the
  “overloading” of identifiers. Our view is that these concepts, and also run-time type
  manipulation, are somewhat orthogonal to a compile-time polymorphic type discipline,
  and may (to some extent) be incorporated without invalidating it.
  In Section 2 we illustrate our type discipline by examples in a fragment of ML. This
  fragment should be self-explanatory, but an outline of ML is given in [3] and a full
  description appears in [2]. These illustrations should serve to make the point that we
  are able to handle useful languages. The remainder of the paper justifies the discipline
  using a very simple applicative language, Exp. The justification factors into two parts. 
  350 ROBIN MILNER
  In Section 3 we define the notion of well typing (correct type assignment) and prove the
  Semantic Soundness Theorem, which says that a well-typed program is semantically
  free of type violation. If we were to give an operational definition of the language, this
  would imply that, for example, an integer is never added to a truth value or applied to an
  argument, and consequently need not carry its type around for run-time checking. In
  Section 4 we present a well-type algorithm ?Y- and prove the Syntactic Soundness
  Theorem, which states that YK, if it succeeds, produces a well typing of a program. We
  also give a more efficient algorithm $, which simulates Y$‘-.
  The types in Exp are just the hierarchy of purely functional types over a set of basic
  types. That is, the polymorphism in Exp is the natural outgrowth of a single primitive
  polymorphic operator, function application, together with variable binding. To add
  other primitive polymorphic operators, such as pairing and list-processing operators
  (as in ML), together with types built from basic ones x (Cartesian Product), list (listforming),
  and + (disjoint sum) in addition to (function type), presents no extra difficulty
  in the two soundness theorems. Indeed, adding an assignment operator is also easy as far
  as the Syntactic Soundness Theorem is concerned, but the Semantic Soundness Theorem
  is harder to extend in this case, due to the extra semantic complication of a memory or
  store which holds the current values of assignable variables. We discuss this further in
  Section 5.
  Our work is a step towards solving the problem expressed by Morris [lo] in his thesis
  as follows: “to design a language and a type system in which a programmer may define
  functions whose parameters may have different types for different calls of the function.”
  We recommend Chapter 4 of this thesis as a lucid introduction to the problem. Although
  Morris does not discuss the semantics of types formally, or give a polymorphic type
  system, he describes how a valid type assignment may be found for a term of the Xcalculus
  by solving a set of simultaneous linear equations; we take this idea further in the
  next section.
  After doing this work we became aware of Hindley’s [5] method for deriving the
  “principal type scheme” (which is what we call a polymorphic type) for a term in combinatory
  logic. Hindley appears to have been the first to notice that the Unification
  Algorithm of Robinson [14] is appropriate to this problem. Our work can be regarded
  as an extension of Hindley’s method to programming languages with local declarations,
  and as a semantic justification of the method.
  In summary, we present a polymorphic type discipline which is syntactically well
  understood and justified for a currently used programming language with imperative
  features, and is also semantically explained for a nontrivial, though nonimperative,
  sublanguage.
  2. ILLUSTRATIONS OF THE TYPE DISCIPLINE
  We illustrate our notion of polymorphism by means of some simple examples. They
  are written in a fragment of ML which we hope is self-explanatory; this fragment is
  indeed no more than Landins ISWIM [7], and we refer the reader to Burge’s book [l] in 
  TYPE POLYMORPHISM 351
  which he uses this style of programming almost exactly. We use no imperative constructs
  here (assignments or jumps). The constructs
  let x = e in e’,
  Zetf(xl ,..., x,) = e in e’
  are used to give x the value of e, and to givef the value of the abstraction h(x, ,..., x,) . e,
  throughout e’. For recursive functions letrec is used in place of let, and when the part in e’
  is omitted we have a declaration.
  The fully determined types (i.e., the monotypes) of ML are built from a set of basic
  types (int, boo& etc.) by the binary infixed operators x (Cartesian product), + (disjoint
  sum) and -+ (function type), and the unary postfixed operator list. Polymorphic types
  (polytypes) are obtained by admitting type variables, which here are represented by
  %AY ...- We represent arbitrary types by p, u, 7. For this section we leave the meaning
  of types to the reader’s intuition; it is made precise in the next section.
  EXAMPLE 1. Mapping a function over a list.
  Zetrec map(f, m) = if null (m) then nil
  else cons (f(hd(m)), map (f, d(m))).
  Intuitively, the function map so declared takes a function from things of one sort to
  things of another sort, and a list of things of the first sort, and produces a list of things
  of the second sort. So we say that map has type
  ((a -+ f9) X LY list) + /3 list,
  where 01, /3 are type variables.
  How is this type determined from the bare declaration of map ? First, the generic
  types (we discuss “generic” later) of the identifiers occurring free in the declaration are
  given by
  null: 01 list -+ bool,
  nil: 01 list,
  hd: 01 list + 01,
  tl: 01 list + a list,
  cons: (IX X ~1 list) + a Zist,
  that is, they are polymorphic, because their types contain one or more type variables,
  and our rule is: To every occurrence of such an identifier is assigned a type which is a
  substitution instance (substituting types for type variables) of its generic type.
  Now each of these identifiers occurs just once in the declaration, so if we denote by
  aid the type assigned to an identifier id we must have for some types or ,..., 75 ,
  anull = TV list -+ boo&
  Unil = T2 list,
  uhd = 73 list + 73 ,
  at1 = r4 list --+ r4 list,
  ucons = (TV x T~ list) -+ T~ list. 
  352 ROBIN MILNER
  The other identifiers (map, f, m) each occur more than once, and our rules demand that
  each occurrence is assigned the same .type. The rules also demand that the following
  equations are satisfied for some types p1 , pz ,...,
  ornap =Of X am-+p1,
  anull = am + bd,
  ahd = Urn -+ p2 ,
  utl = urn + p3 ,
  Uf = P2 -+ P4 ,
  umap = Of X p3 -+ p5 ,
  (Jcom3 = P4 x p5 - PI3 ,
  Pi = Cmil = Pe .
  The first of these conditions relates the type of a function to those of its formal parameters;
  each of the other conditions arises from some subterm which is a function application,
  except the last, which is because a conditional expression has the same type as its two
  arms, and because the definiens and definiendum of a declaration have the same type.
  Now these equations may be solved for the variables pi , TV , and Uid ; Morris [IO]
  discusses the solution of such equations. Indeed, the situation is entirely appropriate
  for the use of the Unification Algorithm of Robinson [14]; our well-typing algorithm is
  based upon this algorithm, and (since in this case nothing more than unification is
  needed) we may conclude from Robinson’s work that the most general type of map is
  obtained, i.e., any other type amap which satisfies the equations must be a substitution
  instance of the type obtained. In fact, the solution of the above equations is
  Umap = (y + 6) X y list -+ 6 list,
  where y, 8 are any distinct type variables. So this is the generic type of map, that is, to
  any occurrence of map within the scope of this declaration must be assigned some
  substitution instance of this type.
  These instances need not be the same. Suppose that tok is a basic type (a token being
  a sequence of characters) and that we have available the identifiers (with their types)
  and
  tokl: tok list (a variable),
  length: tok -+ int,
  sqroot: int + real, two obvious functions.
  Then in the expression
  map(sqroot, map(length, tokl))
  the two occurrence of map will have types
  ((tok --f int) x tok list) --f int list,
  ((id -+ real) X +zt list) -+ real list. 
  TYPE POLYMORPHISM 353
  Similarly, if null, for example, had occurred twice in the definition of map, its types could
  have been different instances of
  ol list ---f boo1
  but our rules demand that different occurrences of a formal parameter (f, for example),
  or of an identifier (map) being recursively defined, shall have the same type.
  In passing, note that the occurrences of map mentioned above can be regarded as uses
  of two separately declared (and monomorphic!) map functions, which differ only in that
  different types are explicitly provided for their arguments and results. As Gries and
  Gehani remark, the compiler could be given the task of generating these distinct declarations-or
  more accurately (since the programmer need not see the replication or even be
  aware of it), the task of generating different code for the body of the map function for
  use at distinct types. This would indeed be necessary in the above example if, for efficiency,
  token lists were implemented differently from integer lists (and the primitive polymorphic
  functions hd, tl etc., were correspondingly different). We are concerned with a conceptual
  framework in which these map functions may all be regarded semantically as the same
  object; then the implementor is left with the freedom to implement as few or many
  variants as he wishes.
  It is clear from our example that the rules of typing need to be carefully framed. We
  leave this task until the next section, but here we look at one more example to illustrate
  what happens when let or letrec is used locally.
  EXAMPLE 2. Tagging. Suppose we want a function tagpair, such that tagpair (a) is a
  function under which
  (6 4 t-+ ((a, 4, (a, 4).
  Of course, we can easily write
  let tagpair = h(b, c) . ((a, b), (a, c)).
  Now we can explain, without setting up equations, how our well-typing algorithm
  tackles this declaration. It first assigns “unknown” types (i.e., type variables) 01, /3, and y
  to a, b, and c. Then ((a, b), ( a, c )) ac q uires type (a x /I) x (CY x ‘y), and the h-expression
  acqmres /3 x y + (CC X /3) X (LX X y); finally tagpair acquires
  a - (18 x y - (d x B> x (u x Y)) (*)
  (no type equations have placed any constraint upon the types of a, b, and c).
  But consider another way of defining tagpair, using the (infixed) function
  #: (cd - 8) x (y - 6) - ((a x Y> -(B x 3)
  such that (f# g)(u, c) = (f(a), g(c)), and the pairing function 
  354 ROBIN MILNER
  such that pair(a)(b) = (a, b). We could write
  let tagpair = ha * (let tag = pair(a) in tag #tag)
  We might then expect the well-typing algorithm to proceed as follows. First, 01 is assigned
  to u. Then, using the generic type of pair, pair(a) acquires 6 + 01 x 6. This is then used
  as the local generic type of tag, and the two occurrences of tag in tag # tag are assigned
  B -+ a1 x B, Y - % x Y, respectively. The occurrence of # is assigned an instance of
  its generic type (again using new type variables) and the type equation for function
  application will cause the type
  P x Y -+ (011 x PI x (012 x Y)
  to be assigned to tag # tag, and to the body of the h-expression, so that tagpair acquires
  the type
  a- (B x Y - (a1 x PJ x (% x Y))* (**)
  Now comparing (*) with (**), something has gone wrong; the second type is too
  general. The problem is that tag and its generic type depend upon the h-bound variable a
  and its type a?, and we do not allow different bound occurrences of a h-bound variable
  to have different type. Indeed, as far as type is concerned, we should get the same as if
  tagpair were defined in yet a third way, by
  let tagpair = ha . (pair(u) # pair(u))
  and the reader may be able to obtain (by setting up some equations as in Example 1) the
  expected type (*) in this case.
  The solution is fortunately straightforward. We decree that in instantiating the type
  of a variable bound by let or by letrec. only those type variables which do not occur in
  the types of enclosing /I-bindings (or formal parameter bindings) may be instantiated.
  We call such instantiable variables (in a generic type) generic type variables.
  Now in the second definition of tagpair, the locally defined tag acquired a generic
  type 6 -+ Q x 6, in which 6 is generic but CL is not. Thus 01 should not have been instantiated
  in assigning types to the occurrences of tag, and then (**) would have been identical
  with (*). This example may appear a little contrived; indeed, our experience has been
  that almost always either all or none of the type variables of a generic type are generic.
  But there seem to be no simple syntactic constraints which would eliminate the exceptions,
  nor does it seem desirable to do so.
  From the examples it becomes clear that the rules for typing variables bound, respectively,
  by let (or Zetrec) and by h are going to be different. Thus, although our semantics
  for the two expressions
  letx = eine’; (Ax . e’)e
  may be (and are) equivalent, it may be possible to assign types correctly to the former
  but not to the latter. An example is the pair
  let1 = Ax . x’inI(1); (AI . I(l))(Ax . x). 
  TYPE POLYMORPHISM 355
  A (partial) intuition for this is that a h-abstraction may often occur without an argument;
  the second expression above contains a special (and rather unnecessary) use of abstraction,
  in that an explicit argument-(hx . x)-is present. Since the let construct (when translated)
  involves this restricted use of an abstraction, it is not unnatural that its rule for type
  assignment is less constrained. A compiler could, of course, treat all explicit occurrences
  of (hx . e’)e in the less constrained manner.
  The treatment of types in the interaction between h-bindings (i.e., formal parameter
  bindings) and let bindings is really the core of our approach. It gives a consistent treatment
  of the nonglobal declaration of a procedure which may contain, as a free variable, a
  formal parameter of an enclosing procedure. This appears to be one of the more crucial
  difficulties with polymorphism, and therefore we feel justified in presenting our analysis
  in terms of a simple language (Exp) which excludes as much as possible that is irrelevant
  to the difficulty.
  The reader may still feel that our rules are arbitrarily chosen and only partly supported
  by intuition. We certainly do not claim that the rules are the only possible ones, but the
  results given later demonstrate that they are semantically justified. In fact, we show that a
  program which admits a correct type assignment cannot fail at run-time due to a type
  error-or more precisely, that type constraints encoded in its semantics are always
  satisfied. It follows from this that compile-time type checking (i.e., the attempt to discover
  a correct type assignment) obviates the need to carry types at run-time, with an obvious
  gain in the efficiency of implementation.
  This is of course a principal aim in compile-time type checking; another is the early
  detection of programming errors (many of which result in ill-typed programs). Our
  achievement is to extend type checking to embrace polymorphism. Moreover the typechecking
  algorithm in its final form (Algorithm $ in Section 3) is remarkably simple,
  even though the proof of the Syntactic Soundness Theorem, which states that-if it
  succeeds-it produces a correct type assignment, is rather tedious.
  We would like to give an independent characterization of the class of programs which
  can be well typed in our system, but we have no idea how to do this. However, we can
  give some pointers. At the suggestion of a referee we looked at Burge [l, Chapt. 31
  concerning general functions for processing data structures. All of the functions there
  (with the exception of Section 3.11 which we did not examine) acquired the expected
  types from the ML type checker after they had been modified in two respects. First,
  Burge leaves implicit the coercion functions between a disjoint sum type and its summand
  types; we needed to make these explicit (this point was mentioned in our Introduction).
  Second, we used the ML abstract type construct (see Section 5 for an example) to
  formulate the recursive type definitions used by Burge. In this construct, the isomorphism
  between a defined abstract type and its representation is made explicit and must be used
  explicitly. To see the need for this requirement consider the case of an a-stream, which
  is defined to be a function which yields a pair consisting of an 01 and an or-stream. The
  type equation
  a-stream = - - - + (a X a-stream)
  cannot be solved by unification (unless we allow infinite type expressions). But by 
  356 ROBIN MILNER
  treating the equation as an isomorphism, and using two functions to convert back and
  forth between an abstract type and its representation, this difficulty is removed. We
  claim that this solution is in keeping with the notion of abstract type (see [8], for example).
  On the negative side, there are certainly useful expressions which we cannot well type,
  though we are not clear how burdensome it is to do without them. An obvious example
  is Curry’s Y combinator.
  y = Af * (Xx ‘fw4w * few)
  since self-application is ill typed for us. But Zetrec avoids the need for Y. More practically,
  consider
  le@(f 1 = A(4 4 . (f W,fW
  which-it may be argued-should accept as argument a function which is polymorphic
  enough to operate upon a and b of different type. For example,
  F(reverse)(x, y)
  produces a pair of reversed lists of different type if x and y are lists of different type.
  Our system rejects such a use of F (since it requires a and b to have the same type), but
  admits
  let reversepair = h(x, y) . (reverse(x), reverse(y))
  or any other specialization of the function argument of F.
  We feel that this example illustrates the main limitation of our system, but that we
  may have kept as much flexibility as is possible without the use of explicit type parameters.
  When these are introduced, the problem arises of the type of types; Reynolds [12] has
  made some progress in solving this problem, but we were anxious to see how much
  could be done while avoiding it.
  3. A SIMPLE APPLICATIVE LANGUAGE AND ITS TYPES
  3.1. The Language Exp
  Let x range over identifiers, that is
  xEId.
  Then the expression language Exp is generated by the following grammar:
  e ..- a*- x I(ee’)l ife then e’ else e” 1
  Ax-eIfixx*ejletx=eine’.
  Here (ee’) means application, jix x . e stands for the least fixed point of hx * e, and the
  last clause binds x to the value of e throughout e’. We often use d, e, f-with primes and 
  TYPE POLYMORPHISM 357
  sutlixes-to range over Exp. Constants are omitted; we can imagine instead some standard
  bindings for certain identifiers in an initial environment.
  We give an ordinary denotational semantics for Exp, in which we include a value
  “wrong,” which corresponds to the detection of a failure at run-time. In this small
  language, the only failures are the occurrence of a non-Boolean value as the condition
  of a conditional, and the occurrence of a nonfunctional value as the operator of an
  application.
  Our semantic domains may be taken to be complete partial orders (cpos); a cpo D
  (see [9]) is a partially ordered set such that (a) there exist a minimum element, ln ,
  (b) every directed subset of D has a least upper bound in D. Take as given a set {Bi} of
  basic domains, with B, = T, the three element truth value domain
  true false
  and we define recursively
  V=B,+B,+--+F+ W (disjoint sum of domains, with
  Iv adjoined as minimum element),
  F=V--+V (continuous functions from V to V),
  W={*> (error).
  The solution (up to isomorphism) of such a set of domain equations is assured by Scott
  [15]. Although he worked with complete lattices, the solution also exists in cpos (see
  Plotkin [ 111).
  The semantic function is d E Exp + Env -+ V, where Env = Id -+ V, the domain
  of environments. We use 7 to range over Env. In defining 8, and later, we use some
  familiar notation of Scott and Strachey [16], illustrated by these examples (where D is
  some summand of V):
  (i) If d E D, then d in V is the image of d under the injection of D into V.
  (ii) If v E V, then
  vED==true ifv =din VforsomedED,
  = J-T ifv = Iv,
  = false otherwise.
  (iii) If v E V, then
  vlD=d ifv =din V,forsomedED,
  = J-D otherwise. 
  358 ROBIN MILNER
  The environment 7’ = ~{v/x} is identical with 7 except that q’(x) = V. The value . in V
  {* E IV) is written “wrong.” We require the conditional function COND E T -+ Y -+
  I’-+ V, where COND tvv’ is written t + V, v’ and takes the value
  V if t = true,
  V’ if t = false,
  IV if t=lT.
  3.2. Semantic Equations for Exp
  In these equations, the open brackets [i ] indicate syntactic arguments.
  b[(e,e,)]v = vl E F -+ (vZ E W - wrong, (q I F)v,),
  wrong
  where vi is 6[e& (i = 1, 2).
  a[$ e, then e2 else e,]r] = vl E B, -+ (vl 1 B, -+ v2 , vJ, wrong
  where vi is &[e& (i = 1, 2, 3)
  S@X . en7 = (A0 . S/p] 77(2)/x)) in V
  &jj-ji~ x - e]7j = Y(hv - b~el7){7$})
  b[Zet x = e, in e,Jq = v1 E W-+ wrong, b[e2] 7.1&/x}
  where q = &[e&.
  Notes. (i) Y is the least fixed-point operation. In many languages the e in jix f * e
  would be restricted to be an abstraction Ay . e’, and then
  letf =fixf - (Ay . e’)
  might receive the syntax
  let reef(y) = e’
  (ii) It is easy to see that “let x = e, in e,” has the same meaning under d as
  “(Ax * e,)e,“. But part of our aim is a type discipline which admits certain expressions in
  the first form and yet rejects their translations into the second form; this is because Xabstractions
  may in general occur without an explicit operand, and need more careful
  treatment.
  (iii) The semantics for (erea) corresponds to call-by-value, since the test “vs E w”
  ensures that the meaning of (erea) is IV if the meaning of e2 is Iv . The omission of
  this test gives a call-by-name semantics (a similar test may be omitted in the semantics of
  the let construct), and the Semantic Soundness Theorem goes through equally in this
  case. 
  TYPE POLYMORPHISM 359


## 3.3. Discussion of Types

  We now proceed, in outline, as follows. We define a new class of expressions which
  we shall call types; then we say what is meant by a value possessing a type. Some values
  have many types, and some have no type at all. In fact “wrong” has no type. But if a
  functional value has a type, then as long as it is applied to the right kind (type) of argument
  it will produce the right kind (type) of result-which
  cannot be “wrong”!
  Now we wish to be able to show that-roughly
  speaking-an
  Exp expression evaluates
  (in an appropriate
  environment)
  to a value which has a type, and so cannot be wrong.
  In fact, we can give a sufficient syntactic condition
  that an expression has this robust
  with respect to the
  quality; the condition
  is just that the expression has a “well-typing”
  environment,
  which means that we can assign types to it and all its subexpressions
  in a
  way which satisfies certain laws.
  So there are two main tasks, once the laws of type assignment are given. The first-to
  show that an expression (or program) with a legal type assignment cannot “go wrong”--
  is tackled in this section; surprisingly
  enough, it is the easier task (at least for an applicative
  language). The second task is to discooer a legal type assignment, given a program with
  incomplete
  type information.
  This task is often called type checking. Of course, this term
  can also mean just verifying that a given type assignment is legal; in a practical situation
  we probably require something between the two, since one cannot expect a programmer
  to attach a type to every subexpression.
  In Section 4 we look at the class of legal type
  assignments
  for a given program
  (the class is infinite in general, since we admit poly-
  morphism),
  and we give an algorithm
  which, if it succeeds, produces a legal type assign-
  ment. We conjecture that if the latter exists then the algorithm
  finds one which is most
  general, in the sense that any other legal type assignment is a substitution
  instance of it
  (substituting
  types for type variables).
  3.4.
  Types and their Semantics
  The syntax
  of types is as follows.
  (1) 10, Ll ,... are (basic) types; one for each Bi .
  (2) There is a denumerable
  set of type variables, which
  to range over type variables.
  (3)
  If p and o are types, so is p ➙ 0.
  A monotype is a type containing
  no type
  monotypes.
  We use the word polytype when
  contain a variable.
  We first give the semantics of monotypes;
  a value v E V possesses a monotype p, which
  (i)
  (ii)
  are types. We use CX, /3, y,...
  v:Liiffzq
  variables. We use CL, V, zr,... to range over
  we wish to imply that a type may, or does,
  that is, we give the conditions
  we write v : CL.
  = lvorvEB,
  a:p➙Viffeitherv=~,,oruEFand(vIF)u:vwheneveru:~.
  under
  which

360

  It is clear then that many values have no type. Examples are
  wrong, (hv E V . wrong) in V,
  (he, E V * v E B, ➙ (v 1 B,, ➙ x in V, y in V), wrong) in V
  (where x E B, , y E B, for example).
  But in the last example if y E B, instead, then the function has type 1s ➙ c1 (and no
  other).
  Some values have many types; the identity function
  (hv E V . v) in Y
  for example has type p + p for every p. And of course Iv has every type (it is the only
  value which has every type).
  This notion of type is derived from Scott [17]. In fact, it is what Scott callsfunctionality
  (after Curry), and is distinct from the notion of a retract. If we temporarily identify a type
  with the set of values which possess it, then it is easy to show that types are downward
  closed and directed complete, that is
  (i)
  (ii)
  Vv, v’ E V * (v : TV and v’ E v) G- v’ : p,
  For each directed subset X of V, (Vv E X * v : p) =P Ll X : p.
  Retracts share the second property, but not the first. Recently Shamir and Wadge [ 181
  have defined a type to be any set with these two properties, and they investigate the
  consequences of identifying a value v with the type {v’ 1 v’ E v}.
  The semantics of polytypes is as follows. First, we use p < a to mean that p may be
  obtained from a by substituting types for type variables ( < is clearly reflexive and
  transitive). For example,
  but
  Then we define
  v:aiffVp<a.v:p.
  For example,
  (hv . v) in v : a ➙ 01.
  Polytypes thereby also stand for subsets of V, and these are also directed complete. The
  reader may like to think of each type variable in a polytype as universally quantified at
  the outermost; for example,
  d + a “means” Va * a ➙ 01,
  where the bound (II ranges over monotypes. In fact, it is because a here ranges over
  monotypes (not all types) and because we do not admit expressions like
  (Va . a ➙ a) ➙ (Va . d ➙ a)TYPE
  361
  POLYMORPHISM
  as types-though
  we can see they “mean ” if the bound variables are taken to range over
  monotypes-that
  we avoid the difficulties (and also some of the interest) of Reynolds [12]
  in his richer notion of type.
  We need the following simple properties, which are immediate from our definitions.
  If
  v : CJ and 7 < o then v : 7.
  PROPOSITION I.
  PROPOSITION 2. If v : u + 7 and v’ : u, then (v 1 F)v’ : T.
  In each case, a property of monotypes is lifted to polytypes.
  3.5. Type Assignments
  To prepare the ground for the theorem that well-typed expressions cannot “go wrong,”
  we need to define what is meant by typing an expression. We need first some notion of
  a type environment to give types to the free variables in an expression.
  A prefix p is a finite sequence whose members have the form let x, fix x, or AX, where x
  is a variable. A prefixed expression (pe) has the form p 1 e, where every variable free in e
  occurs in a member of p. We separate the members of a prefix by a period (.).
  Every pe has sub-pe’s given by the following, together with transitive reflexive closure:
  (i) p 1 x has no sub-pe’s except itself,
  (ii)
  (iii)
  p 1 (ee’) has sub-pe’s p 1 e and p I e’,
  p / (zf e then e’ else e”) has sub-pe’s p / e, p I e’ and p / e”,
  (iv) p j (hx
  (v)
  (vi)
  e) has sub-pe p . hx 1 e,
  . e) has sub-pe p . J;x x 1 e,
  P/($xX
  p j (let x = e in e’) has sub-pe’s p j e and p . let x / e’.
  For example, hy / (let
  f = Ax . (xy)
  in (fy))
  xy j xx . (xy), AY . hx I (XY),
  kv . let f I (b9, b
  * letf
  If,
  has sub-pe’s (besides itself)
  AY . Ax I x,
  h . letf
  xy xx I y,
  I y
  so a sub-pe is just a subexpression prefixed with all the variable bindings which enclose it.
  Note that /\x (x,v) in the above is not enclosed by let f-it
  is not in the scope of this
  binding.
  We say that a member let x or fix x or Xx of p is active in p just if no x occurs to the
  right of it in p.
  Now a typing of a pep 1 e is an assignment of a type to each element ofp, and to each
  subexpression and each /\x, $x x, or let x in e, with the constraint that in a subexpression
  (let x = e’ in e”) the same type is assigned to let x and e’. Thus one typing of the illustrated
  pe (it is nearly, but not quite, a well typing in the sense later defined) is as follows:
  AK I Pfh+)-8
  = (~~,-,(X,-,Y,)B)(,-,)-,
  in (f64+,yJ,),
  .
  We denote a typing of p I e by p I Z, or by p 1 e -O when we want to indicate the type v
  assigned to e itself.362
  ROBIN
  MILNER
  In any 3 1 r?, and any binding let x0 in either $ or #, a type variable in o which does not
  occur in (the type of) any enclosing hy7 or jix y, binding is called a generic type variable
  for the binding let x, . In the example above, p is generic, but 01 is not, for the binding
  let f(m+B)+B . Intuitively, the generic type variables in a binding let x0 represent degrees
  of freedom for the types at which x may be used; they represent the local polymorphism
  of x. Notice that if no h orjx bindings enclose let x0 , then all the type variables in o are
  generic. Ageneric instance of u is an instance of u in which only generic type variables are
  instantiated.
  For technical reasons we require that generic type variables occur in a controlled
  manner. We say that p 1 2 is standard if for every typed sub-pe ji’ 1 c?’ (with induced
  typing) the generic type variables of each member let x, of p’ occur nowhere else in
  p’ 1 Z. Thus in particular, if let x, = fP in S: is a subexpression of 2, with induced typing,
  then the generic type variables in p may not occur in CL (though they must of course
  occur in E,).
  We now define the notion of a well-typed (wt) pe as follows:
  (i)
  jJ 1 x, is wt iff it is standard, and either
  (a) Ax, or fix x, is active in jT, or
  (b) let
  (ii)
  x, is active in jJ, and T is a generic instance of u.
  j7 (t?,,~?:)~
  is wt iff p 1 e and p / t? are both wt, and p = o -
  7.
  (iii) p (if ZO then EL else k$7, is wt iff fi / Z, p 1 E’ and p 1 C” are all wt, p = L,, , and
  0 = 7 = 7’.
  .
  .
  =p+o.
  (iv> P (Ax, . e,),lswtlffp.hx,(eiswtand7
  (9 F (J;x x0 ~~~),iswtiff~~Jixx,I~iswtandp=a=~.
  (vi> F (let x, = E,, in g& is wt iff ji ) z and j5 . let x, / 3 are both wt, and CJ = T.
  Although this recursive definition is useful for some proofs, an alternative charac-
  terization of wt is sometimes useful. The proof of the next proposition is fairly straight-
  forward, and we omit it. Note that a wt p 1 ais necessarily standard, by an easy structural
  induction.
  PROPOSITION 3.
  (A)
  j7 ) a is wt $J the following
  conditions
  hold:
  It is standard.
  (B) For every (bound) occurrence x, , the corresponding
  Ax, , OY fix x, , OT let x, , where o is a generic instance of T.
  (C)
  The following
  conditions
  binding
  occurmtce
  hold for all subexpressions (with induced typing)
  (VA
  (if Ed then C$ else z:)~,
  (~0 * CT,>,
  (fix x, . CA
  (let x, = E;, in iTi),
  p=a+r,
  p=rOando=~=r’,
  7=p-0,
  p=o=r,
  (T = 7.
  is either
  of dTYPE
  POLYMORPHISM
  363
  The typing
  which we illustrated above therefore fails to be wt for only one reason:
  The subexpression (f~u+v)+,,yu)v violates the first of conditions (C) in Proposition 3.
  Consider another example. The following (with 5 empty) is a well typing:
  Note that 0~ is generic in the type OL --f 01 of the declaration of I, so may be instantiated
  (possibly differently) in the types of bound occurrences of 1.
  To illustrate the need to instantiate only generic type variables, for variables declared
  by let, notice first that in hx, x, we must have 01 = /3, by condition (B) of Proposition 3.
  Indeed, we can argue intuitively for this as follows: if we declare
  let I = Ax . x in . . . .
  then we wish to have that any expression (1e) in the scope of this declaration receives the
  same type as the subexpression e. But now suppose we write (with assigned types)
  let LB
  = (Xx, . (let ya = x, in ys)s)m+s in . . .
  then-since this is semantically equivalent to the simpler declaration-we should again
  demand that a = fl. But this is imposed in our definition of well typing,
  just because
  (y
  is not generic for the binding let yoi , so may not be instantiated in a bound occurrence
  of ?‘.
  3.6. Substitutions
  A substitution S is a map from type variables to types. S may be extended in a natural
  way to yield a map from types to types, from typed pe’s to typed pe’s, etc.
  We say that S involves a type variable ti if either Sor # 01, or for some p # (Y, 01 E S/3.
  (a E 7 means 01 occurs in T.)
  We need substitutions extensively in the second part of this paper, but for the present
  we need only one property relating substitutions and wt.
  PROPOSITION
  4. If S involves no generic variables of a wt p 1 a, then S(p / a) is also a wt.
  Proof. We use Proposition 3. First, observe that the assumption on S yields that
  the generic variables for each binding in S(j5 ] a) are exactly those for the corresponding
  binding in p 1 a. Since Sp contains no generic variable when /3 is not generic, S(fi / a) is
  standard.
  Second, if x, is bound by /\x, or Jix x, in p 1 a, then x,, is bound by hx,, or $x xsO in
  S(j5 1 a). If X, is bound by let x, , and u = [p&r ,..., pn /LX,& where oli are the generic
  variables of 7, then in S($ 1 a) x,, is bound by let x,, , and So = [$~,/a~ ,..., SP~/Q~](,%)
  is a generic instance of 5%.
  Third, conditions (C) of Proposition 3 are easily verified for S(p / a), using identities
  like S(u ➙ T) = Sa ---f ST. Q364
  3.7.
  ROBIN
  Well-Typed
  Expressions
  MILNER
  Do Not Go Wrong
  First we need a simple relation between semantic environments 7 and our type en-
  vironments-which
  are typed prefixed p. We say
  r] respects p iff, whenever let x, or hx, or
  $x x, is active in jJ, $c] : p.
  THEOREM
  1 (Semantic Soundness).
  If
  7 respects jj and p 1 & is well
  typed
  then
  &[dJT : 7.
  Proof.
  A fairly simple structural induction. Take the six cases for & .
  (9 x7 . Then either Xx, or fix x7 is active in 3, and ~l[xll : 7, so b//x]7 : 7, or let x0
  is active in p, and ~[[x] : o; but then T < u, so ai[x]~ = ~[x] : 7, by Proposition 1.
  (ii) (&+$,J7 . Then F 1 E,,+., is wt, so b[[ejs : u ➙ 7, and similarly &[e’]v, : 0. Then
  from the semantic equation (remembering that wrong has no type) and by Proposition 2
  we get &[dJ+ : T.
  (iii) (if cLO then 5: else E:). Straightforward; the only extra detail needed here is that
  Iv has every type.
  (iv) (Ax, * G),+. . Then 8 * Xx, 1 E, is wt. Now we require (hv . &[[e] r]{v/x}) in V
  : p ---f 0. Denote this function by fin V. The inverse of Proposition 2 does not hold,
  that is, to show f in V : p + o it is not sufficient (though it is necessary) that whenever
  v:p,fv:a.Whatisrequiredisthatforevery77--,v~p4u,finV:~4~.
  Suppose then that p + v < p ➙ u. Then there is a substitution S, involving only the
  type variables in p and u, such that p ---f v = S(p -➙ u). Then, since none of these type
  variables is generic in p . h, 1 Z~ , it follows that S(p) * hx, 1 S(8)” is wt by Proposition 4.
  Moreover 7 respects S(j) ( since by Proposition 1 whenever q[x]l : a’ and 7’ < u’, q[x] : r’)
  so for any v : p we also have ~{v/x} respects S(j) . hx, .
  It then follows by induction that Sue] $v/x} : v, so we have shown that v : /-L implies
  fv : V, and this yields f in V : TV ➙ v as required.
  (4
  (fix x0 . %,x * Then 4 . fix x, 1 c,, is wt. Now we require that v : p, where
  v =
  Y(Ad
  . aleg
  ~{v’/x}).
  Now v = U, vi, where v,, = Iv, vi+r = Sue] ~{TQ/x}, and by the directed completeness
  of types we only have to show vi : p for each i.
  Clearly v,, : p. Assume vi : p. Since 7 respect P; we have that ~{voi/x} respects p * jix x, ,
  so by the main induction hypothesis vi+1 : p also, and we are done.
  (vi) (let x = ~~ in ~i)~ . Then p 1 E,, is wt, so we immediately have v : p, where
  v = 6?[[&. We require bi@‘] 7(21/x) : u.
  Now p * let x, 1 g: is also wt, and because v : p we have that q{u/x} respects p * let x, ;
  the rest follows by the induction hypothesis. 136.5
  TYPE POLYMORPHISM
  As a corollary, under the conditions of the theorem we have
  WIT
  # wrong,
  since wrong has no type.
  4. A WELL-TYPING
  4.1.
  The Algorithm
  ALGORITHM
  AND ITS CORRECTNESS
  W
  In this section we tackle the question of finding a well typing for a prefixed expression.
  We present an algorithm %‘- for this. We would like to prove that Y#‘- is both syntactically
  sound and (in some sense) complete. By syntactic soundness, we mean that whenever ?Y
  succeeds it produces a wt; by completeness, we mean that whenever a wt exists, V
  succeeds in finding one which is (in some sense) at least as general.
  Although YY is probably complete, it is difficult to find a simple proof. So we con-
  centrate on soundness, and then comment on implementation of YV and on extending
  it to deal with richer languages. Since a type-checking algorithm which simulates w has
  been working successfully for nearly 2 years in the context of the LCF metalanguage
  ML [2], we have evidence for its usefulness and even-to some extent-for
  its com-
  pleteness.
  YK is based on the unification algorithm of Robinson [14]. Indeed, the only feature of
  well typing which does not fall directly within the framework of unification is the condi-
  tion that 7 should be a generic instance of a whenever x, is bound by let x, . The com-
  pleteness (in some sense) of YY should follow from the second part of the following
  proposition concerning unification, but we need only the first half for our proof that Y&‘-
  is sound.
  PROPOSITION 5 (Robinson).
  There is an algorithm
  @‘, taking a pair of expressions
  (over some alphabet of variables) and yielding a substitution,
  such that for any pair of
  expressions o and r
  (A) If %(a, T) succeeds, yielding
  (B) If R unifies a and 7, then %( 0,~) succeeds yielding
  stitution
  U, then U unifies u and T (i.e., Uu = UT).
  a U such that for
  some sub-
  S, R = SU.
  Moreover,
  U involves only variables
  in a and 7.
  To find a well typing of a complete program f, we would expect to supply also a typed
  prefix j?, containing only let bindings, giving the types of values bound to predefined
  identifiers. We would then expect ?Y to yield 3 such that p 1 f is a wt.
  To state (and prove) YY recursively however, prefixes containing all types of binding
  occur, and w in general needs to modify the nongeneric type variables in the prefix to
  meet constraints imposed on the program. We therefore make w return also a sub-
  stitution T, indicating the necessary transformation.366
  ROBIN
  MILNER
  To be precise, we show that if %‘(ji, f) succeeds and returns ( T, f), then ( Tfi) 1 f is a wt.
  We first state YY. At certain points ?Y” requires type variables which have not previously
  occurred; such new type variables are denoted by j? or ,t?( . ?P’@, f) is defined by induction
  on the structure off; the algorithm is expressed in a purely applicative programming
  style, in contrast with the more efficient algorithm $ presented later, which is expressed
  more in the style of imperative programming.
  Algorithm W
  w@ff) = (T,J), where
  (i)
  If f is X, then:
  if hx, or Jix x0 is active in fi then
  T=I,j’=x,;
  if let x, is active in p then
  T=I,f=x,
  where T = (;Bi/tii] O, 01~ are the generic variables of a,
  and fit are new variables.
  (ii)
  If
  f is (de), then:
  let (R, ;E,) = @IF, 4, and (S, .?J = %“(Rjj, e);
  let U = @(Sp, u ➙ fl), /3 new;
  then T = USR, and3 = U(((Sd)&).
  (iii)
  If f is (;f d tha e else e’), then:
  let (R, &) = W@, d) and Us = @(p, co);
  let (S, &,) = %‘j U&, e), and (S’, EL,) = ~(SU,J@,
  let U = %(Su, 0’);
  then T = US’SU,,R, and
  3 = lJ((if SSUoa then SE else a’),).
  (iv)
  Iffis
  (hx . d), then:
  let (R, ii?) = W(ji . Ax,, d), where /? is new;
  then T = R, and3 = (Xx,, . itP)Ra+rr
  .
  (v)
  If
  f is (jx x . d), then:
  let (R, &) = W(p . fix x, , d), B new;
  let U = @(R/l, p);
  then T = UR, and3 = ($x XusB * UJ),
  (vi)
  If
  .
  f is (let x = din e), then:
  let (R, (2b) = W(ji d);
  let (S, CJ = W(Rp . let x, , e);
  then T = SR, and3 = (let x,, = Sa in & . 1
  e’);TYPE
  367
  POLYMORPHISM
  4.2. The Soundness of W
  To show that Y#‘” is sound it is convenient to have a few simple definitions.
  If A is a type, a typed prefix, or a typed pe then
  Vars(A) dz {a 1 01 E A, CL a type variable?.
  If A is a typed prefix or a typed pe then
  Gen(A) dzf {a 1 OL E A, oi a generic type variable).
  Spec(A) gf Vars(A) - Gen(A).
  If S is a substitution, then
  Inv(S) dzf {a ( S involves a>
  = {a 1 3/3 * S/kI # /3 and 01 E (p}
  Vars(.!$)).
  u
  We need the following simple properties, whose proof we omit:
  PROPOSITION
  (B)
  THEOREM
  6. (A) Inv(RS) c Inv(R) u Inv(S)
  Vars(&) C Vars(T)
  u
  Inv(S).
  2 (Syntactic Soundness).
  Let ji be a standard
  prefix,
  and p / f a (closed) pe.
  Then, if@V,ff) = (T,f,),
  (A) Tfi 11 is wt,
  (B) Inv( T) C Spec(j5) u New,
  (C) Vars(T) C Spec($)
  and
  where New is the set of new type variables
  u
  New,
  used by W.
  Proof.
  By induction on the structure off, using the recursive definition of wt. We
  omit the cases of conditional and jix expressions, since nothing new arises there, and we
  treat the easier of the other cases first
  (i) f is X. Then T = I, so (B) is immediate. If XX, or $x X, is active in fi then
  3 = x, and (A), (C) are immediate.
  If let x, is active, then3 = x, , where T = &4+,
  (q} generic in 0, New = {j&}. Then
  Tji j 3 = p 1 x, is standard and (A), (C) follow easily.
  (iv)
  f is (hx . d). Let (R, 4) = %‘(jY . ;\~a , d), using new variables New, , say.368
  ROBIN
  MILNER
  By induction,
  R@ * hx,) 1 &, is wt, so for (A)
  Also by induction,
  Inv(R)
  Var+)
  I
  C Spec(p
  j = RjT 1 (Ax,, * d)Ro+p is wt (defn of wt).
  * Xx,) u New,
  = Spec(p) u (/I}
  = Spec(i) u
  u
  New,
  New
  (since New
  = New, U {p})
  since T = R. For (C)
  and (B) follows
  by Proposition
  Vars(R/3 + p) C Inv(R) U {@ U Vars(p)
  C Spec(p) U New
  6,
  as required.
  (vi) f is (let x = d in e). Then
  say. Then by induction
  let (R, &,) = YY@, d), using new variables
  Rji
  Inv(R)
  Vars(f > I
  Now
  from
  1 a is wt
  C Spec(B)
  New,
  ,
  (1)
  u
  (2)
  New,
  (2)
  Spec(R$)
  C Inv(R)
  Spec(p)
  u
  Z Spec($)
  u
  New,
  (3)
  and from (1) by standardness
  Gen(Rp
  12) n Spec(Rp)
  = 0.
  (4)
  We also have that Gen(Rp)
  = Gen@), which is disjoint from Vars(p) by (2), hence
  RF * let x, is a standard prefix.
  So let S, & = W(R# * let x, , e) using new variables New, . Then by induction
  S(RP
  - let x,) [ T is wt
  Inv(S)
  C Spec(RP
  Vars(a) I
  * let x0) u New,
  But Spec(RF * let x0) = Spec(R$),
  so putting
  (6) and (4) together
  are new variables)
  Inv(S) n Gen(Rp 1 a) = 0
  and it follows
  of wt that
  by Proposition
  4 that S(Rji
  (5)
  (6)
  yields
  (since
  New,
  1 2) is wt, and using (5) we have by definitionTYPE
  369
  POLYMORPHISM
  is wt; but this is just Tp 1 j, so we have proved (A). For (B), we have
  Inv(T) C_ Inv(S) u Inv(R), by Proposition 6,
  C Spec@) u New, u New,, using (6), (3), and (2)
  and for (C), by similar reasoning,
  Vars(o) 23- Spec@)
  This is all we need, since New = New,
  u
  New, u New, .
  u
  New, in this case.
  (ii) f is (de). Let (R, JO) = %‘(p, d), using new variables iVew, , and (S, co,) =
  $fl(Rp, e), using new variables New, ; then by similar reasoning to case (vi) we find that
  SRF I S(;E,) is wt (7)
  SRj5 / cO is wt (8)
  Now if U = %(Sp, (3 --f fi), where p is new, we have by Proposition 5 that
  usp = uff ➙ u/3,
  Inv( U) C Vars(Sp)
  U
  (10)
  Vars(o)
  U {p}.
  (11)
  It follows that U involves no generic variables of the wt’s (7) and (8), and that
  USRP I W(S4~)s)
  is wt; but this is just Tp 1 f, so we have proved (A). For (B), we have first that
  New
  =
  New,
  u
  New,
  u
  {p},
  SO
  Inv( T) Z Inv( U) U Inv(SR),
  c Spec($) U New,
  by Proposition 6,
  from (9) and (1 l),
  and for (C),
  Vars(T) = Vars(U@
  C IMu)
  u {PI,
  C Spec@) u New,
  by Proposition 6,
  again from (9) and (11).
  1
  4.3. Implementation of w; a Simplified Algorithm $
  As it stands, ?Y is hardly an efficient algorithm; substitutions are applied too often.
  It was formulated to aid the proof of soundness. We now present a simpler algorithm f
  which simulates w in a precise sense.370
  ROBIN
  MILNER
  9 differs from ?&‘- in two ways. First, we adopt an idea familiar in the literature on
  resolution-based theorem-proving systems, in which substitutions are composed, but
  only applied when it is essential to do so. Second, we take advantage of the fact that what
  is often needed in practice from a well-typing algorithm is not the whole type assignmentf,
  but only the type assigned to f itself.
  In fact $ builds only one substitution, called E, which is idempotent-that
  is, EE = E
  -which is to say that if /? E I&, then E/3 = /3. This substitution is held in a program
  variable (called E) global to $, and $ works by transforming E. In place of the unification
  function %!, $ calls a unification procedure UNIFY which delivers no result but side-
  effects the variable E. We assume that @ and UNIFY are related as follows: of E and E’
  are the values of E before and after the command
  UNIFY(o,
  7)
  and if
  S(Eu, ET) = U
  then
  E’ = UE.
  Thus, applying UNIFY to types 0 and 7 in the presence of E corresponds to applying
  9% to types Ea and ET; D and 7 may be thought of as implicit types standing for the explicit
  types which would be gained by applying the “explicating” substitution E (the idem-
  potency of E means that further explication is unnecessary). The effect of UNIFY (if
  successful) is to generate the new explicating substitution E’. f will similarly handle an
  implicit typed prefix 3, which can be explicated when necessary by applying E. We
  assume that f has local variables p, u, and (T’, whose values are implicit types, and
  generates its result in a fourth variable 7.
  Assuming an initial idempotent E, j is given a typed prefix 3 and an expression f
  such that EjY is standard and p 1 f is a pe (i.e., all free identifiers in f are bound in p).
  Here is the algorithm:
  Algorithm $
  $(ji, f) = T, where
  (i)
  Iff is x, then:
  If hx, or jix x0 is active in p, 7 : = a.
  If let x, is active in 3, 7 := L;s,/q]Eu, where
  ai are the generic type variables of let x,, in Ep,
  and /3i are new variables.
  (ii)
  If
  f is (de) then:
  p := f@, d); u := $(jY, e);
  UNIFY (p, (I ➙ 8); (j3 new)
  7 := /3TYPE
  (iii)
  If f is (if d then e else e’), then:
  ,J := $(p,
  u := $(F,
  UNIFY(a,
  (iv)
  d); UNIFY
  (p, ~0);
  e); CT’ := $(p, e’);
  u’); T :== u
  If f is (kc . d), then:
  p := $@
  ‘:-=+p
  (v)
  371
  POLYMORPHISM
  If f is (jx
  x
  Ax, , d); (/I new)
  d), then:
  P : = d(P . $x xf3 ,4; (P new)
  UNIFY@,
  (vi)
  Iffis
  /3
  (Zet x === d in e) then:
  p :=
  7 :=
  What
  following
  involving
  p); 7 :=
  $@, d); u :=
  u. I
  $(p
  . let X, , e);
  is the simulation
  relation between 2 and W? It is simply expressed by the
  proposition,
  whose proof we omit, since it is an easy structural
  induction
  few of the subtleties which we encountered
  in the soundness theorem.
  PROPOSITION
  7. Let p / f be a pe, E be idempotent, and E1; be standard. Then $(ji, f)
  succeeds ( producing 7’ and a new value E’for E) ifJ Y(Ej?, f) succeeds ( producing T andfT),
  and moreover if both succeed
  (A) E’ = TE,
  (B) E’T’ = 7.
  Thus the type produced
  by $, when explicated,
  is exactly that ascribed to f by yf.
  In practice, E may be efficiently represented
  by a table INST of variable-type
  pairs,
  representing
  those variables which have hitherto been instantiated.
  The effect of UNIFY
  is merely to add some entries to INST,
  representing
  instantiation
  of previously
  un-
  instantiated
  variables. The substitution
  E represented
  by INST is given recursively
  as
  follows
  E(c) = c
  (basic types),
  EC4 = E(P)
  ar
  E(u -
  if (or, p) E INST
  otherwise,
  for some p,
  T) = Eu ---f ET.
  In fact, in the extended version of f implemented
  for ML, which is written in LISP,
  INST itself is represented by a property (in the LISP sense) INSTANCE
  of type variables.
  Each type variable 01 has p as its INSTANCE
  property value, if (a, p) E INST for some p;
  otherwise the property value is NIL.372
  ROBIN MILNER
  5. TYPES IN EXTENDED LANGUAGES
  We now consider some extensions of our language, and how our results may be
  strengthened to apply to them.
  (1) As we said in the introduction, the addition of extra (primitive) type operators
  such as x (Cartesian product), + (disjoint sum) and list (list forming), causes no
  difficulty. Together with ➙, these are the primitive type operators in the language ML.
  For x one has the standard polymorphic functions
  pair:cL-➙/I➙(cu. X/3)
  fst: a! x /3 ➙ 01,
  snd: 01 x /3 ➙ /I.
  (one could add the syntax (e, e’) for pair (e)(e’)),
  For +, one has
  inl: a➙~4+/?,
  outl: a + p ➙ a,
  isl: ol + p---f bool,
  inr: /3 ➙ OL + fl
  outr: (~+fi➙fl
  isr: ol + p --f bool
  (left and right injections),
  (left and right projections),
  (left and right discriminators)
  with natural interpretations. For list, one has the standard list-processing functions
  mentioned in Section 2. Notice that all members of a list must have the same type.
  With appropriate adjustment to the semantic domains, the Semantic Soundness
  Theorem extends naturally; the Syntactic Soundness Theorem goes through virtually
  without change.
  (2) Next, we consider assignable variables and assignments. One way (used in ML)
  of adding these is to allow the assignment expression form “x := e” (whose value is
  the value of e), and the expression form “Zetref x = e in e”’ to declare an assignable
  variable, initialized to the value of e. The first effect of these additions is a major change
  in the semantic domains, since now expressions may have side effects. Although we
  believe that a Semantic Soundness Theorem may be proved, it appears to be a cumber-
  some task. The reason for the difficulty is illustrated by considering
  Ax. (y :=x)
  which is an identity function with a side effect. To say that it has type 01--f Q, meaning
  that whenever it is given an argument of type p it gives a result of type p, takes no account
  of the side effect on y. What is required is a more careful definition (in terms of the
  semantic domains) of what such functional types mean, which takes side effects into
  account.
  By contrast, it is easy enough to give well-typing rules for the two new kinds of expres-
  sion. We would extend the definition of wt by the following clauses:
  (i) (c) If letref X, is active in a standard prefix p, then fi ( X, is wt. (Thus, all
  letref-bound occurrences of x must have the same type).TYPE
  (vii)
  (viii)
  373
  POLYMORPHISM
  p \ (xp := 2i,), is wt iff 5 ( B is wt, and letref x, is active in 5, and p = u = T.
  5 1 (1 ere
  t f x,=e,in~~),iswtiffpIeandh.letrefx,Ia’arewt,anda=r.
  It is a routine matter to extend the algorithm w and the Syntactic Soundness Theorem
  to handle these clauses. But returning again to semantic considerations, there is a problem
  concerned with nonlocal assignments within X-abstractions (procedures). Consider
  let g = (Zetrefy = nil in hz . y : = cons(z, y))
  which sets up y as a hidden (own) variable for the function g. With our rules, g obtains
  generic type 01➙ ol list. Remembering that the value of an assignment is the value
  assigned, each call of g augments the hidden y, returning the augmented list as value.
  Now since 01 is generic, the expressions
  g(2),
  dtrue)
  are both admissible within the scope of g, with types int list and boo1 list, respectively.
  But if they are evaluated in this order, the value of the second expression is a list whose
  two members are true and 2; it is not a list of Boolean values!
  There are at least two possible solutions to this dilemma. One is to decree that in such
  a situation (Y is not a generic type variable. Another, which we adopt in ML, is to forbid
  nonlocal assignments to polymorphic
  assignable variables within h-abstractions (proce-
  dures). Polymorphic assignable variables are still useful, for example, in programming
  iterations (whiZe statements) which themselves can be given simple wt rules.
  I believe that the second solution, even slightly relaxed, admits a Semantic Soundness
  Theorem. The details, however, are unattractive, and I have been discouraged (particularly
  after a useful discussion with John Reynolds) from attempting to complete the proof.
  What is rather needed is a language design which pays more respect to side effects; one
  approach might be to modify PASCAL by requiring that all variables assigned in a
  procedure be listed as output parameters of the procedure. But how to combine this
  with the rather useful properties of own variables is, as far as I know, an open problem
  in language design, and a good solution would be a valuable step forward. For a recent
  promising attempt to control side effects see Reynolds [13].
  (3) To complete the list of nontrivial extensions which we have included in ML,
  consider the declaration (possibly recursive) of a new type operator in terms of old ones.
  Such a declaration may have nonglobal scope. If it is also accompanied by the declaration
  of a set of functions over the new type operator, and the explicit definition of the type
  operator is only available in defining the set of functions (not within the whole scope of
  the new type operator), one has a version of what is currently called abstract type. In ML,
  we would define the class of binary trees whose tips are labeled by objects of arbitrary
  type as follows, using the type variable 01 to stand for the type of tip labels:
  absrectype
  with
  and
  and
  01 bitree = Q! + (01 bitree
  sons(t) = ...
  maketree(t, t’) = ...
  tiptree
  = ...
  x
  01 bitree)374
  ROBIN
  MILNER
  in which only the omitted defining expressions are given access to the representation
  of bitrees. The defined functions are polymorphic, with generic types
  ti bitree ➙ (a bitree)2,
  (CX bitree)2 + (y. bitree,
  a: --f 01 bitree.
  For full details of this construct, see [2]; we owe the construct partly to Lockwood Morris
  and to discussion with Jerry Schwarz. In this case the wt rules are also rather easy; we
  have not checked syntactic and semantic soundness, but suspect that there should be no
  great difficulty.
  (4) Two features which contribute a kind of polymorphism have been completely
  ignored so far. The first is coercions. The expression x : = 42, where x is a real assignable
  variable, is ill typed for us. However, there is no barrier to having the type checker
  report such instances of ill typing and allowing the compiler to receive the report and
  insert a coercion to rectify it.
  The second feature is to allow certain procedures-either
  standard or user defined-to
  possess more than one type. We may wish “+”
  to possess int2 ➙ int and real2 ---f real,
  without of course possessing 01~ ➙ ~11
  (which is the least general polytype having the two
  given types as instances). While we have not investigated the question, there appears to
  be a good possibility of superposing this feature upon our discipline.
  6.
  CONCLUSION
  We have presented a discipline of polymorphic programming which appears to be
  practically useful, and have given a rather simple type-checking algorithm. In a restricted
  language we have shown that this algorithm can be proved correct (the proof was factored
  into two Soundness Theorems). Though much work remains to be done, we hope to
  have made the point that the practice of type checking can and should by supported by
  semantic theory and proof.
  ACKNOWLEDGMENTS
  I am indebted
  to the referees
  for their apposite
  comments
  on the first version
  of this paper,
  which.drew
  my attention
  to some inaccuracies
  and also led me to a clearer
  exposition
  in several
  places. My thanks also go to Dorothy
  McKie
  for her careful preparation
  of two versions
  of the paper.
  REFERENCES
  1. W. H. BURGE, “Recursive
  2. R.
  M.
  GORDON,
  Science
  3.
  M.
  Dept.,
  Edinburgh
  R. MILNER,
  proof
  in LCF,
  of Programming
  GORDON,
  interactive
  Principles
  MILNER,
  Addison-Wesley,
  Reading,
  Mass., 1975.
  Programming
  Techniques,”
  AND C. WADSWORTH,
  “Edinburgh
  LCF,”
  CSR-11-77,
  Computer
  University,
  1977.
  L. MORRIS,
  M. NEWEY, AND C. WADSWORTH,
  A metalanguage
  for
  in “Proc.
  5th Annual
  ACM
  SIGACT-SIGPLAN
  Symposium
  on
  Languages,
  Tucson,
  Arizona,
  1978.”TYPE
  POLYMORPHISM
  375
  4. D. GRIES AND N. GEHANI,
  Some ideas on data type in high-level
  lcnguages,
  Comm. ACM.
  20
  (1977),
  414-420.
  5. R. HINDLEY,
  The principal
  type-scheme
  of an object in combinatory
  logic, %zx~. Amer. &ikth.
  Sot. 146 (1969),
  29-60.
  6. B. W. LAMPSON,
  J. J. HORNING,
  R. L. LONDON,
  J. G. MITCHELL,
  AND G. L. POPEK, Report
  on
  the programming
  language
  Euclid,
  SIGPLAN
  Notices
  (ACM)
  12, 2 (1977).
  7. P. J. LANDIN,
  The next 700 programming
  languages,
  Comm. ACM
  9 (1966),
  157-164.
  8. B. H. LISKOV AND S. ZrLLEs,Programming
  with abstract
  data types, in “Proc.
  of ACM
  SIGPLAN
  SIGPLAN
  Notices
  (ACM)
  9 (1974),
  50-59.
  conference
  on Very
  High Level Languages,”
  9. R. MILNER,
  “Models
  of LCF,”
  Mathematical
  Centre,
  Amsterdam,
  Tracts,
  Vol. 82, pp. 49-63,
  1976.
  10. J. H. MORRIS,
  “Lambda-Calculus
  Models
  of Programming
  Languages,”
  Ph.D.
  Thesis,
  M.4C-
  TR-57,
  MIT,
  1968.
  11. G. PLOTKIN,
  A power-domain
  construction,
  SIAM
  J. Comput.
  5 (1976),
  452-487.
  Systems
  and Inform.
  Sci., Syracuse
  12. J. C. REYNOLDS,
  “Towards
  a Theory
  of Type Structure,”
  University,
  1974.
  13. J. C. REYNOLDS, Syntactic
  control
  of interference,
  in “Proc.
  5th ACM
  Symposium
  on Principles
  of Programming
  Languages,
  Tucson,
  Arizona,
  1978,”
  pp. 39-46.
  14. J. A. ROBINSON,
  A machine-oriented
  logic based on the resolution
  principle,
  J. Assoc. Comput.
  Much.
  12 (1965),
  23-41.
  15. D. SCOTT, Lattice
  theoretic
  models
  for various
  type-free
  calculi,
  in “Proc.
  4th International
  Congress
  for Logic,
  Methodology
  and Philosophy
  of Science,
  Bucharest,
  Rumania,
  1972.”
  16. D. SCOTT AND C. STRACHEY,
  Towards
  a mathematical
  semantics
  for computer
  languages,
  in
  “Proc.
  Symposium
  on Computers
  and Automata,”
  Vol. 21, Microwave
  Res. Inst. Symposia
  Series, Polytech.
  Inst. of Brooklyn,
  1971.
  17. D. SCOTT, Data types as lattices,
  SIAM
  j. Comput.
  5 (1976),
  522-587.
  18. A. SHAMIR AND W. WADGE,
  Data types as objects,
  in “Proc.
  4th ICALP
  Conference,
  Turku,
  Finland,
  1977.”
  19. C. STRACHEY,
  Fundamental
  concepts
  in programming
  languages,
  Notes
  for the International
  Summer
  School in Computer
  Programming,
  Copenhagen,
  1967.
  20. R. D. TENNENT,
  On a new approach
  to representation-independent
  data classes, Acta Inform.
  8
  (1977),
  315-324.
  21. B. WEGBREIT,
  The treatment
  of types in ELl,
  Comm. ACM
  17 (1974),
  251-264.
  22. A. VAN WIJNGAARDEN
  ET AL., Revised
  report
  on the algorithmic
  language
  ALGOL
  68, Acta
  Informutica
  5 (1975),
  l-236.
  23. R. A. WULF,
  R. L. LONDON,
  AND M. SHAW, “Abstraction
  and Verification
  in ALPHARD:
  Introduction
  to Language
  and Methodology,”
  ISIIRR-76-46,
  Univ.
  of California,
  1976.
  571:17/3-7
