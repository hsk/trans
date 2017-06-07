# パラメトリックおよびアドホック多型の型制約解法

Bart Demoen 1、Maria Garcia de la Banda 2、Peter J. Stuckey 3

1 コンピュータサイエンス学科、K.U.ルーベン、ベルギー
2 モナッシュ大学コンピュータサイエンス学科
3 メルボルン大学コンピュータサイエンス学科

## 要約

単一化は、関数型プログラミングで Hindley-Milner 型の型検査と型推論のメカニズムとして長く使われてきました。
プログラマは可能な型を定義し、コンパイラは関数定義の型をチェックしたり推論したりするために単一化を使います。
制約論理プログラミングでは、述語と関数定義のオーバーロード、つまりアドホック多相型を許可することによって、関数プログラミングの場合を拡張するのは当然です。
Mycroft と O'Keefe は、これらの前提のもとで述語型宣言をチェックする方法を示しました。
本論文では、与えられた型の制約論理プログラムを型上の論理プログラムに変換することにより、述語型を推論する方法を示します。
次に、プログラムを使用して、元のプログラムに現れる述部および変数の可能なタイプを検査および推論することができます。

翻訳されたプログラムを実行することは、異種の型が存在する場合には効果的ではないため、効率的な型推論を達成するために、伝播に基づく制約解消とメモの方法を使用します。

## 1 Introduction

  Unication has long been used as a mechanism for type checking and type inference
  of Hindley-Milner types [Milner, 1978]. For example, in functional languages
  the programmer is required to dene the possible types, so that the compiler can
  use unication to check and infer types for functions. In this scheme, the type of
  a function is considered as a conjunction of tree equations over type constructors.
  This method neatly handles parametric polymorphism and higher-order types.

  Mycroft and O'Keefe [Mycroft and O'Keefe, 1984] applied the same approach
  to (constraint) logic programs. Given type denitions and declaration of predicate
  types they check the predicate types are valid. Unlike the functional programming
  case they allow multiple types to be declared for functors and predicates.
  They also realised that this type checking can be expressed as a logic program.
  In this paper we extend this approach, allowing predicate types to be inferred
  rather than declared. We explicitly dene a translation of a (constraint) logic
  program with type denitions to a logic program which can be used to infer

  ----

  Proceedings of the Twenty Second Australasian Computer Science Conference, Auckland,
  New Zealand, January 18{21 1999. Copyright Springer-Verlag, Singapore. Permission
  to copy this work for personal or classroom use is granted without fee provided
  that: copies are not made or distributed for prot or personal advantage; and this
  copyright notice, the title of the publication, and its date appear. Any other use or
  copying of this document requires specic prior permission from Springer-Verlag.

  ----

<!-- page 2 -->

  types of predicates. We then investigate more robust approaches to executing
  the resulting program using constraint solving methods. Note that modern CLP
  languages (e.g. Mercury [Somogyi et al., 1996] and CIAO [Hermenegildo, 1994])
  increasingly include facilities for types to be dened and used.

  The constraint based view of typing is simply a rephrasing of the approach
  of Milner [Milner, 1978]: variables represent their own type (they become type
  variables) and each fragment of program text denes type constraints on the variables
  involved. For example, assuming that the (parametric) type of the cons
  function is (T ; list(T ); list(T )),1
  the equation [Y |Y s] = X s denes the type
  constraint Y = T 0
  ^Y s = list(T 0
  )^X s = list(T 0
  ) where T 0
  is a new type variable.
  Each usage of cons inherits a new copy of this constraint: this corresponds
  to the generic types of Milner. Similarly the atom p(X,Y ) where predicate p
  has type (int; fn(T ; T )) yields the constraint X = int ^ Y = fn(T 00 ; T 00). The
  type constraint of a conjunctive goal is obtained by conjoining the constraints
  arising from its literals. The type constraint of a rule is the constraint of its body,
  with the exception that recursive calls are constrained to have the same type as
  the head. This is Milner's non-generic type for identiers which are not free in
  the body (the recursive literals). The type constraint of a predicate is obtained
  by conjoining the constraints dened by its rules.

  We (as [Mycroft and O'Keefe, 1984]) extend Milner's framework by allowing
  overloading of predicates and functions, that is, ad-hoc polymorphism. In
  order to do this we must consider the type of a predicate or function as a
  disjunctive constraint formula over types, one disjunct for each ad-hoc polymorphic
  denition. This modication allows us to handle ad-hoc polymorphism
  in a straightforward way. Assume, for example, that we overload the + functor
  for use on both ints and floats. We can then dene its type constraint as T+ =
  (int; int; int) _ T+ = (float; int; float)_ T+ = (int; float; float) _ T+ =
  (float; float; float): Now, consider the following program (left) for summing
  a list of numbers, and the type constraints dened by each of its rules (right):

      sumlist(L, S) :- Tsumlist = (L; S) ^
      [S] = L. (S; (Tnil); L) = 0
      (Tcons) ^
      sumlist(L,S) :-
      [X|Xs] = L, (X; X s; L) = 00(Tcons) ^
      X + S1 = S, (X; S1; S) = 000(T+) ^
      sumlist(Xs, S1) (X s; S1) = Tsumlist:

  where Tnil = (list(T )), Tcons = (T ; list(T ); list(T )) and ; 0
  ; 00 and 000 are
  renamings which rename to new disjoint variables. Note how generic uses are
  renamed to use new variables, while the non-generic recursive call is replaced by
  an equation which equates the type of the predicate (the head variables) with
  the type of the recursive call. Then, if we conjoin the constraints dened by each
  rule, place the resulting constraint in DNF and pro ject away variables, we obtain
  the (disjunctive) answer constraint: Tsumlist = (list(int); int)_Tsumlist =
  (list(float); float).

  ----

  1 We use a relational form of type declarations

  ----

<!-- page 3 -->

  In the terminology of type systems for logic programs, the type system we
  describe is a prescriptive type that adds extra information to the program. For
  example, the unique type for append is (list(T ); list(T ); list(T )) which disallows
  some of its success set. The key property of a Milner type system is that
  type correct programs do not go wrong (have type incorrect execution) and hence
  types can be completely compiled away. This continues to hold for type correct
  programs where types may be disjunctive.

  Type checking of logic programs is performed by several systems like those
  described in [Mycroft and O'Keefe, 1984; Rouzaud and Nguyen-Phuong, 1992]
  and [Meyer, 1996]. However, none of the implemented systems infer predicate
  types nor deal with higher-order predicates in a complete way. Type inference
  is performed by several abstract interpretation frameworks: without an attempt
  to cover literature completely, we mention [Hentenryck et al., 1995; Gallagher
  and de Waal, 1994; Janssens and Bruynooghe, 1992]. However, in these frameworks
  the type denitions themselves are derived from the program clauses. We
  assume given type denitions and we check and/or infer predicate typings using
  only these type declarations: in this respect, our work is closer to [Codish
  and Demoen, 1994] which also assumes the set of possible types to be given
  to the analysis. The closest in spirit to our type checking/inference approach
  is the Mercury system [Somogyi et al., 1996]: the Mercury compiler contains a
  type checker that also infers types for local predicates missing a pred-declaration
  and it uses only the user provided types, i.e. it does not infer new type denitions.
  However, Mercury's implementation diers substantially from ours: it
  uses a xpoint algorithm that essentially infers types bottom-up by successive
  approximation. This works very well in practice, but it can have performance
  problems for exceptional cases and also for badly typed programs. In particular,
  the bottom-up approach can lead to non-terminating inference. Our approach is
  based on a program transformation; it applies constraint technology and selective
  memoing: together these techniques resulted in an implementation with high
  performance even if the transformed program is executed under Prolog, which is
  several times slower than Mercury. The nal dierence with Mercury is that by
  treating recursive calls non-generically, we eliminate the need for a xed point
  computation.

## 2 Translating a CLP program with type denitions

  In the following we use Mercury syntax for dening types. A type constructor f
  is a functor of arity n. A type expression is a type variable v or a term of the
  form f (t1; : : : ; tn) where f is an n-ary type constructor, and t1; : : : ; tn are type
  expressions. A type denition for f is of the form

      :- type f (v1; : : : ; vn) ---> (f1(t11; : : : ; t1m1 );    ; fk(tk1; : : : ; tkmk )).

  where v1; : : : ; vn are distinct type variables, f1; : : : fk are distinct functors, and
  t11; : : : ; tkmk are type expressions, involving at most the variables v1; : : : ; vn.

  Mercury also allows hidden type denitions and equational type denitions
  which are handled by our approach but we omit these due to lack of space.

<!-- page 4 -->

  Example 1. Consider the following type denitions 2
  for lists, pairs, nodes (for
  representing a graph, a node has an identier and a list of adjacent node identi
  ers), arithmetic expressions, and functional applications.

      :- type list(T) ---> ([] ; [T|list(T)]).
      :- type pair(T) ---> (T - T).
      :- type node(T) ---> (T-list(T)).
      :- type expr ---> (var(pair(int)) ; expr * expr ; expr - expr ; - expr).
      :- type fn(I,O) ---> (I --> O).

  Translating type denitions: A type denition denes types for the functors
  appearing in the denition. The set of types for a functor denes a type
  constraint that applies whenever the functor is used in a program, which is
  encoded by the atom func f, by our translation. The translation of a type
  denition of the form

      :- type f (v1; : : : ; vn) ---> (f1(t11; : : : ; t1m1 );    ; fk (tk1; : : : ; tkmk )):

  results in the rule func fi(ti1; : : : ; timi; f (v1; : : : ; vn)) for each functor fi in the
  denition.

  Functors may appear in several type denitions (contrary to the functional
  programming case, but also allowed by [Mycroft and O'Keefe, 1984]), thus resulting
  in predicates with multiple rules: one for each type denition in which
  they appear. Note that, to obtain legitimate Prolog programs, we replace nonalphabetic
  functor names by a descriptive name beginning with an underscore.

  Example 2. The translation of denitions in Example 1 results in the following

      rules for - (minus):
      func minus(T,T,pair(T)).
      func minus(T,list(T),node(T)).
      func minus(expr,expr,expr).
      func minus(expr,expr).

  Translating program rules: We assume programs rules are normalized
  and fully attened, that is, each literal except equality has only variables as
  arguments, and each equation involves at most one functor occurrence. It is
  straightforward to convert any program to this form.

  Let us rst restrict our attention to programs without mutually recursive predicates.
  A (non-recursive) literal p(v1; : : : ; vn) is translated to pred p(v1; : : : ; vn).
  The equation f (v1; : : : ; vn) = v is translated to f unc f (v1; : : : ; vn; v). The equation
  v1 = v2 is translated as itself. A (directly recursive) literal p(v1; : : : ; vn) in
  a rule with head p(w1; : : : ; wn) is translated as v1 = w1; : : : ; vn = wn. This is
  the non-generic instance where the call must have the same type as the head.
  The i'th rule for predicate p, with head p(w1; : : : ; wn) creates a rule with head
  rulei p(w1; : : : ; wn) and body given by the translation of the body. Finally, the
  constraints dened by the k rules of a predicate are conjoined by adding the rule
  pred p(V1,...,Vn) :- rule1 p(V1,...,Vn), ..., rulek p(V1,...,Vn).

  ----

  2 We write functors in inx notation where this is usual.

  ----

<!-- page 5 -->

  Example 3. The following gure illustrates the translation (right) of a program
  (left) which denes a simple chained lookup mechanism.

      pred member(X,L) :-
      rule1 member(X,L),
      rule2 member(X,L).
      member(X, T1) :- rule1 member(X, T1) :-
      [X| ] = T1. func cons(X, , T1).
      member(X, T2) :- rule2 member(X, T2) :-
      [ |R] = T2, func cons( ,R,T2),
      member(X,R). X = X, R = T2.
      pred deref(V,B,DV) :-
      rule1 deref(V,B,DV),
      rule2 deref(V,B,DV).
      deref(V, Binds, DV) :- rule1 deref(V, Binds, DV) :-
      V-V1 = T3, func minus(V,V1,T3),
      member(T3, Binds), pred member(T3, Binds),
      deref(V1, Binds, DV). V1 = V, Binds = Binds, DV = DV.
      deref(V, , T4) :- rule2 deref(V, , T4) :-
      V = T4. V = T4.

  The translation of mutually recursive predicates is somewhat more dicult.
  Milner's inference scheme requires all mutually recursive calls of the same predicate
  to share the same type. We achieve this by a program transformation
  which makes the appropriate variables available.

  Let us assume (for simplicity) that for each predicate p the heads of its
  rules are identical (with arguments xp say), and the rest of the variables in each
  rule are disjoint from those in any other program rule. Consider the strongly
  connected component fp1; : : : ; png in the predicate call graph. In the rst step
  of the translation, each rule for predicate pi of the form pi(xpi ) :
  L1; : : : ; Lm
  is replaced by the rule pi(xp1 : : : xpi
  : : : xpn ) :
  L1; : : : ; Lm, and the new rule
  pi(xpi ) :
  p1(xp1 : : : xpn ); : : : ; pn(xp1 : : : xpn ) is added. After this, the types of
  the mutually recursive heads are always available. In the nal step, any mutually
  recursive call is replaced by equations unifying it with the recursive head.

  The special handling of recursive calls guarantees that the translated program
  is non-recursive. Hence, any evaluation of them is nite.

  Type inference: The translated program can be used for type inference.
  To infer the types for a predicate p of arity n we simply execute the goal
  pred p(V1; : : : ; Vn). Note that we need to use execution with occurs check since
  otherwise, the translated programs can act erroneously.

  Example 4. Consider the program of Example 3. The goal pred deref(V,B,DV)
  has two answers: V = DV ^ B = list(pair(DV )), and V = expr ^ B =
  list(expr) ^ DV = expr. Thus, there are two possible (polymorphic) types for
  deref. The goal pred member(X,L) has the unique answer L = list(X). Note
  that we can also use the program to infer types of variables in rules.

<!-- page 6 -->

  One of the principal motivations of Milner style types is that, when used,
  type correct programs are ensured not to go wrong. This result continues to
  hold in the disjunctive framework. For any well-typed initial goal G for program
  P we can show that every goal appearing in a derivation of G is also well-typed.

  Translating predicate type declarations: Rules for predicates may be accompanied
  by a type declaration. During modular compilation, predicates whose
  rules are given in other modules are not available to the type checker. Instead,
  only the predicate type declarations will be available.

  A predicate type declaration for predicate p with arity n is of the form :- pred
  p(t1; : : : ; tn), were t1; : : : ; tn are type expressions. The translation of the the type
  declaration above results in a predicate dened by the rule decl p(t1,...,tn).
  In languages like Mercury, the same predicate may be assigned dierent types
  (usually in dierent modules), and type inference is used to determine which
  version of the predicate should be called. This allows another form of overloading.

  Given a predicate type declaration for a predicate p, the translation of atoms
  for p can be modied to make use of the information from the predicate type
  declaration decl p, rather than the information from the rules pred p. In particular,
  recursive calls for predicates p with type declarations can be translated
  using a generic type constraint, rather than a non-generic constraint. This is a
  relaxation of the Milner scheme also used in functional languages like Haskell
  and Miranda. There are two good reasons to allow this. First, the type declaration
  contains more accurate information given by the programmer (if it is wrong
  this will be discovered while checking it). It may be that it is stricter than the
  type that can be inferred from the rules of p. Second, the resulting programs
  are simpler, since decl p is dened only in terms of type constructors, whereas
  pred p is in general more complicated.

  Example 5. Consider the original program in Example 3 and the following predicate
  type declarations for deref and member:

      :- pred deref(T, list(pair(T)), T).
      :- pred member(T,list(T)).

  The modied part of the translated program is:

      decl_deref(T,list(pair(T)),T)).
      decl_member(T,list(T)).

      rule1_deref(V, Binds, DV) :- func_minus(V,V1,T3),
          decl_member(T3, Binds), decl_deref(V1, Binds, DV).

  Note how in the translation of deref we use the type declaration predicate
  decl deref rather than the translation for recursive predicates.

  We can use type inference to create predicate type declarations. Given an
  answer to the goal pred p(V1; : : : ; Vn) of the form V1 = t1 ^    ^ Vn = tn, we can
  dene a predicate type declaration for p as :- pred p(t1; : : : ; tn): This allows us
  to build an inference methodology which works bottom up. We use the inferred
  types for predicates in the leaves of the call graph, and use these results to infer
  types for the predicates higher up in the call graph. This process can avoid the

<!-- page 7 -->

  evaluation of the pred p rules for predicates which call p. Instead, the simpler
  decl p rules are used.

  Example 6. In the translated program of Example 3, we can determine the types
  for member independently of deref. Converting this to a predicate declaration we
  obtain :- pred member(X, list(X)). If the rules for deref use this denition,
  we obtain the same answers as before, for the goal pred deref(V,B,DV) but
  with a smaller derivation tree.

  Type checking: Predicate type declarations dene a superset of the intended
  meaning of the predicate. Thus, each local predicate type declaration
  must be consistent with the type constraints determined by the predicate rules,
  and it must constrain the types at least as much as the predicate rules do. For
  simplicity we assume only one type declaration for each local predicate. The
  predicate type declaration :- pred p(t1; : : : ; tn) is correct if decl p(v1; : : : ; vn)
  ! pred p(v1; : : : ; vn). Hence to check the program we simply need to execute
  the goal forall [V1, ..., Vn] decl p(V1, ..., Vn) => pred p(V1, ..., Vn). In
  practice, using Prolog we can write a simpler check making use of the non-logical
  features.

## 3 More ecient constraint solving

  The advantage of translating a type program into a logic program is that we can
  then use any logic programming system as a constraint solver for the disjunctive
  type constraints (since they are Herbrand constraints). Unfortunately, this
  evaluation approach may not be ecient in some cases.

  Example 7. Consider the following program, where + is overloaded, as dened
  in the introduction:

      what(O) :- A + B = C, D + E = F, G + H = I, J + K = L,
      C + F = M, I + L = N, M + N = O.

  Evaluating the goal pred what(O), int = O using a logic programming system
  explores 256 choices to nd that the only solution is O = int.

  As illustrated in the above example, the logic programming evaluation can be
  very inecient for our particular needs, with an exponential worst case. It seems
  impossible to avoid this worst case in general, because the problem of nding a
  correct type for a program with ad-hoc polymorphism is NP-hard, since we can
  reduce 3-SAT to this problem.

  Even though the worst case is unavoidable, our interest is in type checking
  real programs. Constraint programming [Marriott and Stuckey, 1998] is routinely
  used to solve NP-hard problems and techniques used in this eld are applicable
  to our problem. We will show in this section how we can represent disjunctive
  information about types using ideas from nite domain representation [Hentenryck,
  1989], and use generalized propagation [Le Provost and Wallace, 1993]
  to eciently ensure that constraints are satised.

<!-- page 8 -->

  Type Variables with Domains: Finding a solution to the type constraints
  can be seen as a constraint satisfaction problem. The possible types are given by
  the Herbrand Universe of the type constructors. As a result, there is an innite
  number of possible types for any variable. For this reason, nite domains are not
  directly usable in the form provided by current CLP(FD) systems. However, a
  lazy representation allows us to use the essential ideas in nite domains.

  We associate with every type variable a domain of possible values. Initially,
  this is given by the nite set of type constructors dened in the program. Given
  the type denitions in Example 1, the initial domain of a type variable X
  would be flist(T1); pair(T2); node(T3); expr; fn(T4; T5)g where T1; : : : ; T5 are
  new type variables. Conceptually, the type variables T1; : : : ; T5 have an initial
  domain of a similar form. This would give an innite number of variables. This
  problem can be avoided by representing unconstrained type variables without
  producing an initial domain. For eciency, and because we handle unconstrained
  variables dierently in any case, we allow the binding of one variable to another
  (as in Prolog). This means all further references to the bound variable, reference
  the variable it is bound to.

  Constraints for Type Variables with Domains: Since we can normalize
  and atten the rules in the type constraint program we can assume that the
  constraints appearing in the translated type program have one of two forms:
  X = Y and f (X1; : : : ; Xn) = Y . When they are reached in a derivation, the
  procedures equals(X; Y ) and fequals(f (X1; : : : ; Xn); Y ), respectively, are called.
  These two procedures are dened below.

      equals(X; Y )
      if free(X) then bind(X,Y )
      elseif free(Y ) then bind(Y ,X)
      else
      DX := domain of(X)
      DY := domain of(Y )
      D := domain intersect(DX,DY )
      if D 6= ; then
      set domain of(X,D)
      bind(Y ,X)
      else return f alse
      return true
      fequals(f (X1 ; : : : ; Xn); Y )
      let V be a new variable
      set domain of(V ,ff (X1 ; : : : ; Xn)g)
      return equals(Y ,V )
      domain intersect(D1 , D2)
      D := ;;
      for each e1  f (t1 ; : : : ; tn) 2 D1
      if exists e2  f (s1 ; : : : ; sn) 2 D2 and
      equals(e1,e2) then
      D := D [ ff (t1 ; : : : ; tn)g
      return D
      equals(f (t1; : : : ; fn); f (s1; : : : ; sn))
      in := true; i := 1
      while in ^ i  n
      in := in ^ equals(ti ; si)
      i := i + 1;
      return in

  The procedure free succeeds if the variable is unconstrained. The procedure
  bind binds the rst argument to the second. The procedure domain of returns the
  current domain of a variable, while set domain of updates the current domain of
  the rst argument to be the second argument. All these procedures are assumed
  to work with an implicit backtrackable state, in particular whenever a call to
  equals, fequals or equals returns f alse then all the changes to the state made

<!-- page 9 -->

  within its execution are undone.

  Handling disjunctive constraints: The aim of the constraint solver is
  to avoid using choice when handling disjunctive constraints. Instead, any predicate
  in the translated program with more than one dening rule is treated
  as a (propagation) constraint. Propagation constraints are used to reduce the
  domains of the variables involved in it without creating choices. Every time
  the variables involved in a propagation constraint are modied, the propagation
  constraint is invoked to (possibly) further reduce the domains of the variables
  involved.

  The algorithm to invoke a propagation constraint is illustrated below. It
  simply executes the goal, collects all answers, and then performs the union of
  the possible domains that arise in each answer.

      propagate(p(v1; : : : ; vn))
      for i = 1 to n
      Di := ;
      while next answer(p(v1; : : : ; vn))
      for i = 1 to n
      Ni := domain of(vi)
      Di := domain union(Di, Ni)
      for i = 1 to n
      if Di = ; then return f alse
      set domain of(vi ; Di)
      return true
      domain union(D1, D2 )
      D := ;
      for each f (t1; : : : ; tn) 2 D1
      if exists f (s1; : : : ; sn) 2 D2 then
      D := D [ ff (v1 ; : : : ; vn)g
      where v1 ; : : : vn are new variables
      if jD1j = 1 ^ jD2j = 1 then
      for i = 1 to n
      if : free(si) ^ : free(ti) then
      Ds = domain of(si )
      Dt = domain of(ti)
      DU = domain union(Ds,Dt)
      set domain of(vi ,DU)
      else D := D [ ff (t1 ; : : : ; tn)g
      for each f (s1; : : : ; sn) 2 D2
      if not exists f (t1 ; : : : ; tn) 2 D1 then
      D := D [ ff (s1; : : : ; sn)g
      return D

  The propagate procedure nds all the answers to an atom p(v1; : : : ; vn) given
  the current domains for each vi , and unions the resulting domains using domain
  union. It assumes next answer returns true with the state set to that of
  a successful derivation for the atom for each possible successful derivation, and
  then returns f alse while resetting the state to that existing on the call to propagate.
  The domain union procedure unions two domains. If the result is a singleton
  set, then the procedure unions the domain of each argument individually, otherwise
  the union is only made on the principal functors of the domains. Note that
  this is a strict generalization of the usual case for nite domains.

  Example 8. Consider the goal pred what(O),int = O. The execution of goal
  func plus(A,B,C) inside propagate returns four answers, which are unioned to
  give domain ffloat; intg for variables A{C. After the execution of pred what(O)
  each of the func plus atoms has been executed similarly and determined the
  domain of ffloat; intg for the variables A{O. The func plus atoms remain as
  propagation constraints. The evaluation of int = O reduces the domain of O to
  fintg, which means the constraint func plus(M ; N ; O) must be re-executed to

<!-- page 10 -->

  determine if there is any more information. There is now only one answer, and
  the domains of M and N are set to fintg. These in turn cause re-execution of
  other delayed constraints, until all the variables have domain fintg. In total 56
  choices are examined to solve the problem.


  Labelling: The constraint solver is incomplete. That is, after propagation
  quiesces so that no variables domain is changed if any of the remaining propagation
  constraints are reexecuted, the type constraints may not have an answer.

  Example 9. Consider the program

      :- pred p(int,float,float).
      :- pred p(float,int,int).
      q(X,Y,Z) :- p(X,Y,Z), p(Y,X,Z).

  The propagation constraint solver on the goal pred q(X,Y,Z) obtains the domain
  fint; floatg for each variable, but no typing for q exists. The propagation
  constraints corresponding to the p atoms cannot infer any more information but
  are mutually incompatible.


  In order to guarantee an answer we need to enforce that the propagation
  constraints hold. In nite domains, the labelling method is often used. Simply
  labelling each variable by assigning it one of its domain of values may not work,
  because there can be an innite depth of type expression. Instead, we label only
  the variables whose domain is not the complete set of declared types: since unconstrained
  type variables are represented in the implementation as free variables
  this is straightforward.


  Example 10. For the translation of the program above, the goal pred q(X,Y,Z)
  nished with each variable having domain ffloat; intg, and two delayed propagation
  constraints decl p(X,Y,Z) and decl p(Y,X,Z). Setting X to int wakes the
  two delayed constraints. The rst sets Y and Z to float and the second fails.
  Similarly, setting X to float causes failure. Hence, no types for q are inferred.


## 4 Implementation and Evaluation

  It is reasonably straightforward to implement the constraint solver described
  in the previous section using the meta-programming and attributed variable
  features of modern Prologs. A type variable has an attached domain and list
  of propagation constraints. When the domain is updated the propagation constraints
  are re-evaluated. The backtrackable state is automatically maintained
  by the system. The simple algorithms presented in the previous section are optimized
  in a number of ways in the implementation.

  The largest class of typed constraint logic programs we are aware of are Mercury
  programs. Thus, our 10 benchmarks come from the modules in the Mercury
  library and compiler. Among them, we choose the 5 largest les in number of
  predicates to check and the 5 largest in number of literals per predicate. To these

<!-- page 11 -->

  All Half Other None
  Benchmark Lits Preds Translate Solv LP Merc Solv LP Solv LP Solv LP Merc
  typecheck 8 160 520 (43-3) 112 56 1350 186 96 196 84 336 212 1710
  make hlds 11 116 500 (47-3) 284 142 1430 470 212 458 214 862 488 1750
  code info 5 169 490 (27-0) 180 80 1020 318 152 300 154 452 234 1660
  prog io 3 115 410 (6-1) 118 68 500 422 376 276 106 700 594 1570
  llds out 12 113 480 (56-5) 114 62 1140 214 114 182 92 362 198 1290
  optimize 35 6 240 (2-0) 6 4 390 10 6 10 6 12 10 350
  tree234 26 36 120 (103-2) 26 22 610 42 32 50 38 68 50 710
  mdchk unify 26 15 250 (8-0) 52 30 590 86 34 72 38 114 58 850
  ite gen 26 4 230 (0-0) 10 6 150 10 4 16 6 18 10 500
  jumpopt 25 10 220 (6-0) 30 20 530 72 40 48 28 88 46 390
  boyer 8 18 80 (47-5) 10 6 80 20 12 22 14 30 16 220
  what 14 1 10 (0-0) 7 87 220 7 87 16 12393 16 12392 42420

  Table 1. Translation and Checking and inferring times

  real-life modules, we added the well-known boyer benchmark (in Mercury) and
  the articial \what" benchmark in which the same clause occurs twice.

  We tested the approach using 4 dierent versions of each benchmark: All,
  where all type declarations are included; Half, where a random half are omitted;
  Other, where the other half are omitted; and None, where all are omitted.

  Table 1 gives an idea of the complexity of the benchmarks (number of literals
  per predicate and total number of predicates) and shows the time (in milliseconds)
  spent in the translation of the None benchmark version (which is the
  most complex) and (between brackets) the number of simply{mutually recursive
  cycles that had to be broken during the translation. Translation times for other
  verions: All, Half, Other, are close to that of None. As usual, the time spent
  in I/O is not added. The transformation program is written in SICStus Prolog
  3.1#5, the translated benchmarks were run on a SPARCstation 20 under Solaris
  2.5. The time in milliseconds is shown to check each (local) type declaration
  and to infer all types for predicates with rules but no type declarations. We give
  results for the propagation based solver (Solv), the logic programming system
  as a solver (LP), and the time spent by the Mercury type checker (Merc). In the
  rst two cases we used ECLi
  PSe
  3.5. For Mercury, we used the Melbourne implementation,
  release 0.7 with Boehm garbage collector; each benchmark module.m,
  was compiled with \mmc -v -S .... module.m".

  When comparing the timings between our system and Mercury, one has to
  take into account that Mercury computes and stores types for all the variables
  in a program, while we infer types for the variables only implicitly. Also, Mercury
  reports type errors at the level of the variables, while our system does it at
  the level of predicates. Furthermore, when an error is found|or an ambiguity|
  Mercury gives up, i.e. it doesn't enter a new cycle in its xpoint computation,
  while our system continues the inference for other parts of the program. Finally,
  we by no means claim to implement exactly the same type inference as Mercury

<!-- page 12 -->

  does, but we might be closer to what it is supposed to implement. The comparison
  with Mercury is only important in that it sets a yardstick with which to
  measure new implementations for performing the same task.

  The gures suggest that our translation schema leads to very ecient type
  checking and inference. Surprisingly, the LP system performs very well and most
  often better than the solver, despite the fact that there is quite a bit of overloading
  of functors in the Mercury modules. However, the solver is always within a
  small constant factor of the LP system and it performs much more reliably for
  highly ambiguous cases. Our implementation is more than a toy: it deals with
  almost all features of the full-blown language Mercury including: modules, functions,
  ad-hoc polymorphism, parametric polymorphism, higher-order predicates,
  etc. Currently, the system is being used by others for type checking/inferencing
  their Prolog programs according to the Mercury model.

## References

  Codish, M. and Demoen, B. (1994). Deriving polymorphic type dependencies for logic
  programs using multiple incarnations of prop. In Proc. of Static Analysis
  Symposium, number 864 in LNCS, pages 281{297. Springer-Verlag.
  Gallagher, J. and de Waal, A. (1994). Fast and precise regular approximations of logic
  programs. In Proc. of 11th International Conference on Logic Programming, pages
  599{616.
  Hall, C., Hammond, K., Peyton Jones, S., and Wadler, P. (1994). Type classes in
  Haskell. In European Symposium on Programming, volume 788 of Lecture Notes
  in Computer Science. Springer Verlag.
  Hentenryck, P. Van (1989). Constraint Satisfaction in Logic Programming. MIT Press.
  Hentenryck, P. Van, Cortesi, A., and Charlier, B. Le (1995). Type analysis of prolog
  using type graphs. Journal of Logic Programming, 22:179{209.
  Hermenegildo, M. (1994). Some methodological issues in the design of CIAO - a generic,
  parallel, concurrent constraint system. In Proc. of Principles and Practice of
  Constraint Programming, number 874 in LNCS, pages 123{133. Springer-Verlag.
  Janssens, G. and Bruynooghe, M. (1992). Deriving descriptions of possible values of
  program variables by means of abstract interpretation. Journal of Logic Programming,
  13:205{258.
  Le Provost, T. and Wallace, M. (1993). Generalized constraint propagation over the
  CLP scheme. Journal of Logic Programming, 16:319{359.
  Marriott, K. and Stuckey, P.J. (1998). Programming with Constraints: an Introduction.
  MIT Press.
  Meyer, G. (1996). Type checking and type inferencing for logic programs with subtypes
  and parametric polymorphism. Technical report, FernUniversitat Hagen.
  Milner, R. (1978). A theory of type polymorphism in programming. Journal of Computer
  and System Sciences, 17:348{375.
  Mycroft, A. and O'Keefe, R. (1984). A polymorphic type system for Prolog. Articial
  Intel ligence, 23:295{307.
  Rouzaud, Y. and Nguyen-Phuong, L. (1992). Integrating modes and subtypes into a
  Prolog type-checker. In In Proc., Joint Int. Conf. and Symp. on Logic Programming,
  pages 85{97.
  Somogyi, Z., Henderson, F., and Conway, T. (1996). The execution algorithm of Mercury:
  an ecient purely declarative logic programming language. Journal of Logic
  Programming, 29:17{64.
