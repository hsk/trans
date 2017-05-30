# Type classes

  Type classes documents for translation to Japanese.

## 1. [A second look at overloading](overload2.md)

  Martin Odersky, Philip Wadler, Martin Wehr. 7'th International Conference on Functional Programming and Computer Architecture, ACM Press, San Diego, California, June 1995.

  We study a minimal extension of the Hindley/Milner system that supports overloading and polymorphic records.
  We show that the type system is sound with respect to a standard untyped compositional semantics.
  We also show that every typable term in this system has a principal type and give an algorithm to reconstruct that type.

## 2 [How to make ad-hoc polymorphism less ad hoc](make.md)

  Philip Wadler and Stephen Blott. 16'th Symposium on Principles of Programming Languages, ACM Press, Austin, Texas, January 1989.

  This paper presents type classes, a new approach to ad-hoc polymorphism.
  Type classes permit overloading of arithmetic operators such as multiplication, and generalise the ``eqtype variables'' of Standard ML.
  Type classes extend the Hindley-Milner polymorphic type system, and provide a new approach to issues that arise in object-oriented programming, bounded type quantification, and abstract data types.
  This paper provides an informal introduction to type classes, and defines them formally by means of type inference rules.

## 3. [Implementing Type Classes](impl_tclass.md)

  John Peterson and Mark Jones Department of Computer Science, Yale University,

  We describe the implementation of a type checker for the functional programming language Haskell that supports the use of type classes.
  This extends the type system of ML to support overloading (ad-hoc polymorphism) and can be used to implement features such as equality types and numeric overloading in a simple and general way

  The theory of type classes is well understood, but the practical issues involved in the implementation of such systems have not received a great deal of attention.
  In addition to the basic type checking algorithm, an implementation of type classes also requires some form of program transformation.

  In all current Haskell compilers this takes the form of dictionary conversion, using functions as hidden parameters to overloaded values.
  We present efficient techniques for type checking and dictionary conversion.
  A number of optimizations and extensions to the basic type class system are also described.

## 4. [Poor Mans Type Class](poor_mans_type_classes.md)

  Martin Odersky EPFL IFIP WG2.8 working group meeting Boston, July 2006.

## 5. [Proposal: Overloading in Haskell](class-leter.md)

  Philip Wadler. Letter to Haskell working group (fplangc), 24 February 1988.

  [The original proposal for type classes. Never published.]
