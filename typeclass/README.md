# Type classes

  Philip Wadler

## The Implicit Calculus: A New Foundation for Generic Programming

  Bruno C. D. S. Oliveira, Tom Schrijvers, Wontae Choi, Wonchan Lee, Kwangkeun Yi, Philip Wadler. Draft paper, 2014.

  Generic programming (GP) is an increasingly important trend in programming languages.
  Well-known GP mechanisms, such as type classes and the C++0x concepts proposal, usually combine two features:
  1) a special type of interfaces; and
  2) implicit instantiation of implementations of those interfaces.

  Scala implicits are a GP language mechanism, inspired by type classes, that break with the tradition of coupling implicit instantiation with a special type of interface.
  Instead, implicits provide only implicit instantiation,
  which is generalized to work for any types.
  Scala implicits turn out to be quite powerful and useful to address many limitations that show up in other GP mechanisms.

  This paper synthesizes the key ideas of implicits formally in a minimal and general core calculus called the implicit calculus (\lambda_?), and it shows how to build source languages supporting implicit instantiation on top of it.
  A novelty of the calculus is its support for partial resolution and higher-order rules (a feature that has been proposed before, but was never formalized or implemented).
  Ultimately, the implicit calculus provides a formal model of implicits, which can be used by language designers to study and inform implementations of similar mechanisms in their own languages.

  Available in: pdf.

## A second look at overloading

  Martin Odersky, Philip Wadler, Martin Wehr. 7'th International Conference on Functional Programming and Computer Architecture, ACM Press, San Diego, California, June 1995.

  We study a minimal extension of the Hindley/Milner system that supports overloading and polymorphic records.
  We show that the type system is sound with respect to a standard untyped compositional semantics.
  We also show that every typable term in this system has a principal type and give an algorithm to reconstruct that type.

  Available in: dvi, ps, dvi.gz, ps.gz.

## Type classes in Haskell

Cordelia Hall, Kevin Hammond, Simon Peyton Jones, and Philip Wadler. European Symposium On Programming, LNCS 788, Springer Verlag, pp. 241-256, April 1994.

  This paper defines a set of type inference rules for resolving overloading introduced by type classes.
  Programs including type classes are transformed into ones which may be typed by the Hindley-Milner inference rules.
  In contrast to an other work on type classes, the rules presented here relate directly to user programs.
  An innovative aspect of this work is the use of second-order lambda calculus to record type information in the program.

  Available in: dvi, ps, dvi.gz, ps.gz.

## A static semantics for Haskell

  Simon Peyton Jones and Philip Wadler. Draft paper, Glasgow, 1991.

  Available in: dvi, ps, dvi.gz, ps.gz.

## How to make ad-hoc polymorphism less ad hoc

  Philip Wadler and Stephen Blott. 16'th Symposium on Principles of Programming Languages, ACM Press, Austin, Texas, January 1989.

  This paper presents type classes, a new approach to ad-hoc polymorphism.
  Type classes permit overloading of arithmetic operators such as multiplication, and generalise the ``eqtype variables'' of Standard ML.
  Type classes extend the Hindley-Milner polymorphic type system, and provide a new approach to issues that arise in object-oriented programming, bounded type quantification, and abstract data types.
  This paper provides an informal introduction to type classes, and defines them formally by means of type inference rules.

  Available in: dvi, ps, dvi.gz, ps.gz.

  Philip Wadler,
