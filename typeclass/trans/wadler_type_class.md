# Type classes

Philip Wadler

## 1. 暗黙の計算： ジェネリックプログラミングの新しい基盤 - The Implicit Calculus: A New Foundation for Generic Programming
  <!--
    Bruno C. D. S. Oliveira, Tom Schrijvers, Wontae Choi, Wonchan Lee, Kwangkeun Yi, Philip Wadler. Draft paper, 2014.
    -->
  Bruno C. D. S. Oliveira、Tom Schrijvers、Wontae Choi、Wonchanchan、Kwangkeun Yi、Philip Wadler。ドラフト・ペーパー、2014年。

  <!--
    Generic programming (GP) is an increasingly important trend in programming languages.
    Well-known GP mechanisms, such as type classes and the C++0x concepts proposal, usually combine two features:
    1) a special type of interfaces; and
    2) implicit instantiation of implementations of those interfaces.
    -->
  汎用プログラミング（GP）は、プログラミング言語においてますます重要な傾向となっている。
  型クラスやC++0xのコンセプト提案などのよく知られているGPメカニズムは、通常、2つの機能を組み合わせています:
  1）特別なインターフェースの型; と
  2）それらのインタフェースの実装の暗黙的なインスタンス化です。

  <!--
    Scala implicits are a GP language mechanism, inspired by type classes, that break with the tradition of coupling implicit instantiation with a special type of interface.
    Instead, implicits provide only implicit instantiation,
    which is generalized to work for any types.
    Scala implicits turn out to be quite powerful and useful to address many limitations that show up in other GP mechanisms.
    -->
  Scala implicitsは、特別なインタフェースの型で暗黙的なインスタンスを結合するという伝統に打ち勝つ、型クラスに触発されたGP言語メカニズムです。
  代わりに、implicitsは、暗黙的なインスタンス化のみを提供します。
  これは、どのような型でも機能するように一般化されています。 
  Scalaのimplicitsは、他のGPメカニズムに現れる多くの制限に対処するために、非常に強力で有用であることが分かります。

  <!--
    This paper synthesizes the key ideas of implicits formally in a minimal and general core calculus called the implicit calculus (\lambda_?), and it shows how to build source languages supporting implicit instantiation on top of it.
    A novelty of the calculus is its support for partial resolution and higher-order rules (a feature that has been proposed before, but was never formalized or implemented).
    Ultimately, the implicit calculus provides a formal model of implicits, which can be used by language designers to study and inform implementations of similar mechanisms in their own languages.
    -->
  この論文では、暗黙の計算(\lambda_?)と呼ばれる最小限の一般的なコア計算で正式なimplicitsの重要なアイデアを合成し、その上に暗黙的なインスタンス化をサポートするソース言語を構築する方法を示します。
  この計算の新規性は、部分解決と高階規則（以前に提案されているが、形式化も実装もされていない機能）のサポートです。
  最終的に、暗黙の計算は、言語設計者が自分の言語で同様のメカニズムの実装を研究し、通知するために使用できる形式的なimplicitsのモデルを提供します。

## 2. オーバーロードの第2の見方 - A second look at overloading
  <!--
    Martin Odersky, Philip Wadler, Martin Wehr. 7'th International Conference on Functional Programming and Computer Architecture, ACM Press, San Diego, California, June 1995.
    -->
  Martin Odersky、Philip Wadler、Martin Wehr。 1995年6月、カリフォルニア州サンディエゴのACM Press、Functional Programming and Computer Architectureに関する第7回国際会議。

  <!--
    We study a minimal extension of the Hindley/Milner system that supports overloading and polymorphic records.
    We show that the type system is sound with respect to a standard untyped compositional semantics.
    We also show that every typable term in this system has a principal type and give an algorithm to reconstruct that type.
    -->
  我々は、オーバーロードと多相レコードをサポートするHindley/Milnerシステムの最小限の拡張を研究します。
  標準の型なしの構成的セマンティクスに関して型システムは健全であることを示します。
  また、このシステムのすべての型付け可能な項には主要型があり、その型を再構築(型推論)するアルゴリズムが示されています。

## 3. Haskellで型クラス - Type classes in Haskell
  <!--
    Cordelia Hall, Kevin Hammond, Simon Peyton Jones, and Philip Wadler. European Symposium On Programming, LNCS 788, Springer Verlag, pp. 241-256, April 1994.
    -->
  コーデリア・ホール、ケヴィン・ハモンド、サイモン・ペイトン・ジョーンズ、フィリップ・ワドラー。プログラミングに関する欧州シンポジウム、LNCS 788、Springer Verlag、pp。241-256、1994年4月。

  <!--
    This paper defines a set of type inference rules for resolving overloading introduced by type classes.
    Programs including type classes are transformed into ones which may be typed by the Hindley-Milner inference rules.
    In contrast to an other work on type classes, the rules presented here relate directly to user programs.
    An innovative aspect of this work is the use of second-order lambda calculus to record type information in the program.
    -->
  この論文では、型クラスによって導入されたオーバーロードを解決するための型推論規則のセットを定義します。
  型クラスを含むプログラムは、Hindley-Milner推論規則によって型付けされたプログラムに変換されます。
  型クラスに関する他の作業とは対照的に、ここに示す規則はユーザープログラムに直接関連しています。
  この作業の革新的な側面は、2階のラムダ計算を使用してプログラムの型情報を記録することです。

## 4. Haskellの静的セマンティクス - A static semantics for Haskell
  <!--
    Simon Peyton Jones and Philip Wadler. Draft paper, Glasgow, 1991.
    -->
  サイモン・ペイトン・ジョーンズとフィリップ・ワドラー。 ドラフト紙、グラスゴー、1991年。

## 5. アドホック多相をより随所に作る方法 - How to make ad-hoc polymorphism less ad hoc
  <!--
    Philip Wadler and Stephen Blott. 16'th Symposium on Principles of Programming Languages, ACM Press, Austin, Texas, January 1989.
    -->
  フィリップ・ワドラーとスティーブン・ブロット。 プログラミング言語の原則に関する第16回シンポジウム、ACM Press、Austin、Texas、1989年1月。

  <!--
    This paper presents type classes, a new approach to ad-hoc polymorphism.
    Type classes permit overloading of arithmetic operators such as multiplication, and generalise the ``eqtype variables'' of Standard ML.
    Type classes extend the Hindley-Milner polymorphic type system, and provide a new approach to issues that arise in object-oriented programming, bounded type quantification, and abstract data types.
    This paper provides an informal introduction to type classes, and defines them formally by means of type inference rules.
  -->
  この論文では、アドホック多相への新しいアプローチである型クラスを紹介する。
  型クラスは、乗算などの算術演算子のオーバーロードを許し、 Standard ML の ``eqtype変数'' を一般化します。
  型クラスは Hindley-Milner多相型システムを拡張し、オブジェクト指向プログラミング、有界型の定量化、抽象データ型で発生する問題に対する新しいアプローチを提供します。
  この論文では、型クラスを非公式に紹介し、型推論ルールを使用して形式的に定義します。
