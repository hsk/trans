# 型クラスの実装 - Implementing Type Classes
  <!--
    # Implementing Type Classes
    -->
  http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.3952&rep=rep1&type=pdf

  の適当な翻訳

  John Peterson and Mark Jones

  Department of Computer Science, Yale University,

  P.O. Box 2158 Yale Station, New Haven, CT 06520-2158, USA.

  Electronic mail: peterson-john@cs.yale.edu, jones-mark@cs.yale.edu.

  ジョン・ピーターソン、マーク・ジョーンズ

  エール大学コンピュータサイエンス学科

  電子メール：peterson-john@cs.yale.edu、 jones-mark@cs.yale.edu。

## 要約
  <!--
    ## Abstract
    We describe the implementation of a type checker for the functional programming language Haskell that supports the use of type classes.
    This extends the type system of ML to support overloading (ad-hoc polymorphism) and can be used to implement features such as equality types and numeric overloading in a simple and general way
    -->
  型クラスの使用をサポートする関数型プログラミング言語Haskell用の型チェッカの実装について説明します。
  これは、オーバーロード（アドホック多相）をサポートするためにMLの型システムを拡張し、等価型や数値オーバーロードなどの機能を単純で一般的な方法で実装するために使用できます。

  <!--
    The theory of type classes is well understood, but the practical issues involved in the implementation of such systems have not received a great deal of attention.
    In addition to the basic type checking algorithm, an implementation of type classes also requires some form of program transformation.
    -->
  タイプクラスの理論はよく理解されているが、そのようなシステムの実装に関わる実用的な問題はあまり注目されていません。
  基本型チェックアルゴリズムに加えて、型クラスの実装には何らかの形のプログラム変換が必要です。

  <!--
    In all current Haskell compilers this takes the form of dictionary conversion, using functions as hidden parameters to overloaded values.
    We present ecient techniques for type checking and dictionary conversion.
    A number of optimizations and extensions to the basic type class system are also described.
    -->
  現在のすべてのHaskellコンパイラでは、関数は関数をオーバーロードされた値の隠しパラメータとして使用して、辞書変換の形式をとります。
  我々は、型チェックと辞書変換のための効率的な手法を提示しています。
  基本型クラスシステムに対する多数の最適化および拡張についても説明します。

## 1 導入
  <!--
    ## 1 Introduction
    In the study of programming languages, the term overloading is used to describe the ability of a single symbol to have dierent interpretations as determined by the context in which it appears.
    Standard examples of this include the use of + to represent both addition of integers and addition of oating point quantities, or the use of == to compare both character values and pointers.
    In each case, the intended meaning of the overloaded symbol can be determined from the types of the arguments to which it is applied.
    One common approach is to completely resolve overloading at compile time.
    The compiler installs type specic meanings for all overloaded symbols, based either on type information attached to operands (the usual case) or some more general overloading resolution mechanism.
    A significant drawback to this approach is that overloaded operations cannot be abstracted while retaining their overloaded nature.
    -->
  プログラミング言語の研究では、オーバーロードという用語は、単一のシンボルが現れるコンテキストによって決定されるような異なる解釈を持つ能力を記述するために使用されます。
  これの標準的な例には、整数の加算と浮動小数点数の加算の両方を表す+を使用すること、または文字値とポインタの両方を比較するための==の使用が含まれます。
  いずれの場合も、オーバーロードされたシンボルの意図された意味は、それが適用される引数のタイプから判断できます。
  一般的なアプローチの1つは、コンパイル時にオーバーロードを完全に解決することです。
  コンパイラは、オペランドに添付された型情報（通常の場合）またはより一般的なオーバーロード解決メカニズムのいずれかに基づいて、すべてのオーバーロードされたシンボルの型固有の意味をインストールします。
  このアプローチの重大な欠点は、オーバーロードされた操作を抽象化することができず、オーバーロードされた性質を保持できないことです。

  <!--
    A more dynamic approach to overloading which preserves the ability to abstract overloaded denitions is found in object oriented languages.
    Here, resolution of overloaded operations occurs at run time.
    There are two particular problems to be dealt with.
    -->
  オーバーロードされた定義を抽象化する能力を保持するオーバーロードに対するより動的なアプローチは、オブジェクト指向言語で見られます。
  ここでは、オーバーロードされた操作の解決は実行時に行われます。
  対処すべき2つの特定の問題があります。

  <!--
    How do we determine which interpretation of an overloaded operator should be used in any particular situation? There are many examples for which the appropriate overloading cannot be determined at compile time.
    For example, in a program that uses the function double = x :x + x to double both integer and oating point values, there is no way to x any single interpretation for the + symbol.
    -->
  どんな特定の状況で過負荷のオペレータのどの解釈を使用すべきかをどのように決定するのか？ コンパイル時に適切なオーバーロードを特定できない例はたくさんあります。
  たとえば、関数double =？x：x + xを使用して整数値と浮動小数点値を二重にするプログラムでは、+記号に対して単一の解釈をxにする方法はありません。

  <!--
    How do we ensure that overloaded values are only ever used with appropriate arguments?
    For example, it would probably not make sense to try to add two character values.
    As a result, we must also ensure that the double function is never applied to a character value.
    Standard ML uses two dierent approaches to overloading.
    -->
  オーバーロードされた値が適切な引数で使用されることを保証するにはどうすればよいですか？
  たとえば、2つの文字値を追加しようとすると意味がありません。
  その結果、double値が文字値に適用されないようにする必要があります。
  標準MLは、過負荷に対して2つの異なるアプローチを使用します。

  <!--
    The type of each arithmetic operator such as + must be uniquely determined from its context, possibly by inserting an explicit type declaration.
    This compile time resolution of overloaded operators is not able to preserve the full overloading of + in the double function; one specic implementation of + must be chosen.
    -->
  +などの各算術演算子の型は、明示的な型宣言を挿入することによって、そのコンテキストから一意に決定されなければなりません。
  オーバーロードされた演算子のこのコンパイル時間の分解能は、double関数の+の完全なオーバーロードを維持することができません。 +の特定の実装を1つ選択する必要があります。

  <!--
    Standard ML introduces a notion of equality types to deal with the typing of the equality function.
    This is undesirable because it forces the programmer to accept a particular structural denition of equality { one which tests for equality of representation rather than equality of represented value.
    In addition, Appel [1] reports that \Equality types add signicant complexity to the language and its implementation".
    -->
  標準MLは、等価関数の型付けに対処する等式の概念を導入しています。
  これは、プログラマが表現された値の平等ではなく、表現の平等をテストする等価性の特定の構造定義を受け入れるよう強制するため、望ましくありません。
  さらに、Appel [1]は、\ Equality型が言語とその実装に重大な複雑さを加えることを報告しています。

  <!--
    An alternative approach to the treatment of overloading was introduced by Wadler and Blott [11] based on the notion of a type class and is intended to provide a uniform and general framework for solving exactly these kinds of problems.
    Type classes are most widely known for their use in the functional programming language Haskell [6] where they are used mostly to deal with standard primitive functions such as + and ==.
    In addition, we have also found that type classes can be useful in more specic application areas where they can help to produce clear and modular programs [7].
    We should also mention that there does not appear to be any significant reason why the use of type classes should be limited to non-strict, purely functional languages: in principle, any language based on the basic Hindley/Milner/Damas type system could be extended to support the use of type classes.
    -->
  オーバーロードの処理に対する代替アプローチは、WadlerとBlott [11]によって導入され、タイプクラスの概念に基づいており、これらの種類の問題を正確に解決するための統一されたフレームワークを提供することを意図しています。
  型クラスは、関数型プログラミング言語Haskell [6]での使用で最も広く知られています。ここでは、+や==などの標準的なプリミティブ関数を処理するために主に使用されます。
  さらに、タイプクラスは、明確でモジュラーなプログラムを作成するのに役立つ、より具体的なアプリケーション分野で有用であることがわかっています[7]。
  また、基本的なHindley / Milner / Damas型のシステムに基づく言語は、原則として、非厳密で純粋に機能的な言語に限定されるべきであるという重要な理由はないようです。型クラスの使用をサポートするように拡張されました。

  <!--
    This paper is meant to serve as a practical guide for the implementation of type classes.
    Previous work in this area has concentrated on the typing rules and has culminated in a set of syntax-directed typing derivations which are the basis for our type checker.
    Here we will use the typing rules to create a concrete algorithm which both type checks and transforms the program.
    We hope to reveal the essential simplicity of both the theory and implementation of type classes.
    Our concerns are type checking programs eciently, generating the best possible code from the type checker, and introducing a number of simple extensions to type classes which can be incorporated into our basic framework.
    This work is the result of our experience implementing type classes in both the Yale Haskell compiler and the Gofer interpreter..
    -->
  このペーパーは、型クラスの実装のための実践的なガイドとして役立つことを意図しています。
  この分野のこれまでの研究は、タイピングルールに重点を置いており、タイプチェッカーの基礎となる構文指向のタイピング派生の集合に達しました。
  ここでは入力規則を使用して、プログラムをタイプして変換する具体的なアルゴリズムを作成します。
  我々は、型クラスの理論と実装の両方の本質的な単純さを明らかにすることを望みます。
  私たちの懸念事項は、型チェックプログラムを効率よくチェックし、型チェッカから最良のコードを生成し、基本フレームワークに組み込むことができる型クラスにいくつかの簡単な拡張を導入することです。
  この作業は、Yale HaskellコンパイラとGoferインタプリタの両方で型クラスを実装した経験の結果です。

## 2 型クラス - 要約
  <!--
    ## 2 Type classes - a summary
    We begin by summarizing the main features of a system of type classes for a very simple and well known example - the denition of an equality operator, written as `==`, that is:
    -->
  まずは、非常に簡単でよく知られている例である等式演算子の定義のために、 `==`と書かれた型クラスのシステムの主な特徴を要約することから始めましょう。

  <!-- - polymorphic: use of the operator is not restricted to values of any single type.
    - overloaded: the interpretation of equality is determined by the types of its arguments.
    - extensible: the denition of equality can be extended to include new datatypes.
    -->

  - polymorphic： 演算子の使用は、単一の型の値に限定されません。
  - overloaded： 平等の解釈は、引数の型によって決まります。
  - 拡張可能： 新しいデータ型を含むように、等価の定義を拡張することができます。

  <!--
    Our example programs will be written using the concrete syntax of Haskell with explanatory comments where necessary.
    -->
  私たちのサンプルプログラムは、Haskellの具体的な構文を使用して、必要に応じてコメントを記述します。
  <!--
    Further details may be found in [6].
    We will also use the following terms while describing the class system:
    method A primitive overloaded operator such as == will be called a method.
    Methods are found in expressions.
    class A group of related methods is packaged into a class.
    -->
  詳細は、[6]を参照してください。
  また、クラスシステムについて説明する際には、次の用語を使用します。
  メソッド `==` などのプリミティブのオーバーロードされた演算子をメソッドと呼びます。
  メソッドは式にあります。
  クラス `A` に関連するメソッドのグループは、クラスにパッケージ化されています。
  <!--
    Each class has a name which is used in the type language.
    data type Type classes use the same sort of data types used by the ML type system.
    A type constructor names a data type in the type language while data constructors create values in the expression language.
    instance An instance binds a data type to operations which implement the methods of a specied class for that type.
    -->
  各クラスには、型言語で使用される名前があります。
  データ型型クラスは、ML型システムで使用されるものと同じ種類のデータ型を使用します。
  型コンストラクタは型言語のデータ型に名前を付け、データコンストラクタは式言語で値を作成します。
  インスタンスインスタンスは、データ型を、その型の指定されたクラスのメソッドを実装する操作にバインドします。

  <!--
    The basic idea is to dene a set of types Eq, known as a type class in Haskell, that contains precisely those types for which a suitable denition of equality has been given using an instance declaration.
    The denition of the class Eq is as follows:
    -->
  基本的な考え方は、Haskellの型クラスとして知られている一連の型Eqを定義することです。これには、インスタンス宣言を使用して等式の適切な定義が与えられた型が正確に含まれています。
  クラスEqの定義は以下の通りです：

    class Eq a where
      (==) :: a -> a -> Bool

  <!--
    The rst line introduces a name for the class and indicates that the type variable a will be used to represent an arbitrary instance of the class in the following part of the denition.
    (In the general case, we use an expression of the form C t to represent the assertion that the type t is an instance of the class C.) The remaining part of the declaration lists a collection of method functions which are associated with the class.
    In this particular example, there is only a single method function, written as an inx operator, ==.
    The type signature a -> a -> Bool indicates that, for each instance a of Eq, the == symbol behaves like a function that takes two arguments of type a and returns a value of type Bool.
    -->
  最初の行はクラスの名前を導入し、タイプ変数aが定義の次の部分のクラスの任意のインスタンスを表すために使用されることを示します。
  （一般的なケースでは、タイプtがクラスCのインスタンスであるという主張を表すために、C tの式の式を使用します）。宣言の残りの部分は、クラスに関連付けられたメソッド関数のコレクションをリストします。
  この特定の例では、in x演算子（==）として記述されたメソッド関数が1つしかありません。
  型シグネチャa - > a - > Boolは、各インスタンスaがEqの場合、==記号がa型の2つの引数を取り、Bool型の値を返す関数のように振舞うことを示します。

  <!--
    A class declaration may also dene a set of superclasses for a given class.
    The use of superclasses does not signicantly complicate this type system and will be discussed later.
    -->
  クラス宣言は、与えられたクラスのスーパークラスのセットを定義することもできます。
  スーパークラスの使用は、このタイプのシステムを著しく複雑にすることはなく、後で説明します。

  <!--
    Using the notation of Haskell, the full type of == is written as (Eq a) => a -> a -> Bool.
    Note the convention that all free variables in a type expression are implicitly bound by a universal quantier at the outermost level.
    Thus == is `polymorphic' in a, but the choice of types for a is restricted to instances of Eq.
    Type class constraints like this are often described as the context part of a type.
    -->
  Haskellの表記法を使うと、==の完全な型は（Eq a）=> a - > a - > Boolと書かれます。
  型式のすべての自由変数は、最も外側のレベルの汎用量子化によって暗黙的に束縛されるという規則に注意してください。
  したがって、==はaの `多形性` ですが、aの型の選択は式のインスタンスに制限されます。
  このような型クラスの制約は、しばしば型のコンテキスト部分として記述されます。

  <!--
    Even before we have dened a single instance of the class, we can use the == operator, either indirectly or directly, to dene other values.
    The restriction to instances of Eq is reected in the types assigned to these values.
    For example:
    -->
  クラスの1つのインスタンスを定義する前に、==演算子を間接的または直接的に使用して他の値を定義することができます。
  Eqのインスタンスに対する制限は、これらの値に割り当てられた型に反映されます。
  例えば：

      member :: Eq a => a -> [a] -> Bool
      member x [] = False
      member x (y:ys) = x==y || member x ys

  <!--
    The rst line of this denition gives the type of member.
    Note that, in Haskell, [a] represents the type of lists of values of type a.
    As in the basic ML type system, user supplied type signatures are not actually required since they can be inferred automatically by the type system.
    We provide such signatures in our examples as documentation.
    The second and third lines are typical of the way that functions are dened in Haskell.
    In this example there are two equations, using pattern matching on the left hand side to distinguish between the two cases when the list argument to member is empty, [], or non-empty, written (y:ys) where y and ys are the head and tail, respectively, of the list.
    -->
  この定義の最初の行は、メンバーのタイプを示します。
  ハスケルでは、[a]はa型の値のリストの型を表すことに注意してください。
  基本的なML型システムの場合と同様に、ユーザーが提供する型シグネチャは、型システムによって自動的に推論できるため、実際には必要ありません。
  このような署名は、例としてドキュメントとして提供しています。
  2行目と3行目は、関数がHaskellで定義されている方法の典型です。
  この例では、左側のパターンマッチングを使用して、メンバーへのリスト引数が空、[]、または空でない、書かれた（y：ys）の2つのケースを区別する2つの式があります.yとysは リストのそれぞれ先頭と末尾。

  <!--
    The types which are members of a class are dened by a collection of instance declarations which may be distributed throughout the program, typically in dierent program modules where new datatypes are introduced.
    For built-in types, the denition of equality may well be provided by a primitive function:
    -->
  クラスのメンバーである型は、プログラム全体に分散されたインスタンス宣言の集合、通常は新しいデータ型が導入される異なるプログラムモジュールで定義されます。
  組み込み型の場合、等価の定義は基本関数によって提供されることがあります。

    instance Eq Int where
      (==) = primEqInt

  <!--
    More generally, we can dene instances of the class Eq for any built-in and user-dened algebraic data types as in the following denition of equality on lists:
    -->
  より一般的には、次のようなリストの等式のように、組み込みデータ型とユーザー定義代数データ型のクラスEqのインスタンスを定義できます。

    instance Eq a => Eq [a] where
      [] == []         = True
      (x:xs) == (y:ys) = x==y && xs==ys
      _ == _           = False

  <!--
    (The underscore character _ in the last line is used as a wildcard; it indicates that, if neither of the rst two cases can be applied, the the equality test will produce a result of False.)
    The expression Eq a => Eq [a] in the rst line indicates that the denition of equality on lists depends on the denition of equality used for the elements held in the list:
    if a is an instance of Eq, then so is [a].
    -->
  （最後の行のアンダースコア_はワイルドカードとして使用され、最初の2つのケースのどちらも適用できない場合、等価テストはFalseの結果を生成します）。
  最初の行の式Eq a => Eq [a]は、リストの等価性の定義が、リストに保持されている要素に使用されている等式の定義に依存することを示します。
  `a` が `Eq` のインスタンスならば、 `[a]` も `Eq` です。

  <!--
    The set of types dened by a nite collection of instance declarations may be innite (but recursively enumerable).
    For example, the denitions given above describe the equality operator for integers, lists of integers, lists of lists of integers and so forth.
    -->
  インスタンス宣言のniteコレクションによって定義されるタイプのセットは、nite（再帰的に列挙可能）である可能性があります。
  例えば、上で与えられた定義は、整数の等価演算子、整数のリスト、整数のリストのリストなどを記述します。

## 3 オーバーロードの実装
  <!--
    ## 3 Implementing Overloading
    One standard technique used in the implementation of runtime overloading is to attach some kind of tag to the concrete representation of each object.
    Overloaded functions such as the equality operator described above can be implemented by inspecting the tags of their arguments and dispatching the appropriate function based on the tag value.
    -->
  実行時のオーバーロードの実装で使用される標準的な技術の1つは、各オブジェクトの具象表現にある種のタグを付けることです。
  上で説明した等価演算子のようなオーバーロードされた関数は、引数のタグを検査し、タグ値に基づいて適切な関数をディスパッチすることによって実装できます。

  <!--
    Many schemes exist for the encoding of tags to make the tag dispatch ecient.
    This is essentially the method used to deal with the equality function in Standard ML of New Jersey [2].
    One of the benets of static type checking is that it provides a compile-time check which ensures that the the equality function will never be applied to an object for which there is no corresponding denition of equality.
    -->
  タグをエンコードするための多くの方式が存在し、タグディスパッチを効率的にします。
  これは本質的にニュージャージーの標準ML [2]の平等関数に対処するために使用される方法です。
  静的型検査の利点の1つは、コンパイル時検査を提供し、等価関数が対応する等価定義が存在しないオブジェクトに適用されないことを保証することです。

  <!--
    Unfortunately, the use of tags as described above has a number of drawbacks.
    It can complicate data representation and may not be well suited to the underlying hardware.
    Perhaps more signicantly, there are some forms of overloading that cannot be implemented using this approach.
    In particular, it is not possible to implement functions where the overloading is dened by the returned type.
    A simple example of this is the read function used in Haskell to parse a string as a value of any type that is an instance of the Text class, the set of readable (and printable) types.
    -->
  残念なことに、上記のようなタグの使用には多くの欠点があります。
  データ表現が複雑になる可能性があり、基礎となるハードウェアにはあまり適していない可能性があります。
  おそらくもっと重要なことですが、このアプローチを使用して実装できないオーバーロードのいくつかの形式があります。
  特に、オーバーロードが返される型によって定義される関数を実装することはできません。
  これの簡単な例は、Haskellで文字列をTextクラスのインスタンスである任意の型の値、つまり読み込み可能な（印刷可能な）型の値として解析するために使用されるread関数です。

  <!--
    An elegant way to avoid these problems is to separate objects from their tags, treating tags as data objects in their own right.
    For example, we can implement read as a function that takes an extra argument which gives the tag of the result value.
    This amounts to passing type information around at run-time but this is only necessary when overloaded functions are actually involved.
    This is potentially more ecient than uniformly tagging every data object regardless how it will be used.
    -->
  これらの問題を避けるためのエレガントな方法は、タグをオブジェクトから分離し、タグをデータオブジェクトとして扱うことです。
  たとえば、結果値のタグを与える余分な引数を取る関数としてreadを実装することができます。
  これは、実行時にタイプ情報を渡すことになりますが、実際にオーバーロードされた関数が実際に関与している場合にのみ必要です。
  これは、どのように使用されるかにかかわらず、すべてのデータオブジェクトに一様にタグを付けるよりも、潜在的により効率的です。

  <!--
    Using this approach, the member function in the previous section might be implemented by translating the original denition to:
    -->
  このアプローチを使用すると、前のセクションのメンバ関数は、元の定義を次のように翻訳することによって実装されます。

    member' :: (a -> a -> Bool) -> a -> [a] -> Bool
    member' eq x [] = False
    member' eq x (y:ys) = eq x y || member' eq x ys

  <!--
    In other words, the implementation of member is simply parameterized by the appropriate denition of equality.
    The tag in this case is the equality function itself.
    -->
  言い換えると、メンバーの実装は、適切な平等の定義によって単純にパラメータ化されます。
  この場合のタグは、等価関数そのものです。

  <!--
    In this example, we could evaluate member 2 [1,2,3] by rewriting it as member' primEqInt 2 [1,2,3] and evaluating that expression instead.
    For a more interesting example,
    if xs is a list of lists of integers, then we could evaluate member [1] xs in a similar way, rewriting it as member' (eqList primEqInt) [1] xs, where:
    -->
  この例では、メンバー2 [1,2,3]をメンバ 'primEqInt 2 [1,2,3]として書き直し、代わりにその式を評価することで評価できます。
  より興味深い例として、 xsが整数のリストであれば、メンバ '（eqList primEqInt）[1] xsに書き換えて同様の方法でメンバ[1] xsを評価することができます。


    eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
    eqList eq [] [] = True
    eqList eq (x:xs) (y:ys) = eq x y &&
    eqList eq xs ys
    eqList eq _ _ = False

  <!--
  The denition of eqList can be obtained directly from the instance declaration on lists in Section 2 in much the same way as the denition of member' was obtained from that of member.
  Type classes do not require a particular denition of equality for a data type;
  any function of the appropriate type can by supplied by the user to check equality.
    -->
  eqListの定義は、メンバーの定義がメンバーの定義から得られたのとまったく同じ方法で、セクション2のリストのインスタンス宣言から直接取得できます。
  型クラスは、データ型のための特定の等価の定義を必要としません。
  適切な型の関数は、ユーザーが平等をチェックするために提供することができます。

  <!--
  As a convenience, Haskell allows the programmer to use derived instances for some of the standard classes like Eq, automatically generating appropriate instance denitions.
    -->
  便宜上、Haskellは、プログラマーがEqのような標準クラスのいくつかに派生インスタンスを使用して、適切なインスタンス定義を自動的に生成することを可能にします。

  <!--
  Note that this feature is not itself part of the underlying type system.
    -->
  この機能は、それ自体が基礎となる型システムの一部ではないことに注意してください。

  <!--
  One of our goals in the remainder of this paper is to describe how these translations can be obtained automatically as part of the type checking process.
    -->
  このホワイトペーパーの残りの部分の目標の1つは、型チェックプロセスの一部としてこれらの変換がどのように自動的に得られるかを記述することです。

## 4 静的解析
  <!--
    ## 4 Static Analysis
    Before type checking, the compiler must assemble the components of the static type environment.
    The data type, class, and instance declarations (all top-level declarations in Haskell) must be collected and processed.
    One constraint on these declarations is that instances must be unique: only one instance declaration for a particular combination of data type and class is allowed.
    This ensures that the meaning of overloaded operations with respect to parameter data types is consistent throughout the program.
    -->
  型チェックの前に、コンパイラは静的型環境のコンポーネントをアセンブルする必要があります。
  データ型、クラス、およびインスタンス宣言（Haskellのすべての最上位宣言）を収集して処理する必要があります。
  これらの宣言の1つの制約は、インスタンスが一意でなければならないということです。データ型とクラスの特定の組み合わせに対して1つのインスタンス宣言のみが許可されます。
  これにより、パラメータ・データ型に関するオーバーロードされた操作の意味がプログラム全体で一貫していることが保証されます。
  <!--
    In the previous section we described how the member function can be implemented by parameterizing its denition with respect to an implementation of the `==` method.
    In the general case, a class may have several dierent methods and it is sensible to parameterize the denitions of overloaded functions using dictionary values; tuples containing implementations for each of the methods for a particular instance of a class.
    -->
  前のセクションでは、 `==`メソッドの実装に関してその定義をパラメータ化することによってメンバ関数をどのように実装できるかを説明しました。
  一般的なケースでは、クラスにはいくつかの異なるメソッドがあり、ディクショナリ値を使用してオーバーロードされた関数の定義をパラメータ化することは賢明です。クラスの特定のインスタンスに対する各メソッドの実装を含むタプル
  <!--
    Static analysis generates a dictionary for each instance declaration and these dictionaries themselves may be overloaded.
    When a dictionary contains overloaded functions, as manifested in the context component of an instance declaration, it will reference further subdictionaries when constructed.
    -->
  静的解析では、インスタンス宣言ごとに辞書が生成され、これらの辞書自体が過負荷になる可能性があります。
  辞書には、インスタンス宣言のコンテキストコンポーネントに明示されているように、オーバーロードされた関数が含まれていると、構築時にさらにサブディクショナリを参照します。
  <!--
    A dictionary containing eqList would need to be overloaded to provide the eq argument to eqList.
    In our implementation, this captured dictionary is stored by partially applying eqList to just the eq argument when the dictionary containing eqList is created.
    -->
  eqListを含むディクショナリは、eqListにeq引数を提供するためにオーバーロードする必要があります。
  我々の実装では、この捕捉された辞書は、eqListを含む辞書が作成されたときにeqListを部分的にeq引数に適用することによって格納されます。

  <!--
    Each instance can be converted to a 4-tuple containing the data type, the class, a dictionary, and the context associated with the instance.
    A denition is inserted into the program which binds the dictionary value, a tuple of method functions, to a variable, the dictionary variable.
    The instance context can be represented by a list of class constraints, one class constraint for each argument to the data type dened by the instance.
    A class constraint is the (possibly empty) list of classes which must apply to the constituent type.
    -->
  各インスタンスは、データ型、クラス、ディクショナリ、およびインスタンスに関連付けられたコンテキストを含む4タプルに変換できます。
  定義は、プログラムに挿入されます。この定義は、メソッド関数のタプルである辞書の値を変数、辞書変数にバインドします。
  インスタンスコンテキストは、クラス制約のリスト、インスタンスによって定義されたデータ型への各引数の1つのクラス制約によって表すことができます。
  クラス制約は、構成要素タイプに適用しなければならない（おそらく空の）クラスのリストです。

  <!--
    The instance declaration for list equality would create this dictionary:
    -->
  リストの等価性のためのインスタンス宣言はこの辞書を作成します：

    d-Eq-List = eqList

  <!--
    and the declaration itself would be represented by:
    -->
  宣言自体は次のように表されます:

    (List,Eq,d-Eq-List,[[Eq]])

  <!--
    where List is the name of the list type data constructor.
    -->
  Listはリスト型データコンストラクタの名前です。
  <!--
    Since this class has only one method a tuple is not needed; normally a dictionary would be tuple containing a denition for each method.
    The context indicates that the rst argument to the List type constructor must be in the Eq class.
    -->
  このクラスには1つのメソッドしかないので、タプルは必要ありません。通常、辞書は各メソッドの定義を含むタプルです。
  コンテキストは、List型のコンストラクタの最初の引数がEqクラス内になければならないことを示します。
  <!--
    Dispatching a method requires selection of the appropriate function from a dictionary.
    Selector functions which retreive a method from a dictionary are also dened as the static type environment is processed.
    (In the previous example no selectors are needed since there is no tuple in the dictionary.)
    -->
  メソッドをディスパッチするには、適切な関数を辞書から選択する必要があります。
  メソッドを辞書から取得するセレクタ関数も、静的型環境が処理されるときに定義されます。
  （前の例では、辞書にタプルがないため、セレクタは必要ありません）。
  <!--
    These simply extract a component of a dictionary tuple, a constant time operation since each member function is located at a specic place in the dictionary.
    Dictionaries are only used where overloading cannot be resolved at compile time.
    When the type associated with a method is known at compile type the type specic version of the method is called directly without using the dictionary.
    -->
  これらは、各メンバー関数が辞書内の特定の場所に配置されているため、一定の時間操作である辞書タプルのコンポーネントを単純に抽出します。
  ディクショナリは、コンパイル時にオーバーロードを解決できない場合にのみ使用されます。
  メソッドに関連付けられた型がコンパイル・タイプで認識されている場合、メソッドの型固有バージョンはディクショナリを使用せずに直接呼び出されます。

## 5 型推論
  <!--
    ## 5 Type Inference
    We will separate the issues of type inference, in which each program expression is assigned a (possibly overloaded) type, and dictionary conversion, in which the program code is transformed to explicitly extract method functions from dictionaries.
    -->
  我々は型推論の問題を、各プログラムの式に（おそらくオーバーロードされた）型を割り当てる問題と、プログラムコードが明示的な辞書のメソッド関数を取り出す辞書変換の問題に分離します。

  <!--
    The use and implementation of ML style type inference is well documented and we will not repeat this here (see [4] for example).
    Instead, we concentrate on the relatively minor changes that are needed to extend ML style type inference with support for type classes.
    -->
  MLスタイルの型推論を使用する実装は十分に文書化されており（例えば[4]を参照）、我々はここで、これを繰り返すことはしません。

  <!--
    As in ordinary ML typechecking, type variables and unification play a central role.
    Type variables are initially unbound, corresponding to `unknown' types.
    As type checking proceeds, various constraints on the values that can be assigned to type variables are exposed, for example by ensuring that the argument type of a given function is the same as the type of the value to which it is actually applied.
    These constraints are solved by instantiating unbound type variables to more accurate types.
    Type classes require an additional field in each uninstantiated type variable: the context, a set (represented by a list) of classes.
    -->
  通常のMLの型検査と同様に、型変数と単一化が中心的な役割を果たします。
  型変数は 'unknown'(未知)の型に対応し、最初は結合していません。
  型検査が進むにつれ、変数の型に割り当て可能な値の様々な制約は、例えば、値の型と実際に適用されるように指定された関数引数の型が同じであることを保証することとして表れます。
  これらの制約は、より正確な型に結合していない型変数をインスタンス化することによって解決されます。
  型クラスは、各インスタンス生成型変数に追加フィールド（コンテキスト、（リストによって表される）クラスの集合）を必要とします。

  <!--
    Unification is affected in a very simple way: when a type variable is instantiated, its class constraints must be passed on to the instantiated value.
    If this is another type variable, its context is augmented, using set union, by the context of the instantiated variable.
    When a context is passed on to a type constructor context reduction is required.
    Context reduction uses the instance declarations in the static type environment to propagate all class constraints to type variables.
    -->
  単一化は非常に単純な方法で影響を受けます: 型変数がインスタンス化されるとき、そのクラス制約がインスタンス化値に渡さなければなりません。
  これは別の型の変数である場合は、そのコンテキストがインスタンス化された変数のコンテキストによって、和集合を使用して、強化されています。
  コンテキストが型に渡された場合、コンストラクタコンテキストの削除が必要とされます。
  コンテキストの削除は、型変数のすべてのクラス制約を伝播するために、静的な型環境でインスタンス宣言を使用します。

  <!--
    The type constructor being reduced by context reduction must be an instance of the reducing class.
    If not, type checking fails with an error that an attempt has been made to use an overloaded operator at a type that is not an instance of the corresponding class.
    If an instance declaration is found linking the data type and the class, the context of the instance declaration propagates to the type constructor arguments.
    This process continues until contexts have been propagated exclusively to type variables.
    -->
  コンテキスト削除により削除される型のコンストラクタは、削除クラスのインスタンスでなければなりません。
  そうでない場合、型検査は試行が対応するクラスのインスタンスではない型で、オーバーロード演算子を使用するためになされたもので、エラーで失敗します。
  インスタンス宣言は、データ型とクラスのリンクが発見された場合、インスタンス宣言のコンテキストは型コンストラクタの引数に伝播します。
  このプロセスはコンテキストが型変数を排他的に伝播されるまで、継続します。

  <!--
    As an example, consider the unification of Eq a => a, a type variable with an Eq context, and the type [Integer].
    The type variable is instantiated to [Integer].
    Before context reduction, the resulting type is Eq [Integer] => [Integer].
    The instance declaration for class Eq over the list data type exists (otherwise a type error occurs) and propagates the context Eq to the argument to the list type constructor.
    This leads to the type Eq Integer => [Integer].
    Now we can see that the program must also include an instance declaration that makes Integer an instance of the class Eq.
    Assuming that this is true, and since the Integer type constructor does not take any arguments, no further constraints can exist leaving only [Integer] as the resulting type.
    Note, however, that the unification would have failed if the required instance declarations were not found in the static type environment.
    By a similar process, unification of Eq a => a and [b] would yield the type Eq b => [b].
    Here, contexts remain attached to the resulting type variables.
    -->
  例えば、 `Eq a => a`(`Eq` コンテキストを持つ型変数 `a`)と型 `[Integer]` の単一化を検討しましょう。
  型変数は、 `[Integer]` にインスタンス化されます。
  コンテキストの削除をする前では、結果の型は、 `Eq [Integer] => [Integer]` です。
  リストデータの型のEqクラスのインスタンス宣言のオーバーロードが存在し（そうでなければ、型エラーが発生する）、そしてリスト型コンストラクタへの引数にコンテキスト `Eq` が伝播します。
  これは、型 `Eq Integer ​​=> [Integer]`につながります。
  ここで、我々はまた、上記プログラムがクラス `Eq` のインスタンス `Integer` を作成するインスタンス宣言を含まなければならないことがわかります。
  これが真であると仮定すると、`Integer` 型コンストラクタが引数を取らないので、結果の型として `[Integer]` 以外の制約は存在しません。
  しかし、必要なインスタンス宣言が静的型環境で検出されなかった場合は、単一化が失敗することに、注意してください。
  同様の処理により、 `Eq a => a` と `[b]` 型の単一化は `Eq b => [b]` をもたらすでしょう。
  ここで、コンテキストは、結果の型変数に付属したままです。

  <!--
    The following code implements type variable instantiation in the presence of type classes.
    Each type variable has a value field which is either null (uninstantiated) or contains an instaniated type.
    The context field is a list of classes attached to uninstantiated type variables.
    The findInstanceContext function searches the static type environment for an instance with the selected class and data type.
    If not is found this function signals a type error.
    It returns a list of contexts, one for each argument to the data type.
    -->
  次のコードでは型クラスがある型変数のインスタンス化を実装しています。
  各型変数は、（インスタンス生成されていない） `null` か、またはインスタンス化された型が含まれているいずれかの値フィールドがあります。
  コンテキストのフ​​ィールドはインスタンス化されていない型変数に付属しているクラスのリストです。
  `findInstanceContext` 関数は、選択されたクラスとデータ型を持つインスタンスの静的型環境を検索します。
  発見されていない場合、この関数は、型エラーを通知します。
  これは、各引数ごとにデータ型への引数が１つあるコンテキストのリストを返します。

    instantiateTyvar(tyvar,type)
      tyvar.value := type
      propagateClasses(tyvar.context,type)

    propagateClasses(classes,type)
      if tyvar(type)
        then type.context := union(classes,type.context)
        else for each c in classes
              propagateClassTycon(c,type)

    propagateClassTycon(class,type)
      s = findInstanceContext(type.tycon,class)
      for each classSet in s, typeArg in tycon.args
       propagateClasses(classSet,typeArg)

  <!--
    One other minor change to ML type inference is required.
    When a letrec is typechecked all variables defined by the letrec share a common context.
    This will be discussed in Section 8.3.
    -->
  MLの型推論にもう一つの小さな変更が必要です。
  `letrec` が型検査されたときに、 `letrec` により定義されたすべての変数は共通のコンテキストを共有します。
  これは、8.3節で考察します。

  <!--
    It is worth emphasizing that context reduction is the only significant change to the ML type inference process necessary to infer correct typings for Haskell programs involving type classes.
    On the other hand, dictionary conversion, as described in the following section (or some similar process), must be carried out to implement overloading in the final executable version of the type checked program.
    -->
  コンテキストの削除は型クラスを含むHaskellプログラムの正しい型付けを推論するために必要なMLの型推論処理への唯一の重要な変更であり、強調する価値があります。
  一方、辞書変換（またはいくつかの同様のプロセス）は、次のセクションで説明され、型検査されたプログラムの最終的な実行可能ファイルのバージョンでオーバーロードを実現するために行われなければなりません。

## 6 辞書変換
  <!--
    ## 6 Dictionary Conversion
    Dictionary conversion affects the generated code in two ways.
    First, overloaded definitions receive additional parameter variables to bind dictionaries.
    Second, a reference to an overloaded definition must be passed dictionaries.
    Thus the typechecker needs only two basic changes: when reference to an overloaded definition (which is usually a function but may be of any type) is type checked the hidden dictionary parameters must be inserted.
    When a definition (either at the top level or in a local definition using a let or letrec) is typed hidden dictionary arguments are inserted to bind any necessary dictionaries needed to resolve the overloading at run time.
    -->
  辞書変換は、2つの方法で生成されたコードに影響を与えます。
  第一に、オーバーロード定義は辞書結合用の追加パラメータ変数を受け取るようになります。
  第二に、オーバーロード定義を参照するために辞書を渡すようになります。
  したがって、型検査器は、2つの基本的な変更を必要とします; オーバーロード定義が参照されたとき(これは通常は関数だが任意の型であってもよい)、型が暗黙的な辞書引数が挿入されなければならないか検査されます。
  （トップレベルまたは `let` か `letrec` を使ったローカル定義のいずれかで）暗黙的な辞書引数が定義されたときは、実行時にオーバーロードを解決するために必要なすべての必要な辞書を結合するために挿入されるように解決されます。

  <!--
    The relation between a type signature and dictionary parameters is simple: each element of the context corresponds to a dictionary passed into or recieved by an overloaded definition.
    For example, a function with the type `(Eq a, Text b) => a -> b` would require two dictionaries, one for the `class Eq` and another for `Text`.
    The ordering of a context is arbitrary; dictionaries can be passed in any order so long as the same ordering is used consistently.
    -->
  型シグネチャと辞書パラメータとの関係は単純です：コンテキストの各要素は、オーバーロードの定義によって渡されるか、受け取った辞書に対応しています。
  例えば、型 `(Eq a, Text b) => a -> b` の関数は2つの辞書、 `class Eq` と他に `Text` を必要とするでしょう。
  コンテキストの順序は任意です; 辞書があれば、同じ順序が一貫して使用されるように、任意の順序で渡すことができます。

  <!--
    Adding dictionary passing code to the program during the code walk performed by the standard ML typechecker is perhaps the essential implementation issue addressed here.
    The type associated with an expression may change due to unification as the type checker proceeds.
    Since types only stablize at generalization the appropriate dictionaries needed to resolve overloading cannot be determined until the entire expression being generalized has already been walked over.
    To avoid a second pass over the code after generalization, we will hold onto the necessary bits of unresolvable code using placeholders.
    A placeholder captures a type and an object to be resolved based on that type.
    During generalization, placeholders are replaced by the required type-dependent code.
    -->
  標準的なMLの型検査器によって実行されるコードをトラバースしている間にプログラムに辞書通過コードを追加する事が、おそらく本質的な実装上の問題の対処方法です。
  式に関連する型が原因で型検査が進むにつれて単一化を変更することがあります。
  一般化でのみオーバーロードを解決するために必要な適切な辞書を決定できるので、一般化された式全体のすでにトラバースした箇所の判断ができません。
  一般化後のコードを二回走査する事を避けるために、我々はプレースホルダを使って解決不能なコードの必要な情報を保持します。
  プレースホルダは、型と型に基づいたオブジェクトの解決をキャプチャします。
  一般化されている箇所は、プレースホルダは必要な型依存コードに置換されます。

### 6.1 プレースホルダの挿入
  <!--
    ### 6.1 Inserting Placeholders
    Placeholders are inserted when the type checker encounters either an overloaded variable, a method, or a letrec bound variable.
    Slightly different forms of placeholder are used in each case.
    -->
  型検査器が、オーバーロードされた変数、メソッド、または `letrec` 束縛変数のいずれかに遭遇したときにプレースホルダが挿入されます。
  わずかに異なる形のプレースホルダがそれぞれの場合で使われます。

  <!--
    Overloaded variables are rewritten as an application to placeholders that will ultimately be replaced by the dictionaries implied by the variable's context.
    The fresh type variables associated with the variable are captured in the placeholders.
    For example, if f has type (Num a, Text b) => a -> b, the type checker will first freshly instantiate the type variables in f, yielding a typing of (Num t1,Text t2) => t1 -> t2.
    This fresh instantiation of type variables is part of ordinary ML style type checking.
    The value f will be rewritten as an application: f <Num,t1> <Text,t2>.
    The <object,type> notation will be used to represent placeholders.
    These placeholders become additional arguments to f which will be placed ahead of any other arguments.
    The classes Text and Num which appear in the placeholders indicate that the placeholder must resolve to an expression yielding a dictionary for that class.
    -->
  オーバーロードされた変数は、最終的には、変数のコンテキストによって暗黙の辞書に置換され、プレースホルダーへの関数適用に書き換えられます。
  変数に関連する新たな型変数はプレースホルダにキャプチャされます。
  たとえば、 `f` が型 `(Num a, Text b) => a -> b` の場合、型検査器は、最初にf内の型変数を新たにインスタンス化し、 `(Num t1, Text t2) => t1 -> t2` の型とします。
  型変数のこの新たなインスタンスは、通常のMLスタイルの型検査の一部です。
  値 `f` は、以下の関数適用に書き換えられるでしょう:

    f <Num, t1> <Text, t2>

  `<object, type>` の表記は、プレースホルダを表すために使用されます。
  これらのプレースホルダは、 `f` の他の引数の前に渡される追加の引数になります。
  プレースホルダに表れる `Text` クラスと `Num` クラスはプレースホルダがそのクラスの辞書をもたらす式として解決されなければならないことを示しています。

  ----

  <!--
    Method functions are converted directly to placeholders.
    The type variable in the placeholder corresponds to the type variable which defines the class in the class declaration.
    For example, the == method in class Eq would be typechecked by freshly instantiating its type, yielding Eq t1 => t1 -> t1 -> Bool, and returning the placeholder < == ,t1>.
    Since the object in the placeholder is a method, it will be resolved to either a specific implementation of the method (if the type variable becomes instantiated to a concrete type) or code to select a == function from an Eq dictionary.
    -->
  メソッド関数は、直接プレースホルダに変換されます。
  プレースホルダ内の型変数は、クラス宣言内のクラス定義の型変数に対応しています。
  たとえば、 `Eq` クラスで `==` メソッドは、 `Eq t1 => t1 -> t1 -> Bool` で、プレースホルダ `<==, t1>` を返す、型の新たなインスタンスによって型検査されるでしょう。
  プレースホルダー内のオブジェクトがメソッドであるため、メソッドの特定の実装（型変数が具体的な型にインスタンス化されている場合）または、 `Eq` 辞書から `==` 関数を選択するコードのいずれかに解決されます。

  ----

  <!--
    Recursively defined variables cannot be converted until their type is known.
    References to such variables encountered before they are generalized are simply replaced by a placeholder until the correct context has been determined.
    For example, in a simple recursive definition such as member, the recursive call to member becomes a placeholder until its type is generalized.
    After generalization, it is treated as an ordinary overloaded variable.
    -->
  再帰的に定義された変数は、その型が分かるまで、変換できません。
  正しいコンテキストが決定されるまで、それらが一般化される前に遭遇するような変数への参照は、単にプレースホルダーによって置き換えられます。
  例えば、その型が一般化されるまでは単純な再帰定義の中でこのようなメンバーとして、メンバーへの再帰呼び出しはプレースホルダになります。
  一般化した後、それは通常のオーバーロードされた変数として扱われます。

### 6.2 辞書パラメータの挿入
  <!--
    ### 6.2 Inserting Dictionary Parameters
    Once a definition has been typed, any context associated with the type variables in the definition is used to generate dictionary parameter variables which will bind the dictionaries needed to resolve the overloading.
    This occurs during the generalization portion of type inference.
    Generalization gathers all uninstantiated type variables in the type of a definition and creates a new dictionary variable for every element of every context in these type variables.
    A lambda which binds the dictionaries is wrapped around the body of the definition and a parameter environment is created.
    This environment is used to resolve placeholders created during typechecking of the definition.
    This environment maps a pair containing a class and type variable onto a dictionary parameter variable.
    -->
  定義が型付けされると、定義内の型変数に関連付けられた任意のコンテキストは、オーバーロードの解決に必要な辞書を結合する辞書パラメータ変数を生成するために使用されます。
  これは、型推論の一般化部分の間に現れます。
  一般化は、定義の型のすべてのインスタンス生成型変数を収集し、これらの型変数内のすべてのコンテキストのすべての要素の新しい辞書変数を作成します。
  辞書に結合するラムダは、定義の本体にラップされ、引数環境が作成されます。
  この環境は、定義の型検査中に作成されたプレースホルダを解決するために使用されます。
  この環境は、辞書パラメータ変数にクラスと型変数のペアにマップします。

  <!--
    As a simple example, if the inferred type of f is (Num t1,Text t2) => t1 -> t2, then the definition of f is changed to f = \d1 d2 -> f' where f' is the original definition of f.
    This creates the following parameter environment: [((Num,t1),d1),((Text,t2),d2)].
    -->
  簡単な例としては、fの推論された型が `(Num t1,Text t2) => t1 -> t2` の場合、 fの定義は `f = \d1 d2 -> f'` に変更され、ここで `f'` は `f` の元の定義です。
  これは、次の引数環境を作成します: `[((Num,t1),d1),((Text,t2),d2)]`。

### 6.3 プレースホルダの解決

  <!--
    ### 6.3 Resolving Placeholders
    At generalization, placeholders inserted into a definition can be resolved.
    A list of all placeholders, updated as each new placeholder is created, can be used to avoid walking through the code in search of placeholders.
    After dictionary parameters have been inserted, each placeholder is examined.
    For placeholders associated with either methods or classes, the type associated with the placeholder determines how it will be resolved.
    -->
  一般化では、定義中に挿入されたプレースホルダを解決することができます。
  すべてのプレースホルダのリスト(それぞれの新しいプレースホルダが作成され上書きされたもの)は、プレースホルダ検索をするためにコードのトラバースする事を回避するために使用できます。
  辞書パラメータが挿入された後に、各プレースホルダは検証されます。
  メソッドやクラスのいずれかに関連付けられているプレースホルダは、プレースホルダに関連付けられている型が、それが解決される方法を決定します。

  <!--There are four possibilities:
    - 1. The type is a type variable in the parameter environment.
      In this case, the mapping defines a variable which will carry the dictionary at run-time.
      A class placeholder is resolved to the dictionary parameter variable; a method placeholder requires a selector function to be applied to the dictionary variable.
    - 2. The type has been instantiated to a type constructor.
      An instance declaration associated with this type supplies either the method itself for a method placeholder or a dictionary variable for a class placeholder.
      Since dictionaries or methods themselves may be overloaded the type checker may need to recursively generate placeholders to resolve this additional overloading.
    - 3. The type variable may still be bound in an outer type environment.
      The processing of the placeholder must be deferred to the outer declaration.
    - 4. If none of the above conditions hold, an ambiguity has been detected.
      The ambiguity may be resolved by some language specific mechanism or simply signal a type error.
    -->
  4つの可能性があります:

  - 1 . 型は、引数環境内の型変数です。

    この場合のマッピングは実行時に辞書のキャリアとなる変数を定義します。
    クラスのプレースホルダは、辞書引数変数として解決されます; メソッドのプレースホルダは、辞書変数に適用するセレクタ関数を必要とします。

  - 2 . 型は、型コンストラクタにインスタンス化されています。

    インスタンス宣言はこの型の与えるメソッドのプレースホルダのためのメソッド自体、またはクラスのプレースホルダの辞書変数のいずれかに関連付けられています。
    辞書やメソッドそれ自体はオーバーロードすることができるので、型検査器は、この追加のオーバーロードを解決するために再帰的にプレースホルダを生成する必要があるかもしれません。

  - 3 . 型変数は、依然として外部の型環境で結合可能です。

    プレースホルダの処理は、外側の宣言に繰り越されなければなりません。

  - 4 . 上記の条件のいずれもが成立しない場合、あいまいさが検出されます。

    あいまいさは、いくつかの言語固有のメカニズムによって解決されるか、単に型エラーを通知出来ます。

  <!--
    Placeholders associated with recursive calls can be resolved in two different ways.
    The simplest way is to generate an overloaded variable reference which is no different than for other overloaded variables.
    This can only be done after generalization since the context of the recursive call is unknown until this time.
    However, since any dictionaries passed to a recursive call remain unchanged from the original entry to the function, the need to pass dictionaries to inner recursive calls can be eliminated by using an inner entry point where the dictionaries have already been bound.
    An example of this is shown in section 7.
    -->
  再帰呼び出しに関連付けられたプレースホルダは、2つの異なる方法で解決することができます。
  最も簡単な方法は、オーバーロードされた変数の参照を生成することで、他のオーバーロードされた変数と変わりありません。
  再帰呼び出しのコンテキストはこの時まで分からないので、この方法は一般化した後にだけ行うことができます。
  再帰呼び出しに渡される辞書は関数への元の引数と変わらないので、内側の再帰呼び出しに辞書を渡す必要性は辞書が既にバインドされている内側のエントリポイントを使用して除去することができます。
  この例はセクション7で示します。

## 7 例
  <!--
    ## 7 Examples
    We will illustrate the operation of our type checker with a couple of examples, each of which consists of three code trees.
    The first code tree shows freshly instantiated type variables (the ti) and inserted placeholders.
    The rules for instantiating type variables and the type templates are the same as for ML type checking.
    The second tree shows the result of unification.
    Types are unified pairwise along the lines in the diagrams.
    Finally, the result of generalization and placeholder resolution will be shown.
    The actual type checker performs unification continuously instead of after all type variables have been instatuated;
    these steps are separated here for clarity.
    -->

  我々は、それぞれが3つのコードツリーで構成された、２つの例で我々の型検査器の動作を説明します。
  最初のコードツリーは新たにインスタンス化された型変数(ti)と挿入されたプレースホルダを示しています。
  型変数と型テンプレートをインスタンス化するためのルールは、MLの型検査と同じです。
  第二の木は単一化した結果を示しています。
  型は、図中の線(訳の図ではコメント内の'`=`')に沿って対で単一化されています。
  最後は、一般化及びプレースホルダ分解の結果を示します。
  実際の型検査器は、継続的に代わりにすべての型の変数がinstatuatedされた後の単一化を行います;
  これらのステップは、明確にするために、ここで分離されています。

  ----

  <!--
    The following function f uses a method, +, and a recursive call to itself.
    -->
  次の関数 `f` は、メソッド、 `+`、 および自身への再帰呼び出しを使用しています。

  ----

  <!--
    Rather than write the context associated with a type variable each time it is mentioned all type variable context information will be shown at the side.
    -->
  型変数ごとに関連付けられたコンテキストを書くのではなく、それは記載されているすべての型変数コンテキスト情報の側に表れます。

    class Num a where
      (+) :: a -> a -> a
      f = \x -> x + f x

  <!--
    Type variable instantiation and placeholder insertion produce the following expression tree.
    The @ nodes are curried applications.
    -->
  型変数のインスタンス化とプレースホルダの挿入は、以下の式ツリーを生成します。
  `@` ノードはカリー化適用です。

    (* Context: Num t6 *)
    letrec f =
      ((* t1 = t2→t3 *) \x
        ((* t3 = t6 *) @
          ((* t4 -> t5 -> t6 = t7->t7->t7 *) <+,t7>)
          ((* t4 = t2 *) x)
          ((* t5 = t9 *) @
            ((* t8->t9 = t1 *) <f,t1>)
            ((* t8 = t2 *) x))))

  <!--
    After unification, this becomes:
    -->
  単一化後、次のようになります:

    (* Context: Num t2 *)
    letrec f =
      ((* t2->t2 *) \x
        ((* t2 *) @
          ((* t2->t2->t2 *) <+,t2>)
          ((* t2 *) x)
          ((* t2 *) @
            ((* t2->t2 *) <f,t2>)
            ((* t2 *) x) )))

  <!--
    The type in the placeholder associated with + is part of the parameter environment.
    This indicates that a dictionary passed into f will contain the implementation of + appropriate for the parameter x.
    At execution time, the sel+ function will retrieve this addition function from the dictionary.
    This is the simplest translation in which the recursive call passes the dictionary d unchanged.
    A better choice would have been to create an inner entry to f after d is bound and use this for the recursive call to avoid passing d repeatedly.
    -->
  `+` に関連付けられたプレースホルダ内の型は、環境パラメータの一部です。
  これが `f` に渡される辞書はパラメータ `x` のための `+` の適切なの実装が含まれていることを示します。
  実行時に、 `sel+` 関数は、辞書から、この加算関数を取得します。
  これは再帰呼び出しが辞書 `d` は変化せずに引き渡す最も簡単な変換です。
  より良い選択は、 `d` がバインドされた後、 `f` の内側のエントリを作成し、繰り返し `d` を渡す事を避けるために、再帰呼び出しして使う事でしょう。

    letrec f =
      (\d
        (\x
          (@
            (@
              sel+
              d)
            x
            (@
              (@
                f
                d)
              x))))

  -------

  <!--
    The next example uses a previously defined overloaded function, length, with type [a] -> Int.
    The necessary class and instance declarations are included.
    We will use the convention that dictionaries are named d-class-type.
    -->
  次の例では、型 `[a] -> Int` を使い、以前に定義されたオーバーロードされた関数 `length` を、使用しています。
  必要なクラスとインスタンス宣言が含まれています。
  我々は、辞書は `d` クラス型と命名されている規則を使用します。

    class Text a where
      print :: a -> String

    instance (Text a, Text b) => Text (a, b) where
      print = print-tuple2

    instance Text Int where ....
    instance Text a => Text [a] where ....

    g = \x -> print (x, length x)

  <!--
    After placeholder insertion and type variable instantiation:
    -->
  プレースホルダの挿入と型の変数インスタンス化後：

    (* Context: Text t5 *)
    let g =
      ((* t1->t2 *) \x
        ((* t2 = t4 *) @
          ((* t3->t4 = t5->String *) <print,t5>)
          ((* t3 = (t6, t7) *) 2-tuple
            ((* t6 = t1 *) x)
            ((* t7 = Int *) length
              ((* [t8] = t1 *) x)))))

  <!--
    After unification, this becomes:
    -->
  単一化後、次のようになります:

    (* Context: Text t5 *)
    let g =
      ((* [t8]->String *) \x
        ((* String *) @
          ((* ([t8],Int)->String *) <print,([t8],Int)>)
          ((* ([t8], Int) *) 2-tuple
            ((* [t8] *) x)
            ((* Int *) length
              (* [t8] *) x))))

  <!--
    The placeholder is resolved to a specific printer for 2-tuples.
    As this function is overloaded, further placeholder resolution is required for the types associated with the tuple components.
    -->
  プレースホルダは、2つのタプルのための特定のプリンタとして解決されます。
  この関数がオーバーロードされているように、さらにプレースホルダの分解は、タプルのコンポーネントに関連付けられている型のために必要とされます。

    let g =
      (\d
        (\x
          (@
            (@
              print-tuple2
              (@
                d-Text-List
                d)
              d-Text-Int)
            (2-tuple
              x
              (length
                x)))))

## 8 拡張機能
  <!--
    ## 8 Extensions
    This implementation of type classes can be extended in a number of ways to both improve the generated code and increase the expressiveness of the type system.
    -->
  この型クラスの実装は、生成されたコードを改善し、型システムの表現力を高めるために、さまざまな方法で拡張できます。

### 8.1 クラス階層の使用
  <!--
    ## 8.1 Using a Class Hierarchy
    In a Haskell class declaration, a set of classes may be declared as superclasses of the dened class.
    For example, in the declaration:
    -->
  Haskellクラス宣言では、クラスのセットは、定義されたクラスのスーパークラスとして宣言されるかもしれません。
  たとえば、次の宣言では：

    class Text a => Num a where
    ...

  <!--
    class `Text` is a superclass of `Num`.
    This implies that all datatypes declared to be in class `Num` must also be declared to be in `Text`.
    This superclass relation allows a type such as `(Num a, Text a) => a` to be abbreviated as `Num a => a`.
    -->
  `Text`クラスは` Num`のスーパークラスです。
  これは、クラス `Num 'にあると宣言されたすべてのデータ型も` Text`に宣言されなければならないことを意味します。
  このスーパークラスの関係は `（Num a、Text a）=> a`のような型を` Num a => a`と省略することができます。
  <!--
    Within the type checker, superclasses require few changes.
    -->
  型チェッカー内では、スーパークラスはほとんど変更を必要としません。
  <!--
    When class sets for type variables are constructed, contexts implied by the superclass relation can be removed.
    This compacts the class sets and requires fewer dictionary parameters.
    Superclasses also require that dictionaries contain all superclass dictionaries.
    During dictionary conversion, a dictionary may not be directly available if the associated class has been absorbed as a superclass.
    In this case, the dictionary or method must be fetched from an embedded superclass dictionary.
    -->
  型変数のクラスセットが構築されると、スーパークラスの関係が暗示するコンテキストを削除することができます。
  これにより、クラスセットがコンパクトになり、必要な辞書パラメータが少なくなります。
  また、スーパークラスでは、辞書にすべてのスーパークラス辞書が含まれている必要があります。
  辞書変換中に、関連するクラスがスーパークラスとして吸収された場合、辞書を直接利用することはできません。
  この場合、辞書またはメソッドを埋め込みスーパークラス辞書からフェッチする必要があります。<!--
    Dictionary representation aects the speed of method selection.
    Deeply nested dictionaries can be avoided by attening dictionaries to include all methods in both the associated class and in all superclasses at the top level of the structure.
    This slows down dictionary construction but speeds up selection operations.
    The eect of this tradeo in real programs is not yet known.
    Optimizations which avoid dictionary construction make attening more attractive.
    -->
  ディクショナリ表現はメソッド選択のスピードを向上させます。
  深くネストされた辞書は、関連するクラスと構造の最上位にあるすべてのスーパークラスのすべてのメソッドを含むようにディクショナリを付けることで回避できます。
  これにより、辞書の作成が遅くなりますが、選択操作が高速になります。
  この取引の効果は？ 実際のプログラムではまだ知られていません。
  辞書の構築を避ける最適化により、より魅力的なものになります。

### 8.2 デフォルトメソッド宣言
  <!--
    ## 8.2 Default Method Declarations
    Class declarations may supply a default method to be used when an instance declaration does not provide an implementation of a method in the class.
    This requires only that this denition bound to a variable for use during dictionary construction.
    This variable is placed into any dictionary in which the method is not specied by the instance declaration.
    -->
  クラス宣言は、インスタンス宣言がクラス内のメソッドの実装を提供しない場合に使用されるデフォルトのメソッドを提供することがあります。
  これは、辞書作成中に使用する変数にバインドされていることのみを必要とします。
  この変数は、メソッドがインスタンス宣言によって指定されていない辞書に置かれます。

### 8.3 再帰定義型の入力
  <!--
    ## 8.3 Typing Recursive Denitions
    So far we have assumed that the letrec construct binds only one variable.
    Mutually recursive denitions can be understood as a tupling of functions.
    Mutually recursive functions `f` and `g` could be dened as follows:
    -->
  ここまでは、letrec構造体が1つの変数だけをバインドすると仮定しています。
  相互に再帰的な定義は、関数のタプル化として理解できます。
  相互再帰関数 `f`と` g`は以下のように定義することができます：

    letrec (f,g) = (fbody,gbody) in ...

  <!--
    Here there is only a single recursive value, the tuple.
    Notice that the context of `f` and `g` are combined by this translation.
    Although mutually recursive functions are not actually implemented as tuples, they are type checked in this manner.
    All functions dened by a single `letrec` share a common context.
    This may create ambiguous functions when the type of a `letrec` bound variable does not contain the full context of the `letrec`.
    Such a function can be called within the `letrec` but not from outside.
    This is not an error in itself but the compiler provides a warning about such functions.
    -->
  ここでは、単一の再帰的な値、タプルのみが存在します。
  この変換によって `f`と` g`の文脈が結合されていることに注意してください。
  相互再帰関数はタプルとして実際には実装されていませんが、このように型チェックされます。
  単一の `letrec`によって定義されるすべての関数は、共通のコンテキストを共有します。
  これは `letrec`バインド変数の型が` letrec`の完全な文脈を含んでいないときにあいまいな関数を作ります。
  そのような関数は `letrec`の中で呼び出すことができますが、外部からは呼び出すことはできません。
  これはエラーではありませんが、コンパイラはそのような機能に関する警告を出します。

  <!--
    While it is easy for a single recursive function to use a local entry point to avoid passing dictionaries to recursive calls, this is harder to do for more than one function.
    It is simplest to pass all dictionaries to each recursive call within the `letrec`.
    Otherwise, all outside entries into the recursive group of functions needs to be funneled through a single lambda binding all dictionaries and then some sort of switch is required to enter the proper function.
    Other approaches may be possible but this does not seem to be a critical performance issue.
    -->
  単一の再帰関数がローカルエントリポイントを使用して辞書を再帰呼び出しに渡すのを避けるのは簡単ですが、これは複数の関数では難しいです。
  すべての辞書を `letrec`内の各再帰呼び出しに渡すのが最も簡単です。
  さもなければ、関数の再帰的なグループへのすべての外側のエントリーは、すべての辞書を単一のラムダバインディングで分担し、正しい関数を入力するためにある種のスイッチが必要です。
  他のアプローチも可能かもしれませんが、これは重大なパフォーマンス上の問題ではないようです。

### 8.4 定数辞書を減らす
  <!--
    ## 8.4 Reducing Constant Dictionaries
    Another source of ineciency are local functions which are inferred to have an overloaded type but are used at only one overloading.
    These can be detected during optimization or during type inference.
    During type inference, this involves saving the type variables created by freshly instantiation of the signature as it is referenced.
    If all of these variables are instantiated to the same concrete type the dictionary can be reduced to a constant.
    Flow analysis of the dictionaries can accomplish this same task and is perhaps superior since optimizations may remove some function calls which would prevent a dictionary from being marked invariant.
    -->
  有効性のもう一つの原因は、オーバーロードされた型があると推測されるが、1つのオーバーロードでのみ使用されるローカル関数です。
  これらは、最適化中または型推論中に検出できます。
  型推論の際には、これが参照されるときに署名を新たにインスタンス化することによって作成された型変数を保存します。
  これらの変数のすべてが同じ具体的な型にインスタンス化されている場合、辞書は定数に減らすことができます。
  辞書のフロー分析はこの同じタスクを達成することができ、おそらく最適化は辞書が不変とマークされるのを妨げるいくつかの関数呼び出しを取り除く可能性があるので優れています。

### 8.5 オーバーロードされたメソッド
  <!--
    ## 8.5 Overloaded Methods
    Haskell allows method functions to be overloaded in more than the type variable dened by class.
    For example, a class denition may contain:
    -->
  Haskellは、クラスによって定義された型変数以上にメソッド関数がオーバーロードされることを許しています。
  たとえば、クラス定義には次のものが含まれます。

    class Foo a where
      m1 :: Bar b => a -> b
      m2 :: a -> a
  <!--
    Here, `m1` contains an extra overloading.
    A dictionary for this class should have a type `(Bar b => T -> b,T -> T)` for some type `T` in the class `Foo`.
    That is, the rst component should be an overloaded function with `Bar` in the context while the second component is independent of `Bar`.
    -->
  ここで、 `m1` は特別なオーバーロードを含んでいます。
  このクラスの辞書は `Foo`クラスの` T`型に対して `(Bar b => T -> b,T -> T)` 型を持たなければなりません。
  すなわち、第1の成分は文脈において `Bar` を有する過負荷関数であり、第2の成分は `Bar` から独立しているべきです。

  <!--
    Unfortunately, this type signature is not valid since the context will oat outside the tuple.
    In implementation terms, the tuple will attempt to bind a dictionary for `Bar` when the dictionary is constructed instead of simply placing a function which binds a `Bar` dictionary inside the tuple.
    This requires the implementation of such dictionaries to go outside the standard type class system when generating such dictionaries.
    The cleanest solution to this problem would probably involve existential types.
    The Yale compiler avoids this issue using an internal construct similar to a type cast.
    -->
  残念なことに、この型シグネチャは、コンテキストがタプルの外側にあるので無効です。
  実装の観点では、タプルは、タプルの中に `Bar` 辞書をバインドする関数を単に置くのではなく、辞書が構築されるときに `Bar` のための辞書をバインドしようと試みます。
  そのためには、そのような辞書を生成する際に、そのような辞書を標準タイプのクラスシステムの外に出す必要があります。
  この問題に対する最もきれいな解決策は、おそらく存在するタイプを含むでしょう。
  Yaleコンパイラは、型キャストに似た内部構造を使用してこの問題を回避します。

### 8.6 ユーザー提供の署名
  <!--
    ## 8.6 User Supplied Signatures
    User supplied type signatures are a very necessary part of the type system.
    They can be used to avoid unwanted overloading and are essential for eciency.
    Unlike the ML type system, user supplied signatures have a signicant impact on the generated code, possibly replacing higher order function calls (method selectors) with direct calls to instance functions.
    -->
  ユーザが提供するタイプシグネチャは、タイプシステムの非常に必要な部分です。
  これらは不要な過負荷を避けるために使用でき、効率にとって不可欠です。
  ML型システムとは異なり、ユーザー提供のシグネチャは生成されたコードに大きな影響を与えます。
  インスタンス関数への直接呼び出しで上位関数呼び出し（メソッドセレクタ）を置き換える可能性があります。

  <!--
    While there are numerous ways of implementing these signatures, our system does this in a very clean way using readonly type variables.
    Type variables in signatures are marked as read-only to prevent type instantiation from violating the signatures.
    A read-only type variable cannot be instantiated or have its context augmented.
    -->
  これらのシグネチャを実装する方法は数多くありますが、私たちのシステムでは、読み取り専用の変数を使用して非常にクリーンな方法でこれを行います。
  シグネチャのタイプ変数は、タイプインスタンシエーションがシグネチャに違反しないように、読み取り専用としてマークされます。
  読み取り専用型変数は、インスタンス化することも、そのコンテキストを拡張することもできません。

  <!--
    Another use of user-supplied signatures is to `x` the ordering of dictionaries during dictionary conversion.
    Haskell uses interface les to support separate compilation.
    These interfaces provide the signature of each denition in a module.
    These interface signatures dene a specic ordering on the dictionaries passed to resolve overloading;
    at the implementation level the types `(Foo a,Bar b) => a -> b` and `(Bar b,Foo a) => a -> b` are dierent in a very important way.
    The compiler must be aware of any interface for the module being compiled and use that signature to determine the dictionary ordering during generalization.
    -->
  ユーザ提供のシグネチャの別の使用法は、辞書変換中の辞書の順序付けを「x」することです。
  Haskellはインターフェイスファイルを使って別々のコンパイルをサポートしています。
  これらのインタフェースは、モジュール内の各定義のシグネチャを提供します。
  これらのインタフェースシグネチャは、オーバーロードを解決するために渡されたディクショナリの特定の順序付けを定義します。
  （Foo a、Bar b）=> a - > b `と`（Bar b、Foo a）=> a - > b 'は非常に重要な意味を持ちます。
  コンパイラーは、コンパイルされるモジュールのインターフェースを認識し、そのシグニチャーを使用して、一般化中に辞書の順序を判別する必要があります。

### 8.7 単相性制限
  <!--
    ## 8.7 The Monomorphism Restriction
    The Haskell report [6] imposes a constraint known as the monomorphism restriction on the generalization of overloaded variables.
    This is intended to avoid problems with the loss of laziness that can occur when an overloaded variable is translated to a function with one or more dictionary parameters.
    Explicit type signatures can be used to avoid the monomorphism restriction in those cases where overloading would otherwise be restricted.
    Regardless of how the monomorphism issue is treated, it has a very simple implementation.
    When this restriction applies to a variable, type variables in its context must not be generalized: they must remain in the type environment to avoid fresh instan- tiation while the body of the dening let expression is type checked.
    -->
  Haskellの報告[6]は、多重定義された変数の汎化に関する単相性制約として知られている制約を課しています。
  これは、オーバーロードされた変数が1つ以上の辞書パラメータを持つ関数に変換されたときに発生する怠惰の喪失に関する問題を回避するためのものです。
  明示的な型シグニチャを使用すると、過負荷が制限される場合の単相性の制限を避けることができます。
  単相性問題がどのように扱われるかにかかわらず、非常に簡単な実装をしています。
  この制限が変数に適用されるとき、その文脈内の型変数は一般化されてはならない。型の本体が型チェックされている間に新鮮なインスタンス化を避けるために型環境に留まらなければならません。

### 8.8 不要な辞書の作成を避ける
  <!--
  ## 8.8 Avoiding Unnecessary Dictionary Construction
  Overloaded dictionaries are not constants and will be constructed dynamicly at run-time.
  The algorithm presented here may repeatedly reconstruct identical copies of overloaded dictionaries if the underlying implementation is not fully-lazy.
  To illustrate how this problem can occur, consider the following implementation of the equality on lists in essentially the same form given by [11]:
    -->
  オーバーロードされた辞書は定数ではなく、実行時に動的に構築されます。
  基礎となる実装が完全に怠惰でない場合、ここに提示されたアルゴリズムは、オーバーロードされた辞書の同一のコピーを繰り返し再構成することができます。
  この問題がどのように起こるかを説明するために、[11]で与えられたものと本質的に同じ形式で、リスト上の等式の次の実装を検討してください。

    eqList d [] []         = True
    eqList d (x:xs) (y:ys) = eq d x y &&
                            eq (eqDList d) xs ys
    eqList d _ _           = False

  <!--
    The `eqDList` function constructs a dictionary for equality on lists of type `[a]` given a dictionary `d` for equality on values of type `a`.
    The `eq` function denotes the selector which extracts the method for `==` from a corresponding dictionary.
    As it is written, many implementations of this denition will repeat the construction of the dictionary `eqDList d` at each step of the recursion.
    One simple way to avoid this is to rewrite the denition in the form:
    -->
  `eqDList` 関数は `a` 型の値に等価の辞書 `d` が与えられた `[a]` 型のリストに対して等価の辞書を構築します。
  `eq` 関数は対応する辞書から `==` のメソッドを抽出するセレクタを示します。
  書かれているように、この定義の多くの実装は、再帰の各ステップで辞書 `eqDList d` の構築を繰り返すでしょう。
  これを回避する簡単な方法の1つは、定義を次の形式で書き換えることです。

    eqList d
      = let eql = eq (eqDList d)
            eqa = eq d
            e [] []         = True
            e (x:xs) (y:ys) = eqa x y && eql xs ys
            e _ _           = False
      in e

  <!--
    As a further example of the same thing, consider a function doOne of type `C a -> a -> Bool` for some class `C` and suppose that the denition of this function requires the construction of a dictionary value.
    Note that this fact may well be hidden from the compilation system if the denition of doOne appears in an external module.
    -->
  同じことの別の例として、 `C a - > a - > Bool`型の `doOne` 関数をいくつかのクラス `C` に対して考えると、この関数の定義は辞書値の構築を必要とするものとします。
  この事実は、 `doOne` の定義が外部モジュールに現れている場合、コンパイルシステムから隠される可能性があることに注意してください。

  <!--
    Now suppose that we dene a function:
    -->
  ここで関数を定義するとしましょう：

    doList []     = []
    doList (x:xs) = doOne x : doList xs

  <!--
    A naive implementation of doList might use the denition:
    -->
  `doList` の単純な実装では、次の定義を使用します。

    doList d []     = []
    doList d (x:xs) = doOne d x : doList d xs

  <!--
    Any attempt to evaluate the complete list produced by an application of this function will repeat the construction of the redex doOne d (and hence repeat the dictionary construction in doOne) for each element in the argument list.
    -->
  この関数のアプリケーションによって生成された完全なリストを評価しようとすると、引数リストの各要素に対してredex doOne dの構築が繰り返されます（したがってdoOneの辞書構造が繰り返されます）。

  <!--
    Happily, the same observation also makes the solution to this problem quite obvious;
    we need to abstract not just the dictionaries involved but also the application of overloaded operators to dictionaries, giving the translation:
    -->
  幸いなことに、同じことがこの問題の解決法を明確にしています。
  私たちは関連する辞書だけでなく、辞書に過負荷の演算子を適用して、翻訳を提供する必要があります。

    doList  d = doList'
      where doList' []     = []
            doList' (x:xs) = doOne' x : doList' xs
            doOne'         = doOne d

  <!--
    An additional benet of this is that the garbage collector can reclaim the storage used for dictionary values as soon as the implementations of the required methods have been extracted from it.
    -->
  これのもう一つの利点は、ガベージコレクタが、必要なメソッドの実装が抽出されるとすぐに辞書値に使用される記憶域を再利用できることです。

  <!--
    Note that these problems will not occur in an implementation that supports full laziness.
    Indeed, in each of the examples above, the improved translation can be obtained from the original version using a translation to fully-lazy form as described in [9].
    -->
  完全な怠惰をサポートする実装では、これらの問題は発生しないことに注意してください。
  実際、上記の各例では、改良された翻訳は、[9]で説明されているように、完全に遅延した形式への翻訳を使用して元のバージョンから得ることができます。

## 9 パフォーマンスの課題
  <!--
    ## 9 Performance Issues
    How do type classes aect the compiler? Our observation is that they increase compilation time only slightly.
    A minor increase in the cost of unication and the placement and resolution of placeholders make up the majority of the extra processing required for type classes.
    -->
  型クラスはコンパイラをどのようにしていますか？ 私たちの観察では、コンパイル時間をわずかに増加させるだけです。
  ユニファイドのコストのわずかな増加、プレースホルダの配置と解決は、型クラスに必要な余分な処理の大部分を占めます。

  <!--
    As far as program execution is concerned, type classes have two costs: the extra level of indirection when dispatching a method function and the time and space required to propagate dictionaries through overloaded functions.
    The cost of instance function dispatch is actually quite small since this requires only a reference to a tuple element followed by a function call.
    For all but the simplest method functions this should be negligible.
    The cost of dictionary creation and propagation is harder to pin down.
    Passing and storing extra arguments to overloaded functions will incur slightly more function call overhead.
    -->
  プログラムの実行に関しては、型クラスには2つのコストがあります。メソッド関数をディスパッチする際の間接的なレベルと、オーバーロードされた関数を使用してディクショナリを伝播するのに必要な時間とスペースです。
  インスタンス関数ディスパッチのコストは、実際には非常に小さい。これは、タプル要素への参照とそれに続く関数呼び出しのみを必要とするからです。
  最も単純なメソッド関数以外は、これは無視してください。
  辞書の作成と伝播のコストは、ピン止めするのが難しいです。
  オーバーロードされた関数に余分な引数を渡して格納すると、関数呼び出しのオーバーヘッドが若干増加します。<!--
    Only overloaded dictionaries consume a non-constant amount of space.
    However, for code which does not use overloaded functions (but still may use method functions) the class system adds no overhead at all since the specic instance functions are called directly.
    In the case of a lazy language such as Haskell the overhead of overloaded functions may be greater since overloading is implemented using higher order functions.
    Higher order functions may be much more expensive in Haskell than ML since it is much harder to apply strictness or uncurrying optimizations.
    This is very noticable for very simple functions such as basic arithmetic operators but for more complex functions, such as in the I/O system, the overhead of overloading is not noticable.
    -->
  オーバーロードされたディクショナリだけが不変の量のスペースを消費します。
  ただし、オーバーロードされた関数を使用しないコード（ただしメソッド関数を使用する可能性があります）では、特定のインスタンス関数が直接呼び出されるため、クラスシステムはオーバーヘッドをまったく追加しません。
  Haskellのような遅延言語の場合、オーバーロードされる関数のオーバーヘッドは、高次関数を使用して実装されるため、より大きくなる可能性があります。
  厳密性を適用することや最適化を解除することははるかに難しいため、高次関数はMLよりもはるかに高価になる可能性があります。
  これは、基本的な算術演算子のような非常に単純な関数では非常に顕著ですが、I / Oシステムなどのより複雑な関数の場合、オーバーロードのオーバーヘッドは目立ちません。

  <!--
    It is possible to completely eliminate dynamic method dispatch within an overloaded function at specic overloadings by creating type specic clones of overloaded function.
    This could be implemented in a more general partial evaluation context or be controlled through program annotations.
    -->
  オーバーロードされた関数の型指定クローンを作成することにより、特定のオーバーロードでのオーバーロードされた関数内の動的メソッドディスパッチを完全に排除することができます。
  これは、より一般的な部分評価コンテキストで実装することも、プログラム注釈によって制御することもできます。

## 10 結論と関連研究
  <!--
    ## 10 Conclusions and Related Work
    While type classes are a relatively new addition to type theory, we argue that they should no longer be considered exotic or experimental.
    Type classes provide an elegant solution to a number of serious language design problems and should be considered as an important tool in programming language construction.
    Type classes provide a simple and regular framework by which a program can be parameterized.
    They do not provide the expressiveness of, for example, the ML module system where the parameterization is explicit.
    On the other hand, they are particularly convenient for some applications because the code needed to support overloading is handled automatically by the compiler.
    -->
  タイプ・クラスは型理論に対する比較的新しい付加であるが、もはやエキゾチックまたは実験的ではないと主張します。
  型クラスは、いくつかの重大な言語設計上の問題に対する洗練された解決策を提供し、プログラミング言語の構築において重要なツールとみなされるべきです。
  型クラスは、プログラムをパラメータ化するためのシンプルで規則的なフレームワークを提供します。
  それらは、例えば、パラメータ化が明白なMLモジュールシステムの表現力を提供しません。
  一方、オーバーロードをサポートするために必要なコードは、コンパイラによって自動的に処理されるため、アプリケーションによっては特に便利です。

  <!--
    We have shown an implementation of type classes which is relatively simple, requiring only a few extensions to the basic ML type checking algorithms.
    The addition of type classes does not severely impact either compiler or program performance.
    -->
  比較的簡単な型クラスの実装を示しました。基本的なML型検査アルゴリズムを少し拡張するだけで済みます。
  型クラスの追加は、コンパイラまたはプログラムのパフォーマンスに重大な影響を与えません。

  <!--
    The basis for a translation from the Haskell syntax for declaring and using type classes was set out by Wadler and Blott [11] and some results from an early implementation based directly on these ideas have been presented by Hammond and Blott [5].
    Further ideas, mostly at a fairly abstract level, were presented in the static semantics for Haskell [10] and also, concentrating on the problems of repeated dictionary construction, in [8].
    Some of the techniques used to improve the performance of Haskell overloading in the Chalmers Haskell compiler are described in [3].
    In summary, experience with the use and development of Haskell systems has done much to reduce the costs of type class overloading.
    -->
  タイプクラスの宣言と使用のためのHaskell構文の翻訳の基礎は、WadlerとBlott [11]によって設定され、これらのアイデアに直接基づく早期実装からのいくつかの結果がHammond and Blott [5]によって提示されています。
  Haskell [10]の静的セマンティクスや、辞書の繰り返し作成の問題に焦点を当てました[8]。
  Chalmers HaskellコンパイラでのHaskellオーバーロードのパフォーマンスを改善するために使用される技法のいくつかは、[3]で説明されています。
  要約すると、Haskellシステムの使用と開発の経験は、型クラスのオーバーロードのコストを削減するために多くのことを行っています。

## 謝辞
  <!--
  ## 11 Acknowledgments
  This work was supported by grants from DARPA, contract number N00014-91-J-4043, and from NSF, contract number CCR-9104987.
    -->
  この作業は、DARPA（契約番号N00014-91-J-4043）およびNSF（契約番号CCR-9104987）からの助成金によって支えられました。

## 参考文献
  <!--
  ## References
    -->

  [1] A.W. Appel. A critique of Standard ML. Princeton University CS-TR-364-92, February 1992.

  [2] A.W. Appel. Compiling with continuations. Cambridge University Press, 1992.

  [3] L. Augustsson. Implementing Haskell overloading. To appear in Conference on Functional Programming Languages and Computer Architecture, Copenhagen, Denmark, June 1993.

  [4] L. Damas and R. Milner. Principal type schemes for functional programs. In 8th Annual ACM Symposium on Principles of Programming languages, 1982.

  [5] K. Hammond and S. Blott. Implementing Haskell type classes. Proceedings of the 1989 Glasgow Workshop on Functional Programming, Fraserburgh, Scotland. Workshops in computing series, Springer Verlag.

  [6] P. Hudak, S.L. Peyton Jones and P. Wadler (eds.). Report on the programming language Haskell, version 1.2. ACM SIGPLAN notices, 27, 5, May 1992.

  [7] M.P. Jones. Computing with lattices: An application of type classes. Journal of Functional Programming, Volume 2, Part 4, October 1992.

  [8] M.P. Jones. Qualied types: Theory and Practice. D. Phil. Thesis. Programming Research Group, Oxford University Computing Laboratory. July 1992.

  [9] S.L. Peyton Jones and D. Lester. A modular fully-lazy lambda lifter in Haskell. Software { Practice and Experience, 21(5), May 1991.

  [10] S.L. Peyton Jones and P. Wadler. A static semantics for Haskell (draft). Manuscript, Department of Computing Science, University of Glasgow, February 1992.

  [11] P. Wadler and S. Blott. How to make ad-hoc polymorphism less ad-hoc. In ACM Principles of Programming Languages, 1989.
