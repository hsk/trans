# 貧者のタイプクラス

マーティン オダスキー

EPFL

IFIP WG2.8ワーキンググループ会議

ボストン、2006年7月。

1/15

----

## 目標

型クラスは素晴らしいです。
ハスケルプログラマーのコテージ業界がその周りに出現しています。
OO言語、特にScalaに型クラスを追加する必要がありますか？

_問題_：概念的費用

- キーワード type と class はすでに使いました！

- 型クラスは本質的に暗黙的に渡された辞書であり、辞書は本質的にオブジェクトです。

- それを複製したくない。

_アイディア_：OOクラスと型の間のデルタに集中する

_クラス_: implicits

2/15

----

## タイプクラスのない生活

SemiGroup と Monoid のいくつかの標準クラス:

    abstract class SemiGroup[a] {
      def add(x: a, y: a): a
    }
    abstract class Monoid[a] extends SemiGroup[a] {
      def unit: a
    }

モノイドの2つの実装:

    object stringMonoid extends Monoid[String] {
      def add(x: String, y: String): String = x.concat(y)
      def unit: String = ""
    }
    object intMonoid extends Monoid[Int] {
      def add(x: Int, y: Int) Int = x + y
      def unit: Int = 0
    }

3/15

----

任意のモノイド上で動作するsumメソッド:

    def sum[a](xs: List[a])(m: Monoid[a]): a =
      if (xs.isEmpty) m.unit
      else m.add(xs.head, sum(xs.tail)(m))

次のようなコードでこのsumメソッドを呼び出します:

    sum(List("a","bc","de"))(stringMonoid)
    sum(List(1,2,3))(intMonoid)

4/15

----

## 暗黙的なパラメータ：基本

次のわずかなsumの書き換えは、暗黙のパラメータとしてmを導入します。

    def sum[a](xs: List[a])(implicit m: Monoid[a]): a =
      if (xs.isEmpty) m.unit
      else m.add(xs.head, sum(xs.tail)(m))

- 通常のパラメータと暗黙のパラメータを組み合わせることができます。
- ただし、暗黙的なパラメータリストは1つしかなく、最後に来る必要があります。

5/15

----

implicitは、定義の修飾子としても使用できます:

    implicit object stringMonoid extends Monoid[String] {
      def add(x: String, y: String): String = x.concat(y)
      def unit: String = ""
    }
    implicit object intMonoid extends Monoid[Int] {
      def add(x: Int, y: Int): Int = x + y
      def unit: Int = 0
    }

暗黙のパラメータへの引数は推論できます:

    sum(List(1, 2, 3))

これは次のように展開されます:

    sum(List(1, 2, 3)(intMonoid)

6/15

----

## 暗黙の引数の推論

型Tの暗黙のパラメータの引数がない場合は、それが推論されます。

引数の値が渡されるのは、

- それ自体は暗黙的にラベル付けされており、
- それはTと互換性があり、
- 次のいずれかが成立する:
    1. xは、呼出し時に単純な識別子によってアクセス可能である（すなわち、同じスコープで定義され、継承またはインポートされる）
    2. xはTの（ある種のスーパークラスの）静的な値として定義されます。

いくつかの議論が適格であれば、最も具体的な議論を選ぶ。

7/15

----

## ローカリティ

暗黙的な引数解決の結果、同じ型の同じ操作の複数のインスタンス定義を持つことができます。

私たちはいつもコール時に見えるものを選びます。

これは、暗黙的クラスと型クラスの重要な違いです。

一貫性を保つための規則:

- Scalaには関数（オブジェクト）とメソッド（存在しない）があります。
- 部分的に適用されたメソッドは自動的に関数に変換されます。
- メソッドのみが暗黙のパラメーターを含むことができます。
- 暗黙のパラメーターは、メソッド値が削除される（関数に適用または変換される）場合にインスタンス化されます。

8/15

----

## 条件付きインプリシット

暗黙のメソッド自体は、暗黙のパラメーターを持つことができます。

例:

    implicit def listMonoid[a](implicit m: Monoid[a]) =
      new Monoid[List[a]] {
        def add(xs: List[a], ys: List[a]): List[a] =
          if (xs.isEmpty) ys
          else if (ys.isEmpty) xs
          else m.add(xs.head, ys.head) :: add(xs.tail, ys.tail)
        def unit = List()
      }

次に:

    println(sum(List(List(1, 2, 3), List(1, 2))))
    translates to
    println(sum(List(List(1, 2, 3), List(1, 2))))(listMonoid(intMonoid))
    ==>
    List(2, 4, 3)

9/15

----

## 外部拡張性

多くの場合、既存のタイプにいくつかの新しい機能を追加したいと考えています。

    trait Orderd[a] {
      def compare(that: a): Int
      def < (that: a): Boolean = (this compare that) < 0
      def > (that: a): Boolean = (this compare that) > 0
      def <= (that: a): Boolean = (this compare that) <= 0
      def >= (that: a): Boolean = (this compare that) >= 0
    }

Int、Stringなどを順序付けしたい。
それらの要素がある場合、リストを順序付けしたい。
共通の解決策: 「オープンクラス」。

10/15

----

## 暗黙的な変換

暗黙の変換は、SからTへの単項関数であり、暗黙的にラベル付けされています。

例:

    implicit def int2orderd(x: Int) = new Orderd[Int] {
      def compare(y: Int) = if (x < y) -1 else if (x > y) 1 else 0
    }
    def sort[a](xs: Array[a])(implicit c: a => Orderd[a]): Array[a] =
      if (xs.length <= 1) xs
      else {
        val pivot = xs(xs.length / 2)
        Array.concat(
          sort(xs filter(pivot >))
               xs filter(pivot ==),
          sort(xs filter(pivot <)))
      }
    val xss: Array[List[Int]]
    sort(xss)

11/15

----

## 翻訳

    def sort[a](xs: Array[a])(implicit c: a => Orderd[a]): Array[a] =
      if (xs.length <= 1) xs
      else {
        val pivot = xs(xs.length / 2)
        Array.concat(
          sort(xs filter(c(pivot) >))
               xs filter(pivot ==),
          sort(xs filter(c(pivot) <)))
      }
    val xss: Array[List[Int]]
    sort(xss)(list2orderd(int2ordered))


12/15

----

## 暗黙的な変換のアプリケーション

項eに暗黙の変換が適用される場合、

- e は期待される型と互換性がありません:

      val x: Ordered[Int] = 1
      ==>
      val x: Ordered[Int] = int2ordered(1)

- 選択e.mにおいて、eにメンバーmがない場合。

      x < (1)
      ==>
      int2ordered(x).<(1)

- アプリケーションe.m（a1; ...; an）で、eにメンバーがない場合
    mは（a1; ...; an）に適用できます。

    val x = BigInt(10);
    1 + x
    ==>
    BigInt(1) + x

13/15

----

## 要約

Scalaは次のような類推を実装しています:

    型クラス                   = クラス
    インスタンス宣言           = 暗黙の定義
    クラス内のコンテキスト     = 継承
    型のコンテキスト           = 暗黙的なパラメータ
    辞書                       = オブジェクト
    クラスのデフォルトメソッド = コンクリートメンバー
    クラスのメソッドシグネチャ = 抽象メンバ


14/15

----

## 結論

暗黙的なパラメータは、貧しい人の型クラスです。

- 概念的に軽量です。
- オブジェクトシステムでピギーバックします。
- Scalaバージョン2で実装され、
- モデル化も可能
    + マルチパラメータ型クラス
    + パラメトリック型クラス（ただし、一般的な関数の依存関係はありません）。
    + 関連タイプ
    + コンストラクタクラス（エンコーディングを使用して、より親切な型変数を追加すると便利です）


15/15

