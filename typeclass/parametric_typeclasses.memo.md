# パラメトリック型クラスを読む

    この論文はイェール大学のカンチェン、ポールフタグ、マーティンオダスキーによって1992年の6月に書かれたものです。
    イェール大学はアメリカ合衆国 〒06520 コネチカット州 ニューヘブンにあります。
    ニューヨークから１００ｋｍほど北東に行ったところにあるので東京から千葉か、茨城かってところでしょう。
    どうも、オダスキー先生は昔アメリカに住んでたことがあるようです。

    なぜこの論文を読もうと思ったのかというと、構文主導(syntax directed)というキーワードがあったからです。

## 要約

    Haskellの型クラスのプレースホルダ変数の他に型パラメータを持つことが出来るようにする提案らしいです。

    オーバーロードされたデータコンストラクタとセレクタ捜査でコンテナクラスを表すというのがよくわからないのですけど、とにかくこのような目的のためには型パラメータを持つ一般化が必要であるそうです。

    で、得られた型システムには、主要型と現在の単一化型再構成のアルゴリズムがあるということで、よくある論文だと、型システムの型付け規則が宣言的に書いてあるけれど、アルゴリズムまで乗ってなくて困ることがあるわけですけど、この論文ではアルゴリズムまでかいてあるそうなので期待するものなのでありますｗ

## 1 はじめに

    はじめにはまぁ、あんまり本気で読む必要もないと思うんですけど、
    Haskellの型クラスはすばらしいぞと。
    しかし、データの洗濯と構築のためにオーバーロードされた機能が不足しているー。
    よって、それらのサポートを追加したいと。

### 動機付けの例

  でもって、型クラスを使ってオーバーロードすればいいじゃないかってことになると。

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

  具体的にはこんな宣言になるのかなというね。
  で、これは、オブジェクト指向のコンテナクラスに似ているよねってことですな。
  確かに似ております。

  このコードの問題はa s って２つの制限が必要なので１個じゃ足りないよと。
  また、複数引数型クラスが許されてても、lenの型指定を見るとダメだーっとなりますと。

    Sequence a s => s -> Int

  s -> Int な型はうまくいかんぞと。mapとか考えるともっとだめだーってなりますということで、必要なんだーってことらしいです。

### 我々の貢献

  でこれらの問題を解決するのがこの論文であり、パラメトリック型クラスの概念を導入したのが貢献でありー、箇条書すると5つくらいありますと。


  1. パラメトリック型クラスは制限付き方変数だけではなくて、型引数も持てるので、Sequenceなどのクラスが表現できるようになるぞ

  2. 単純な符号化スキームで、型クラスコンストラクタという概念が取り込めるのでmapなどのオーバーロードされた演算子を定義できるぞ

  3. パラメトリック型クラスは、Haskellの型システムをちょっとだけ拡張したものだぞ

  4. 決定可能であることを証明したし、型推論アルゴリズムを作ったぞ

  5. デモとしてモナドへのリストを一般化するクラスを書いたぞ


  ってことらしい。

### 関連研究

  似たような研究はすでにあって、 Wadler と Blott が [WB89] で型クラスを導入してHM型推論を拡張していて、述語型という新しい型を提案したと。

  シンボリック計算ってもののための Scratchpad II システムってので同じ概念ああったり、[JT81]する。

  また、パラメトリックオーバーロード[Kae88]、オブジェクト志向プログラミング[CCH + 89] と [Rou90] での F bound 限定多相性の研究も関連してるらしい。

  型クラスの概念はHaskell に取り上げられていて、とにかく様々あるよってことですな。

### 残りの説明

  - ２章でパラメトリック型クラスの説明
  - ３章で非決定的な型システムの形式化
  - 4章で非決定的システムと型再構成アルゴリズムの間を埋める構文志向システムを提示
  - 5章で型再構成と単一化
  - 6章で型スキームが曖昧であるときの説明
  - 7章でモナドをパラメトリッククラスとして定義する説明
  - 8章で結論という流れです


  とくに注目したいのが、４章、５章あたりですな。

## 2 パラメトリック型クラス

2章はそんな長くないです。

パラメトリック型クラスの説明が書いてあるので読んでいきましょう。

パラメトリックス型クラスとは、クラス宣言に存在するプレースホルダ変数に加えて型パラメータを持つクラスです。

型クラスのパラメータ以外に型パラメータが加えられるぞーっていうことですな。

パラメトリック型クラスではプレースホルダと型パラメータを区別するには、クラスの前にプレースホルダを:で区切って記述するので

例えば、

    class t :: Eq where
    class s :: Sequence a where

と書くらしい。:で区切ると言っておいて::で区切ってるじゃないかと思うので後で調べるとして、とりあえず、::の手前がプレースホルダであると。

Eqはパラメータ無しで、Sequenceがパラメータありですよということらしい。

    inst List a :: Sequence a where ...
    inst Vector a:: Sequence a where ...

って書くらしいと。

ということでさらにさらにまとめを後で書くとするならこれだなっと。

だらだらと、読みながら感想を書いているので長いｗ
まぁよい。

#### パラメトリック型クラスの制限事項

２つほど制限があるらしいので箇条書してみましょう。

  1. `T :: Sequence a`という形式のインスタンス宣言では、`T`型は変数であってはなりません。
  2. さらに、 さらに、「T1」と「T2」の2つの型が両方ともSequenceのインスタンスとして宣言されている場合、その最上位型のコンストラクタは異なる必要があります。

ということで、 List aと Vector aは別なので問題ないです。
問題ある例は

    inst a :: Sequence (List a)

で最初に反しているのでエラーです。

    inst List Int :: Sequence Int
    inst List Char :: Sequence Char

これは、２番めに反しててエラーです。
この制限があるおかげで、インスタンス関係のすべてのステップでクラス名とプレースホルダ位置の型で決定可能であることが証明できます。決定可能になりますと。

これらの制限で、型がクラスのインスタンスである推論方法が１つしかなくなるので良いらしい。

もうひとついいことは、consistency creation の対象になっているということらしく何を言っているのかわからないんですけど、

  `T :: Sequence a` と `T :: Sequence b` の両方がある場合、 `a = b`でないといけないらしい。

つまり、 a = b であることは、こういうことだみたいな。


## 3 パラメトリッククラスの型システム

３章は長いんですけど、BNFによる構文や、束縛の推論規則があります。


    Type variables     α
    Type constructors  κ
    Class constructors c
    Types              τ ::= () | κ τ | α | τ1 * τ2 | τ1 -> τ2
    Type schemes       σ ::= ∀α::Γ.σ | τ
    Type classes       γ ::= c τ
    Class sets         Γ ::= {c1 τ1,..., cn τn}       (n ≥ 0,ci pairwise disjoint)
    Contexts           C ::= {α1::Γ1, ..., αn::Γn} (n ≥ 0)

    Expressions        e ::= x | e e' | λx : e | let x = e' in e
    Programs           p ::= class α::γ where x : σ in p
                        |  inst C ⇒ τ::γ where x = e in p
                        |  e
    
  図1: Mini-Haskell+ の抽象構文

こんな言語です。
この言語の特徴は `class α::γ where x : σ in p`の αがつくてんで型パラメータを暮らす宣言につけることが出来るわけです。

### インスタンス理論

加えて以下の束縛規則なるものがあります。

    C ⊦⊦ α :: γ     (α::{...γ...} ∈ C)

    C ⊦⊦ C'
    ---------    ("inst C' ⇒ τ::γ" ∈ Ds)
    C ⊦⊦ τ::γ

    C ⊦⊦ τ::γ1   ...   C ⊦⊦ τ::γn
    -----------------------------    (n ≥ 0)
    C ⊦⊦ τ:: {γ1 ,..., γn}

    C ⊦⊦ τ1::Γ1   ...   C ⊦⊦ τn::Γn
    ------------------------------------ (n ≥ 0)
    C ⊦⊦ {τ1::Γ1,  ...  ,fτn::Γn}

  図2: 束縛の推論規則

この規則はよくわからんのですけどクラスの集合と型パラメータと、
、型コンストラクタが文脈Cとなるのだけど文脈の推論はこのようにできますよというもののようです。

### 文脈 Context

そろそろわけわからなくなってきたので心が折れそうなわけですが、とりあえず目を通していきましょー。
訳もわけわからないところは修正しつつ。
文脈はCで表して型クラスの情報が入っているんですよね。
このCというやつはインスタンス仮定 α::Γの集合であるということですので、型クラスの情報の中でもインスタンス情報が入っているわけです。
で、型変数から暮らす集合への写像とも取れますよということで、mapとして実装できますみたいな話です。

Cは α::Γのsetだけど、Map[α,Γ]ともいえるだろうというね。従って、C αを Γであると言えるとしますよと。

で、Cは型変数を持てるのでパラメータを含めることが出来るのであるからコンテキストC
の領域が定義できて、

      reg(C) = ∪_{α ∈ dom(C)} fv(C)

と書けるらしく、
方程式の答えか何かで

    C*(Δ) = Δ ∪ C(C* (Δ))

ってあるけどC1とかC2とかがないので、これ写し間違えてませんかということで寝る。
ではなくて、調べますと、間違えてなかった。

なんか翻訳がおかしいので修正してみました。
いろいろ領域定義して、含まれるとか含まれないとかの関係を定義できるんですけど、継承関係みたいなものが循環してしまうと困るので循環しないものだけに限定して説明するそうだ。

### 制約付き置換

置換は何かしら必要になるわけですけど、今回の場合は制約がつくので制約付き置換について書いてあるようです。眠いです。もう先生帰っていいですか。ダメですってことです。

この次は型付け規則なのでまぁ頑張れよ俺ということで読んでまいりましょう。

とりあえず目を通そうなぁ。定義やら定義がダダダッと書いてありますな。
様々な定義や定理は性質を表しているので読んでおくと良いんですな。



何やらよくわからないけどノーマライザというものがあって正規化出来るらしい。
