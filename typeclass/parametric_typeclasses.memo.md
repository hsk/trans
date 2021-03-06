# パラメトリック型クラス

  - イェール大学 アメリカ合衆国 〒06520 コネチカット州 ニューヘブン
    - ニューヨークから１００ｋｍほど北東に行ったところ
    - 東京から千葉か、茨城かってところ
  - カンチェン、ポールフタグ、マーティンオダスキー 1992年の6月
    - どうも、オダスキー先生は昔アメリカに住んでたことがあるようだ

  ----

#### どうしてこの論文？

  - 構文主導(syntax directed)というキーワードがあったから
  - 型クラスを実装したい
    - 型付け規則だけだと実装できないことがある
    - 構文主導規則があれば、実装しやすいはず

  ----

## 要約

  - 型クラスのプレースホルダ変数の他に型パラメータを持つことが出来るようにする

  - オーバーロードされたデータコンストラクタとセレクタ捜査でコンテナクラスを表したい
    - 型パラメータを持つ一般化が必要

  - 得られた型システム
    - 主要型と現在の単一化型再構成のアルゴリズムがある
    - 型推論のアルゴリズムまで書いてある
  - 期待するものなのであります!!

  ----

## 1 はじめに

  - Haskellの型クラスはすばらしいぞ
  - しかし、データの選択と構築のためにオーバーロードされた機能が不足している
  - よって、それらのサポートを追加したい

  ----

### 動機付けの例

  型クラスを使ってオーバーロードする例

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

  - オブジェクト指向のコンテナクラスに似ております。確かに

  ----

#### このコードの問題

  - a s って２つの制限が必要なので１個じゃ足りない
  - 複数引数型クラスが許されてても、lenの型指定を見るとダメ

    Sequence a s => s -> Int

  - s -> Int な型はうまくいかん！
  - mapとか考えるともっとだめだ
  - パラメトリック型クラスが必要だ

  ----

### 貢献

  - これらの問題を解決するのがこの論文
  - パラメトリック型クラスの概念を導入したのが貢献
  - 箇条書すると5つ

  1. パラメトリック型クラスは制限付き方変数だけではなくて、型引数も持てるので、Sequenceなどのクラスが表現できるようになるぞ

  2. 単純な符号化スキームで、型クラスコンストラクタという概念が取り込めるのでmapなどのオーバーロードされた演算子を定義できるぞ

  3. パラメトリック型クラスは、Haskellの型システムをちょっとだけ拡張したものだぞ

  4. 決定可能であることを証明したし、型推論アルゴリズムを作ったぞ

  5. デモとしてモナドへのリストを一般化するクラスを書いたぞ

  ----

### 関連研究

  - [WB89] Wadler と Blott が型クラスを導入してHM型推論を拡張
    - 述語型という新しい型を提案した
  - [JT81] シンボリック計算ってもののための Scratchpad II システムで 同じ概念がある。
  - [Kae88] パラメトリックオーバーロード
  - [CCH + 89] オブジェクト志向プログラミング
  - [Rou90] での F bound 限定多相性

  - [NS91] 一方、Haskellのソースレベルの構文には、デシジョン可能性を保証するのに十分な数の静的制約があります。

    これはに示されています.NipkowとSneltingは、3レベルの値、型、および部分的に順序付けされたソートの型クラスをモデル化しました。
    それらのシステムでは、クラスはソートに対応し、型はクラス階層に従ってソートされます。

  - [MGS89] 順序のソートされた統合は、型の再構築時のオーバーロードを解決するために使用されます。

    順序ソート手法の使用は数学的にはエレガントですが、クラス間の順序関係は構文メカニズムであり、型クラスの型システムを開発するためには必要ではないと主張しています。
    さらに、提案された拡張を組み込むためにシステムを拡張する方法は明らかではない。

  - 型クラスの概念を複数の型にわたる述語に拡張する作業も行われました。
  
    - [VS91] VolpanoとSmith は、型再構築の決定可能性を保証し、よく型された表現のシャープな概念を得るために、[WB89]の元のシステムの修正を検討しました。

    - [Jon91、Jon92b] Jones は、修飾型の一般的なフレームワークを提供しました。

      彼の述語セットの使用は、我々の文脈制約型インスタンス理論に非常によく似ています。

    - 2つのアプローチの主な違いは、通常のフォーム(ジョーンズはこの問題に対処していない)と制約付き変数と従属変数の区別です。
    この違いにより、コンテナクラスの定義で以前に遭遇したあいまい性の問題を解決できます。

  などなど

  ----

### 残りの説明

  - ２章でパラメトリック型クラスの説明
  - ３章で非決定的な型システムの形式化
  - 4章で非決定的システムと型再構成アルゴリズムの間を埋める構文志向システムを提示
  - 5章で型再構成と単一化
  - 6章で型スキームが曖昧であるときの説明
  - 7章でモナドをパラメトリッククラスとして定義する説明
  - 8章で結論

  とくに注目したいのが、４章、５章

  ----

## 2 パラメトリック型クラス

#### パラメトリックス型クラスとは

  - クラス宣言に存在するプレースホルダ変数に加えて型パラメータを持つクラス
  - 型クラスのパラメータ以外に型パラメータが加えられる

  ----

#### パラメトリック型クラスでのプレースホルダと型パラメータを区別

  - クラスの前にプレースホルダを::で区切って記述する

  例）

    class t :: Eq where
    class s :: Sequence a where

  - ::の手前がプレースホルダ
  - Eqはパラメータ無しで、Sequenceがパラメータあり

  ----

#### 例）

    inst List a :: Sequence a where ...
    inst Vector a:: Sequence a where ...

  リストやベクターは以上のように書きます。

  ----

#### パラメトリック型クラスの制限事項

  ２つほど制限があるらしいので箇条書してみましょう。

  1. `T :: Sequence a`という形式のインスタンス宣言では、`T`型は変数であってはなりません。
  2. さらに、「T1」と「T2」の2つの型が両方ともSequenceのインスタンスとして宣言されている場合、その最上位型のコンストラクタは異なる必要があります。


  ということで、 List aと Vector aは別なので問題ないですが、
  問題ある例は

    inst a :: Sequence (List a)

  で最初に反しているのでエラーです。

    inst List Int :: Sequence Int
    inst List Char :: Sequence Char

  これは、２番めに反しててエラーです。
  この制限があるおかげで、インスタンス関係のすべてのステップでクラス名とプレースホルダ位置の型で決定可能であることが証明できます。決定可能になります。

  これらの制限で、型がクラスのインスタンスである推論方法が１つしかなくなるので良いらしい。

  もうひとついいことは、consistency creation の対象になっているということらしく何を言っているのかわからないんですけど、

  `T :: Sequence a` と `T :: Sequence b` の両方がある場合、 `a = b`でないといけないそうです。

  a = b であることは、こういうことだみたいな。


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
  この言語の特徴は `class α::γ where x : σ in p`の αがつくてんで型パラメータをクラス宣言につけることが出来るわけです。

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
  いろいろ領域定義して、含まれるとか含まれないとかの関係を定義できるんですけど、継承関係みたいなものが循環してしまうと困るので循環しないものだけに限定して説明するそう。

### 制約付き置換

  置換は何かしら必要になるわけですけど、今回の場合は制約がつくので制約付き置換について書いてあります。

  様々な定義や定理は性質を表しているので読んでおくと良いんですな。
  何やらよくわからないけどノーマライザというものがあって正規化出来るらしい。

### 型付け規則

    (var)       A,C ⊢ x : σ    (x : σ ∈ A)

                A,C ⊢ e : ∀α::Γ.σ    C ⊦⊦ τ::Γ
    (∀-elim)   --------------------------------
                A,C ⊢ e : [α -> τ] σ

                A,C.α::Γ ⊢ e : σ
    (∀-intro)  ------------------------ (α ∉ fv A ∪ reg C)
                A,C ⊢ e : ∀α::Γ.σ
    
                A,C ⊢ e :τ' -> τ    A,C ⊢ e' : τ'
    (λ-elim)    --------------------------------
                A,C ⊢ e e' : τ

                A.x:τ',C ⊢ e : τ
    (λ-intro)   --------------------------------
                A,C ⊢ λx.e :τ' -> τ

                A,C ⊢ e' : σ    A.x : σ,C ⊢ e : τ
    (let)       --------------------------------------
                A,C ⊢ let x = e' in e : τ

  図3: 式の型付け規則

  標準の Hindley/Milner システム[DM82]に沿って、式のための非決定論的型システムを形成します。

#### HMシステムとの相違点

  1. 型スキーム `∀α::Γ.σ` の束縛された変数は、 文脈から `τ::Γ` (規則 ∀-elim) です 。
  2. 規則 (∀-intro) があり、一般化された変数 `α` のインスタンス述語は文脈から `排出` され、型スキーム `∀α::Γ.σ` に移動します。

  排出とかいみわからないんですけど。後で調べる。

### 図4: 宣言の型付け規則

                A.x : ∀_{fvγ} ∀α :: {γ}.σ, C ⊢ p : τ
    (class)     -------------------------------------------
                A,C ⊢ class α :: γ　where x : σ in p : τ

                A,C ⊢ x : ∀α :: {γ} :σ    A,C ⊢ e :[α ->τ'] σ    A,C ⊢ p : τ
    (inst)      --------------------------------------------------------------
                A,C ⊢ inst C' ⇒ τ'::γ where x = e in p : τ

  
  - 式からプログラムまで拡張
  - 規則(class) オーバーロードされた識別子 `x` が仮定セットに追加
  - 規則(inst) オーバーロードされた識別子とそのインスタンス式の間の互換性要件
  
  これらのルールは、最後のサブセクションにリストされているインスタンス宣言についての要件(a)、(b)と関連して取られなければならない。
  プログラム `p = Ds e` は、 `Ds` がこれらの条件を満たす場合に種別スキーム `σ` を持ち、包含関係 `⊦⊦`、 `A0,{} ⊢ p : σ` を生成する初期仮定は `A0` に設定されます。


### インスタンス関係と主要型スキーム


  - 式 `e` が型を持つ
    - 汎用インスタンスの概念を通した `e` に導き出せる他のすべての型の集合を取り込む主要型スキームがある。
  
  - 一般的なインスタンスと主要型スキームの定義を紹介


  難しいのでさらっと

#### 定義

  型スキーム `σ' = ∀αj' :: Γj'.τ'` は、文脈 `C` の下での型スキーム `σ = ∀αi::Γi.τ` の一般的なインスタンスです。 `Sτ = τ'` のような `{αi}` に置換 `S` が存在する場合、 `αj'` は `σ` と `C ⊎ {αj' :: Γj'} ⊦⊦ Sαi :: SΓi` で自由ではありません。
  この場合、 `σ' ≼C σ` と書きます。

  `≼C` の定義は [DM82] で定義された順序関係の拡張です。
  パラメトリック型クラスを持つ拡張では、インスタンスの含意に関する唯一の新しい要件が必要です。
  `≼C` が型スキーム集合のプリオーダを定義することは容易に分かります。


#### 補題

  次の性質は、定義から直接得られます。

  **補題3.2** `σ' ≼C σ` かつ `C ≼ C'` ならば、 `σ' ≼C' σ` です。

  次の補題は、型スキームの順序付けが制約のある置換によって保持されることを示しています。

  **補題3.3** `σ' ≼C σ` かつ `C' ⊦⊦ SC` の場合、`Sσ' ≼C' Sσ` です。

  型スキームの順序付けの定義によって、我々のシステムにおける主要型スキームの概念を定義することができます。

#### 定義

  `A`、 `C`、 `e` が与えられたとき、 `A,C ⊢ e : σ` ならば `A` と `C` の間で `e` の主要型スキームを `σ` と呼び、 すべての `σ'` で `A,C ⊢ e : σ'` ならば `σ' ≼C σ` です。

  次のセクションでは、主要型スキームを計算するアルゴリズムを開発します。

TODO>>> とりあえず、日本語で一旦最後まで見るのを目標にしてみよう。

  ----

## 4 決定論的型推論システム

  - 3章の型付け規則と比較すると、、、。
  - 与えられた式 `e` の型推論が `e` の構文構造によって一意的に決定できる
  - 型推論アルゴリズム
  - 表現力の面で前のものと同等
  
### 決定論的型付け規則

#### 図5: 式の決定論的型付け規則

    (var')      A,C ⊢' x : τ    (x : σ ∈ A, τ ≼C σ)

                A,C ⊢' e : τ' -> τ    A,C ⊢' e' : τ'
    (∀-elim')  ------------------------------
                A,C ⊢' e e' : τ

                A.x : τ', C ⊢' e : τ
    (∀-intro') -----------------------
                A,C ⊢' λx.e : τ' -> τ
    
                A,C ⊢' e' : τ'    A.x : σ,C ⊢' e : τ
    (let')      ---------------------------------------- (σ, C'') = gen(τ',A,C'), C'' ≼ C
                A,C ⊢' let x = e' in e : τ


  - `∀-intro` と `∀-elim` 規則は削除
  - 型判定は `A,C ⊢ e : τ` の形式に

  - 他の主な違い
    - `(var')` 規則はジェネリックインスタンスの定義に従って型スキームをインスタンス化
    - `(let')` 型スキームを導入するために汎化関数 `gen` を使用

#### 関数 `gen` 

    gen (σ,A,C) =
        if ∃α ∈ dom(C) \ (fv A ∪ reg C) then
            gen (∀α :: C α.σ,A,C\α)
        else (σ,C)

  - 型スキーム、仮説集合、および文脈を引数として取る。
  - 一般化された型スキームと排出されたコンテキストを返す



  言い換えれば、
  仮説セット内の制約型変数を除いて、与えられた文脈におけるインスタンス仮定は、
  型変数が適切に定量化されるように、より一般的な型スキームを形成するために放電され、移動されます。

### 2つのシステムの等価性

  ここで、決定論的型システムの多くの有用な特性を提示します。
  それらは、2つの型システムの合同性を確立するだけでなく、型システムと型再構成アルゴリズムとの間の関係を調べる際にも有用です。

  **補題4.1** (置換補題) もし `A,C ⊢' e : τ` かつ `C' ⊦⊦ SC` ならば `SA,C' ⊢' e : Sτ` です。

  この結果は、制約のある置換のもとで型の導出が保持されることを保証します。

  次の2つの補題は、文脈と仮説セットに関する型付け導出の単調性の形を表します。

  **補題4.2** もし `A,C ⊢ e : τ` と `C ≼ C'` ならば `A,C' ⊢' e : τ` です。

  **補題4.3** もし `A.x : σ,C ⊢ e : τ` かつ `σ ≼C σ'` ならば `A.x : σ',C ⊢ e : τ` です。

  ここで、決定論的システム `⊢'` は、以下の意味での非決定論的システム `⊢` と同等であることを示すことができます。

  **定理4.4** `A,C ⊢' e : τ` ならば `A,C ⊢ e : τ`。

  **定理4.5** `A,C ⊢ e : σ` ならば、`C ≼ C'`, `A,C' ⊢' e :τ` かつ `σ ≼C σ'` のような型と文脈 `C'` が存在します。ここで `(σ',C'') = gen (τ,A,C')` です。

## 5 単一化と型再構築

  - いつものように、型再構築は単一化に依存
    - パラメトリック型クラスにはどのような単一化が必要かを考えます。
  - 型再構築アルゴリズムを示し、前のセクションの規則とそこに確立された等価な結果を使用して、
  - セクション3で与えられた推論規則に関してその健全性と完全性を述べます。
  - これらの結果の結果として、我々は[DM82]のものと類似の我々のシステムの主要型スキーム特性を得ます。
  - 型再構築アルゴリズムは Yale Haskell コンパイラで実装されています。
  - サイズと複雑さで、 以前の Haskell コンパイラより優れています。

### 文脈保存単一化

  - 型の再構築は、通常、最も一般的な型を計算するために単一化に依存
  - 規則(∀-elim)の結果
    - 型への変数の置換がすべて、与えられたインスタンス制約を満たすわけではない
    - Robinson [Rob65]のよく知られている構文単一化アルゴリズムを使うことができない。
  - NipkowとSneltingは、Haskell [NS91]で型の再構成に順序ソートされた単一化を使用できることを示している。
    - その結果をパラメトリック型クラスに拡張する方法は明確ではない。
  - 図6に示すアルゴリズムmguが2つの型の最も一般的な文脈保存 unifier を生成することを示す。

<!------>

    mgu : τ -> τ -> S * C -> S * C
    mgn : τ -> Γ -> S * C -> S * C

    mgu τ1 τ2 (S,C)           = mgu' (Sτ1) (Sτ2) (S,C)

    mgu' α α                  = id_{S*C}
    mgu' α,(S,C) | α ∉ fv (τ) = mgn τ (Cα) ([α->τ] o S,[α->τ] C\α)
    mgu' ,α (S,C)             = mgu α (S,C)
    mgu' () ()                = id S C
    mgu' κ τ κ τ' (S,C)       = mgu τ τ' (S,C)
    mgu' (τ1,τ2) (τ1,τ2)      = (mgu τ1 τ1') o ( mgu τ2 τ2')
    mgu' (τ1->τ2) (τ1->τ2)    = (mgu τ1 τ1') o ( mgu τ2 τ2')

    mgn τ {}                  = id_{S*C}
    mgn τ {γ} (S,C)           = mgn' (Sτ) (Sγ) (S,C)
    mgn τ (Γ1 ∪ Γ2)          = (mgn τ Γ1) o (mgn τ Γ2)

    mgn' α c τ (S,C)          = if ∃τ: (c,2 C α) then mgu,fi (S,C)
                                else ( S,C [ α 7-> C α f c,g ])
    mgn' κ τ' c τ (S,C) | ∃ "inst C => κ ~τ' :: c,~τ" ∈ Ds
                                = let S' = match ~τ' τ'
                                    (S'',C'') = mgu τ (S' ~τ) (S,C)
                                    {τ1 :: Γ1, ...,τn :: Γn} = S'C'
                                in (mgn τ1 Γ1 (... (mgn τn Γn (S'',C''))))

    (and similarly for ->, *, ())

  図6: 単一化アルゴリズムと正規化アルゴリズム

  関数 mgu は2つの型をとり、制約付き置換で変換器を返します。
  アプリケーション `mgu τ1 τ2 (S0,C0)` は、型 `τ1` と `τ2` を単一化し、そのような置換があれば `(S0,C0)` を保存する最も一般的な制約付き置換を返します。
  アルゴリズムは Robinson のものと同様であるが、 `mgu α τ (S0,C0)` の場合を除き、`τ` が ` C0 α` です。
  この制約は、 `τ` と `C α` に補助関数 `mgn` を適用することに変換されます。
  コール `mgn τ Γ (S0,C0)` は、存在するならば、`C0 ∪ {τ::Γ}` の最も一般的な正規化を計算します。

  **定理5.1** 束縛された置換 `(S0,C0)` と型 `τ1`、`τ2`が与えられ、 `τ1` と `τ2` のユニファイアが `(S0,C0)` であるならば、  `mgu τ1 τ2 (S0,C0)` は、最も一般的なそのようなユニファイアを返します。
  そのようなユニファイアがない場合、 `mgu τ1 τ2 (S0,C0)` は有限個のステップで失敗します。

### 型再構築

  型再構築のアルゴリズムを図7に示します。


    tp (x,A,S,C)                = inst (S(Ax),S,C)
    tp (e1 e 2,A,S,C)           = let (τ1,S1,C1) = tp (e1,A,S,C)
                                        (τ2,S2,C2) = tp (e2,A,S1,C1)
                                        α a fresh type variable
                                        (S3,C3) = mgu τ1 (τ2->α) (S2,C2.α :: {})
                                    in (S3 α, S3,C3)
    tp (λx.e,A,S,C)             = let α a fresh type variable
                                        (τ1,S1,C1) = tp (e1,A.x:α,S,C.α :: {})
                                    in (S1 α ->τ1,S1,C1)
    tp (let x = e1 in e2,A,S,C) = let (τ1,S1,C1) = tp (e1,A,S,C)
                                        (σ,C2)     = tpgen (τ1,S1 A,C1,C)
                                    in tp (e2,A.x:σ,S1,C2)

    where

    inst (∀α :: Γ.σ,S,C)        = let β a fresh type variable
                                    in inst ([α -> β] σ,S,C.β :: Γ)
    inst (τ,S,C)                 = (τ,S,C)

    tpgen (σ,A,C,C)              = if ∃α ∈ dom(C)\(fv(A) ∪ reg(C) ∪ dom(C)) then
                                        tpgen(∀α :: C α.σ,A,C\α,C')
                                    else (σ, C)

  図7: 型再構築アルゴリズム

  [1](＃1) 関数 `tp` は、式として式、仮定集合、初期制約付き置換を引数とし、型と最終制約付き置換を返します。
  この機能はプログラムに直接拡張されています。
  このセクションの残りの部分は、 `tp` とセクション4の型システム、それによってセクション3の型システムとの間の対応関係を確立します。

  我々のアルゴリズムの健全性と完全性を確立するためには、以下の補題が必要である。
  最初に、 `tp` が本当に制約のある置換変圧器であることを示すことから始めます。

  **補題5.2** `(S,C)` は制約付き置換であり、`(τ,S',C') = tp (e,A,S,C)` ならば、 `(S',C')` は束縛された置換です。

  したがって、我々は今から制約付き置換の要件を省略します。

  **補題5.3** もし `tp (e,A,S,C) = (τ,S,C)` ならば `(S,C) ≼ (S,C)` です。

  この結果は、**let**-ケースを除いて直接的誘導によって確立することができます。
  セクション4で提示された型付け規則 (let') を思い出してください。
  この規則の前件部には、 let-definition の型を導出するためのものと let-body の型のためのものの2つの文脈があります。
  しかし、第2のものだけが結論部分に現れ、第1のものに含まれ、 `gen` 関数によって一般化されるそれらのインスタンス仮定です。
  `tp` では、単一のコンテキストを維持し、それをアルゴリズム全体に渡します。
  `tp` の **let**-ケースで `gen` 関数を使用する場合は、前のステージで生成されたインスタンスの仮定を過大化し、初期コンテキストの一部として `tp` に渡します。

  ----

  1 これは実際には、単一化関数の呼び出しの後に循環的なコンテキストを得ることができるため、コンテキストに対する我々の制限に違反するので、実際のアルゴリズムの単純化です。
  ここで欠けているのは、cliquedetectionアルゴリズムです。これは、単に出現検査のバリエーションです。
  わかりやすくするために、ここでは省略します。

  ----

  <!-- page 8 -->

  このような過大化を避けるためには、 let-definition の型を再構築する際に生成されるインスタンスの仮定のみに一般化のドメインを限定する必要があります。
  新しい一般化関数 `tpgen` を定義します。これは、 `gen` と比較して、インスタンスの仮定が一般化から除外される余分なコンテキストパラメータ `C` を取ります。
  アルゴリズムでは、一般化を行うときに、一般化のドメインを制限するために、最初のコンテキストを2番目のコンテキスト引数として `tpgen` に渡します。
  したがって、新たに生成されたインスタンスの仮定のみが一般化されます。

  今度はアルゴリズムの健全性を述べることができます。

  **定理5.4** `tp(e,A,S,C) = (τ,S,C)` ならば `S'A,C' ⊢' e :τ`。

  定理4.4と合わせて、我々は以下の健全性の結果を得ます。

  **系5.5** (`tp`の健全性) `tp(e,A,S,C) = (τ,S',C')` ならば `S'A,C' ⊢ e : τ`。

  最終的には、主要型付け結果を述べます。

  **定理5.6** `S'A,C' ⊢' e : τ'` かつ `(S',C') ≼ (S0,C0)` ならば
  `tp(e,A,S0,C0)` は `(τ,S,C)` で成功し、以下のような置換 `R` が存在します:

    S'A = RSA,   C' ⊦⊦ RC,   かつ,   τ'= Rτ.

  定理4.5と合わせて、我々は完全性の結果を得ました。

  **系5.7** (`tp`の完全性) `S'A,C' ⊢ e : σ'` と `(S',C') ≼ (S0,C0)` とするならば、
  `tp(e,A,S0,C0)` は `(τ,S,C)` で成功し、 以下のような置換 `R` が存在し、

      S'A = RSA,   かつ    σ' ≼C' Rσ

  ここで `(σ、〜C)= gen(τ、SA,C)`です。

  当然のこととして、主要型スキームについては次のような結果が得られます。

  `tp(e,A,S0,C0) = (τ,S,C)` and `gen(τ,SA,C) = (σ,C')`とするならば、 `σ` は `SA` と `C` の下で `e` の主要型スキームです。

## 6 あいまいさ再訪

  はじめに見てきたように、パラメトリック型クラスは、型スキームがあいまいかもしれないという問題を標準型のクラスと共有します。

  **定義**　型スキーム `σ = ∀αi :: Γi.τ` が与えられたとき、`Cσ = {αi :: Γi}` を `σ` の一般的な文脈とします。

  **定義**　型スキーム `σ = ∀αi :: Γi .τ` のジェネリック型変数αは、(1) `Cσ α ≠ ∅`、 (2) `α ∉ Cσ* (fv τ)` となります。

  あいまいな型変数は、実装の問題を引き起こします。
  多相性のオーバーロードを実装する通常の方法は、関数シグネチャのコンテキスト内のすべての型クラスに対して余分な辞書引数を渡すことです。
  あいまいな変数の制約は空でないため(1)、辞書を渡す必要があります。
  しかし、あいまいな変数は型(2)で空にならないので、インスタンス化されることはありません。したがって、どの辞書を渡すべきかわかりません。
  別の見方から見ると、適切なインスタンス型の辞書がありますが、一貫性の問題があります。異なるセマンティクス[Jon92a]を持つ式の実装がいくつかあります。

  <!-- page 9 -->

  この問題は、明示的な型シグネチャを使用して、プログラマが必要に応じて式の曖昧性を排除するように要求することによって回避されます。
  概念的には、曖昧さチェックは型再構築後に行われます。それが型の再構成の一部である場合、主要型のプロパティは失われます。
  ある意味では、あいまいさの問題は、再構成された型があまりにも一般的であることを示しています。
  すべてのあいまいな型には明白な置換インスタンスがあります(あいまいな変数をインスタンス化するだけです)。
  問題は、最も一般的で曖昧ではない型が常にあるということです。

  多項式型のクラスと比較して、型システムはしばしばあいまいさの少ない型を生成します。
  以下の式を考えましょう:

    len :: (sa :: Sequence a) => sa -> Int

  複数引数型のクラスとして見れば、`a`は述語では発生しますが型ではないのであいまいです。
  しかし、パラメトリック型クラスとして見ると、`a` はあいまいではありません。型には発生しませんが、`(sa :: Sequence a)`によって `sa` に制約されず依存します。
  したがって、(1)と(2)の両方が失敗します。

  3項の制約(b)のために、型のトップレベル型コンストラクタは、渡す必要がある辞書を一意に決定します。次のようにして、曖昧さの問題をさらに減らすことができます。
  したがって、2つの型が同じトップレベル型コンストラクタ(ただし型引数が異なる可能性があります)がある場合、それらのディクショナリは同じデータコンストラクタを共有します(ただしパラメータは異なる可能性があります)。
  次の手法を使用して、トップレベル型コンストラクタの等価性を静的に認識できます。

  1つの型パラメータを持ち操作を持たない特別な `ルート` クラス `TC` を紹介します。
  すべての型は、以下のインスタンス宣言(すべての型 `κτ` に対して暗黙的に生成されると考えることができる)によって、 `TC` のインスタンスです。

    inst κ τ:: TC (κ ())

  実際には、 `TC` は型のトップレベル型コンストラクタを `isolate` するために使用されます。
  つまり、2つの型が `TC` 制約によって関連付けられている場合、それらは同じトップレベル型コンストラクタを持つことがわかります。
  この2つの型は、同様に呼ばれます。

  **定義** 文脈 `C` を与えられたとき、 `C`、 `(~C)` の類似性は、 `C ⊦⊦ τ1:: TC τ2` が `τ1 ~C τ2` となります。

   `TC` は型の再構築中に他のすべての型クラスと同様に扱われます。
  これはあいまいチェックで特に扱われ、あいまいさの基準を強化することができます:

  **定義** 型スキーム `σ` におけるジェネリック型変数αは、 `α` が `σ` において弱く曖昧である場合は強くあいまいであり、全ての型 `τ, α ~ Cσ` は `τ` を意味する `σ` では強くあいまいな型変数です。

  `TC` 技術は、我々が正確に型をマップすることを可能にします[2](#2)

    map : ∀a.∀b.∀t.

        ∀sa :: {Sequence a,TC t}.
        ∀sb :: {Sequence b,TC t}.(a -> b) -> sa -> sb

  ----

  <a name="2"> </a> 2これまでに、順序単一化。

  ----

  <!-- page 10 -->

  これは、 `sa` と `sb` は要素型 `a` と `b` を持つ Sequence のインスタンス型で、 `sa` と `sb` は同じ型のコンストラクタを共有していると言います。

  `sa` と `sb` が同じ型のコンストラクタを持っているという知識は、最初はコンパイラ生成のインスタンス宣言の形から導かれたメタレベル上にあります。
  これを型システムで次のように形式化することができます。

  **定義** 型スキーム `σ = ∀αi :: Γi.τ'` は、 `Γi` のどれも `TC (κ τ)`を含まない場合、任意のコンストラクタ `κ` のための縮小された形式です。 `τ` と入力します。
  型スキームには `σR` を使用します。

  **定義** すべての縮小型スキーム `σR` について、2つの型スキーム `σ1` 、 `σ2` は、 文脈 `C` かつ `σ1 ≃C σ2`の下で同一です。

    σR ≼C σ1    ⇔    σR ≼C σ2.

  等式を含むようにジェネリックインスタンスの定義を拡張します。型スキーム `σ1` は、型スキーム `σ' s.t` が存在する場合、コンテキスト `C` の下で型スキーム `σ2` の一般的なインスタンスです。
  第3章の `≼C` の定義に従って、 `σ1 ≃C σ'` と `σ' ≼C σ2` を定義します。
  一般的なインスタンスのこの強力な概念は、ユーザー定義の型シグネチャをチェックするために重要です。

  **例:** `sa` に `List a` を代入すると、 `map` の型シグネチャは次のようになります:

    ∀sb :: {Sequence b,TC (List())} : (a -> b) -> List a -> sb

  一方、リストのマップの通常の定義には、次のような型があります:

    (a -> b) -> List a -> List b

  第1の型が第2の型のインスタンスであることを検証するためには、等価が必要です。

  文脈を短くするために、次のセクションでは、類似性の関係を `TC` の定義ではなく `(~)` に直接使用します。

## 7 Monads からリストへ

  このセクションでは、パラメトリック型クラスを使用して、以前はリストに限定されていた操作と概念の多くを一般化する方法を示します。
  はじめにスケッチされているように、最初のステップは、シーケンスのすべての実装に共通の操作をオーバーロードします。
  いくつかの重要な操作は、より一般的な Moand コンテキスト[Wad90]でも適用できます。したがって、 "Monad" と "Monad with zero" を "Sequence" のスーパークラスとして持つことは理にかなっています。
  次の列挙は、階層内のどのレベルでよく知られたリスト操作が定義されているかを示します。

  **モナド:** unit、join、map、monad comprehensions。

  **Monad0:* nil、filter、filter付きのcomprehensions。

  **Sequence:** cons, hd, tl, reverse, foldl, foldr, (++).

  関数プログラミングにおけるモナドの使用は[Wad90、Wad91]で調査されました。概念の動機付けのために、読者にそこに与えられた例を紹介します。
  ここで調査したいのは、具体的な実装から抽象化できるように、プログラミング言語の型システムでモナド(とその特殊化)を表現する方法です。
  パラメトリック型クラスを使用して、モナド演算がどのようにオーバーロードされるかを示します。
  これは、任意のモナド上の関数を定義し、異なるモナド上の演算に同じ名前を再利用し、現在の構文を変更することなくリスト内包を一般化することができるので便利です。

  Monadクラスを次のように定式化します:

    class ma :: Monad a where
        unit :: a -> ma
        bind :: (mb :: Monad b, ma ~ mb)
                => ma -> (a -> mb) -> mb
        map  :: (mb :: Monad b, ma ~ mb)
                => (a -> b) -> ma -> mb
        join :: (mma :: Monad ma, mma ~ ma)
                => mma -> ma
    -- Default definitions:
        map f xs  = xs `bind` (unit . f)
        join xss  = xss `bind` id
        bind xs f = join (map f xs)

  これは、モナドの2つの等価な公式を導入します。一つは `unit` と `bind` であり、もう一つは `unit`、`map`、`join` の観点からです。
  クラスのデフォルト定義では、一方の定式化を他方の定式化で表します。インスタンスは代わりに `bind` または `map` と `join` を定義することができます。
  モナドの資格を得るためには、インスタンスは3つの法則を満たさなければならず、これは型システムによって強制されません。
  `bind` は左と右の単位として `unit` を伴って結合的でなければなりません:

    (m `bind` f) `bind` g = m `bind` \x -> f x `bind` g
    \x -> unit x `bind` f = f
    m `bind` unit         = m

  リストは、次のインスタンス宣言とモナド法が保有するチェックによって目撃されるように、モナドを形成します。

    inst List a :: Monad a where
        unit x          = [x]
        map f [] xs     = []
        map f (x:xs)    = f x : map f xs
        join []         = []
        join (xs::xss)  = xs ++ join xss

  モナドのもう一つの例は、次のような "reply"-型です:

    data Maybe a = Some a | None

    inst Maybe a :: Monad a where
        unit x          = Some x
        bind (Some x) f = f x
        bind None f     = None

  その結果、リストや応答型や他のモナドインスタンスで動作するコードを書くことができるようになりました。
  特に、 `unit`、` map`、 `join`に標準翻訳を適用することで、それぞれの場合にリスト理解表記を使用できます。

  <!-- page 11 -->

    [t]           ≙ unit t
    [t | g1,g2]   ≙ join [[t | g2] | g1]
    [t | x <- e]  ≙ map (x : t) e

  ここで、 `t` と `e` は項、 `x` は変数、 `g1` と `g2` はジェネレータ `x <- e` です。

  Monad0 は Monad のサブクラスです。
  これはゼロモナド、`nil`、および `filter` 関数を追加します。

    class (ma :: Monad a) => ma :: Monad0 a where
        nil     :: ma
        filter  :: (a -> Bool) -> ma -> ma

  ゼロのつくモナドは、フィルタ付きのリスト内包を定義できる最も一般的な型クラスです。
  標準翻訳関数は( `p` はフィルタ、すなわちブール項です)。

    []      ≙ nil
    [t | p] ≙ filter p (unit t)

  リストと返信の両方の型は、次のようにゼロになります。

    instance List a :: Monad0 a where
        nil               = []
        filter p []       = []
        filter p (x:xs)   = if p x then
                                x : filter p xs
                            else filter p xs

    instance Maybe a : Monad0 a where
        nil               = None
        filter p None     = None
        filter p (Some x) = if p x then Some x
                            else None

  Monadsを使ったプログラミングの例として、抽象パーサについて議論し、[Wad90]の例を適用して拡張します。
  パーサーは、入力シンボルのシーケンスを出力にマップする関数です。正当な解析が存在しない場合は、失敗した値にマップします。
  解析が存在する場合は、入力ストリームの未使用部分と解析ツリーなどのアプリケーション依存の結果値で構成されます。
  パーサーがバックトラックを使用する場合、そのような解析がいくつか存在する可能性がありますが、determinsticの場合はゼロまたは1が存在します。
  以下では、決定性パーサのためのライブラリを構築します。
  そのようなパーサーはすべて型シグネチャを持ちます:

    data Parser a = P (String -> Maybe (a, String))

  インスタンスはデータ型でしか構成できないという制約があるため、コンストラクタタグ `P` が必要です。
  パーサーは、以下のインスタンス宣言によって目撃されるように、ゼロを持つモナドを形成します。

    inst Parser a :: Monad a where
        unit x      = P (\i -> [(x, i)])
        map f (P p) = P (\i ->
                            [(f x, i') | (x, i') <- p i])
        join (P pp) = P (\i ->
                            [(x, i'') | (P p, i') <- pp i,
                                        (x, i'') <- p i'])

    inst Parser a :: Monad0 a where
        nil             = P (\i -> [])
        filter b (P p)  = P (\i ->
                                [(x, i') | (x, i') <- p i,
                                        b x])

  我々は理解の表記に過負荷をかけていることに注意してください。
  前の2つのインスタンス宣言のモナドの補完は、リストではなくオプションの型で機能します。

  2つのプリミティブパーサーともう1つのパーサーコンビネータが必要です。

    sym         :: Parser Char
    sym         = P p
                    where p Nil         = []
                        p (Cons c cs) = [(c, cs)]

    lookahead   :: Parser Char
    lookahead   = P p
                    where p Nil = []
                        p cs  = [(hd cs, cs)]

    (|||)       :: Parser a -> Parser a -> Parser a
    P p ||| P q = P (\i -> case p i of
                                None   => q i
                            | Some x => Some x)

  ラムダ項の決定論的パーサーは、次のように書くことができます。

    data Term = Lambda Term Term
                | Apply Term Term
                | Id Char
                | Error

    term     :: Parser Term
    term     =  [Lambda x y | '\' <- sym,
                                x <- ident, y <- term]
            ||| fly | x <- aterm, y <- aterms x]

    aterm    :: Parser Term
    aterm    = [x | '(' <- sym, x <- aterm']
            ||| ident

    aterm'   :: Parser Term
    aterm'   = [x | x <- term, ')' <- sym]
            ||| [Error]

    aterms   :: Term -> Parser Term
    aterms x =  [z | c <- lookahead,
                    'a' <= c && c <= 'z' || c = '(',
                    y <- aterm,
                    z <- aterms (Apply x y)]
            ||| [x]

    ident    :: Parser Term
    ident    =  [Id c | c <- sym, 'a' <= c && c <= 'z']
            ||| [Error]

  <!-- page 12 -->

  定義されたパーサーは決定的です。それは決して逆戻りしません。
  したがって、解析の失敗は、プロダクションの開始時に発生するのか、途中で発生するのかによって異なるように扱われなければなりません。
  生産開始時に障害が発生した場合は、別の代替案を試してください。
  プロダクションの途中でエラーが発生すると、エラー・ノードを戻すことによって報告される構文エラーが通知されます。

  生産の大部分はモナド理解の観点から表現されていることに注意してください。
  今回は、オプションの種類やリストの代わりにパーサを参照しています。
  [Wad90]とは違って、モナドの包摂は参照するモナドにラベルを付ける必要はありません。我々は代わりに曖昧さを除去するために型システムに依存します(あいまいさがなければプログラマが定義した型付けを含みます)。
  モナドスタイルは、解析と抽象的なツリー生成との間の柔軟なインターフェースを提供します。
  生成されるパーサーは、合成された属性と継承された属性の両方を持つ属性文法に似ています(atermの定義を参照)。

## 8 結論

  オーバーロードされたデータコンストラクタとセレクタを持つコンテナクラスをサポートするために、Haskellの型クラスの一般化を提案しました。
  基礎となる型のシステムは、パラメトリック型クラスを持つ Hindley/Milner システムの拡張です。
  この拡張は、元のシステムの2つの重要な特性、すなわち決定可能な型定義性とプリンシパル型を保持します。
  その型スキームは、限定された定量化を使用し、その導入および除去は、別の状況制約付きインスタンス理論に依存します。
  型推論システムからインスタンス理論を切り離すことで、我々のシステムは以前の作業よりもモジュール化されています。
  得られたモジュール性は、実装者にとって大きな助けになると考えています。

  これまで説明していない点は、実行時にパラメトリック型クラスを実装する方法です。
  本質的に、[WB89]の行に沿った Haskell への翻訳スキームを採用することができます。
  型クラスの追加パラメータは、ランタイム辞書のパラメータに変換されます。
  そのような変換は、パラメトリック型クラスの(変換的)セマンティクスを提供することができます。
  良いランタイムモデルを提供できるかどうかは議論の余地があります。
  この翻訳スキームに基づいた既存の実装は、実行時のパフォーマンスが批判されています。
  原則として、型クラスを持つプログラムの実行時のパフォーマンスは、オブジェクト指向言語で記述されたプログラムのパフォーマンスよりも悪くあってはならないと主張します。
  さらに、同様の最適化手法を用いることができます[CU90]。
