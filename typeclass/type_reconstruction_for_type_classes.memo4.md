## アジェンダ

  - 要約
  - 1 はじめに
  - 2 Mini-Haskell
  - **3 型推論システム**
  - ４ ソート制約付き単一化
  - 5 アルゴリズムW
  - 6 アルゴリズムI

---

## 4 ソート制約付きの型単一化

  この節では、ソート制約の存在下でのコンテキストの形式による単一化について説明します。
  原則として、この問題は、(NS91) で `⪯` に関して行われたように、順序ソートされた単一化に縮小することができます。
  しかし、われわれは、シンプルさを求める私たちの探求に反しているので、そうしないことを控えています。すなわち、オーダーソートされた単一化を伴い、アルゴリズムは実際よりも複雑に見えます。
  さらに、オーダーソートされた単一化の標準理論は、とにかく再形式化する必要があります。変数は、コンテキストを使用するのではなく、ソートでタグ付けされていると仮定します。

---

  この論文の残りの部分では、固定シグネチャ `Σ = (∆, ≤)` を仮定します。
  これは、過度のパラメータ化を避ける表記的なデバイスにすぎません。

---

  ソート情報はコンテキスト内で維持されるため、頻繁にコンテキストと置換のペアを処理します。
  置換 `θ` は、すべての `α` に対して `Γ` の文脈における `Γ` のソート制約に従い、`Γ` :`Γ、Γ、Γθα:Γα` と書かれています。
  `Γα = {}` ならば、 すべての `α ∈ Dom(Γ)` に対して `Σ,Γ' ⊢ θα : Γα` がすべて成り立つので、 `Σ,Γ' ⊢ θα : Γα`  を必要とすれば十分です。
  例えば、セクション2の例のように `Eq` とリストを定義します。
  それから `[β:Eq] ⊢ {α |→ list(β)} : [α:Eq]` となります。

---

  コンテキスト置換ペアの順序付けを定義します。

    (Γ,θ) ≥ (Γ',θ') ⇔ ∃δ. δθ = θ' ∧ Γ' ⊢ δ : Γ

  ここで `δθ` は合成として定義されます: `(δθ)(s) = δ(θ(s))`。

---

  `τ1` と `τ2` の unifier の集合 w.r.t. `Γ` は、`U(Γ, τ1 = τ2)` と書かれ、以下のコンテキスト置換対からなります。

    U(Γ, τ1 = τ2) = {(Γ',θ) | θτ1 = θτ2 ∧ Γ' ⊢ θ : Γ}

---

  すべての `(Γ1, θ1) ∈ U(Γ, τ1 = τ2)` に対して、 `(Γ0, θ0) ≥ (Γ1, θ1)` ならば、ユニファイア `(Γ0 , θ0) ∈ U(Γ, τ1 = τ2)` は最も一般的です。
  すべての `Γ` と `τ1 = τ2` に対して集合 `U(Γ, τ1 = τ2)` が空であるか、最も一般的な統合を含むならば、 `Σ` を法とする単一化は単一であるといいます。

---

  シグネチャ `Σ` は、すべての型のコンストラクタ `t` に対してすべてのクラス `C` がセットされていれば、コアグラルと呼ばれます

    D(t, C) = {(Sn)~ | ∃D ≤ C. (t : ((Sn)~)D) ∈ ∆}

  は空であるか、または `⪯` に関して最大の要素を含みます。
  `D(t,C)` が空であれば `Σ` がコアグラルであると `Dom(t,C)` は `D(t,C)` の最大要素を返します。
  例えば、 `Dom(list, Eq) = Eq` でも `Dom(list, Ord)` は失敗します。

---

  ソートされた単一化は、ソートされていない単一化+制約の解決として表現できます。
  コアギュラシグネチャ `Σ` が与えられた場合、これは以下の簡単な形式をとります:

    unify(Γ,τ1 = τ2) =
      let θ  = mgu(τ1 = τ2)
          Γc = Constrain(θ,Γ)
      in (Γc ∪ (Γ \ Dom(θ)),θ)

  <!-- page 10 -->

---

  ここで

  - `mgu` は未分類の `mgu` を計算する(特に、 `θ` が冪等であり、`Dom(θ) ∪ Cod(θ) ⊆ FV(τ1 = τ2))` 又は存在しない

  - 2つのソート・コンテキストの和集合は、

         Γ1 ∪ Γ2 = [α : Γ1α ∪ Γ2α | α ∈ Dom(Γ1) ∪ Dom(Γ2)]

   - `Constrain(θ,Γ)` は、 `Γc ⊢ θ : Γ` となるような最も一般的な文脈 `Γc` を計算します。
  
        Constrain(θ,Γ) = ∪_{α∈Dom(θ)} constrain(θα,Γα)

   - `constrain(τ,S)` は `Σ,Γ ⊢ τ : S` となるような最も一般的な文脈 `Γ` を計算します。

        constrain(α,S)        = [α:S]
        constrain(t((τn)~), S)  = ∪_{C∈S} constrains((τn)~, Dom(t,C))
        constrains((τn)~,(Sn)~) = ∪_{i=1...n} constrain(τi, Si)
        
---

  したがって、 `mgu` が失敗した場合、または `Constrain` で使用されている `Dom(t,C)` が存在しない場合、統合は失敗します。
  拘束の第一引数の誘導によって、
  
    constrain(t((τn)~,S) = ∪_{C∈S} constrains((τn)~, Dom(t,C))
  

  これは、以下の証明においても有用な制約の代替定義を提供します。
  制約がどのように働くかを見るには、 `Eq` を仮定し、セクション2の例のように再度リストします。
  それから、 `constrain(list(β), Eq) = constrains(β, Dom(list, Eq)) = [β:Eq]` を制約します。

---

  Constrainの健全性と完全性は、 `Σ` の中核性を仮定し、 `τ` の構造上の誘導によって証明される以下の補題によって捕捉されます:

---

  補題 4.1

  `constrain(τ,S)` が定義されていれば `Σ`、`constrain(τ,S) ⊢ τ : S` を定義します。

  補題 4.2

  `constrain(θτ,S)` が定義されていれば `constrain(τ,S)` も定義され、さらに `constrain(θτ,S) ⊢ θ : constrain(τ,S)` が成り立ちます。

  補題 4.3

  `Σ,Γ ⊢ τ : S` ならば `constrain(τ,S)` が定義され、 `Γ` よりも一般的です。

---

  最後に、主定理:

  定理 4.4

  `Σ` がコアグラムの場合、 unify は最も一般的なユニファイアーを計算します。

  証明健全性を示すために、 `(Γ,τ1 = τ2)` の単一化を`(Γ0, θ0)` の結果で終わらせます。
  それは `θ0 τ1 = θ0 τ2` に直接従います。
  すべての`α` に対して`Γ0 ⊢ θ0 α : Γα` が残っていることは変わりません。
  `α ∉ Dom(θ0)` ならば、 `Γα ⊆ Γ0 α` となり、主張は自明です。
  `α ∈ Dom(θ0)` ならば、 `Γ0 ⪯ Γc = Constrain(θ0,Γ) ⪯ constrain(θ0 α,Γα)` を制約し、補題 4.1 に従います。

---

  <!-- page 11 -->

  完全性を示すためには、`(Γ1,θ1) ∈ U(Γ, τ1 = τ2)`、すなわち、`θ1 τ1 = θ1 τ2` かつ `Γ1 ⊢ θ1 : Γ` とします。
  `τ1` と `τ2` はunorted unifier `θ1` を持っているので、 `mgu(τ1 = τ2)` が定義され、ある `δ` の `θ1 = δθ0` となるような置換 `θ0` が得られます。
  `unify(Γ,τ1 = τ2)` の定義は、`constrain(θ0 α, Γα)` の定義を必要とします: 補題4.2は `constrain(θ0 α, Γα)` の定義を与えます。
  したがって、 `unify(Γ,τ1 = τ2)` は結果 `(Γ0,θ0)` で終了します。

---

  それは `Γ1 ⊢ δ : Γ0` であることが分かります。
  `β ∈ Dom(θ0)` ならば `Γ0 β = {}` したがって `Γ1 ⊢ δβ : Γ0 β` が保たれるのは自明です。
  ここで、 `β ∉ Dom(θ0)` とします。
  従って、`Γ0 β = Γc β ∪ Γβ` です。
  `Γ1 ⊢ θ1 : Γ` から `Γ1 ⊢ δβ : Γβ` となります。
  `Γ1 ⊢ δ : Γc` の証明はより複雑です。
  補題 4.2 から、(α1α、Γα)⊢δ:任意の`α` に対して `constrain(θ1 α, Γα) ⊢ δ : constrain(θ0 α, Γα)` です。
  `Γ1 ⊢ θ1 α : Γα` から、 補題 4.3 は `Γ1 ⪯ constrain(θ1 α, Γα)`、したがって単調性 `Γ1 ⊢ δ : constrain(θ0 α, Γα)` を意味します。
  これは簡単に `Γ1 ⊢ δ : Constrain(θ0, Γ)`、すなわち `Γ1 ⊢ δ : Γc` を容易に生成します。 □

---

  定理 4.5

  `Σ` が中核である場合、`Σ` を単一化は単一です。

  証明 "if" 方向は定理4.4の結果です。
  "唯一の" 方向については、`Σ` は中括弧ではありません。
  したがって、第3の宣言 `t : ((Un)~)E'`, `E' ⪯ E`, かつ `(Sn)~`, `(Tn)~ ⪯ (Un)~` が存在しないようなクラス `C`, `D ≤ E` 及び宣言  `t : ((Sn)~)C` and `t : ((Tn)~)D, (Sn)~ ⪯/ (Tn)~` 及び `(Tn)~ ⪯/ (Sn)~` が存在します。
  したがって、単一化問題([β:E]、t((αn)〜)=β) `は、最も一般的な単一化を持ちません。
  2つの最大値は、`([(αn : Sn)~],θ)` と `([(αn:Tn)~], θ)` です。ここで `θ = {β → t((αn)~)}` です。 □

---

  したがって、プリンシパル型が存在するシグネチャの正確な特徴付けが行われます。

---

  Mini-HaskellのCLASS宣言とINST宣言がコアギュラシグネチャを生成するかどうかはまだ分かりません。
  実際には、第3章で述べた非形式化された文脈条件によって制限された場合に行います。
  後者の文脈条件は、有効なクラス宣言およびインスタンス宣言から導出されたすべての `∆` および `≤` が以下の強い特性を有することを意味します。
  `D(t、C) `は `t((Sn)~)C` が `t` と結果 `C` のための一意の宣言であれば、シングルトン `{(Sn)~}` で、そのような宣言がないならば、空です。
  したがって、 `Dom(t,C)` は `≤` を参照することなく、 `∆` だけを使って計算することができます。
  これは、型の単一化、したがって次のセクションの型推論で見られるように、サブクラス階層を完全に無視できるという観察につながります。

---

  サブクラス階層を無視するということは、セクション2で定義されているソートで等価 `≈` によって与えられる自由度を放棄することを意味します。
  例えば、 `unify([α : {Eq}, β : {Ord}], α = β)` は `([α : {Eq, Ord}], {β |→ α})` を返します。
  `≈` を考慮すると、`([α : {Ord}], {β |→ α})` を返すこともできます。
  次の展開がこれらのユニファイアのどれが計算されているかに依存しないことを示すために、 `unify` は任意の関数であると仮定し、 `Σ` がコアギュラであると仮定し、最も一般的な湯にファイアを返すと仮定すると: もしも `U(Γ, τ1 = τ2) ≠ {}` ならば

---

  - `unify(Γ, τ1 = τ2) ∈ U(Γ, τ1 = τ2)` かつ
  - すべての `(Γ',θ) ∈ U(Γ, τ1 = τ2)` に対して、 `(Γ',θ) ≤ unify(Γ, τ1 = τ2)` です。


  これには、いくつかの単純なプロパティが含まれます。

---

  <!-- page 12 -->

  事実 4.6

  もし `unify(Γ,τ1 = τ2) = (Γ',θ)` ならば

  - `θ` は `τ1` と `τ2` の最も一般的な統合であり、
  - `Dom(Γ') ∪ FV(θ) ⊆ Dom(Γ) ∪ FV(τ1) ∪ FV(τ2)`、
  - `Dom(Γ') ∩ Dom(θ) = {}` です。


---

  第2の事実は、 `unify` が新しい変数を導入しないことを示し、最後は `Γ'` が `θ` によってインスタンス化された変数を制約しないことを表しています。
  `Γ` は`≈` までしか決まらないことは容易に分かります。
  したがって、単一化アルゴリズムは、冗長要素を各ソートから削除することによって、常に`Γ` が "最小化" されることを保証することができます。

---

  最後に、 Haskell の文脈条件よりも厳密に弱いという事実が、後者が緩和できるかどうかを疑問に思うかもしれません。
  私たちは、些細なリラクゼーションはないと考えていますが、セマンティクスと語用論を考慮に入れて型システムを超えなければならないため、このテーマを拡大したくありません。

---
