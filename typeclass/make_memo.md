## アドホック多相下でのアドホックの作り方

http://www.cse.iitk.ac.in/users/karkare/courses/2010/cs653/Papers/ad-hoc-polymorphism.pdf

あたりを読んだ感想というかようやく

この論文では型クラス(アドホック多相)の新しいといっても1998年アプローチが提案されています。

大きく本文パートと付録パートに分かれていて、本文ではざっくりとした説明がされていて、付録で形式的な説明がされています。

本文での最初の変換例は図1から図2へ変換するというものです。

    class Num a where
    (+), (*) :: a -> a -> a
    negate   :: a -> a

    instance Num Int where
    (+)    = addInt
    (*)    = mulInt
    negate = negInt

    instance Num Float where
    (+)    = addFloat
    (*)    = mulFloat
    negate = negFloat

    square   :: Num a => a -> a
    square x = x * х

    squares           :: Num a, Num b, Num c => (a, b, c) -> (a, b, c)
    squares (x, y, z) =  (square x, square y, square z)

  図1： 算術演算の定義

    data NumD a = NumDict (a -> a -> a) (a -> a -> a) (a -> a)

    add (NumDict a m n) = a
    mul (NumDict a m n) = m
    neg (NumDict a m n) = n

    numDInt   :: NumD Tnt
    numDInt   =  NumDict addInt mulInt negInt
    numDFloat :: MumD F1oat
    numDFloat =  NumDict addFloat mulFloat negFloat

    square'         :: NumD a -> a -> a
    square' numDa X =  mul numDa X X

    squares' :: (NumD a, NumD b, NumD c) -> (a,b,c) -> (a,b,c)

    squares' (numD a, numD b, numD c) (x, y, z)
        = (square' numDa x, square' numDb y, square' numDc z)

  図2： 算術演算の変換


オーバーロードの型付けおよび変換ルールをPrologに書きなおしたものを以下に示します：

    atom(X), member(X : m(S \ X_), Γ)
    --%-------------------------------- (TAUT) 単相型の変数
    Γ ⊢ X :: S \ X_.

    atom(X),
    member(X : i(S \ X_), Γ)
    --%-------------------------------- (TAUT) 多相型の変数
    Γ ⊢ X :: S \ X_.

    Γ ⊢ E :: ∀ A => S \ E_, subst(A,_, S, S_)
    --%-------------------------------- (SPEC) 特殊化、具体化
    Γ ⊢ E :: S_ \ E_.

    \+ member(A::_,Γ),
    Γ ⊢ E :: S \ E_
    --%-------------------------------- (GEN) 一般化
    Γ ⊢ E :: ∀ A => S \ E_.

    Γ ⊢ E :: (T1 -> T) \ E_,
    Γ ⊢ E1:: T1 \ E1_
    --%-------------------------------- (COMB) 関数適用
    Γ ⊢ (E $ E1) :: T \ (E_ $ E1_).

    [X::m(T1 \ X)|Γ] ⊢ E :: T \ E_
    --%-------------------------------- (ABS) ラムダ抽象
    Γ ⊢ (λ X->E) :: (T1 -> T) \ (λ X->E_).

    Γ ⊢ E :: S \ E_,
    [X :: S \ X|Γ] ⊢ E1 :: T \ E1_
    --%----------------------------------------------- (LET) let式
    Γ ⊢ (let X = E in E1) :: T \ (let X = E_ in E1_).

最初の７つの規則は、Let多相の型システムに変更を加えたものです。
全体的に\の後ろに式の変換が追加されており、変数はオーバーロードされる可能性があるので２パターンの規則があります。
それぞれ、TAUT(Tautoはギリシャ語の同一、Tautology？)は変数、SPEC(Specialize)は特殊化、GEN(generalize)は一般化、COMB(Combine)は関数適用、ABS(Abstract)はラムダ抽象、LETはLet式の規則です。
この規則の中でアルゴリズミックでない規則がSPECとGENです。
SPECとGENを動作させるにはSPECのルールは変数のルールに含め、GEN規則はLETルールに含めるように書き換えるとよいことが知られています。
GENは一般化された型を追加しSPECは一般化された型を削除する規則なので論文によっては(∀ I) (∀ E)と記述されることもあります。

    member(X : ο(_), Γ),
    [X :: T \ XT|Γ] ⊢ E :: P \ E_
    --%---------------------------------- (PRED) 述語を型に追加(ラムダ式に変換)
    Γ ⊢ E :: ((X :: T)->P) \ (λ XT-> E_).

    member(X : ο(_), Γ),
    Γ ⊢ E :: ((X :: T)->P) \ E_,
    Γ ⊢ X :: T \ E1_
    --%---------------------------------- (REL) 型から述語を削除(関数適用に変換)
    Γ ⊢ E :: P \ (E_ $ E1_).

    [X : ο(S)|Γ] ⊢ E :: T \ E_
    --%---------------------------------- (OVER)
    Γ  ⊢ (over X :: S in E) :: T \ E_.

    member(X : ο(_), Γ),
    [X : i(S1 \ XS_) | Γ] ⊢ E1 :: S1 \ E1_,
    [X : i(S1 \ XS_) | Γ] ⊢ E  :: T  \ E_
    --%------------------------------------------------------ (INST)
    Γ ⊢ (inst X :: S1 = E1 in E) :: T \ (let XS_ = E1_ in E_).


残り４つのルールはオーバーロードを定義する式(OVER)とインスタンスを定義する式(INST)の規則と、
述語の追加(PRED)および削除(REL)です。
PRED,RELの規則は、GENやSPECと同様にアルゴリズミックではない規則なので別の規則に埋め込む形に変形する必要があります。
PREDはGENに対応し、RELはSPECに対応します。GENが一般化型を追加するようにPREDは述語型を追加し、SPECが一般化型を削除するようにRELが述語型を削除します。
GENのルールがletに組み込まれるようにPREDもまたletに組み込み、SPECが変数の参照に組み込まれるようにRELも変数の参照に組み込むと良さそうであることが洞察できます。
