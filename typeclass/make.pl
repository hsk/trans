/*

構文

識別子           X
式               E ::= X
                     | E0 $ E1
                     | λX -> E
                     | let X = E0 in E1
                     | over X :: S in E
                     | inst X :: S = E0 in E1
型変数           A
型コンストラクタ C
型               T ::= (T -> T1) | A | C(T1, ..., Tn)
述語型            P ::= (X :: T)->P | T
型スキーム         S ::= ∀ A => S | P

*/

:- op(1, fx, [∀ , λ ]).
:- op(699, xfy, [ $ ]).
:- op(700, xfx, [ = ]).
:- op(878, xfy, [ ∧  ]).
:- op(879, xfy, [ ->, => ]).
:- op(881, yfx, [ :: , : ]).
:- op(882, fx, [let, over, inst ]).
:- op(883, xfy, [ in ]).
:- op(901, yfx, [ ⊢ , \ ]).
:- op(1200, xfx, [ -- ]).
:- set_prolog_flag(occurs_check,true).
term_expansion(A -- B, B :- A).

test(let a in b).
test(let a in let b in c).
test(inst a in inst a in b).

example1(
  over eq :: ∀ a => 'Eq'(a) in
  inst eq :: 'Eq'('Int') = eqInt in
  inst eq :: 'Eq'('Char') = eqChar in
  inst eq :: (∀ a => ∀ b => (eq :: 'Eq'(a))->(eq :: 'Eq'(b))-> 'Eq'(a, b))
           = (λp-> λq-> eq $ (fst $ p) $ (fst $ q) ∧  eq $ (snd $ p) $ (snd $ q)) in
  eq $ (1, 'a') $ (2, 'b')
).

example2(
  over numD :: ∀ a => 'Num'(a) in
  inst numD :: 'Num'('Int') = (addInt, mulInt, negInt) in
  inst numD :: 'Num'('Float') = (addFloat, mulFloat, negFloat) in
  let '+' = fst $ numD in
  let '*' = snd $ numD in
  let negate = thd $ numD in
  let square = λ x -> x * x in
  square $ 3
).

assumps([
  (eq : o(∀ a=> 'Eq'(a))),
  (eq : i('Eq'('Int' ) \ eq('Eq'('Int' )))),
  (eq : i('Eq'('Char') \ eq('Eq'('Char')))),
  (eq : i(∀ a => ∀ b => (eq :: 'Eq'(a))->(eq :: 'Eq'(b))->'Eq'(a, b) \ eq(∀ a=> ∀ b=>(eq::'Eq'(a))->(eq::'Eq'(b))->'Eq'(a,b)) )),
  (eq :: 'Eq'(a) \ eq('Eq'(a))),
  (eq :: 'Eq'(b) \ eq('Eq'(b))),
  (p :: (a, b) \ p),
  (q :: (a, b) \ q)
]).

writeln([] ⊢ X), fail
--%----------------- (Dummy)
_ ⊢ X :: _ \ _.

% 多相型の変数
atom(X), member(X : m(S \ X_), Γ)
--%-------------------------------- (TAUT)
Γ ⊢ X :: S \ X_.

/*
% オーバーロードの変数
atom(X),
member(X : i(S \ X_), Γ)
--%-------------------------------- (TAUT)
Γ ⊢ X :: S \ X_.
*/

% 関数適用
Γ ⊢ E :: (T1 -> T) \ E_,
Γ ⊢ E1:: T1 \ E1_
--%-------------------------------- (COMB)
Γ ⊢ (E $ E1) :: T \ (E_ $ E1_).

% ラムダ抽象
[X:m(T1 \ X)|Γ] ⊢ E :: T \ E_
--%-------------------------------- (ABS)
Γ ⊢ (λ X->E) :: (T1 -> T) \ (λ X->E_).

% let式
Γ ⊢ E :: S \ E_,
[X : m(S \ X)|Γ] ⊢ E1 :: T \ E1_
--%----------------------------------------------- (LET)
Γ ⊢ (let X = E in E1) :: T \ (let X = E_ in E1_).


/*
% 特殊化、具体化、インスタンス化

Γ ⊢ E :: ∀ A => S \ E_, subst(A,_, S, S_)
--%-------------------------------- (SPEC)
Γ ⊢ E :: S_ \ E_.

この規則は左再帰があって無限ループするので使えない。
一般化はletによって値が束縛された場合に行う。

% 一般化

\+ member(A::_,Γ),
Γ ⊢ E :: S \ E_
--%-------------------------------- (GEN)
Γ ⊢ E :: ∀ A => S \ E_.

この規則も左再帰があるのでプログラムで使えない。
インスタンス化するタイミングは変数を参照した時点で行う。

*/

/*
% 述語を型に追加します(ラムダ式に変換します)

member(X : ο(_), Γ),
[X :: T \ XT|Γ] ⊢ E :: P \ E_
--%----------------------------------------------- (PRED)
Γ ⊢ E :: ((X :: T)->P) \ (λ XT-> E_).

% 型から述語を削除します(関数適用に変換します)。

member(X : ο(_), Γ),
Γ ⊢ E :: ((X :: T)->P) \ E_,
Γ ⊢ X :: T \ E1_
--%---------------------------------- (REL)
Γ ⊢ E :: P \ (E_ $ E1_).

この２つの規則も左再帰があって使えない。
基本的にオーバーロードに対しての規則で、インスタンス化と具体化と似た性質を持っているから、let と 変数の参照時にそれぞれ適用すればよいはず。

*/
/*
[X : ο(S)|Γ] ⊢ E :: T \ E_
--%---------------------------------- (OVER)
Γ  ⊢ (over X :: S in E) :: T \ E_.

member(X : ο(_), Γ),
[X : i(S1 \ XS_) | Γ] ⊢ E1 :: S1 \ E1_,
[X : i(S1 \ XS_) | Γ] ⊢ E  :: T  \ E_
--%------------------------------------------------------ (INST)
Γ ⊢ (inst X :: S1 = E1 in E) :: T \ (let XS_ = E1_ in E_).
*/
/*
  OVERルールは、適切な `(::o)` バインディングを環境に追加し、INSTルールは適切な `(::i)` バインディングを環境に追加します。
  仮定集合に対する妥当性条件は、オーバーロードされた識別子が有効な型でのみインスタンス化されることを保証します。

  いずれの変換も `over` 式または `inst` 式を含んでいないので、オーバーロードは含まれていません。

*/

:- ([] ⊢ (λ x -> x) :: T \ E_), writeln(T \ E_).
:- ([] ⊢ (let x = (λ x -> x) in x) :: T \ E_), writeln(T \ E_).
:- ([] ⊢ (let id = (λ x -> x) in (id $ id)):: T \ E_), writeln(T \ E_).
%:- example1(E), ([] ⊢  E :: T \ E_), writeln(T \ E_).

:- halt.
