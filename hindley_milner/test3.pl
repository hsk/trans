:- op(740,xfx,[∈, ⊑ , ˄, $]).
:- op(900,xfx,ⱶS).
:- op(1200, xfx, --).
term_expansion((A--B), (B:-A)).

member_eq(X,[Y|_]) :- X == Y.
member_eq(X,[_|Xs]) :- member_eq(X,Xs).

union_eq([],Ys,Ys).
union_eq([X|Xs],Ys,Zs) :- member_eq(X,Ys), union_eq(Xs,Ys,Zs).
union_eq([X|Xs],Ys,[X|Zs]) :- union_eq(Xs,Ys,Zs).

subtract_eq([],_,[]).
subtract_eq([X|Xs],Ys,Zs) :- member_eq(X,Ys),subtract_eq(Xs,Ys,Zs).
subtract_eq([X|Xs],Ys,[X|Zs]) :- subtract_eq(Xs,Ys,Zs).

inter_eq([],_,[]).
inter_eq([X|Xs],Ys,[X|Zs]) :- member_eq(X,Ys),inter_eq(Xs,Ys,Zs).
inter_eq([_|Xs],Ys,Zs) :- inter_eq(Xs,Ys,Zs).

ftv(X,[X]) :- var(X). 
ftv(m(T),Fvs) :- ftv(T,Fvs).
ftv((T1->T2),Fvs) :- ftv(T1,Fv1),ftv(T2,Fv2), union_eq(Fv1,Fv2,Fvs). 
ftv(∀(Γ,Ʈ),Fvs) :- ftv(Ʈ,Fv1), subtract_eq(Fv1,Γ,Fvs).
ftv([],[]).
ftv([_:T],Fvs) :- ftv(T,Fvs).

(X : Ⳓ) ∈ [Y : Ⳓ |_] :- X == Y.
(X : Ⳓ) ∈ [_ : _ |Γ] :- (X : Ⳓ) ∈ Γ.

% Generalization
    Γ˄(Ʈ) : ∀(As,Ʈ) :- ftv(Ʈ,Asʹ),ftv(Γ,Asʹʹ), subtract_eq(Asʹ,Asʹʹ,As) .

% Instantiation
    ∀(As,X) ⊑ _ :- var(X), member_eq(X,As).
    ∀(_,X) ⊑ X :- var(X).
    ∀(As,(T1->T2)) ⊑ (T1ʹ->T2ʹ) :- ∀(As,T1) ⊑ T1ʹ,∀(As,T2) ⊑ T2ʹ.
    m(Ʈ) ⊑ Ʈ.

% Syntactical Rule System

    atom(X), (X : Ⳓ) ∈ Γ, Ⳓ ⊑ Ʈ
    --%-------------------------        [Var]
    Γ ⱶS X : Ʈ.

    Γ ⱶS E0 : (Ʈ -> Ʈʹ), Γ ⱶS E1 : Ʈ
    --%-------------------------------- [App]
    Γ ⱶS (E0 $ E1) : Ʈʹ.

    [X : m(Ʈ)|Γ] ⱶS E : Ʈʹ
    --%------------------               [Abs]
    Γ ⱶS fun(X->E) : (Ʈ -> Ʈʹ).

    Γ ⱶS E0 : Ʈ, Γ˄(Ʈ) : ΓƮ, [X : ΓƮ|Γ] ⱶS E1 : Ʈʹ
    --%------------------------------------------- [Let]
    Γ ⱶS let(X = E0; E1) : Ʈʹ.


:- [] ⱶS fun(X->X) : (Ʈ->Ʈ).
:- [] ⱶS let(id=fun(X->X); id $ id) : (Ʈ->Ʈ).

:- halt.
