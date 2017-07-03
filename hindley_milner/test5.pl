:- op(740,xfx,[∈, ⊑ , ˄, $]).
:- op(900,xfx,ⱶW).
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

(X : Ⳓ) ∈ Γ :- member(X : Ⳓ, Γ).

% Generalization
    Γ˄(Ʈ) : ∀(As,Ʈ) :- ftv(Ʈ,Asʹ),ftv(Γ,Asʹʹ), subtract_eq(Asʹ,Asʹʹ,As) .

% Instantiation
    inst(∀(As,X), _) :- var(X), member_eq(X,As).
    inst(∀(_,X), X) :- var(X).
    inst(∀(As,(T1->T2)), (T1ʹ->T2ʹ)) :- inst(∀(As,T1), T1ʹ),inst(∀(As,T2), T2ʹ).
    inst(m(Ʈ), Ʈ).

newvar(_).
unify(A,A).

% Algorithm W

    atom(X), (X : Ⳓ) ∈ Γ, inst(Ⳓ,Ʈ)
    --%-----------------------------        [Var]
    Γ ⱶW X : Ʈ.

    Γ ⱶW E0 : Ʈ0, Γ ⱶW E1 : Ʈ,
    newvar(Ʈʹ), unify(Ʈ0, (Ʈ -> Ʈʹ))
    --%--------------------------------     [App]
    Γ ⱶW (E0 $ E1) : Ʈʹ.

    newvar(Ʈ), [X : m(Ʈ)|Γ] ⱶW E : Ʈʹ
    --%------------------------------       [Abs]
    Γ ⱶW fun(X->E) : (Ʈ -> Ʈʹ).

    Γ ⱶW E0 : Ʈ, Γ˄(Ʈ) : ΓƮ, [X : ΓƮ|Γ] ⱶW E1 : Ʈʹ
    --%------------------------------------------- [Let]
    Γ ⱶW let(X = E0 ; E1) : Ʈʹ.

:- [] ⱶW fun(x->x) : (Ʈ->Ʈ).
:- [] ⱶW let(id=fun(x->x); id $ id) : (Ʈ->Ʈ).

:- halt.
