
unify(Γ,τ1 = τ2,R) :-
    mgu(τ1 = τ2,_θ),
    'Constrain'(_θ,Γ,Γc),
    R=(Γc ∪ (Γ \ 'Dom'(_θ)),_θ).

'Constrain'(_θ,Γ,R) :- R = ∪_{_α∈Dom(_θ)} constrain(_θ _α, Γ _α).

constrain(_α,S,R) :- R = [α:S].
constrain(_t((τn)~), S, R)  = R = ∪_{C∈S} constrains((τn)~, Dom(_t,C))
constrains((τn)~,(Sn)~,R) = R=∪_{i=1...n} constrain(τi, Si).

        !
        --%------------------ ASM
        (Σ,Γ,E) ⊢ _x : E(_x).

        (Σ,Γ,E) ⊢ _e1 : _τ2 → _τ1,
        (Σ,Γ,E) ⊢ _e2 : _τ2
        --%--------------------------------------- APP
        (Σ,Γ,E) ⊢ (_e1 $ _e2) : _τ1.

        (Σ,Γ,[_x:_τ1|E]) ⊢ _e : _τ2
        --%------------------------- ABS
        (Σ,Γ,E) ⊢ λ(_x → _e) : _τ1 → _τ2.

        (Σ,Γ,E) ⊢ _e1 : _σ1,
        (Σ,Γ,[_x:_σ1|E]) ⊢ _e2 : _σ2
        --%---------------------------------------- LET
        (Σ,Γ,E) ⊢ let(_x = _e1; _e2) : _σ2.

% variable names...

% e -> ᰀ

:- _e = 1, write(_e).
:- _℮ = 1, write(_℮).

:- ℰ = 1, write(ℰ).
:- Ⲉ = 1, write(Ⲉ).
:- Ⲑ = 1, write(Ⲑ).
:- Ⳝ = 1, write(Ⳝ).
:- _ⱸ = 1, write(_ⱸ).
%:- ⒠ = 1, write(⒠).
%:- ⓔ = 1, write(ⓔ).
%:- Ⓔ = 1, write(Ⓔ).

:- _ၔ = 1, write(_ၔ).
/*
Ꮛ
ᗴ
ᥱ
౿
ര
Æ
È	É	Ê	Ë
Ē	ē	Ĕ	ĕ	Ė	ė	Ę	ę	Ě	ě
Ǝ	Ə	Ɛ
ǝ
Ȅ	ȅ	Ȇ	ȇ
Ɇ	ɇ	
Έ
Є
е
ѐ	ё
Ә	ә	Ӛ	ӛ
Ԑ	ԑ
Յ
*/

:- _θ = 1, write(_θ).
:- Θ = 1, write(Θ).

:- nl,halt.

