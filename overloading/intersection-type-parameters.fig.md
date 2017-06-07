
    Intersection types     ι    ::= τ | ι ∧ τ
    Monointersection types τ    ::= a | ι → τ
    Polymorphic types      σ    ::= ∀a. P ⇒ τ

    Type constraint        P, Q ::= | P, C τ

    Typing context         Γ    ::= | Γ, (x : σ) | Γ, (x : ι)

    Terms                  t, u ::= x                Variable
                                |  λx. t            Functional abstraction
                                |  λ(x :: ρ). t     Annotated functional abstraction
                                |  tu               Function application
                                |  let x = u in t   Let-binding
                                |  t :: σ           Type annotated expression (σ closed)

Figure 1. Syntax of types and terms


    [P ; Γ |- t : τ]

    |-inst σ ≤ P ⇒ τ
    ------------------------ (VAR)
    P ; Γ, (x : σ) |- x : τ

    P ; Γ, (x : τ') |- t : τ
    -------------------------- (ABS)
    P ; Γ |- λx. t : τ'→ τ

    τ ∈ ι
    -------------------------- (VARi)
    P ; Γ, (x : ι) |- x : τ

    P ; Γ, (x : ι) |- t : τ
    ----------------------------- (ABSA)
    P ; Γ |- λ(x :: ι). t : ι → τ

    P ; Γ |- t : ι → τ
    Q; Γ |-∧I u : ι
    ------------------ (APP)
    P, Q; Γ |- t u : τ

    Γ |-gen u : σ
    P ; Γ, (x : σ) |- t : τ
    ---------------------------- (LET)
    P ; Γ |- let x = u in t : τ

    Γ |-gen t : σ
    |-inst σ ≤ P ⇒ τ
    ---------------------- (ANNOT)
    P ; Γ |- (t :: σ) : τ

    [Γ |-gen t : σ]

    P ; Γ |- t : τ
    a* = ftv(P ⇒ τ) − ftv(Γ)
    ---------------------------- (GEN)
    Γ |-gen t : ∀a*. P ⇒ τ

    [|-inst σ ≤ P ⇒ τ]

    Q |= [(a |→ τ')*]P
    ------------------------------------- (INST)
    |-inst ∀a*. P ⇒ τ ≤ Q ⇒ [(a |→ τ')*]τ

    [P ; Γ |-∧I t : ι]

    Pi ; Γ |- t : τi , for i = 1, ..., n
    ------------------------------------------------------- (GENi)
    (P1, ..., Pn); Γ |-∧I t : τ1 ∧ ... ∧ τn

Figure 2. Type system CTi


[P ; Γ |-δ t : τ] δ = ⇑⇓

|-inst σ ≤ P ⇒ τ
-------------------------- (VAR)
P ; Γ, (x : σ) |-δ x : τ

P ; Γ, (x : τ') |-δ t : (τ, P', Γ')
--------------------------- (ABS)
P ; Γ |-δ λx. t : τ'→ τ


P ; Γ |-⇑ t : ι → τ
Q ; Γ |-∧I⇓ u : ι
---------------------------(APP1)
P, Q; Γ |-δ t u : τ

τ ∈ ι
--------------------------- (VARi)
P ; Γ, (x : ι) |-⇓ x : τ

P ; Γ, (x : ι) |-δ t : τ
------------------------------ (ABSA)
P ; Γ |-δ λ(x :: ι). t : ι → τ

(x : ι) ∈ Γ, for some ι
Q; Γ |-⇑ u : τ'
P ; Γ |-⇓ x : τ' → τ
------------------------ (APP2)
P, Q; Γ |-δ x u : τ

(x1 : ι1) ∈ Γ and (x2 : ι2) ∈ Γ
∃ τ'. (τ'→ τ ∈ ι1) and (τ'∈ ι2)
-------------------------------------- (APP3)
P ; Γ |-δ x 1 x 2 : τ

Γ |-gen⇑ u : σ
P ; Γ, (x : σ) |-δ t : τ
------------------------------ (LET)
P ; Γ |-δ let x = u in t : τ

P, Q'; Γ |-⇓ t : τ'
|-inst ∀a*. Q'⇒ τ'≤ Q ⇒ τ
---------------------------------- (ANNOT)
P, Q; Γ |-δ (t :: ∀a*. Q'⇒ τ') : τ

[Γ |-gen⇑ t : σ]

P ; Γ |-⇑ t : τ
a* = ftv(P ⇒ τ) − ftv(Γ')
-------------------------- (GEN)
Γ |-gen⇑ t : ∀a. P ⇒ τ

[|-instδ σ ≤ ρ]

τ'= [(a |→ τ'')*]τ
Q |= [(a |→ τ')*]P
----------------------------- (INST)
|-instδ ∀a. P ⇒ τ ≤ Q ⇒ τ'

[P ; Γ |-⇓∧I t : ι]

P ; Γ |-⇓ t : τ for each τ ∈ ι
------------------------------- (GENi)
P ; Γ |-⇓∧I t : ι

Figure 3. Bidirectional type inference for type system CTi
