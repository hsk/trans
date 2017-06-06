    σ ::= ∀α.σ             (quantified type)
       |  α                 (type variable)
       |  c σ1...σn         (type constructor application)

    ρ ::= α | c σ1 ... σn   (uniquantified types)
    τ ::= α | c τ1 ... τn   (monomorphic types)

Figure1. HMF types

            x : σ ∈ Γ
    VAR     -----------
            Γ |- x : σ

            Γ |- e : σ    α ∉ ftv (Γ)
    GEN     -------------------------
            Γ |- e : ∀α.σ

            Γ |- e : σ1    σ1 ⊑ σ2
    INST    ----------------------
            Γ |- e : σ2

            Γ, x : τ |- e : ρ
    FUN     -------------------
            Γ |- λx .e : τ → ρ

            Γ, x : τ |- e : ρ
    FUN-ANN ------------------
            Γ |- λx.e : τ → ρ

            Γ |- e1 : σ1   Γ, x : σ1 |- e2 : σ2
            ∀σ1' . Γ |- e1 : σ1' ⇒ σ1 ⊑ σ1'
    LET     -----------------------------------
            Γ |- let x = e1 in e2 : σ2

            Γ |- e1 : σ2 → σ    Γ |- e2 : σ2
            (∀σ'σ2'.(Γ |- e1 : σ2' → σ' ∧ Γ |- e2 : σ2')
                     ⇒ [[σ2 → σ]] ≤ [[σ2' → σ']])
    APP     ----------------------------------------------
            Γ |- e1 e2 : σ

Figure2. Type rules for Plain HMF

    P[[(let x = e1 in e2) :: ∃α*. σ]]
      = let x = e1 in P[[e2 :: ∃α*. σ]]
    P[[(λx .e) :: ∃α*. ∀β*. σ1 → σ2]]
      = λ(x :: ∃α*β*. σ1).P[[e :: ∃α*β*. σ2]]

Figure 3. Type annotation propagation

    F[[x]]Γ     = x
    F[[Λα.e]]Γ  = F[[e]]Γ
    F[[e σ]]Γ   = F[[e]]Γ
    F[[λ(x : σ).e]]Γ
      = λ(x :: σ).(F[[e]](Γ,x:σ) :: σ2)   iff Γ |-F e : σ2 ∧ σ2 ∈ Q
      = λ(x :: σ).F[[e]](Γ,x:σ)           otherwise
    F[[e1 e2]]Γ
      = F[[e1]]Γ (F[[e2]]Γ :: σ2)         iff Γ |-F e2 : σ2 ∧ σ2 ∈ Q
      = F[[e1]]Γ F[[e2]]Γ                 otherwise

Figure 4. System F to HMF translation

    unify :: (σ1, σ2) → S
      where σ1 and σ2 are in normal form
    unify(α, α) =
      return []
    unify(α, σ) or unify(σ, α) =
      fail if (α ∈ ftv (σ))     (‘occurs’ check)
      return [α := σ]
    unify(c σ1...σn , c σ1'...σn') =
      let S1 = []
      let Si+1 = unify(Si σi, Si σi') o Si for i ∈ 1...n
      return Sn+1
    unify(∀α. σ1, ∀β. σ2) =
      assume c is a fresh (skolem) constant
      let S = unify([α := c]σ1, [β := c]σ2 )
      fail if (c ∈ con(codom(S)))    (‘escape’ check)
      return S

Figure 5. Unification

    subsume :: (σ1, σ2) → S
      where σ1 and σ2 are in normal form
    subsume(∀α*. ρ1 , ∀β*. ρ2) =
      assume β* are fresh, and c* are fresh (skolem) constants
      let S = unify([α* := c*]ρ1 , ρ2)
      fail if not (c* /∩ con(codom(S − β*)))    (‘escape’ check)
      return (S − β*)

Figure 6. Subsumption

            β* /∩ ftv(∀α*.σ)
    GEN-INST-----------------------------------------------
            ∀α*.σ ⊑ ∀β*.[α* := σ*]σ ~> λ(e:∀α.σ).Λβ.e σ

    infer :: (Γ, e) → (θ, σ)
    infer (Γ, x) =
      return ([], Γ(x))
    infer (Γ, let x = e1 in e2) =
      let (θ1 , σ1) = infer(Γ, e1)
      let (θ2 , σ2) = infer((θ1 Γ, x : σ1), e2)
      return (θ2 o θ1 , σ2)
    infer (Γ, λx .e) =
      assume α and β are fresh
      let (θ, ∀β. ρ) = infer ((Γ, x : α), e)
      return (θ, generalize(θΓ, θ(α → ρ)))
    infer (Γ, λ(x :: ∃α*. σ).e) =
      assume α* and β* are fresh
      let (θ, ∀β*. ρ) = infer ((Γ, x : σ), e)
      return (θ, generalize(θΓ, θ(σ → ρ)))
    infer (Γ, e1 e2) =
      assume α* are fresh
      let (θ0 , ∀α*. ρ) = infer (Γ, e1)
      let (θ1 , σ1 → σ)  = funmatch(ρ)
      let (θ2 , σ2)      = infer (θ1 Γ, e2)
      let (Θ3 , θ3)      = split(subsume(θ2 σ1 , σ2))
      let θ4             = θ3 o θ2 o θ1
      fail if not (dom(Θ3) 6 ∩ ftv (θ4 Γ))
      return (θ4 , generalize(θ4 Γ, Θ3 θ4 σ))


Figure 7. Type inference for Plain HMF

    funmatch(σ1 → σ2) =
      return ([], σ1 → σ2)
    funmatch(α) =
      assume β1 and β2 are fresh
      return ([α := β1 → β2], β1 → β2)
    generalize(Γ, σ) =
      let α* = ftv (σ) − ftv (Γ)
      return ∀α*.σ
    split(S) =
      let θ1 = [α := σ | (α := σ) ∈ S ∧ σ ∈ T ]
      let lΘ1 = [α := σ | (α := σ) ∉ S ∧ σ ∉ T ]
      return (lΘ1, θ1)

Figure 8. Helper functions


Figure 9. System F transformation function for generic instantiation.

            x : σ ∈ Γ
    VAR     -----------
            Γ |- x : σ

            Γ |- e : σ ~> e    α ∉ ftv(Γ)
    GEN     -------------------------------
            Γ |- e : ∀α.σ ~> Λα. e

            Γ |- e : σ1 ~> e    σ1 ⊑ σ2 ~> f
    INST    --------------------------------
            Γ |- e : σ2 ~> f e

            Γ, x : τ |- e : ρ ~> e
    FUN     ---------------------------
            Γ |- λx.e : τ → ρ ~> λx.e

            Γ, x : τ |- e : ρ ~> e
    FUN-ANN ---------------------------
            Γ |- λx.e : τ → ρ ~> λ(x:τ).e

            Γ |- e1 : σ2 → σ ~> e1   Γ |- e2 : σ2 ~> e2
            minimal([[σ2 → σ]])
    APP     ----------------------------------------------
            Γ |- e1 e2 : σ ~> e1 e2

            Γ |- e1 : σ1 ~> e1    Γ, x : σ1 |- e2 : σ2 ~> e2
            mostgen(σ1)
    LET     ---------------------------------------------
            Γ |- let x = e1 in e2 : σ2　~> (λ(x:σ1).e2) e1

Figure10. Type directed translation to System F.


                x : σ ∈ Γ
    VAR_F       ------------
                Γ |-F x : σ

                Γ, x : σ1 |-F e : σ2
    FUN_F       ------------------------------------
                Γ |-F λ(x : σ1).e : σ1 → σ2

                Γ |-F e1 : σ2 → σ    Γ |-F e2 : σ2
    APP_F       --------------------------------------
                Γ |-F e1 e2 : σ

                Γ |-F e : σ    α ∉ ftv(Γ)
    TYPE-FUN_F  ------------------------------------
                Γ |-F Λα.e : ∀α.σ

Figure11. System F type rules


                x : σ ∈ Γ
    VAR_s       ---------------------
                Γ |-s x : σ

                Γ |-s e1 : σ1    Γ, x : σ1 |-s e2 : σ2
                mostgen(σ1)
    LET_s       -------------------------------------------
                Γ |-s let x = e1 in e2 : σ2

                Γ, x : τ |-s e : σ    σ ⊑ ρ    α* /∩ ftv(Γ)
    FUN_s       --------------------------------------------
                Γ |-s λx .e : ∀α*. τ → ρ

                Γ, x : σ1 |-s e : σ    σ ⊑ ρ    α /∩ ftv(Γ)
    FUN-ANN_s   --------------------------------------------
                Γ |-s λ(x :: σ1).e : ∀α*. σ1 → ρ

                Γ |-s e1 : σ1    Γ |-s e2 : σ2
                σ1 ⊑ σ3 → σ    σ2 ⊑ σ3    α* /∩ ftv(Γ)
                minimal ([[σ3 → σ]])
    APP_s       -----------------------------------------
                Γ |-s e1 e2 : ∀α*.σ

Figure 12. Syntax directed type rules

                x : σ ∈ Γ
    VAR_€s      ---------------------
                Γ |-€s x : gen€(Γ,σ)

                Γ |-true_s e1 : σ1    Γ, x : σ1 |-€s e2 : σ2
                mostgen(σ1)
    LET_€s      -------------------------------------------
                Γ |-€s let x = e1 in e2 : σ2

                Γ, x : τ |-false_s e : ρ
    FUN_€s      --------------------------------------------
                Γ |-s λx .e : gen€(Γ,τ → ρ)

                Γ, x : σ |-false_s e : ρ
    FUN-ANN_€s  --------------------------------------------
                Γ |-€s λ(x :: σ).e : gen€(Γ,σ → ρ)

                Γ |-false_s e1 : σ3    Γ |-σ3∊Q_s e2 : σ2
                σ1 ⊑ σ3    minimal([[σ3 → σ]])
    APP_€s      -----------------------------------------
                Γ |-€s e1 e2 : gen€(Γ,σ)


    gen_true(Γ, σ)       = ∀α*. σ where α* = ftv (σ) − ftv (Γ)
    gen_false(Γ, ∀α*. ρ) = [α* := σ*]ρ

Figure 13. Alternative syntax directed type rules that reduce the number of generalizations and instantiations.
