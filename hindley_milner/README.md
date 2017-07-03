# Hindley/Milner の型型システムいろいろ


## 1. Hindley/Milner 

  英語のWikipediaをみると以下のようなBNF や 規則があります。
  これはプログラムで実装することではなく数学的議論をしやすいように書かれていると言ってよいでしょう。

  Syntax

  Expressions

    e ::= x                 variable
       |  e1 e2             application
       |  λx.e              abstraction
       |  let x = e1 in e2  let expression

  Types

    mono τ ::= α           variable
            |  D τ1...τn   application

    poly σ ::= τ
            |  ∀α.σ       quantifier

  Free Type Variables

    free(α)         = { α }
    free(D τ1...τn) = ∪{i=1 to n} free(τi)
    free(∀α.σ)     = free(σ) - { α }

  Example 1

    let bar [∀α.∀β.α->(β->α)] = λx.
      let foo [∀β.β->α] = λy.x
      in foo
    in bar

  Context and typing

  Syntax

    Context Γ ::= € (empty)
               |  Γ, x : σ
    Typing    ::= Γ ⱶ e : σ

  Free Type Variables

    free(Γ) = ∪{x:σ∈Γ} free(α)

  Specialization Rule

    τ' = [αi := τi]τ    βi ∉ free(∀α1...∀αn.τ)
    --------------------------------------------
        ∀α1...∀αn.τ ⊑ ∀β1...∀βm.τ'

  Deductive system

  The Syntax of Rules

    Predicate ::= σ ⊑ σ'
               |  α ∉ free(Γ)
               |  x : α ∈ Γ
    Judgment  ::= Typing
    Premise   ::= Judgement | Predicate
    Conclusion::= Judgement

                  Premise ...
    Rule      ::= -------------- [Name]
                  Conclusion

  Typing rules

  Declarative Rule System

    x : σ ∈ Γ
    -----------                        [Var]
    Γ ⱶD x : σ

    Γ ⱶD e0 : τ -> τ'   Γ ⱶD e1 : τ
    ---------------------------------- [App]
    Γ ⱶD e0 e1 : τ'

    Γ, x : τ ⱶD e : τ'
    --------------------               [Abs]
    Γ ⱶD λx.e : τ -> τ'

    Γ ⱶD e0 : σ   Γ, x : σ ⱶD e1 : τ
    ---------------------------------- [Let]
    Γ ⱶD let x = e0 in e1 : τ


    Γ ⱶD e : σ'   σ' ⊑ σ
    ----------------------             [Inst]
    Γ ⱶD e : σ

    Γ ⱶD e : σ   α ∉ free(Γ)
    -------------------------          [Gen]
    Γ ⱶD e : ∀α.σ

  Syntax-directed rule system

  Syntactical Rule System

    x : σ ∈ Γ   σ ⊑ τ
    ------------------                 [Var]
    Γ ⱶS x : τ

    Γ ⱶS e0 : τ -> τ'   Γ ⱶS e1 : τ
    ---------------------------------- [App]
    Γ ⱶS e0 e1 : τ'

    Γ, x : τ ⱶS e : τ'
    --------------------               [Abs]
    Γ ⱶS λx.e : τ -> τ'

    Γ ⱶS e0 : τ   Γ, x : Γ*(τ) ⱶS e1 : τ'
    -------------------------------------- [Let]
    Γ ⱶS let x = e0 in e1 : τ'

  Generalization

    Γ*(τ) = ∀α^.Γ    α^ = free(τ) - free(Γ)

  Unify

    unify(ta,tb):
      ta = find(ta)
      tb = find(tb)
      if both ta,tb are terms of the form D p1..pn with identical D,n then
        unify(ta[i],tb[i]) for each corresponding ith parameter
      else
      if at least one of ta,tb is a type variable then
        union(ta,tb)
      else
        error 'types do not match'

  Algorithm W

    x : σ ∈ Γ   τ = inst(σ)
    ------------------------               [Var]
    Γ ⱶW x : τ

    Γ ⱶW e0 : τ0   Γ ⱶW e1 : τ
    τ' = newvar   unify(τ0, τ1 -> τ')
    ----------------------------------     [App]
    Γ ⱶW e0 e1 : τ'

    τ = newvar   Γ, x : τ ⱶW e : τ'
    --------------------------------       [Abs]
    Γ ⱶW λx.e : τ -> τ'

    Γ ⱶW e0 : τ   Γ, x : Γ*(τ) ⱶW e1 : τ'
    -------------------------------------- [Let]
    Γ ⱶW let x = e0 in e1 : τ'
