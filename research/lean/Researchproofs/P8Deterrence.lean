/-
  Researchproofs/P8Deterrence.lean

  P8. Deterrence: certainty, severity and celerity are never varied alone.

  Take the linear structural response  y = β₀ + β_p·p + β_s·s + β_c·c
  over a finite design of observed sanction regimes (p, s, c). The three
  partial effects are the coefficients. They are identified exactly when
  no other coefficient vector reproduces the same fitted values on the
  design, i.e. when the design columns (1, p, s, c) are linearly
  independent.

  Proved:
    * constant_dimension_not_identified: if one dimension (say celerity)
      takes the same value in every regime, there are two different
      coefficient vectors with identical fitted values on every regime, so
      that dimension's effect is not identified by any estimator, however
      large the sample. Explicit witness, no rank machinery.
    * identified_iff_injective: the coefficient vector is identified
      exactly when the map from coefficients to fitted values is
      injective (restated to make the design condition explicit).

  This is the formal reason why a study that varies certainty at a fixed
  severity and celerity cannot report "the effect of certainty holding
  the others fixed" as if the others had been varied: β_c is free.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P8

/-- A sanction regime: certainty, severity, celerity. -/
structure Regime where
  p : ℝ
  s : ℝ
  c : ℝ

/-- Coefficients of the linear response. -/
structure Coef where
  b0 : ℝ
  bp : ℝ
  bs : ℝ
  bc : ℝ

/-- Fitted response of a coefficient vector on a regime. -/
def fit (β : Coef) (r : Regime) : ℝ := β.b0 + β.bp * r.p + β.bs * r.s + β.bc * r.c

/-- Identification of the coefficients by a design `D`: equal fitted values on
every regime of the design force equal coefficients. -/
def Identified {ι : Type*} (D : ι → Regime) : Prop :=
  ∀ β β' : Coef, (∀ i, fit β (D i) = fit β' (D i)) → β = β'

/-- If celerity is constant across the design, the coefficients are not identified:
shifting `bc` and compensating in the intercept leaves every fitted value unchanged. -/
theorem constant_dimension_not_identified {ι : Type*} (D : ι → Regime) (c0 : ℝ)
    (hconst : ∀ i, (D i).c = c0) : ¬ Identified D := by
  intro hid
  let β : Coef := ⟨0, 0, 0, 0⟩
  let β' : Coef := ⟨-c0, 0, 0, 1⟩
  have hsame : ∀ i, fit β (D i) = fit β' (D i) := by
    intro i
    simp only [fit, β, β', hconst i]
    ring
  have := hid β β' hsame
  have hbc : β.bc = β'.bc := by rw [this]
  simp [β, β'] at hbc

/-- Identification is exactly injectivity of the coefficient-to-fit map. -/
theorem identified_iff_injective {ι : Type*} (D : ι → Regime) :
    Identified D ↔ Function.Injective (fun β : Coef => fun i => fit β (D i)) := by
  constructor
  · intro h β β' hβ
    exact h β β' (fun i => congrFun hβ i)
  · intro h β β' hβ
    exact h (funext hβ)

/-- A design that does vary all three dimensions independently identifies the
coefficients: four regimes forming a "corner" design. -/
theorem corner_design_identified :
    Identified (fun i : Fin 4 =>
      match i with
      | 0 => ⟨0, 0, 0⟩
      | 1 => ⟨1, 0, 0⟩
      | 2 => ⟨0, 1, 0⟩
      | 3 => ⟨0, 0, 1⟩) := by
  intro β β' h
  have h0 := h 0
  have h1 := h 1
  have h2 := h 2
  have h3 := h 3
  simp only [fit] at h0 h1 h2 h3
  simp at h0 h1 h2 h3
  cases β; cases β'
  simp only [Coef.mk.injEq]
  refine ⟨?_, ?_, ?_, ?_⟩ <;> linarith

end Research.P8
