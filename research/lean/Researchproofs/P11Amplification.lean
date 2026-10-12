/-
  Researchproofs/P11Amplification.lean

  P11, continued: amplification of a sensitivity analysis — one Γ is a curve
  of (Λ, Δ) pairs (library scan B18; Rosenbaum & Silber; Handbook ch. 2 §2.3.4
  eq. 2.12).

  Γ = (ΛΔ + 1)/(Λ + Δ) with Λ, Δ ≥ 1 (Λ the odds of treatment, Δ the odds of
  a positive outcome, attributable to the unobserved covariate). The map is
  symmetric, at least one, equals Λ when Δ → ∞ in the sense Γ ≤ min(Λ, Δ)
  (`amplification_le_min`), is non-decreasing in each argument
  (`amplification_mono`), and every Γ ≥ 1 is reached: the level set is a
  curve, parametrised by Λ > Γ through Δ = (ΛΓ − 1)/(Λ − Γ)
  (`level_curve`), so a sensitivity statement at Γ is the same statement as
  any (Λ, Δ) on that curve.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P11.Amplification

def gamma (Λ Δ : ℝ) : ℝ := (Λ * Δ + 1) / (Λ + Δ)

theorem symmetric (Λ Δ : ℝ) : gamma Λ Δ = gamma Δ Λ := by unfold gamma; ring_nf

theorem ge_one (Λ Δ : ℝ) (hΛ : 1 ≤ Λ) (hΔ : 1 ≤ Δ) : 1 ≤ gamma Λ Δ := by
  unfold gamma
  rw [le_div_iff₀ (by linarith)]
  nlinarith

theorem le_min (Λ Δ : ℝ) (hΛ : 1 ≤ Λ) (hΔ : 1 ≤ Δ) : gamma Λ Δ ≤ Λ ∧ gamma Λ Δ ≤ Δ := by
  unfold gamma
  constructor <;> (rw [div_le_iff₀ (by linarith)]; nlinarith)

theorem mono_left (Λ Λ' Δ : ℝ) (hΛ : 1 ≤ Λ) (hΛ' : Λ ≤ Λ') (hΔ : 1 ≤ Δ) : gamma Λ Δ ≤ gamma Λ' Δ := by
  unfold gamma
  rw [div_le_div_iff₀ (by linarith) (by linarith)]
  have h : 0 ≤ (Λ' - Λ) * (Δ ^ 2 - 1) := mul_nonneg (by linarith) (by nlinarith)
  nlinarith [h]

/-- Every `Γ ≥ 1` is reached, and for each `Λ > Γ` by exactly one `Δ ≥ 1`. -/
theorem level_curve (Γ Λ : ℝ) (hΓ : 1 ≤ Γ) (hΛ : Γ < Λ) :
    1 ≤ (Λ * Γ - 1) / (Λ - Γ) ∧ gamma Λ ((Λ * Γ - 1) / (Λ - Γ)) = Γ := by
  have hpos : 0 < Λ - Γ := by linarith
  constructor
  · rw [le_div_iff₀ hpos]; nlinarith
  · unfold gamma
    have hne : Λ - Γ ≠ 0 := hpos.ne'
    have hden : Λ + (Λ * Γ - 1) / (Λ - Γ) ≠ 0 := by
      have : 0 < (Λ * Γ - 1) / (Λ - Γ) := by apply div_pos _ hpos; nlinarith
      linarith
    rw [div_eq_iff hden]
    field_simp
    ring

end Research.P11.Amplification
