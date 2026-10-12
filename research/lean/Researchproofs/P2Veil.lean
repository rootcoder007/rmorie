/-
  Researchproofs/P2Veil.lean

  P2, continued: the veil-of-darkness contrast as a formal sign test
  (Grogger & Ridgeway 2006).

  Two groups are on the road with latent exposures E₁, E₂ and stop
  propensities r₁, r₂ that an officer applies when race is NOT visible
  (darkness). In daylight race is visible and each group's propensity is
  multiplied by (1 + δ_g) with δ_g ≥ 0: the visible-race bias. The data
  are the four stop counts, daylight D_g = E_g r_g (1 + δ_g) and darkness
  N_g = E_g r_g, in a window where the same people drive under both
  conditions (the inter-twilight window), so E and r are the same in
  both.

    * odds_ratio_identifies_bias: (D₁/D₂)/(N₁/N₂) = (1 + δ₁)/(1 + δ₂):
      the exposures and the dark propensities cancel, and the relative
      visible-race bias is point-identified — not only its sign.
    * share_sign: the daylight share of group 1 exceeds the darkness
      share iff δ₁ > δ₂, and equals it iff δ₁ = δ₂ (the sign test).
    * exposure_shift_confounds: if daylight exposure is instead
      κ_g E_g, the odds ratio is (κ₁/κ₂)(1 + δ₁)/(1 + δ₂): a differential
      change in who drives by light condition is observationally
      equivalent to bias, and the two are not separable from the counts.
    * bias_not_absolute: δ₁ and δ₂ are not identified separately: scaling
      both (1 + δ) by the same factor leaves every count ratio unchanged.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

namespace Research.P2.Veil

/-- Daylight count. -/
def D (E r δ : ℝ) : ℝ := E * r * (1 + δ)
/-- Darkness count. -/
def N (E r : ℝ) : ℝ := E * r

theorem odds_ratio_identifies_bias (E₁ E₂ r₁ r₂ δ₁ δ₂ : ℝ)
    (hE₁ : 0 < E₁) (hE₂ : 0 < E₂) (hr₁ : 0 < r₁) (hr₂ : 0 < r₂) (hδ₂ : 0 ≤ δ₂) :
    (D E₁ r₁ δ₁ / D E₂ r₂ δ₂) / (N E₁ r₁ / N E₂ r₂) = (1 + δ₁) / (1 + δ₂) := by
  unfold D N
  have h2 : (1 + δ₂) ≠ 0 := by linarith
  field_simp

/-- The daylight share of group 1 against its darkness share. -/
theorem share_sign (E₁ E₂ r₁ r₂ δ₁ δ₂ : ℝ)
    (hE₁ : 0 < E₁) (hE₂ : 0 < E₂) (hr₁ : 0 < r₁) (hr₂ : 0 < r₂) (hδ₁ : 0 ≤ δ₁) (hδ₂ : 0 ≤ δ₂) :
    (δ₂ < δ₁ ↔ N E₁ r₁ / (N E₁ r₁ + N E₂ r₂) < D E₁ r₁ δ₁ / (D E₁ r₁ δ₁ + D E₂ r₂ δ₂)) ∧
    (δ₁ = δ₂ ↔ N E₁ r₁ / (N E₁ r₁ + N E₂ r₂) = D E₁ r₁ δ₁ / (D E₁ r₁ δ₁ + D E₂ r₂ δ₂)) := by
  unfold D N
  have a₁ : 0 < E₁ * r₁ := by positivity
  have a₂ : 0 < E₂ * r₂ := by positivity
  have b₁ : 0 < E₁ * r₁ * (1 + δ₁) := by positivity
  have b₂ : 0 < E₂ * r₂ * (1 + δ₂) := by positivity
  have hden : 0 < E₁ * r₁ + E₂ * r₂ := by positivity
  have hden' : 0 < E₁ * r₁ * (1 + δ₁) + E₂ * r₂ * (1 + δ₂) := by positivity
  constructor
  · rw [div_lt_div_iff₀ hden hden']
    constructor
    · intro h
      have : 0 < E₁ * r₁ * (E₂ * r₂) * (δ₁ - δ₂) := by
        apply mul_pos (by positivity); linarith
      nlinarith
    · intro h
      by_contra hc
      push_neg at hc
      have : 0 ≤ E₁ * r₁ * (E₂ * r₂) * (δ₂ - δ₁) := by
        apply mul_nonneg (by positivity); linarith
      nlinarith
  · rw [div_eq_div_iff hden.ne' hden'.ne']
    constructor
    · intro h; subst h; ring
    · intro h
      have : E₁ * r₁ * (E₂ * r₂) * (δ₁ - δ₂) = 0 := by linarith
      have hne : E₁ * r₁ * (E₂ * r₂) ≠ 0 := by positivity
      have := (mul_eq_zero.mp this).resolve_left hne
      linarith

/-- A differential shift of exposure by light condition (κ₁, κ₂) is observationally a bias. -/
theorem exposure_shift_confounds (E₁ E₂ r₁ r₂ δ₁ δ₂ κ₁ κ₂ : ℝ)
    (hE₁ : 0 < E₁) (hE₂ : 0 < E₂) (hr₁ : 0 < r₁) (hr₂ : 0 < r₂) (hδ₂ : 0 ≤ δ₂) (hκ₂ : 0 < κ₂) :
    (D (κ₁ * E₁) r₁ δ₁ / D (κ₂ * E₂) r₂ δ₂) / (N E₁ r₁ / N E₂ r₂) = (κ₁ / κ₂) * ((1 + δ₁) / (1 + δ₂)) := by
  unfold D N
  have h2 : (1 + δ₂) ≠ 0 := by linarith
  field_simp

/-- Only the ratio of visible-race factors is identified: a common scale of both is invisible. -/
theorem bias_not_absolute (E₁ E₂ r₁ r₂ δ₁ δ₂ c : ℝ) (hc : 0 < c) (hE₁ : 0 < E₁) (hE₂ : 0 < E₂)
    (hr₁ : 0 < r₁) (hr₂ : 0 < r₂) :
    D E₁ r₁ (c * (1 + δ₁) - 1) / D E₂ r₂ (c * (1 + δ₂) - 1) = D E₁ r₁ δ₁ / D E₂ r₂ δ₂ ∧
    D E₁ r₁ (c * (1 + δ₁) - 1) / (D E₁ r₁ (c * (1 + δ₁) - 1) + D E₂ r₂ (c * (1 + δ₂) - 1)) =
      D E₁ r₁ δ₁ / (D E₁ r₁ δ₁ + D E₂ r₂ δ₂) := by
  unfold D
  constructor
  · have h1 : (1 + (c * (1 + δ₁) - 1)) = c * (1 + δ₁) := by ring
    have h2 : (1 + (c * (1 + δ₂) - 1)) = c * (1 + δ₂) := by ring
    rw [h1, h2]
    rcases eq_or_ne (1 + δ₂) 0 with h | h
    · rw [h]; simp
    · field_simp
  · have h1 : (1 + (c * (1 + δ₁) - 1)) = c * (1 + δ₁) := by ring
    have h2 : (1 + (c * (1 + δ₂) - 1)) = c * (1 + δ₂) := by ring
    rw [h1, h2]
    have : E₁ * r₁ * (c * (1 + δ₁)) + E₂ * r₂ * (c * (1 + δ₂)) = c * (E₁ * r₁ * (1 + δ₁) + E₂ * r₂ * (1 + δ₂)) := by ring
    rw [this]
    rcases eq_or_ne (E₁ * r₁ * (1 + δ₁) + E₂ * r₂ * (1 + δ₂)) 0 with h | h
    · rw [h]; simp
    · field_simp

end Research.P2.Veil
