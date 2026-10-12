/-
  Researchproofs/P5Attenuation.lean

  P5, continued: measurement error attenuates a coefficient toward zero only in
  the one-regressor case (library scan A4; Weisburd & Britt ch. 2).

  Population regression coefficients are ratios of covariances. With true
  regressor x of variance s11 and classical error u of variance su2
  (independent of x and of the disturbance), the regression of y on the
  observed x* = x + u has slope β s11 / (s11 + su2): attenuation
  (`single_attenuation`). With two regressors (x1 observed with error, x2
  without) the population coefficients are
      b1 = β1 (s11 s22 − s12²) / D*,     b2 = β2 + β1 su2 s12 / D*,
  D* = (s11 + su2) s22 − s12² (`two_regressor_coeffs`): the error-free
  regressor's coefficient is biased by β1 su2 s12 / D*, zero iff β1 = 0,
  su2 = 0 or s12 = 0 (`error_free_biased_iff`), and its sign is the sign of
  β1 s12, away from zero as often as toward it (`bias_sign`). So "any
  measurement error attenuates" is a one-regressor fact.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P5.Attenuation

/-- The one-regressor population slope of `y = β x + e` on `x* = x + u`. -/
def slopeStar (β s11 su2 : ℝ) : ℝ := β * s11 / (s11 + su2)

theorem single_attenuation (β s11 su2 : ℝ) (h11 : 0 < s11) (hu : 0 ≤ su2) :
    slopeStar β s11 su2 = β * (s11 / (s11 + su2)) ∧ 0 < s11 / (s11 + su2) ∧ s11 / (s11 + su2) ≤ 1 := by
  refine ⟨by unfold slopeStar; ring, div_pos h11 (by linarith), ?_⟩
  rw [div_le_one (by linarith)]; linarith

/-- The two-regressor population moments: `s11, s22, s12` of the true regressors, `su2` the
error variance on the first. -/
structure Moments where
  s11 : ℝ
  s22 : ℝ
  s12 : ℝ
  su2 : ℝ
  s11_pos : 0 < s11
  s22_pos : 0 < s22
  su2_nonneg : 0 ≤ su2
  pd : s12 ^ 2 < s11 * s22

/-- The determinant of the observed regressor covariance. -/
def dstar (m : Moments) : ℝ := (m.s11 + m.su2) * m.s22 - m.s12 ^ 2

theorem dstar_pos (m : Moments) : 0 < dstar m := by
  unfold dstar; nlinarith [m.pd, m.su2_nonneg, m.s22_pos]

/-- The population coefficients of `y` on `(x1*, x2)` when `y = β1 x1 + β2 x2 + e`: the
solution of the 2×2 normal equations. -/
def b1 (m : Moments) (β1 β2 : ℝ) : ℝ :=
  ((β1 * m.s11 + β2 * m.s12) * m.s22 - (β1 * m.s12 + β2 * m.s22) * m.s12) / dstar m
def b2 (m : Moments) (β1 β2 : ℝ) : ℝ :=
  ((m.s11 + m.su2) * (β1 * m.s12 + β2 * m.s22) - m.s12 * (β1 * m.s11 + β2 * m.s12)) / dstar m

/-- The coefficients solve the normal equations of the observed regressors. -/
theorem normal_equations (m : Moments) (β1 β2 : ℝ) :
    (m.s11 + m.su2) * b1 m β1 β2 + m.s12 * b2 m β1 β2 = β1 * m.s11 + β2 * m.s12 ∧
    m.s12 * b1 m β1 β2 + m.s22 * b2 m β1 β2 = β1 * m.s12 + β2 * m.s22 := by
  have hd := (dstar_pos m).ne'
  unfold b1 b2
  constructor <;> (field_simp; unfold dstar; ring)

theorem two_regressor_coeffs (m : Moments) (β1 β2 : ℝ) :
    b1 m β1 β2 = β1 * (m.s11 * m.s22 - m.s12 ^ 2) / dstar m ∧
    b2 m β1 β2 = β2 + β1 * m.su2 * m.s12 / dstar m := by
  have hd := (dstar_pos m).ne'
  unfold b1 b2
  constructor
  · congr 1; ring
  · field_simp; unfold dstar; ring

/-- The error-free regressor's coefficient is unbiased iff `β1 = 0`, `su2 = 0` or `s12 = 0`. -/
theorem error_free_biased_iff (m : Moments) (β1 β2 : ℝ) :
    b2 m β1 β2 = β2 ↔ β1 = 0 ∨ m.su2 = 0 ∨ m.s12 = 0 := by
  rw [(two_regressor_coeffs m β1 β2).2]
  have hd := (dstar_pos m).ne'
  constructor
  · intro h
    have : β1 * m.su2 * m.s12 = 0 := by
      have h' : β1 * m.su2 * m.s12 / dstar m = 0 := by linarith
      rcases div_eq_zero_iff.mp h' with h0 | h0
      · exact h0
      · exact absurd h0 hd
    rcases mul_eq_zero.mp this with h0 | h0
    · rcases mul_eq_zero.mp h0 with h1 | h1
      · exact Or.inl h1
      · exact Or.inr (Or.inl h1)
    · exact Or.inr (Or.inr h0)
  · rintro (h | h | h) <;> simp [h]

/-- The bias of the error-free coefficient has the sign of `β1 s12` when `su2 > 0`. -/
theorem bias_sign (m : Moments) (β1 β2 : ℝ) (hu : 0 < m.su2) (hpos : 0 < β1 * m.s12) :
    β2 < b2 m β1 β2 := by
  rw [(two_regressor_coeffs m β1 β2).2]
  have : 0 < β1 * m.su2 * m.s12 / dstar m := by
    apply div_pos _ (dstar_pos m)
    nlinarith
  linarith

end Research.P5.Attenuation
