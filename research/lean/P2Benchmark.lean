/-
  Researchproofs/P2Benchmark.lean

  P2, continued: two identities that police-oversight reports get wrong,
  and one they get right only by accident.

  * offset_shift: in a log-link rate model μ_g = E_g · exp(β_g) (the
    Poisson or negative-binomial regression with the exposure E_g as an
    offset), replacing the exposure by κ·E_g while fitting the same
    expected counts shifts the group coefficient by exactly −log κ. A
    disparity ratio exp(β_g − β_ref) is therefore identified only up to
    the ratio of the exposure errors of the two groups
    (disparity_ratio_shift). This is the mechanism behind the 2022 review
    of the Toronto Police Service use-of-force analysis, where an
    offset correction moved a 30–58× disparity to 4–5×.

  * benchmark_product: with population, contacts and force counts per
    group, the disparity of force per resident against a reference
    group is the PRODUCT of the disparity of contact per resident and the
    disparity of force per contact. The TPS methodological report calls
    the two "additive"; benchmark_not_additive is a numerical witness
    that they are not (3 × 2 ≠ 3 + 2).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P2

/-- The exposure-offset shift: same expected count, exposure scaled by `κ`, coefficient moves by `−log κ`. -/
theorem offset_shift (E κ μ : ℝ) (hE : 0 < E) (hκ : 0 < κ) (hμ : 0 < μ) :
    Real.log (μ / (κ * E)) = Real.log (μ / E) - Real.log κ := by
  rw [Real.log_div hμ.ne' (by positivity), Real.log_div hμ.ne' hE.ne', Real.log_mul hκ.ne' hE.ne']
  ring

/-- A disparity ratio between two groups is identified only up to the ratio of their exposure errors. -/
theorem disparity_ratio_shift (E₁ E₂ κ₁ κ₂ μ₁ μ₂ : ℝ) (hE₁ : 0 < E₁) (hE₂ : 0 < E₂)
    (hκ₁ : 0 < κ₁) (hκ₂ : 0 < κ₂) (hμ₁ : 0 < μ₁) (hμ₂ : 0 < μ₂) :
    (μ₁ / (κ₁ * E₁)) / (μ₂ / (κ₂ * E₂)) = ((μ₁ / E₁) / (μ₂ / E₂)) * (κ₂ / κ₁) := by
  field_simp

/-- Force-per-resident disparity is the product of contact-per-resident and force-per-contact disparities. -/
theorem benchmark_product (pop_g con_g frc_g pop_r con_r frc_r : ℝ)
    (h1 : 0 < pop_g) (h2 : 0 < con_g) (h3 : 0 < frc_g) (h4 : 0 < pop_r) (h5 : 0 < con_r) (h6 : 0 < frc_r) :
    (frc_g / pop_g) / (frc_r / pop_r)
      = ((con_g / pop_g) / (con_r / pop_r)) * ((frc_g / con_g) / (frc_r / con_r)) := by
  field_simp

/-- Numerical witness that the two stage disparities do not add: contact disparity 3 and force-given-contact disparity 2 give resident disparity 6, not 5. -/
theorem benchmark_not_additive :
    let pop_g : ℝ := 100; let con_g : ℝ := 30; let frc_g : ℝ := 12
    let pop_r : ℝ := 100; let con_r : ℝ := 10; let frc_r : ℝ := 2
    (con_g / pop_g) / (con_r / pop_r) = 3 ∧ (frc_g / con_g) / (frc_r / con_r) = 2 ∧
    (frc_g / pop_g) / (frc_r / pop_r) = 6 ∧ (frc_g / pop_g) / (frc_r / pop_r) ≠ 3 + 2 := by
  refine ⟨by norm_num, by norm_num, by norm_num, by norm_num⟩

end Research.P2
