/-
  Researchproofs/P3Spatial.lean

  P3/P7, continued: spatial-lag omission is bias, spatial-error omission is
  inefficiency, and uncorrected local tests reject somewhere with probability
  tending to one (library scan A25; Kikuchi pp. 3-4, 83-95).

  Two-unit models with the swap weight matrix, errors drawn from a finite
  equiprobable law with mean zero and unit variance.
    * `lag_solution`: y = ρWy + u has the solution y₁ = (u₁ + ρu₂)/(1 − ρ²),
      y₂ = (u₂ + ρu₁)/(1 − ρ²).
    * `lag_regressor_correlated`: the average of (Wy)₁ u₁ over the four draws
      (±1, ±1) is ρ/(1 − ρ²) ≠ 0: the spatial-lag regressor is correlated with
      the error, so least squares on it is inconsistent.
    * `ols_unbiased_correlated_errors`: with perfectly correlated errors
      (the draws (1, 1) and (−1, −1)) the through-origin slope averages to
      the true β: a spatial error leaves least squares unbiased.
    * `gls_more_precise`: with x = (1, 2) and error covariance [[1, ½], [½, 1]]
      the slope variance is 7/25 under least squares and 1/4 under
      generalised least squares: unbiased but inefficient.
    * `fwer_tendsto_one`: m independent local tests at level α ∈ (0, 1] reject
      somewhere with probability 1 − (1 − α)^m → 1.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Filter Topology

noncomputable section

namespace Research.P3.Spatial

def y1 (ρ u1 u2 : ℝ) : ℝ := (u1 + ρ * u2) / (1 - ρ ^ 2)
def y2 (ρ u1 u2 : ℝ) : ℝ := (u2 + ρ * u1) / (1 - ρ ^ 2)

theorem lag_solution (ρ u1 u2 : ℝ) (hρ : ρ ^ 2 ≠ 1) :
    y1 ρ u1 u2 = ρ * y2 ρ u1 u2 + u1 ∧ y2 ρ u1 u2 = ρ * y1 ρ u1 u2 + u2 := by
  unfold y1 y2
  have : 1 - ρ ^ 2 ≠ 0 := sub_ne_zero.mpr (Ne.symm hρ)
  constructor <;> (field_simp; ring)

/-- Average of `(Wy)₁ u₁ = y₂ u₁` over the four equiprobable draws `(±1, ±1)`. -/
theorem lag_regressor_correlated (ρ : ℝ) (hρ : ρ ^ 2 ≠ 1) (hρ0 : ρ ≠ 0) :
    (y2 ρ 1 1 * 1 + y2 ρ 1 (-1) * 1 + y2 ρ (-1) 1 * (-1) + y2 ρ (-1) (-1) * (-1)) / 4 = ρ / (1 - ρ ^ 2)
      ∧ ρ / (1 - ρ ^ 2) ≠ 0 := by
  have : 1 - ρ ^ 2 ≠ 0 := sub_ne_zero.mpr (Ne.symm hρ)
  constructor
  · unfold y2; field_simp; ring
  · exact div_ne_zero hρ0 this

/-- Through-origin slope on `x = (1, 2)` with `y = βx + u`, averaged over the two perfectly
correlated draws `(1, 1)` and `(−1, −1)`. -/
def slope (β u1 u2 : ℝ) : ℝ := (1 * (β * 1 + u1) + 2 * (β * 2 + u2)) / (1 ^ 2 + 2 ^ 2)

theorem ols_unbiased_correlated_errors (β : ℝ) : (slope β 1 1 + slope β (-1) (-1)) / 2 = β := by
  unfold slope; ring

/-- Slope variances for `x = (1, 2)`, `Σ = [[1, 1/2], [1/2, 1]]`:
OLS `xᵀΣx/(xᵀx)²`, GLS `1/(xᵀΣ⁻¹x)` with `Σ⁻¹ = (4/3)[[1, −1/2], [−1/2, 1]]`. -/
theorem gls_more_precise :
    let var_ols : ℝ := (1 * 1 * 1 + 2 * (1 * (1 / 2) * 2) + 2 * 1 * 2) / (1 ^ 2 + 2 ^ 2) ^ 2
    let var_gls : ℝ := 1 / ((4 / 3) * (1 * 1 * 1 + 2 * (1 * (-1 / 2) * 2) + 2 * 1 * 2))
    var_gls < var_ols ∧ var_ols = 7 / 25 ∧ var_gls = 1 / 4 := by
  norm_num

theorem fwer_tendsto_one (α : ℝ) (hα : 0 < α) (hα1 : α ≤ 1) :
    Tendsto (fun m : ℕ => 1 - (1 - α) ^ m) atTop (𝓝 1) := by
  have h : Tendsto (fun m : ℕ => (1 - α) ^ m) atTop (𝓝 0) :=
    tendsto_pow_atTop_nhds_zero_of_lt_one (by linarith) (by linarith)
  have := (tendsto_const_nhds (x := (1 : ℝ))).sub h
  simpa using this

end Research.P3.Spatial
