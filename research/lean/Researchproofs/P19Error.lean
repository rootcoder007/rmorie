/-
  Researchproofs/P19Error.lean

  P19, continued: the sampling error of the shrinkage factor itself.

  P19Shrinkage gave B* = S_e / (S_e + S_θ). In practice S_e is a stated
  noise mass (for counts, the mean) while S_e + S_θ is the total mass
  S_y, which must be estimated from the same counts, say by Ŝ_y. The
  plug-in factor is B̂ = S_e / Ŝ_y.

    * bstar_from_total: B* = S_e / S_y where S_y = ∑ w (y − ȳ)², under the
      noise law (the cross term vanishes, so the total mass is the sum).
    * bhat_error: B̂ − B* = S_e (S_y − Ŝ_y) / (S_y Ŝ_y): the error of the
      factor is proportional to the error of the total mass.
    * bhat_error_bound: |B̂ − B*| ≤ B* |Ŝ_y − S_y| / Ŝ_y: a relative error
      ε in the estimated total mass moves the factor by at most B* ε / (1 − ε).
    * fall_error: the predicted fall B̂ (y − ȳ) is off by (B̂ − B*)(y − ȳ):
      the same relative error, scaled by the place's excess.
    * bhat_antitone: a larger estimated total mass gives a smaller factor.
-/
import Mathlib
import Researchproofs.P19Shrinkage

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P19Error

open Research.P19Shrinkage

variable {Ω : Type*} [Fintype Ω] (w θ e : Ω → ℝ)

/-- Total mass of the counts about their mean. -/
def Sy : ℝ := ∑ i, w i * ((θ i + e i) - mean w (fun j => θ j + e j)) ^ 2

theorem Sy_eq (h0 : ∑ i, w i * e i = 0) (h1 : ∑ i, w i * (θ i * e i) = 0) :
    Sy w θ e = Se w e + Sθ w θ := by
  unfold Sy Se Sθ
  rw [mean_y w θ e h0]
  have hpt : ∀ i, w i * ((θ i + e i) - mean w θ) ^ 2
      = w i * e i ^ 2 + w i * (θ i - mean w θ) ^ 2 + 2 * (w i * (e i * (θ i - mean w θ))) := fun i => by ring
  simp only [hpt, sum_add_distrib, ← mul_sum, cross_zero w θ e h0 h1, mul_zero, add_zero]

theorem bstar_from_total (h0 : ∑ i, w i * e i = 0) (h1 : ∑ i, w i * (θ i * e i) = 0) :
    Bstar w θ e = Se w e / Sy w θ e := by
  unfold Bstar; rw [Sy_eq w θ e h0 h1]

/-- The plug-in factor from an estimated total mass. -/
def Bhat (Se Syhat : ℝ) : ℝ := Se / Syhat

theorem bhat_error (Se Sy Syhat : ℝ) (hSy : Sy ≠ 0) (hSyhat : Syhat ≠ 0) :
    Bhat Se Syhat - Se / Sy = Se * (Sy - Syhat) / (Sy * Syhat) := by
  unfold Bhat
  field_simp
  try ring

theorem bhat_error_bound (Se Sy Syhat : ℝ) (hSe : 0 ≤ Se) (hSy : 0 < Sy) (hSyhat : 0 < Syhat) :
    |Bhat Se Syhat - Se / Sy| ≤ (Se / Sy) * |Syhat - Sy| / Syhat := by
  rw [bhat_error Se Sy Syhat hSy.ne' hSyhat.ne']
  rw [abs_div, abs_mul, abs_of_nonneg hSe, abs_of_pos (mul_pos hSy hSyhat), abs_sub_comm]
  rw [div_le_iff₀ (mul_pos hSy hSyhat)]
  have : Se / Sy * |Syhat - Sy| / Syhat * (Sy * Syhat) = Se * |Syhat - Sy| := by
    field_simp
  rw [this]

theorem fall_error (Se Sy Syhat y ybar : ℝ) :
    Bhat Se Syhat * (y - ybar) - (Se / Sy) * (y - ybar) = (Bhat Se Syhat - Se / Sy) * (y - ybar) := by ring

theorem bhat_antitone (Se S₁ S₂ : ℝ) (hSe : 0 ≤ Se) (h₁ : 0 < S₁) (h : S₁ ≤ S₂) :
    Bhat Se S₂ ≤ Bhat Se S₁ := by
  unfold Bhat
  exact div_le_div_of_nonneg_left hSe h₁ h

end Research.P19Error
