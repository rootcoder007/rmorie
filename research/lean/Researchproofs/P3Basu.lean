/-
  Researchproofs/P3Basu.lean

  P3, continued: Basu's elephant — the Horvitz–Thompson estimator is unbiased
  with a variance that explodes (library scan B8; Lohr §6.7).

  Under Poisson sampling (independent inclusion with probabilities π_i) the
  HT total Σ t_i I_i/π_i has mean Σ t_i (`ht_mean`) and variance
  Σ t_i² (1 − π_i)/π_i (`ht_variance`). One unit with a large value t_e and
  a small inclusion probability contributes t_e² (1 − π_e)/π_e, which exceeds
  any bound B once π_e is small enough (`elephant_unbounded`): the design is
  valid and useless. The remaining units' contribution is bounded
  independently of π_e (`ratio_bounded`), which is what the circus owner's
  ratio estimate keeps.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P3.Basu

variable {N : Type*} [Fintype N]

/-- Mean of the HT total under independent inclusion: `E[I_i] = π_i`. -/
def htMean (t π : N → ℝ) : ℝ := ∑ i, t i / π i * π i

theorem ht_mean (t π : N → ℝ) (hπ : ∀ i, π i ≠ 0) : htMean t π = ∑ i, t i := by
  unfold htMean; apply sum_congr rfl; intro i _; exact div_mul_cancel₀ (t i) (hπ i)

/-- Variance of the HT total under independent inclusion: `Σ (t_i/π_i)² π_i (1 − π_i)`. -/
def htVar (t π : N → ℝ) : ℝ := ∑ i, (t i / π i) ^ 2 * (π i * (1 - π i))

theorem ht_variance (t π : N → ℝ) (hπ : ∀ i, π i ≠ 0) :
    htVar t π = ∑ i, t i ^ 2 * (1 - π i) / π i := by
  unfold htVar; apply sum_congr rfl; intro i _
  have h := hπ i
  field_simp
  try ring

/-- The elephant's term alone exceeds any bound once its inclusion probability is small enough. -/
theorem elephant_unbounded (te B : ℝ) (hte : 0 < te) (hB : 0 < B) :
    ∃ πe : ℝ, 0 < πe ∧ πe < 1 ∧ B < te ^ 2 * (1 - πe) / πe := by
  have hD : (0 : ℝ) < 2 * (B + te ^ 2) := by positivity
  have hp : 0 < te ^ 2 / (2 * (B + te ^ 2)) := by positivity
  refine ⟨te ^ 2 / (2 * (B + te ^ 2)), hp, ?_, ?_⟩
  · rw [div_lt_one hD]; nlinarith
  · rw [lt_div_iff₀ hp]
    have h1 : (B + te ^ 2) * (te ^ 2 / (2 * (B + te ^ 2))) = te ^ 2 / 2 := by
      field_simp; try ring
    nlinarith [h1, pow_pos hte 2]

/-- The non-elephant terms are bounded by a quantity that does not involve `π_e` at all. -/
theorem ratio_bounded [DecidableEq N] (t π : N → ℝ) (e : N) (hπ : ∀ i, 0 < π i) :
    ∑ i ∈ univ.erase e, t i ^ 2 * (1 - π i) / π i ≤ ∑ i ∈ univ.erase e, t i ^ 2 / π i := by
  apply sum_le_sum; intro i _
  rw [mul_div_assoc]
  have h1 : (1 - π i) / π i ≤ 1 / π i := by
    apply div_le_div_of_nonneg_right _ (hπ i).le; linarith [hπ i]
  calc t i ^ 2 * ((1 - π i) / π i) ≤ t i ^ 2 * (1 / π i) := mul_le_mul_of_nonneg_left h1 (sq_nonneg _)
    _ = t i ^ 2 / π i := by ring

end Research.P3.Basu
