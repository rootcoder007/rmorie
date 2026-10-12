/-
  Researchproofs/P3Exposure.lean

  P3, continued: exposure mappings under interference (library scan B21;
  Wager, Causal Inference notes, ch. 11-12).

    * `exposure_prob_tendsto_zero`: under Bernoulli(p) assignment the
      probability that all d neighbours are treated is p^d → 0 as the degree
      grows, so positivity fails for high-degree units;
    * `ht_weight_unbounded`: the Horvitz-Thompson weight 1/p^d → ∞.
    * `permutation_valid`: for an assignment drawn uniformly from a finite set
      A and any statistic T that is a function of the assignment alone (which,
      under interference, is what the sharp null on focal units has to give:
      an imputable statistic), the permutation p-value
      p(z) = #{z' : T(z') ≥ T(z)}/|A| satisfies #{z : p(z) ≤ α} ≤ α|A|: the test
      is exact at every level. The validity is a property of imputability, not
      of the statistic's form.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Filter Topology Finset

noncomputable section

namespace Research.P3.Exposure

theorem exposure_prob_tendsto_zero (p : ℝ) (hp : 0 ≤ p) (hp1 : p < 1) :
    Tendsto (fun d : ℕ => p ^ d) atTop (𝓝 0) :=
  tendsto_pow_atTop_nhds_zero_of_lt_one hp hp1

theorem ht_weight_unbounded (p : ℝ) (hp : 0 < p) (hp1 : p < 1) :
    Tendsto (fun d : ℕ => (p ^ d)⁻¹) atTop atTop := by
  have h : Tendsto (fun d : ℕ => (p⁻¹) ^ d) atTop atTop :=
    tendsto_pow_atTop_atTop_of_one_lt (one_lt_inv₀ hp |>.mpr hp1)
  refine h.congr ?_
  intro d; rw [inv_pow]

variable {Z : Type*} [DecidableEq Z]

/-- The permutation p-value of assignment `z` over the design `A`. -/
def pval (A : Finset Z) (T : Z → ℝ) (z : Z) : ℝ :=
  ((A.filter fun z' => T z ≤ T z').card : ℝ) / A.card

theorem permutation_valid (A : Finset Z) (T : Z → ℝ) (α : ℝ) (hA : A.Nonempty) (hα : 0 ≤ α) :
    ((A.filter fun z => pval A T z ≤ α).card : ℝ) ≤ α * A.card := by
  set S := A.filter fun z => pval A T z ≤ α with hS
  rcases S.eq_empty_or_nonempty with h | h
  · rw [h]; simp; positivity
  · obtain ⟨z0, hz0, hmin⟩ := S.exists_min_image T h
    have hp : pval A T z0 ≤ α := (mem_filter.mp hz0).2
    have hsub : S ⊆ A.filter fun z' => T z0 ≤ T z' := by
      intro z hz
      rw [mem_filter]
      exact ⟨(mem_filter.mp hz).1, hmin z hz⟩
    have hcard : (S.card : ℝ) ≤ (A.filter fun z' => T z0 ≤ T z').card := by
      exact_mod_cast card_le_card hsub
    unfold pval at hp
    have hApos : (0 : ℝ) < A.card := by exact_mod_cast card_pos.mpr hA
    rw [div_le_iff₀ hApos] at hp
    linarith

end Research.P3.Exposure
