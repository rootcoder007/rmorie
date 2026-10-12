/-
  Researchproofs/P10Moments.lean

  P10, continued: the second-moment core of Bartlett's non-identification
  (Bartlett 1964; Diggle 2013 §4.7): contagion and heterogeneity can
  produce the same first and second moments of the counts, which is all a
  K-function or a dispersion index sees.

  The witness is a pair of count laws on {0, 1, 2, 3}:
      p = (1/2, 0, 1/2, 0)        (two cells, half empty: "clustering")
      q = (1/3, 1/2, 0, 1/6)      (a heterogeneous mixture)
  with the same mean 1 and the same variance 1 (dispersion index 1 in
  both), yet P(0) = 1/2 against 1/3.

    * witness_same_mean, witness_same_second_moment: equal first two
      moments.
    * witness_distinct: the laws differ (at zero).
    * second_moments_do_not_identify: hence no function of the mean and
      the variance — the dispersion index, the K-function's constant
      summary on a cell, any over-dispersion test — separates them; the
      K-function of a stationary process is its second-moment measure,
      so a near-repeat "branching ratio" read from K alone is a
      second-moment statement, not a mechanism.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P10.Moments

/-- The two count laws on {0,1,2,3}. -/
def p : Fin 4 → ℝ := ![1/2, 0, 1/2, 0]
def q : Fin 4 → ℝ := ![1/3, 1/2, 0, 1/6]

def moment (r : Fin 4 → ℝ) (k : ℕ) : ℝ := ∑ i : Fin 4, r i * ((i : ℕ) : ℝ) ^ k

theorem witness_laws : (∑ i, p i = 1) ∧ (∑ i, q i = 1) ∧ (∀ i, 0 ≤ p i) ∧ (∀ i, 0 ≤ q i) := by
  refine ⟨?_, ?_, ?_, ?_⟩
  · simp [p, Fin.sum_univ_four]; norm_num
  · simp [q, Fin.sum_univ_four]; norm_num
  · intro i; fin_cases i <;> simp [p]
  · intro i; fin_cases i <;> simp [q]

theorem witness_same_mean : moment p 1 = moment q 1 := by
  simp [moment, p, q, Fin.sum_univ_four]; norm_num

theorem witness_same_second_moment : moment p 2 = moment q 2 := by
  simp [moment, p, q, Fin.sum_univ_four]; norm_num

theorem witness_distinct : p ≠ q := by
  intro h
  have := congrFun h 0
  norm_num [p, q] at this

/-- Any statistic that is a function of the mean and the variance takes the same value on both laws. -/
theorem second_moments_do_not_identify (T : ℝ → ℝ → ℝ) :
    T (moment p 1) (moment p 2 - moment p 1 ^ 2) = T (moment q 1) (moment q 2 - moment q 1 ^ 2) := by
  rw [witness_same_mean, witness_same_second_moment]

/-- The dispersion index of both laws is one: neither looks over-dispersed. -/
theorem witness_dispersion_one :
    (moment p 2 - moment p 1 ^ 2) / moment p 1 = 1 ∧ (moment q 2 - moment q 1 ^ 2) / moment q 1 = 1 := by
  constructor <;> simp [moment, p, q, Fin.sum_univ_four] <;> norm_num

end Research.P10.Moments
