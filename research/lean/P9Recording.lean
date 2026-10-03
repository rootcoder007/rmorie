/-
  Researchproofs/P9Recording.lean

  P9 (new): crime recording as a linear map.

  A police service turns a vector of true offences c (one entry per
  category) into a recorded vector r = M c, where M_ij is the share of
  true category-j offences that end up recorded as category i
  (Eterno, Verma & Silverman, How Countries Count Crime, ch. 14–15).

  * total_invariant_of_colStochastic: if every column of M sums to one
    (every offence is recorded somewhere: pure re-classification, e.g.
    robbery recorded as theft), the recorded total equals the true
    total. Downgrading is invisible in totals.

  * total_le_of_colSubstochastic: if columns sum to at most one (some
    offences are "cuffed" out of the notifiable set), the recorded total
    is at most the true total, with the loss equal to the cuffed mass.

  * reclassification_moves_ratio: under re-classification of a share q
    of category j into category i, the total is unchanged while the
    recorded category ratio r_i / r_j moves from c_i / c_j to
    (c_i + q c_j) / ((1 − q) c_j). Category ratios, not totals, carry the
    signal.

  * detection_rate_rises: moving a share q of a class with detection rate
    d (0 ≤ d < 1) into a disposal detected with probability one raises the
    aggregate detection rate by exactly q n (1 − d) / N > 0 with the
    recorded total N fixed (Patrick, ch. 14). An aggregate clearance rate
    is therefore not a performance signal without the disposal split.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P9

variable {ι : Type*} [Fintype ι]

/-- Recorded counts from true counts through the recording matrix `M`. -/
def recorded (M : ι → ι → ℝ) (c : ι → ℝ) (i : ι) : ℝ := ∑ j, M i j * c j

/-- Column-stochastic: every true offence is recorded in exactly one category. -/
def ColStochastic (M : ι → ι → ℝ) : Prop := (∀ i j, 0 ≤ M i j) ∧ ∀ j, ∑ i, M i j = 1

/-- Column-substochastic: some offences may be dropped. -/
def ColSubstochastic (M : ι → ι → ℝ) : Prop := (∀ i j, 0 ≤ M i j) ∧ ∀ j, ∑ i, M i j ≤ 1

theorem total_recorded (M : ι → ι → ℝ) (c : ι → ℝ) :
    ∑ i, recorded M c i = ∑ j, (∑ i, M i j) * c j := by
  unfold recorded
  rw [sum_comm]
  apply sum_congr rfl; intro j _
  rw [sum_mul]

/-- Pure re-classification leaves the total unchanged. -/
theorem total_invariant_of_colStochastic (M : ι → ι → ℝ) (c : ι → ℝ) (hM : ColStochastic M) :
    ∑ i, recorded M c i = ∑ j, c j := by
  rw [total_recorded]
  apply sum_congr rfl; intro j _
  rw [hM.2 j, one_mul]

/-- Cuffing can only lower the total, by exactly the dropped mass. -/
theorem total_le_of_colSubstochastic (M : ι → ι → ℝ) (c : ι → ℝ) (hM : ColSubstochastic M)
    (hc : ∀ j, 0 ≤ c j) :
    ∑ i, recorded M c i = ∑ j, c j - ∑ j, (1 - ∑ i, M i j) * c j ∧
    ∑ i, recorded M c i ≤ ∑ j, c j := by
  rw [total_recorded]
  constructor
  · rw [← sum_sub_distrib]; apply sum_congr rfl; intro j _; ring
  · apply sum_le_sum; intro j _
    have := hM.2 j
    have := hc j
    nlinarith

/-- Re-classifying a share `q` of category `j` into category `i`: total fixed, ratio moved. -/
theorem reclassification_moves_ratio (ci cj q : ℝ) (hci : 0 < ci) (hcj : 0 < cj) (hq0 : 0 ≤ q) (hq1 : q < 1) :
    (ci + q * cj) + (1 - q) * cj = ci + cj ∧
    (ci + q * cj) / ((1 - q) * cj) = ci / cj + q / (1 - q) * (1 + ci / cj) ∧
    ci / cj ≤ (ci + q * cj) / ((1 - q) * cj) := by
  have h1q : 0 < 1 - q := by linarith
  refine ⟨by ring, ?_, ?_⟩
  · field_simp
    ring
  · rw [div_le_div_iff₀ hcj (by positivity)]
    nlinarith [mul_pos hci hcj, mul_nonneg hq0 hcj.le, mul_nonneg hq0 (mul_pos hci hcj).le, mul_nonneg hq0 (mul_pos hcj hcj).le]

/-- Detection-rate arithmetic: moving share `q` of a class of size `n` with detection rate `d`
into a disposal detected with probability one raises the aggregate rate by `q n (1 - d) / N`. -/
theorem detection_rate_rises (D N n q d : ℝ) (hN : 0 < N) (hn : 0 < n) (hq : 0 < q) (hd0 : 0 ≤ d) (hd1 : d < 1) :
    (D + q * n * (1 - d)) / N - D / N = q * n * (1 - d) / N ∧ 0 < q * n * (1 - d) / N := by
  constructor
  · field_simp
    ring
  · have : 0 < 1 - d := by linarith
    positivity

end Research.P9
