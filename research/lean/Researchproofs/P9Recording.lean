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


/-! ### The two regimes from one recorded series -/

variable [DecidableEq ι]

/-- The identity recording map: every offence recorded in its own category. -/
def idMap : ι → ι → ℝ := fun i j => if i = j then 1 else 0

theorem idMap_nonneg (i j : ι) : 0 ≤ idMap (ι := ι) i j := by
  unfold idMap; split_ifs <;> norm_num

theorem idMap_colsum (j : ι) : ∑ i, idMap (ι := ι) i j = 1 := by
  unfold idMap
  rw [Finset.sum_eq_single j]
  · simp
  · intro i _ hij; simp [hij]
  · intro h; exact absurd (Finset.mem_univ j) h

theorem recorded_idMap (c : ι → ℝ) (i : ι) : recorded (idMap (ι := ι)) c i = c i := by
  unfold recorded idMap
  rw [Finset.sum_eq_single i]
  · simp
  · intro j _ hji; simp [Ne.symm hji]
  · intro h; exact absurd (Finset.mem_univ i) h

theorem recorded_smul (M : ι → ι → ℝ) (c : ι → ℝ) (t : ℝ) (i : ι) :
    recorded (fun i j => t * M i j) c i = t * recorded M c i := by
  unfold recorded; rw [Finset.mul_sum]; apply Finset.sum_congr rfl; intro j _; ring

/-- Two regimes, one series: a non-negative recorded vector `r` is produced both by pure
re-classification of true counts `c = r` (identity map, column-stochastic) and by cuffing a share
`ε` of larger true counts `c' = r / (1 - ε)` (the map `(1 - ε) I`, strictly column-substochastic).
With a positive total the two true totals differ, so the recorded series alone cannot say which
regime produced it. -/
theorem regimes_not_identified (r : ι → ℝ) (hr : ∀ i, 0 ≤ r i) (ε : ℝ) (hε0 : 0 < ε) (hε1 : ε < 1) :
    ∃ (M M' : ι → ι → ℝ) (c c' : ι → ℝ),
      ColStochastic M ∧ ColSubstochastic M' ∧ (∀ j, ∑ i, M' i j < 1) ∧
      (∀ i, recorded M c i = r i) ∧ (∀ i, recorded M' c' i = r i) ∧
      (∀ j, 0 ≤ c j) ∧ (∀ j, 0 ≤ c' j) ∧
      (0 < ∑ j, r j → ∑ j, c j < ∑ j, c' j) := by
  have h1ε : 0 < 1 - ε := by linarith
  refine ⟨idMap, fun i j => (1 - ε) * idMap i j, r, fun j => r j / (1 - ε), ?_, ?_, ?_, ?_, ?_, hr, ?_, ?_⟩
  · exact ⟨idMap_nonneg, idMap_colsum⟩
  · refine ⟨fun i j => mul_nonneg h1ε.le (idMap_nonneg i j), fun j => ?_⟩
    rw [← Finset.mul_sum, idMap_colsum, mul_one]; linarith
  · intro j; rw [← Finset.mul_sum, idMap_colsum, mul_one]; linarith
  · intro i; exact recorded_idMap r i
  · intro i; rw [recorded_smul, recorded_idMap]; field_simp
  · intro j; exact div_nonneg (hr j) h1ε.le
  · intro hpos
    rw [← Finset.sum_div, lt_div_iff₀ h1ε]
    nlinarith [mul_pos hε0 hpos]

/-- With a second source that fixes the true total, the regimes separate: re-classification
reproduces the total exactly, while cuffing a positive share of a category with positive count
records strictly less. -/
theorem second_source_separates (M : ι → ι → ℝ) (c : ι → ℝ) (hc : ∀ j, 0 ≤ c j) :
    (ColStochastic M → ∑ i, recorded M c i = ∑ j, c j) ∧
    (ColSubstochastic M → (∃ j, ∑ i, M i j < 1 ∧ 0 < c j) → ∑ i, recorded M c i < ∑ j, c j) := by
  refine ⟨fun h => total_invariant_of_colStochastic M c h, fun hM ⟨j₀, hj₀, hcj⟩ => ?_⟩
  rw [total_recorded]
  apply Finset.sum_lt_sum
  · intro j _
    have := hM.2 j; have := hc j; nlinarith
  · exact ⟨j₀, Finset.mem_univ _, by nlinarith⟩

end Research.P9
