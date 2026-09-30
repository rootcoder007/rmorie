/-
  Researchproofs/P7Distinct.lean

  P7, continued: how many distinct places (or victims) a preferential
  process produces.

  Under Pólya / Dirichlet-process allocation with concentration M > 0,
  the i-th event lands on a new place with probability M / (M + i − 1),
  independently of the earlier indicators (Ghosal & van der Vaart,
  Fundamentals of Nonparametric Bayesian Inference, Prop. 4.8). The
  expected number of distinct places after n events is therefore

      E K_n = Σ_{i=1}^{n} M / (M + i − 1) = M · S_n,   S_n = Σ_{i<n} 1/(M+i),

  and S_n is squeezed between logarithms:

      log((M+n)/M)  ≤  S_n  ≤  1/M + log((M+n−1)/M)        (n ≥ 1).

  So the count of distinct places grows like M log n — logarithmically,
  not as a power of n. Observed growth of distinct addresses in a repeat
  victimisation series that is polynomial rejects the Pólya
  (concentration-only) story in favour of a Pitman–Yor-type mechanism.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P7

/-- `S M n = Σ_{i<n} 1/(M+i)`. -/
def S (M : ℝ) (n : ℕ) : ℝ := ∑ i ∈ range n, 1 / (M + i)

/-- Expected number of distinct places after `n` events under concentration `M`. -/
def expectedDistinct (M : ℝ) (n : ℕ) : ℝ := M * S M n

theorem S_succ (M : ℝ) (n : ℕ) : S M (n + 1) = S M n + 1 / (M + n) := by
  unfold S; rw [sum_range_succ]

/-- Lower bound: `log((M+n)/M) ≤ S M n`. -/
theorem log_le_S (M : ℝ) (hM : 0 < M) (n : ℕ) : Real.log ((M + n) / M) ≤ S M n := by
  induction n with
  | zero => simp [S]
  | succ n ih =>
    rw [S_succ]
    have hpos : 0 < M + n := by positivity
    have step : Real.log ((M + (n + 1 : ℕ)) / M) - Real.log ((M + n) / M) ≤ 1 / (M + n) := by
      rw [← Real.log_div (by positivity) (by positivity)]
      have e : (M + (n + 1 : ℕ)) / M / ((M + n) / M) = (M + n + 1) / (M + n) := by
        push_cast; field_simp; ring
      rw [e]
      have := Real.log_le_sub_one_of_pos (show 0 < (M + n + 1) / (M + n) by positivity)
      have e2 : (M + n + 1) / (M + n) - 1 = 1 / (M + n) := by field_simp; ring
      linarith
    linarith

/-- Upper bound for `n ≥ 1`: `S M n ≤ 1/M + log((M+n−1)/M)`. -/
theorem S_le_log (M : ℝ) (hM : 0 < M) (n : ℕ) (hn : 1 ≤ n) :
    S M n ≤ 1 / M + Real.log ((M + n - 1) / M) := by
  induction n, hn using Nat.le_induction with
  | base => simp [S]
  | succ n hn ih =>
    rw [S_succ]
    have hpos : 0 < M + n - 1 := by
      have : (1 : ℝ) ≤ n := by exact_mod_cast hn
      linarith
    have hpos2 : 0 < M + (n + 1 : ℕ) - 1 := by push_cast; linarith
    have step : 1 / (M + n) ≤ Real.log ((M + (n + 1 : ℕ) - 1) / M) - Real.log ((M + n - 1) / M) := by
      rw [← Real.log_div (by positivity) (by positivity)]
      have e : (M + (n + 1 : ℕ) - 1) / M / ((M + n - 1) / M) = (M + n) / (M + n - 1) := by
        push_cast; field_simp; ring
      rw [e]
      have := Real.one_sub_inv_le_log_of_pos (show 0 < (M + n) / (M + n - 1) by positivity)
      have e2 : 1 - ((M + n) / (M + n - 1))⁻¹ = 1 / (M + n) := by
        rw [inv_div]; field_simp; ring
      linarith
    linarith

/-- The expected number of distinct places is squeezed between `M log((M+n)/M)` and `1 + M log((M+n−1)/M)`. -/
theorem expectedDistinct_bounds (M : ℝ) (hM : 0 < M) (n : ℕ) (hn : 1 ≤ n) :
    M * Real.log ((M + n) / M) ≤ expectedDistinct M n ∧
    expectedDistinct M n ≤ 1 + M * Real.log ((M + n - 1) / M) := by
  unfold expectedDistinct
  constructor
  · exact mul_le_mul_of_nonneg_left (log_le_S M hM n) hM.le
  · have := mul_le_mul_of_nonneg_left (S_le_log M hM n hn) hM.le
    have e : M * (1 / M + Real.log ((M + n - 1) / M)) = 1 + M * Real.log ((M + n - 1) / M) := by
      field_simp
    linarith

end Research.P7
