/-
  Researchproofs/P4Rate.lean

  P4, continued: how fast can the naive loop run away? At most like a
  harmonic sum.

  The one-step gain is x(1−x)(λA−λB)/(c + λA x + λB(1−x)) ≤ (λA−λB)/4 / c_n,
  and the total count grows by at least λmin = min(λA, λB) per step, so

    x_N − x_0 ≤ ((λA − λB)/4) · Σ_{k<N} 1 / (c₀ + k λmin).

  The right-hand side grows like (λA−λB)/(4 λmin) · log N. This is the
  theorem behind the empirical observation that a 5% rate gap from a 1/99
  start moves the share by half a percentage point in two million steps:
  the destination is 1, the speed is logarithmic.
-/
import Mathlib
import Researchproofs.P4Feedback
import Researchproofs.P4Limit

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Filter Topology

namespace Research.P4

/-- The total count grows by at least `min λA λB` per step. -/
theorem naiveTotal_succ_ge (P : Params) (n : ℕ) :
    naiveTotal P n + min P.lamA P.lamB ≤ naiveTotal P (n + 1) := by
  obtain ⟨hA, hB⟩ := naiveIter_pos P n
  have hs0 := naiveShare_pos P n
  have hs1 := naiveShare_lt_one P n
  unfold naiveShare at hs0 hs1
  have hmA : min P.lamA P.lamB ≤ P.lamA := min_le_left _ _
  have hmB : min P.lamA P.lamB ≤ P.lamB := min_le_right _ _
  unfold naiveTotal
  simp only [naiveIter, naiveStep]
  have e1 : min P.lamA P.lamB * share (naiveIter P n).1 (naiveIter P n).2
      ≤ P.lamA * share (naiveIter P n).1 (naiveIter P n).2 :=
    mul_le_mul_of_nonneg_right hmA hs0.le
  have e2 : min P.lamA P.lamB * (1 - share (naiveIter P n).1 (naiveIter P n).2)
      ≤ P.lamB * (1 - share (naiveIter P n).1 (naiveIter P n).2) :=
    mul_le_mul_of_nonneg_right hmB (by linarith)
  nlinarith

theorem naiveTotal_ge (P : Params) (n : ℕ) :
    naiveTotal P 0 + n * min P.lamA P.lamB ≤ naiveTotal P n := by
  induction n with
  | zero => simp
  | succ n ih =>
    have := naiveTotal_succ_ge P n
    push_cast
    linarith

/-- `x(1-x) ≤ 1/4` on the reals. -/
theorem mul_one_sub_le_quarter (x : ℝ) : x * (1 - x) ≤ 1 / 4 := by
  nlinarith [sq_nonneg (x - 1 / 2)]

/-- One-step gain is at most `(λA − λB)/4` over the current total count. -/
theorem naiveShare_gain_le (P : Params) (hlam : P.lamB < P.lamA) (n : ℕ) :
    naiveShare P (n + 1) - naiveShare P n
      ≤ (P.lamA - P.lamB) / 4 / (naiveTotal P 0 + n * min P.lamA P.lamB) := by
  obtain ⟨hA, hB⟩ := naiveIter_pos P n
  have hd := naive_step_drift P (naiveIter P n).1 (naiveIter P n).2 hA hB
  have hstep : naiveShare P (n + 1) - naiveShare P n
      = share (naiveIter P n).1 (naiveIter P n).2 * (1 - share (naiveIter P n).1 (naiveIter P n).2)
        * (P.lamA - P.lamB)
        / ((naiveIter P n).1 + (naiveIter P n).2 + P.lamA * share (naiveIter P n).1 (naiveIter P n).2
          + P.lamB * (1 - share (naiveIter P n).1 (naiveIter P n).2)) := by
    unfold naiveShare; simp only [naiveIter]; exact hd
  rw [hstep]
  have hδ : 0 < P.lamA - P.lamB := by linarith
  have hmin : 0 < min P.lamA P.lamB := lt_min P.lamA_pos P.lamB_pos
  have hc0 : 0 < naiveTotal P 0 := by
    change 0 < P.cA0 + P.cB0
    have := P.cA0_pos; have := P.cB0_pos; linarith
  have hlow : 0 < naiveTotal P 0 + n * min P.lamA P.lamB := by positivity
  have hden_pos := naive_den_pos P _ _ hA hB
  -- the actual denominator is at least the total count, which is at least the lower bound
  have hge : naiveTotal P 0 + n * min P.lamA P.lamB ≤ (naiveIter P n).1 + (naiveIter P n).2 :=
    naiveTotal_ge P n
  have hs0 : 0 < share (naiveIter P n).1 (naiveIter P n).2 := by unfold share; positivity
  have hs1 := naiveShare_lt_one P n
  unfold naiveShare at hs1
  have hden_ge : naiveTotal P 0 + n * min P.lamA P.lamB
      ≤ (naiveIter P n).1 + (naiveIter P n).2
        + P.lamA * share (naiveIter P n).1 (naiveIter P n).2
        + P.lamB * (1 - share (naiveIter P n).1 (naiveIter P n).2) := by
    have := P.lamA_pos; have := P.lamB_pos
    have h1 : 0 ≤ P.lamA * share (naiveIter P n).1 (naiveIter P n).2 := by positivity
    have h2 : 0 ≤ P.lamB * (1 - share (naiveIter P n).1 (naiveIter P n).2) :=
      mul_nonneg (by linarith) (by linarith)
    linarith
  have hnum : share (naiveIter P n).1 (naiveIter P n).2
      * (1 - share (naiveIter P n).1 (naiveIter P n).2) * (P.lamA - P.lamB)
      ≤ (P.lamA - P.lamB) / 4 := by
    have := mul_one_sub_le_quarter (share (naiveIter P n).1 (naiveIter P n).2)
    have := mul_le_mul_of_nonneg_right this hδ.le
    linarith
  have hnum_nonneg : 0 ≤ share (naiveIter P n).1 (naiveIter P n).2
      * (1 - share (naiveIter P n).1 (naiveIter P n).2) * (P.lamA - P.lamB) :=
    mul_nonneg (mul_nonneg hs0.le (by linarith)) hδ.le
  calc share (naiveIter P n).1 (naiveIter P n).2
          * (1 - share (naiveIter P n).1 (naiveIter P n).2) * (P.lamA - P.lamB)
          / ((naiveIter P n).1 + (naiveIter P n).2 + P.lamA * share (naiveIter P n).1 (naiveIter P n).2
            + P.lamB * (1 - share (naiveIter P n).1 (naiveIter P n).2))
      ≤ share (naiveIter P n).1 (naiveIter P n).2
          * (1 - share (naiveIter P n).1 (naiveIter P n).2) * (P.lamA - P.lamB)
          / (naiveTotal P 0 + n * min P.lamA P.lamB) :=
        div_le_div_of_nonneg_left hnum_nonneg hlow hden_ge
    _ ≤ (P.lamA - P.lamB) / 4 / (naiveTotal P 0 + n * min P.lamA P.lamB) :=
        div_le_div_of_nonneg_right hnum hlow.le

/-- Rate bound: after `N` steps the share has moved at most a harmonic sum. -/
theorem naiveShare_rate_bound (P : Params) (hlam : P.lamB < P.lamA) (N : ℕ) :
    naiveShare P N - naiveShare P 0
      ≤ (P.lamA - P.lamB) / 4 * ∑ k ∈ Finset.range N, 1 / (naiveTotal P 0 + k * min P.lamA P.lamB) := by
  induction N with
  | zero => simp
  | succ N ih =>
    have h := naiveShare_gain_le P hlam N
    rw [Finset.sum_range_succ, mul_add]
    have : (P.lamA - P.lamB) / 4 / (naiveTotal P 0 + N * min P.lamA P.lamB)
        = (P.lamA - P.lamB) / 4 * (1 / (naiveTotal P 0 + N * min P.lamA P.lamB)) := by ring
    linarith

end Research.P4
