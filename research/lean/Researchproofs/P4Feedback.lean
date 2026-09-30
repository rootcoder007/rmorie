/-
  Researchproofs/P4Feedback.lean

  P4. Predictive-policing feedback loops, two-region mean-field model.

  Two regions A and B with true crime rates λA, λB > 0. The department
  keeps cumulative discovered-crime counts (cA, cB) and sends its patrol
  to A with share x = cA / (cA + cB). Crimes are discovered only where the
  patrol is: expected discoveries are λA·x in A and λB·(1 − x) in B.

  Naive update: the discovered counts are added as they are.
  Corrected update (Ensign et al. 2018): each discovery is discounted by
  the share of patrol that produced it, so the increment is the true rate.

  Proved here (mean-field, i.e. the recursion on expected counts):
    * naive_step_drift: one-step change of the share is
        x(1−x)(λA − λB) / (c + λA x + λB (1 − x)),
      so the sign of the drift is the sign of λA − λB, whatever the ratio.
    * naive_share_increasing: with λA > λB and 0 < x < 1 the share to A
      strictly increases at every step (the runaway direction).
    * corrected_share_closed_form and corrected_share_tendsto: the
      corrected recursion has the explicit solution
        x_n = (cA₀ + n λA) / (c₀ + n (λA + λB))
      and converges to λA / (λA + λB), the true-rate proportion, for every
      initial condition.

  What is NOT proved here: the stochastic (urn) statement that the naive
  share converges almost surely to 1; that is the next target and needs
  Mathlib's martingale convergence. Lean checks the implication, never
  whether a department's discovery model looks like this.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Filter Topology

namespace Research.P4

/-- Two-region parameters: true rates and the initial discovered counts. -/
structure Params where
  lamA : ℝ
  lamB : ℝ
  cA0 : ℝ
  cB0 : ℝ
  lamA_pos : 0 < lamA
  lamB_pos : 0 < lamB
  cA0_pos : 0 < cA0
  cB0_pos : 0 < cB0

/-! ### Naive update: one step on the counts -/

/-- Share of patrol sent to A given counts. -/
def share (cA cB : ℝ) : ℝ := cA / (cA + cB)

/-- Naive step: counts grow by the expected discoveries under the current share. -/
def naiveStep (P : Params) (cA cB : ℝ) : ℝ × ℝ :=
  let x := share cA cB
  (cA + P.lamA * x, cB + P.lamB * (1 - x))

/-- `1 - share cA cB` is the share sent to B. -/
theorem one_sub_share (cA cB : ℝ) (hc : cA + cB ≠ 0) :
    1 - share cA cB = cB / (cA + cB) := by
  unfold share; field_simp; ring

/-- The naive denominator is positive. -/
theorem naive_den_pos (P : Params) (cA cB : ℝ) (hA : 0 < cA) (hB : 0 < cB) :
    0 < cA + cB + P.lamA * share cA cB + P.lamB * (1 - share cA cB) := by
  have hc : 0 < cA + cB := by positivity
  rw [one_sub_share cA cB hc.ne']
  unfold share
  have := P.lamA_pos; have := P.lamB_pos
  positivity

/-- One-step drift of the share under the naive update. -/
theorem naive_step_drift (P : Params) (cA cB : ℝ) (hA : 0 < cA) (hB : 0 < cB) :
    share (naiveStep P cA cB).1 (naiveStep P cA cB).2 - share cA cB
      = share cA cB * (1 - share cA cB) * (P.lamA - P.lamB)
        / (cA + cB + P.lamA * share cA cB + P.lamB * (1 - share cA cB)) := by
  have hc : (cA + cB) ≠ 0 := by positivity
  have h1 : 1 - cA / (cA + cB) = cB / (cA + cB) := by field_simp; ring
  have hlA := P.lamA_pos
  have hlB := P.lamB_pos
  simp only [naiveStep, share]
  rw [h1]
  have hden' : cA + P.lamA * (cA / (cA + cB)) + (cB + P.lamB * (cB / (cA + cB))) ≠ 0 := by
    positivity
  have hden'' : cA + cB + P.lamA * (cA / (cA + cB)) + P.lamB * (cB / (cA + cB)) ≠ 0 := by
    positivity
  field_simp
  ring

/-- Runaway direction: with λA > λB the share to A strictly increases. -/
theorem naive_share_increasing (P : Params) (cA cB : ℝ) (hA : 0 < cA) (hB : 0 < cB)
    (hlam : P.lamB < P.lamA) :
    share cA cB < share (naiveStep P cA cB).1 (naiveStep P cA cB).2 := by
  have hd := naive_step_drift P cA cB hA hB
  have hc : 0 < cA + cB := by positivity
  have hx0 : 0 < share cA cB := by unfold share; positivity
  have hx1 : share cA cB < 1 := by
    unfold share; rw [div_lt_one hc]; linarith
  have hden : 0 < cA + cB + P.lamA * share cA cB + P.lamB * (1 - share cA cB) := by
    have := P.lamA_pos; have := P.lamB_pos
    have : 0 < 1 - share cA cB := by linarith
    positivity
  have hpos : 0 < share cA cB * (1 - share cA cB) * (P.lamA - P.lamB)
      / (cA + cB + P.lamA * share cA cB + P.lamB * (1 - share cA cB)) := by
    apply div_pos _ hden
    have : 0 < 1 - share cA cB := by linarith
    have : 0 < P.lamA - P.lamB := by linarith
    positivity
  linarith

/-! ### Corrected update: discoveries discounted by presence -/

/-- Corrected counts after `n` steps: each step adds the true rate. -/
def correctedCounts (P : Params) (n : ℕ) : ℝ × ℝ :=
  (P.cA0 + n * P.lamA, P.cB0 + n * P.lamB)

/-- Closed form of the corrected share. -/
theorem corrected_share_closed_form (P : Params) (n : ℕ) :
    share (correctedCounts P n).1 (correctedCounts P n).2
      = (P.cA0 + n * P.lamA) / (P.cA0 + P.cB0 + n * (P.lamA + P.lamB)) := by
  simp only [correctedCounts, share]
  congr 1; ring

/-- The corrected share converges to the true-rate proportion. -/
theorem corrected_share_tendsto (P : Params) :
    Tendsto (fun n : ℕ => share (correctedCounts P n).1 (correctedCounts P n).2)
      atTop (𝓝 (P.lamA / (P.lamA + P.lamB))) := by
  have hL : 0 < P.lamA + P.lamB := by have := P.lamA_pos; have := P.lamB_pos; linarith
  simp_rw [corrected_share_closed_form]
  -- (a + n·p) / (b + n·q) → p / q : divide numerator and denominator by n.
  have key : ∀ n : ℕ, 0 < n →
      (P.cA0 + n * P.lamA) / (P.cA0 + P.cB0 + n * (P.lamA + P.lamB))
        = (P.cA0 / n + P.lamA) / ((P.cA0 + P.cB0) / n + (P.lamA + P.lamB)) := by
    intro n hn
    have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
    field_simp
  have h1 : Tendsto (fun n : ℕ => P.cA0 / n + P.lamA) atTop (𝓝 (0 + P.lamA)) :=
    (tendsto_const_div_atTop_nhds_zero_nat P.cA0).add tendsto_const_nhds
  have h2 : Tendsto (fun n : ℕ => (P.cA0 + P.cB0) / n + (P.lamA + P.lamB)) atTop
      (𝓝 (0 + (P.lamA + P.lamB))) :=
    (tendsto_const_div_atTop_nhds_zero_nat (P.cA0 + P.cB0)).add tendsto_const_nhds
  have h3 := h1.div h2 (by simp only [zero_add]; exact hL.ne')
  simp only [zero_add] at h3
  refine h3.congr' ?_
  filter_upwards [eventually_gt_atTop 0] with n hn
  exact (key n hn).symm

end Research.P4
