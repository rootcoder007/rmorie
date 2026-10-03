/-
  Researchproofs/P11Bounds.lean

  P11 (new): sentencing effects as intervals (Manski, Identification for
  Prediction and Decision, §7.2; Manski & Nagin 1998).

  Each person receives one sentence z ∈ {a, b} and reveals only y(z).
  For a binary outcome (reconviction), the probability P[y(t) = 1] is
  identified only up to the interval
      [P(y = 1, z = t),  P(y = 1, z = t) + P(z ≠ t)],
  whose width is the share of people who did not receive t. The average
  treatment effect P[y(b)=1] − P[y(a)=1] therefore lies in an interval of
  width exactly P(z ≠ b) + P(z ≠ a) = 1 (two sentences, everybody gets
  one), and that interval always contains 0. Without assumptions on
  selection, observational sentencing data cannot even sign the effect.

  Proved on a finite population with weights (no measure theory):
    * outcome_bounds: the interval and its sharpness (attained by
      filling the unobserved arm with all-0 or all-1).
    * ate_width_one / ate_contains_zero: the interval for the contrast
      has width one and contains zero.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P11

/-- A finite population: weights, observed sentence (`true` = b, `false` = a) and observed outcome. -/
structure Pop (Ω : Type*) [Fintype Ω] where
  w : Ω → ℝ
  z : Ω → Bool
  y : Ω → ℝ
  w_nonneg : ∀ i, 0 ≤ w i
  w_total : ∑ i, w i = 1
  y01 : ∀ i, y i = 0 ∨ y i = 1

namespace Pop

variable {Ω : Type*} [Fintype Ω] (P : Pop Ω)

/-- Weighted share with sentence `t` and outcome 1. -/
def joint (t : Bool) : ℝ := ∑ i, if P.z i = t then P.w i * P.y i else 0
/-- Weighted share with sentence `t`. -/
def pz (t : Bool) : ℝ := ∑ i, if P.z i = t then P.w i else 0

/-- Any completion of the missing arm with values in [0,1]: the implied P[y(t)=1]. -/
def counterfactual (t : Bool) (fill : Ω → ℝ) : ℝ :=
  ∑ i, if P.z i = t then P.w i * P.y i else P.w i * fill i

theorem pz_compl (t : Bool) : P.pz t + P.pz (!t) = 1 := by
  unfold pz
  rw [← sum_add_distrib, ← P.w_total]
  apply sum_congr rfl; intro i _
  cases h : P.z i <;> cases t <;> simp_all

/-- Every completion lies in the interval, and both ends are attained. -/
theorem outcome_bounds (t : Bool) (fill : Ω → ℝ) (h0 : ∀ i, 0 ≤ fill i) (h1 : ∀ i, fill i ≤ 1) :
    P.joint t ≤ P.counterfactual t fill ∧ P.counterfactual t fill ≤ P.joint t + P.pz (!t) := by
  unfold joint counterfactual pz
  constructor
  · apply sum_le_sum; intro i _
    split_ifs
    · exact le_rfl
    · exact mul_nonneg (P.w_nonneg i) (h0 i)
  · rw [← sum_add_distrib]
    apply sum_le_sum; intro i _
    have hw := P.w_nonneg i
    cases hz : P.z i <;> cases t <;> simp [hz] <;> nlinarith [h1 i]

theorem lower_attained (t : Bool) : P.counterfactual t (fun _ => 0) = P.joint t := by
  unfold counterfactual joint; apply sum_congr rfl; intro i _; split_ifs <;> simp

theorem upper_attained (t : Bool) : P.counterfactual t (fun _ => 1) = P.joint t + P.pz (!t) := by
  unfold counterfactual joint pz
  rw [← sum_add_distrib]; apply sum_congr rfl; intro i _
  cases hz : P.z i <;> cases t <;> simp [hz]

/-- The ATE interval `[L, U]` has width exactly one. -/
theorem ate_width_one :
    (P.joint true + P.pz false - P.joint false) - (P.joint true - (P.joint false + P.pz true)) = 1 := by
  have := P.pz_compl true
  simp only [Bool.not_true] at this
  linarith

/-- ... and contains zero. -/
theorem ate_contains_zero :
    P.joint true - (P.joint false + P.pz true) ≤ 0 ∧ 0 ≤ P.joint true + P.pz false - P.joint false := by
  have hj : ∀ t, P.joint t ≤ P.pz t := by
    intro t; unfold joint pz; apply sum_le_sum; intro i _
    split_ifs
    · rcases P.y01 i with h | h <;> rw [h] <;> nlinarith [P.w_nonneg i]
    · exact le_rfl
  have hn : ∀ t, 0 ≤ P.joint t := by
    intro t; unfold joint; apply sum_nonneg; intro i _
    split_ifs
    · rcases P.y01 i with h | h <;> rw [h] <;> nlinarith [P.w_nonneg i]
    · exact le_rfl
  constructor
  · linarith [hj true, hn false]
  · linarith [hj false, hn true]

end Pop

end Research.P11
