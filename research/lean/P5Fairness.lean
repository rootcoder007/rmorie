/-
  Researchproofs/P5Fairness.lean

  P5. Recidivism risk scores: what can be certified about fairness.

  A binary classifier applied to one group produces a confusion table
  (tp, fp, fn, tn) of positive masses. From it: base rate p, positive
  predictive value ppv, false positive rate fpr and false negative rate
  fnr. Chouldechova (2017) observed the identity

      fpr = p / (1 − p) · (1 − ppv) / ppv · (1 − fnr),

  which makes the impossibility result a one-line consequence: two groups
  with equal ppv (calibration in the predictive sense) and equal fnr but
  different base rates must have different fpr. The theorem is about the
  arithmetic of rates, so it holds for any classifier, any data, any
  method — it is not a limitation of current tools.

  Second part: label noise. When the recorded label is a noisy proxy of
  the true outcome (rearrest for reoffending), with false-positive noise
  rate α = P(recorded 1 | true 0) and false-negative noise rate
  β = P(recorded 0 | true 1), the recorded base rate is

      p_obs = p (1 − β) + (1 − p) α,

  and the true base rate is identified only up to the interval implied by
  bounds on α and β. `true_base_rate_bounds` gives the sharp interval when
  α ∈ [0, α̅] and β ∈ [0, β̅]; the map is monotone in each noise rate, so
  the extremes are attained at the corners.

  Lean checks the arithmetic; whether α̅, β̅ are the right bounds for a
  given jurisdiction is a substantive claim that no proof can supply.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P5

/-- A confusion table for one group: masses of the four cells, all positive. -/
structure Table where
  tp : ℝ
  fp : ℝ
  fn : ℝ
  tn : ℝ
  tp_pos : 0 < tp
  fp_pos : 0 < fp
  fn_pos : 0 < fn
  tn_pos : 0 < tn

namespace Table

variable (T : Table)

/-- Total mass. -/
def total : ℝ := T.tp + T.fp + T.fn + T.tn
/-- Base rate: share of true positives among everyone. -/
def p : ℝ := (T.tp + T.fn) / T.total
/-- Positive predictive value. -/
def ppv : ℝ := T.tp / (T.tp + T.fp)
/-- False positive rate. -/
def fpr : ℝ := T.fp / (T.fp + T.tn)
/-- False negative rate. -/
def fnr : ℝ := T.fn / (T.tp + T.fn)

theorem total_pos : 0 < T.total := by unfold total; have := T.tp_pos; have := T.fp_pos; have := T.fn_pos; have := T.tn_pos; linarith
theorem p_pos : 0 < T.p := by unfold p; have := T.total_pos; have := T.tp_pos; have := T.fn_pos; positivity
theorem p_lt_one : T.p < 1 := by
  unfold p; rw [div_lt_one T.total_pos]; unfold total; have := T.fp_pos; have := T.tn_pos; linarith
theorem ppv_pos : 0 < T.ppv := by unfold ppv; have := T.tp_pos; have := T.fp_pos; positivity
theorem one_sub_ppv : 1 - T.ppv = T.fp / (T.tp + T.fp) := by
  unfold ppv; have := T.tp_pos; have := T.fp_pos; field_simp; ring
theorem one_sub_fnr : 1 - T.fnr = T.tp / (T.tp + T.fn) := by
  unfold fnr; have := T.tp_pos; have := T.fn_pos; field_simp; ring
theorem one_sub_p : 1 - T.p = (T.fp + T.tn) / T.total := by
  unfold p total; have := T.tp_pos; have := T.fp_pos; have := T.fn_pos; have := T.tn_pos; field_simp; ring

/-- Chouldechova's identity: `fpr = p/(1-p) · (1-ppv)/ppv · (1-fnr)`. -/
theorem chouldechova : T.fpr = T.p / (1 - T.p) * ((1 - T.ppv) / T.ppv) * (1 - T.fnr) := by
  have h1 := T.one_sub_ppv
  have h2 := T.one_sub_fnr
  have h3 := T.one_sub_p
  rw [h1, h2, h3]
  unfold fpr p ppv total
  have := T.tp_pos; have := T.fp_pos; have := T.fn_pos; have := T.tn_pos
  field_simp

end Table

/-- Impossibility: equal ppv and equal fnr with different base rates force different fpr. -/
theorem impossibility (A B : Table) (hppv : A.ppv = B.ppv) (hfnr : A.fnr = B.fnr)
    (hp : A.p ≠ B.p) : A.fpr ≠ B.fpr := by
  intro hfpr
  rw [A.chouldechova, B.chouldechova, hppv, hfnr] at hfpr
  -- the middle and right factors are equal and nonzero, so p/(1-p) agrees, so p agrees
  have hmid : 0 < (1 - B.ppv) / B.ppv := by
    rw [B.one_sub_ppv]; have := B.tp_pos; have := B.fp_pos; have := B.ppv_pos; positivity
  have hright : 0 < 1 - B.fnr := by
    rw [B.one_sub_fnr]; have := B.tp_pos; have := B.fn_pos; positivity
  have hodds : A.p / (1 - A.p) = B.p / (1 - B.p) := by
    have hk : (1 - B.ppv) / B.ppv * (1 - B.fnr) ≠ 0 := by positivity
    have := hfpr
    rw [mul_assoc, mul_assoc] at this
    exact mul_right_cancel₀ hk this
  have hA1 : 0 < 1 - A.p := by have := A.p_lt_one; linarith
  have hB1 : 0 < 1 - B.p := by have := B.p_lt_one; linarith
  rw [div_eq_div_iff hA1.ne' hB1.ne'] at hodds
  apply hp
  linarith

/-! ### Label noise: what the recorded base rate says about the true one -/

/-- Recorded (proxy) base rate from the true base rate `p` and noise rates
`α = P(recorded 1 | true 0)`, `β = P(recorded 0 | true 1)`. -/
def observedRate (p α β : ℝ) : ℝ := p * (1 - β) + (1 - p) * α

theorem observedRate_mono_alpha (p : ℝ) (hp : p ≤ 1) : Monotone (fun α => observedRate p α β) := by
  intro a b hab; unfold observedRate; nlinarith

theorem observedRate_anti_beta (p : ℝ) (hp : 0 ≤ p) : Antitone (fun β => observedRate p α β) := by
  intro a b hab; unfold observedRate; nlinarith

/-- Inverting the proxy: the true base rate given the recorded one and the noise rates. -/
def trueRate (pobs α β : ℝ) : ℝ := (pobs - α) / (1 - α - β)

theorem trueRate_observedRate (p α β : ℝ) (h : 1 - α - β ≠ 0) :
    trueRate (observedRate p α β) α β = p := by
  unfold trueRate observedRate; field_simp; ring

/-- With noise rates in known boxes `[0, αb]`, `[0, βb]` (αb + βb < 1) and a
recorded rate `pobs` in `[αb, 1 - βb]`, the true base rate lies between the
two corner inversions: it is at least `(pobs - αb) / (1 - αb)` and at most
`pobs / (1 - βb)`. Both ends are attained (at α = αb, β = 0 and at α = 0,
β = βb), so the interval is sharp. -/
theorem true_base_rate_bounds (p α β αb βb : ℝ)
    (hα0 : 0 ≤ α) (hαb : α ≤ αb) (hβ0 : 0 ≤ β) (hβb : β ≤ βb) (hsum : αb + βb < 1)
    (hp0 : 0 ≤ p) (hp1 : p ≤ 1) :
    (observedRate p α β - αb) / (1 - αb) ≤ p ∧ p ≤ observedRate p α β / (1 - βb) := by
  have h1 : 0 < 1 - αb := by linarith
  have h2 : 0 < 1 - βb := by linarith
  constructor
  · rw [div_le_iff₀ h1]
    unfold observedRate
    nlinarith [mul_nonneg hp0 hβ0, mul_nonneg (sub_nonneg.mpr hp1) (sub_nonneg.mpr hαb)]
  · rw [le_div_iff₀ h2]
    unfold observedRate
    nlinarith [mul_nonneg (sub_nonneg.mpr hp1) hα0, mul_nonneg hp0 (sub_nonneg.mpr hβb)]

/-- Sharpness, lower end: with `α = αb`, `β = 0` the lower bound is exact. -/
theorem lower_bound_attained (p αb : ℝ) (h : 1 - αb ≠ 0) :
    (observedRate p αb 0 - αb) / (1 - αb) = p := by
  unfold observedRate; field_simp; ring

/-- Sharpness, upper end: with `α = 0`, `β = βb` the upper bound is exact. -/
theorem upper_bound_attained (p βb : ℝ) (h : 1 - βb ≠ 0) :
    observedRate p 0 βb / (1 - βb) = p := by
  unfold observedRate; field_simp; ring

end Research.P5
