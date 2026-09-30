/-
  Researchproofs/P1Breakdown.lean

  P1, continued: breakdown analysis for the dark figure.

  A conclusion of the form "the true victimisation rate is below t" (or
  "the dark figure ratio v/r is below k") survives the survey noise boxes
  only while the upper bound v_obs/(1−β̄) stays below the threshold. The
  breakdown value of the under-reporting box is therefore

      β̄* = 1 − v_obs / t,

  the largest β̄ at which the conclusion still holds for every admissible
  noise pair. Above it, the attained-bound theorem gives a noise pair
  under which the conclusion fails.

  Proved:
    * conclusion_holds_below_breakdown: β̄ < 1 − v_obs/t (and αb+β̄ < 1)
      implies v < t for every admissible (α, β).
    * conclusion_fails_above_breakdown: β̄ > 1 − v_obs/t admits a true
      rate v ≥ t consistent with the recorded survey rate (witness at
      α = 0, β = β̄).
-/
import Mathlib
import Researchproofs.P5Fairness

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P1

open Research.P5

/-- Breakdown value of the under-reporting box for the conclusion `v < t`. -/
def breakdownBeta (vobs t : ℝ) : ℝ := 1 - vobs / t

theorem conclusion_holds_below_breakdown (v α β αb βb t : ℝ)
    (hα0 : 0 ≤ α) (hαb : α ≤ αb) (hβ0 : 0 ≤ β) (hβb : β ≤ βb) (hsum : αb + βb < 1)
    (hv0 : 0 ≤ v) (hv1 : v ≤ 1) (ht : 0 < t)
    (hbreak : βb < breakdownBeta (observedRate v α β) t) : v < t := by
  obtain ⟨_, hhi⟩ := true_base_rate_bounds v α β αb βb hα0 hαb hβ0 hβb hsum hv0 hv1
  unfold breakdownBeta at hbreak
  have h1 : 0 < 1 - βb := by linarith
  -- hbreak: βb < 1 - vobs/t  ⇔  vobs/t < 1 - βb  ⇔  vobs < t (1 - βb)  ⇔  vobs/(1-βb) < t
  have h2 : observedRate v α β / (1 - βb) < t := by
    rw [div_lt_iff₀ h1]
    have : observedRate v α β / t < 1 - βb := by linarith
    rw [div_lt_iff₀ ht] at this
    linarith
  linarith

/-- Above the breakdown value the conclusion can fail: a true rate at least `t`
reproduces the recorded survey rate with admissible noise. -/
theorem conclusion_fails_above_breakdown (vobs βb t : ℝ) (ht : 0 < t) (hβb : βb < 1)
    (hbreak : breakdownBeta vobs t ≤ βb) :
    ∃ v : ℝ, observedRate v 0 βb = vobs ∧ t ≤ v := by
  refine ⟨vobs / (1 - βb), ?_, ?_⟩
  · unfold observedRate
    have : 1 - βb ≠ 0 := by linarith
    field_simp
    ring
  · unfold breakdownBeta at hbreak
    have h1 : 0 < 1 - βb := by linarith
    rw [le_div_iff₀ h1]
    have : 1 - βb ≤ vobs / t := by linarith
    rw [le_div_iff₀ ht] at this
    linarith

end Research.P1
