/-
  Researchproofs/P5Hazard.lean

  P5, continued: the built-in selection bias of hazard ratios (Hernán &
  Robins, Causal Inference: What If, Fine Point 17.2).

  Two types of people: a share s of "high-risk" with per-period
  reconviction probability h, the rest "low-risk" with probability l < h.
  A programme has NO effect on anyone (every individual hazard is the
  same treated or untreated), but in period 1 the treated arm removes
  (reconvicts) a share of the high-risk type... — the point of the Fine
  Point is the reverse: even with zero individual effect, if the arms
  differ in first-period depletion the second-period hazard ratio is not
  one. Cleanest form: a treatment that lowers the first-period hazard of
  the high-risk type only (harmless, say by delaying) leaves the second
  period's survivors in the treated arm richer in high-risk people, so
  the period-2 hazard ratio exceeds 1 although no one is harmed
  (hr2_gt_one_of_depletion). Symmetrically a treatment with a genuine
  effect in period 1 alone shows a spurious period-2 "harm". Period-by-
  period hazard ratios are not causal contrasts; risk differences at a
  fixed horizon are.

  Formalised: the period-2 hazard among survivors of a mixed population
  is a weighted average of h and l whose weight on h is the surviving
  high-risk share (survivor_hazard); it is increasing in that share
  (survivor_hazard_mono); depleting the high-risk type less in one arm
  raises that arm's period-2 hazard (hr2_gt_one_of_depletion).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P5

/-- Period-2 hazard among survivors when a share `w` of survivors is high-risk. -/
def survivorHazard (h l w : ℝ) : ℝ := w * h + (1 - w) * l

theorem survivor_hazard_mono (h l w₁ w₂ : ℝ) (hl : l < h) (hw : w₁ < w₂) :
    survivorHazard h l w₁ < survivorHazard h l w₂ := by
  unfold survivorHazard; nlinarith

/-- Surviving high-risk share after period 1 when high-risk people survive with probability `a`
and low-risk with probability `b`, from an initial high-risk share `s`. -/
def survivingShare (s a b : ℝ) : ℝ := s * a / (s * a + (1 - s) * b)

/-- If an arm depletes the high-risk type less (`a₂ > a₁`, same `b`), its surviving high-risk
share is larger, hence its period-2 hazard is larger, hence the period-2 hazard ratio exceeds one
with no individual effect in period 2 at all. -/
theorem hr2_gt_one_of_depletion (h l s a₁ a₂ b : ℝ) (hl : l < h) (hs0 : 0 < s) (hs1 : s < 1)
    (ha1 : 0 < a₁) (ha2 : a₁ < a₂) (hb : 0 < b) :
    survivorHazard h l (survivingShare s a₁ b) < survivorHazard h l (survivingShare s a₂ b) := by
  apply survivor_hazard_mono h l _ _ hl
  unfold survivingShare
  have h1s : 0 < 1 - s := by linarith
  have d1 : 0 < s * a₁ + (1 - s) * b := by have := mul_pos hs0 ha1; have := mul_pos h1s hb; linarith
  have d2 : 0 < s * a₂ + (1 - s) * b := by have := mul_pos hs0 (by linarith : 0 < a₂); have := mul_pos h1s hb; linarith
  rw [div_lt_div_iff₀ d1 d2]
  nlinarith [mul_pos hs0 h1s, mul_pos (mul_pos hs0 h1s) hb, mul_pos hs0 (mul_pos h1s hb)]

/-- A concrete witness: s = 1/2, h = 1/2, l = 1/10, treated arm survives high-risk at 4/5 vs 1/2:
period-2 hazards 0.3 (control) vs 0.36 (treated) — a "harmful" ratio 1.2 with zero effect. -/
theorem hr2_witness :
    survivorHazard (1/2) (1/10) (survivingShare (1/2) (1/2) (9/10)) < survivorHazard (1/2) (1/10) (survivingShare (1/2) (4/5) (9/10)) := by
  unfold survivorHazard survivingShare; norm_num

end Research.P5
