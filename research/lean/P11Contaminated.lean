/-
  Researchproofs/P11Contaminated.lean

  P11, continued: contaminated-sample bounds (Manski, Identification for
  Prediction and Decision, §5.2, eq. 5.10).

  A recorded distribution Q is a mixture (1 − p) P + p R of the clean
  distribution P of interest and an unknown contaminating distribution R
  (mis-coded offences, unfounded records, wrong addresses), with the
  contamination share p known. For any event B,

      P(B) = (Q(B) − p R(B)) / (1 − p),   R(B) ∈ [0, 1],

  so P(B) is identified only up to [max(0, (Q(B) − p)/(1 − p)),
  min(1, Q(B)/(1 − p))]; both ends are attained (R(B) = 1, R(B) = 0)
  whenever they lie in [0, 1], the width is p/(1 − p), and the interval is
  informative only when p < min(Q(B), 1 − Q(B)).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P11

/-- Clean probability implied by the observed `q`, contamination share `p` and contaminant probability `r`. -/
def cleanProb (q p r : ℝ) : ℝ := (q - p * r) / (1 - p)

theorem clean_bounds (q p r : ℝ) (hp0 : 0 ≤ p) (hp1 : p < 1) (hr0 : 0 ≤ r) (hr1 : r ≤ 1) :
    (q - p) / (1 - p) ≤ cleanProb q p r ∧ cleanProb q p r ≤ q / (1 - p) := by
  unfold cleanProb
  have h : 0 < 1 - p := by linarith
  constructor
  · rw [div_le_div_iff_of_pos_right h]; nlinarith
  · rw [div_le_div_iff_of_pos_right h]; nlinarith

theorem clean_lower_attained (q p : ℝ) : cleanProb q p 1 = (q - p) / (1 - p) := by
  unfold cleanProb; ring
theorem clean_upper_attained (q p : ℝ) : cleanProb q p 0 = q / (1 - p) := by
  unfold cleanProb; ring

/-- The width of the interval is `p / (1 − p)`. -/
theorem clean_width (q p : ℝ) (hp1 : p < 1) : q / (1 - p) - (q - p) / (1 - p) = p / (1 - p) := by
  have : (1 - p) ≠ 0 := by linarith
  field_simp
  ring

/-- The interval is informative (does not cover all of [0,1]) iff `p < q` or `p < 1 − q`. -/
theorem clean_informative (q p : ℝ) (hp1 : p < 1) :
    (0 < (q - p) / (1 - p) ∨ q / (1 - p) < 1) ↔ (p < q ∨ p < 1 - q) := by
  have h : 0 < 1 - p := by linarith
  constructor
  · rintro (h1 | h1)
    · left; rw [div_pos_iff_of_pos_right h] at h1; linarith
    · right; rw [div_lt_one h] at h1; linarith
  · rintro (h1 | h1)
    · left; rw [div_pos_iff_of_pos_right h]; linarith
    · right; rw [div_lt_one h]; linarith

end Research.P11
