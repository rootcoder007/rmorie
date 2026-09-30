/-
  Researchproofs/P2RelativeRisk.lean

  P2, continued: what an odds ratio says about a relative risk (Manski,
  Identification for Prediction and Decision, §6.2).

  Arrest-only or case-control samples identify the odds ratio
      OR = (a/(1−a)) / (b/(1−b)),   a = P(y=1 | x=1), b = P(y=1 | x=0),
  but not the risks a, b themselves. The relative risk a/b is nevertheless
  squeezed between 1 and OR (rr_between), and equals OR only when the
  outcome is impossible under one of the two conditions... more exactly,
  OR = RR · (1−b)/(1−a), so the rare-outcome approximation RR ≈ OR always
  overstates the relative risk in magnitude (or_overstates). The gap is
  exactly the ratio of the two survival probabilities.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P2

def oddsRatio (a b : ℝ) : ℝ := (a / (1 - a)) / (b / (1 - b))
def relRisk (a b : ℝ) : ℝ := a / b

theorem or_eq_rr_mul (a b : ℝ) (ha0 : 0 < a) (ha1 : a < 1) (hb0 : 0 < b) (hb1 : b < 1) :
    oddsRatio a b = relRisk a b * ((1 - b) / (1 - a)) := by
  unfold oddsRatio relRisk
  have h1 : (1 - a) ≠ 0 := by linarith
  have h2 : (1 - b) ≠ 0 := by linarith
  field_simp

/-- The relative risk lies between 1 and the odds ratio. -/
theorem rr_between (a b : ℝ) (ha0 : 0 < a) (ha1 : a < 1) (hb0 : 0 < b) (hb1 : b < 1) :
    min 1 (oddsRatio a b) ≤ relRisk a b ∧ relRisk a b ≤ max 1 (oddsRatio a b) := by
  rw [or_eq_rr_mul a b ha0 ha1 hb0 hb1]
  have hrr : 0 < relRisk a b := by unfold relRisk; positivity
  have h1a : 0 < 1 - a := by linarith
  have h1b : 0 < 1 - b := by linarith
  rcases le_or_gt b a with hab | hab
  · -- RR ≥ 1 and (1−b)/(1−a) ≥ 1, so 1 ≤ RR ≤ OR
    have hrr1 : 1 ≤ relRisk a b := by unfold relRisk; rw [le_div_iff₀ hb0]; linarith
    have hf : 1 ≤ (1 - b) / (1 - a) := by rw [le_div_iff₀ h1a]; linarith
    constructor
    · exact le_trans (min_le_left _ _) hrr1
    · exact le_trans (le_mul_of_one_le_right hrr.le hf) (le_max_right _ _)
  · have hrr1 : relRisk a b ≤ 1 := by unfold relRisk; rw [div_le_one hb0]; linarith
    have hf : (1 - b) / (1 - a) ≤ 1 := by rw [div_le_one h1a]; linarith
    constructor
    · exact le_trans (min_le_right _ _) (mul_le_of_le_one_right hrr.le hf)
    · exact le_trans hrr1 (le_max_left _ _)

/-- The rare-outcome substitution RR ≈ OR overstates: |log OR| ≥ |log RR|. -/
theorem or_overstates (a b : ℝ) (ha0 : 0 < a) (ha1 : a < 1) (hb0 : 0 < b) (hb1 : b < 1) :
    |Real.log (relRisk a b)| ≤ |Real.log (oddsRatio a b)| := by
  have h1a : 0 < 1 - a := by linarith
  have h1b : 0 < 1 - b := by linarith
  have hrr : 0 < relRisk a b := by unfold relRisk; positivity
  have hf : 0 < (1 - b) / (1 - a) := by positivity
  rw [or_eq_rr_mul a b ha0 ha1 hb0 hb1, Real.log_mul hrr.ne' hf.ne']
  rcases le_or_gt b a with hab | hab
  · have hrr1 : 1 ≤ relRisk a b := by unfold relRisk; rw [le_div_iff₀ hb0]; linarith
    have hf1 : 1 ≤ (1 - b) / (1 - a) := by rw [le_div_iff₀ h1a]; linarith
    have l1 := Real.log_nonneg hrr1
    have l2 := Real.log_nonneg hf1
    rw [abs_of_nonneg l1, abs_of_nonneg (by linarith)]
    linarith
  · have hrr1 : relRisk a b ≤ 1 := by unfold relRisk; rw [div_le_one hb0]; linarith
    have hf1 : (1 - b) / (1 - a) ≤ 1 := by rw [div_le_one h1a]; linarith
    have l1 := Real.log_nonpos hrr.le hrr1
    have l2 := Real.log_nonpos hf.le hf1
    rw [abs_of_nonpos l1, abs_of_nonpos (by linarith)]
    linarith

end Research.P2
