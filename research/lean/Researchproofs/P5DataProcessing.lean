/-
  Researchproofs/P5DataProcessing.lean

  P5, continued: post-processing cannot add information about true
  reoffending, and calibration to the recorded label is calibration to the
  wrong thing when groups' recording differs (library scan C20; MacKay
  Exercise 8.9, Gibbs' inequality).

    * `bayes_risk_data_processing`: for a finite joint law of (W, D) and any
      post-processing R = h(D), every predictor built from R is a predictor
      built from D, so the best squared-error risk from D is at most the best
      from R (`risk_comp`, then the bound against any D-optimal predictor).
      This is the data-processing inequality in its decision form: a risk
      score R computed from the recorded label D knows at most what D knows
      about W.
    * `calibration_incompatible`: with symmetric recording flip rate f_g in
      group g, P(D = 1 | R = r, g) = f_g + (1 − 2f_g) P(W = 1 | R = r, g).
      If the score is calibrated to D in both groups then
      P(W = 1 | R = r, g) = (r − f_g)/(1 − 2f_g), which differs between groups
      whenever f_A ≠ f_B and r ≠ 1/2; and symmetrically calibration to W in
      both groups forces miscalibration to D. The two calibrations cannot hold
      together across groups with different flip rates.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Finset

namespace Research.P5.DataProcessing

variable {W D R : Type*} [Fintype W] [Fintype D] [Fintype R]

/-- Squared-error risk of a predictor `g : D → ℝ` of the real-valued target `w` under the joint
weight `μ` on `W × D`. -/
def riskD (μ : W → D → ℝ) (w : W → ℝ) (g : D → ℝ) : ℝ := ∑ x, ∑ d, μ x d * (w x - g d) ^ 2
def riskR (μ : W → D → ℝ) (w : W → ℝ) (h : D → R) (f : R → ℝ) : ℝ := ∑ x, ∑ d, μ x d * (w x - f (h d)) ^ 2

theorem risk_comp (μ : W → D → ℝ) (w : W → ℝ) (h : D → R) (f : R → ℝ) :
    riskR μ w h f = riskD μ w (f ∘ h) := rfl

theorem bayes_risk_data_processing (μ : W → D → ℝ) (w : W → ℝ) (h : D → R) (gopt : D → ℝ)
    (hopt : ∀ g : D → ℝ, riskD μ w gopt ≤ riskD μ w g) (f : R → ℝ) :
    riskD μ w gopt ≤ riskR μ w h f := by
  rw [risk_comp]; exact hopt (f ∘ h)

/-- Recorded-label rate from the true rate under a symmetric flip rate `f`. -/
def recorded (f pw : ℝ) : ℝ := f + (1 - 2 * f) * pw

theorem calibrated_to_D_true_rate (f r : ℝ) (hf : f ≠ 1 / 2) :
    recorded f ((r - f) / (1 - 2 * f)) = r := by
  unfold recorded
  have h2 : 1 - 2 * f ≠ 0 := by intro h; apply hf; linarith
  rw [mul_div_cancel₀ _ h2]
  ring

/-- Calibrated to `D` in both groups (`recorded f_g pw_g = r`), the true rates differ unless the
flip rates agree or `r = 1/2`. -/
theorem calibration_incompatible (fA fB r pwA pwB : ℝ) (hA : fA ≠ 1 / 2) (hB : fB ≠ 1 / 2)
    (hcalA : recorded fA pwA = r) (hcalB : recorded fB pwB = r) (hf : fA ≠ fB) (hr : r ≠ 1 / 2) :
    pwA ≠ pwB := by
  intro heq
  unfold recorded at hcalA hcalB
  rw [heq] at hcalA
  -- fA + (1 - 2 fA) pwB = fB + (1 - 2 fB) pwB  ⇒  (fA - fB)(1 - 2 pwB) = 0
  have h1 : (fA - fB) * (1 - 2 * pwB) = 0 := by linarith
  rcases mul_eq_zero.mp h1 with h | h
  · exact hf (by linarith)
  · -- pwB = 1/2 forces r = fB + (1 - 2 fB)/2 = 1/2
    have : pwB = 1 / 2 := by linarith
    rw [this] at hcalB
    apply hr; linarith

/-- Calibrated to `W` in both groups (`pw_g = r`), the recorded rates differ unless the flip rates
agree or `r = 1/2`. -/
theorem calibration_to_W_incompatible (fA fB r : ℝ) (hf : fA ≠ fB) (hr : r ≠ 1 / 2) :
    recorded fA r ≠ recorded fB r := by
  unfold recorded
  intro h
  have : (fA - fB) * (1 - 2 * r) = 0 := by linarith
  rcases mul_eq_zero.mp this with h' | h'
  · exact hf (by linarith)
  · exact hr (by linarith)

end Research.P5.DataProcessing
