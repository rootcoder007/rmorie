/-
  Researchproofs/P19Shrinkage.lean

  P19 (continued): the size of the fall under a stated noise law, and the
  empirical-Bayes shrinkage that removes it (James & Stein 1961; Efron &
  Morris 1975; Morris 1983).

  On a finite weighted population of places, the first-period count is
  y = θ + e with θ the place's true rate and e noise. The noise law is
  stated in finite form: ∑ w e = 0 and ∑ w θ e = 0 (noise has mean zero
  and is uncorrelated with the truth). The shrinkage estimator pulls each
  place toward the mean, θ̂(B) = (1 − B) y + B ȳ.

    * loss_eq: the weighted squared error of θ̂(B) is exactly
      (1 − B)² S_e + B² S_θ with S_e = ∑ w e² and S_θ = ∑ w (θ − θ̄)².
    * loss_min: it is minimised at B* = S_e/(S_e + S_θ), the empirical-Bayes
      shrinkage factor, with minimum S_e S_θ/(S_e + S_θ) ≤ S_e = the raw
      error (loss_bstar_le_raw).
    * predicted_fall: y − θ̂(B) = B (y − ȳ): the expected fall of a selected
      place is B times its excess over the mean — the size the P19 sign
      theorem left open, now a formula in the noise and signal variances.

  So the "fall" a hot-spot programme reports has an expected size that a
  no-treatment model predicts from the data alone; the programme's effect
  is the fall beyond that.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P19Shrinkage

variable {Ω : Type*} [Fintype Ω] (w θ e : Ω → ℝ)

def W : ℝ := ∑ i, w i
/-- Weighted mean of a function. -/
def mean (f : Ω → ℝ) : ℝ := (∑ i, w i * f i) / W w
/-- The shrinkage estimator with factor `B`. -/
def shrink (B : ℝ) (i : Ω) : ℝ := (1 - B) * (θ i + e i) + B * mean w (fun j => θ j + e j)
/-- Weighted squared error of the estimator against the truth. -/
def loss (B : ℝ) : ℝ := ∑ i, w i * (shrink w θ e B i - θ i) ^ 2
/-- Noise mass. -/
def Se : ℝ := ∑ i, w i * e i ^ 2
/-- Signal mass. -/
def Sθ : ℝ := ∑ i, w i * (θ i - mean w θ) ^ 2
/-- The empirical-Bayes factor. -/
def Bstar : ℝ := Se w e / (Se w e + Sθ w θ)

theorem mean_y (h0 : ∑ i, w i * e i = 0) : mean w (fun j => θ j + e j) = mean w θ := by
  unfold mean
  congr 1
  simp only [mul_add, sum_add_distrib, h0, add_zero]

theorem cross_zero (h0 : ∑ i, w i * e i = 0) (h1 : ∑ i, w i * (θ i * e i) = 0) :
    ∑ i, w i * (e i * (θ i - mean w θ)) = 0 := by
  have hpt : ∀ i, w i * (e i * (θ i - mean w θ)) = w i * (θ i * e i) - mean w θ * (w i * e i) :=
    fun i => by ring
  simp only [hpt, sum_sub_distrib, ← mul_sum, h0, h1, mul_zero, sub_zero]

/-- The loss as a quadratic in `B`: no cross term under the noise law. -/
theorem loss_eq (h0 : ∑ i, w i * e i = 0) (h1 : ∑ i, w i * (θ i * e i) = 0) (B : ℝ) :
    loss w θ e B = (1 - B) ^ 2 * Se w e + B ^ 2 * Sθ w θ := by
  unfold loss Se Sθ shrink
  rw [mean_y w θ e h0]
  have hpt : ∀ i, w i * ((1 - B) * (θ i + e i) + B * mean w θ - θ i) ^ 2
      = (1 - B) ^ 2 * (w i * e i ^ 2) + B ^ 2 * (w i * (θ i - mean w θ) ^ 2)
        - 2 * B * (1 - B) * (w i * (e i * (θ i - mean w θ))) := fun i => by ring
  simp only [hpt, sum_add_distrib, sum_sub_distrib, ← mul_sum, cross_zero w θ e h0 h1, mul_zero, sub_zero]

theorem loss_min (h0 : ∑ i, w i * e i = 0) (h1 : ∑ i, w i * (θ i * e i) = 0)
    (hpos : 0 < Se w e + Sθ w θ) (B : ℝ) : loss w θ e (Bstar w θ e) ≤ loss w θ e B := by
  rw [loss_eq w θ e h0 h1, loss_eq w θ e h0 h1]
  unfold Bstar
  set a := Se w e
  set b := Sθ w θ
  have hab : a + b ≠ 0 := hpos.ne'
  have key : (1 - B) ^ 2 * a + B ^ 2 * b - ((1 - a / (a + b)) ^ 2 * a + (a / (a + b)) ^ 2 * b)
      = (a + b) * (B - a / (a + b)) ^ 2 := by
    field_simp
    ring
  have := mul_nonneg hpos.le (sq_nonneg (B - a / (a + b)))
  linarith

theorem loss_bstar_eq (h0 : ∑ i, w i * e i = 0) (h1 : ∑ i, w i * (θ i * e i) = 0)
    (hpos : 0 < Se w e + Sθ w θ) : loss w θ e (Bstar w θ e) = Se w e * Sθ w θ / (Se w e + Sθ w θ) := by
  rw [loss_eq w θ e h0 h1]
  unfold Bstar
  have hab : Se w e + Sθ w θ ≠ 0 := hpos.ne'
  field_simp
  ring

theorem loss_raw (h0 : ∑ i, w i * e i = 0) (h1 : ∑ i, w i * (θ i * e i) = 0) :
    loss w θ e 0 = Se w e := by
  rw [loss_eq w θ e h0 h1]; ring

/-- Shrinking never does worse than the raw counts. -/
theorem loss_bstar_le_raw (h0 : ∑ i, w i * e i = 0) (h1 : ∑ i, w i * (θ i * e i) = 0)
    (hpos : 0 < Se w e + Sθ w θ) : loss w θ e (Bstar w θ e) ≤ Se w e := by
  rw [← loss_raw w θ e h0 h1]
  exact loss_min w θ e h0 h1 hpos 0

theorem bstar_mem (hw : ∀ i, 0 ≤ w i) (hpos : 0 < Se w e + Sθ w θ) :
    0 ≤ Bstar w θ e ∧ Bstar w θ e ≤ 1 := by
  have ha : 0 ≤ Se w e := sum_nonneg fun i _ => mul_nonneg (hw i) (sq_nonneg _)
  have hb : 0 ≤ Sθ w θ := sum_nonneg fun i _ => mul_nonneg (hw i) (sq_nonneg _)
  unfold Bstar
  constructor
  · exact div_nonneg ha hpos.le
  · rw [div_le_one hpos]; linarith

/-- The predicted fall of a place is `B` times its excess over the mean. -/
theorem predicted_fall (B : ℝ) (i : Ω) :
    (θ i + e i) - shrink w θ e B i = B * ((θ i + e i) - mean w (fun j => θ j + e j)) := by
  unfold shrink; ring

end Research.P19Shrinkage
