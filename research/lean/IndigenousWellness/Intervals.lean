/-
  IndigenousWellness/Intervals.lean

  The published-interval calculus of the methods paper (Methods, "The published-interval
  calculus", "Two continua", "Healing as process", "The Call to Action 19 scale", and the
  logit-scale interval of "Whole rating distributions").

  R functions certified: `indgap()`, `dualcont()`, `healchg()`, `trchlthgap()`,
  `ordcmp()` (its logit interval), `limfrom()` (symmetric limits).

    * `seLin`, `seLog`: s = (u - l)/(2z), σ = (log u - log l)/(2z); both are nonnegative
      for ordered limits, and `seLin` recovers s from symmetric limits y ± z s.
    * the log-normal interval exp(log ρ ± h) is positive, contains ρ, is geometrically
      symmetric (lo · hi = ρ²), contains 1 iff |log ρ| ≤ h, and is carried by ρ ↦ 1/ρ
      onto the log-normal interval of 1/ρ with the endpoints swapped (the TRC orientation).
    * ratio and log-se are invariant under a common rescaling (percent vs proportion);
      the difference and its se scale with it.
    * sign relations: ρ < 1 ↔ d < 0, ρ > 1 ↔ d > 0 (the strength ratio R > 1 iff W > D).
    * the disadvantage ratio T > 1 iff the group fares worse, for either orientation.
    * the logit interval expit(logit a ± h) lies in (0,1), contains a, and its endpoints are
      strictly increasing in a; logit has derivative 1/(a(1-a)) (the delta-method factor).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Real

namespace IndigenousWellness.Intervals

/-! ### Standard errors from published limits -/

/-- Standard error on the scale of the estimate, `(u - l)/(2z)`. -/
def seLin (l u z : ℝ) : ℝ := (u - l) / (2 * z)

/-- Standard error on the log scale, `(log u - log l)/(2z)`. -/
def seLog (l u z : ℝ) : ℝ := (Real.log u - Real.log l) / (2 * z)

theorem seLin_nonneg {l u z : ℝ} (hlu : l ≤ u) (hz : 0 < z) : 0 ≤ seLin l u z :=
  div_nonneg (by linarith) (by linarith)

theorem seLog_nonneg {l u z : ℝ} (hl : 0 < l) (hlu : l ≤ u) (hz : 0 < z) : 0 ≤ seLog l u z :=
  div_nonneg (by linarith [Real.log_le_log hl hlu]) (by linarith)

/-- Symmetric limits `y ± z s` give back `s` (`limfrom()` builds such limits). -/
theorem seLin_symm (y s : ℝ) {z : ℝ} (hz : z ≠ 0) : seLin (y - z * s) (y + z * s) z = s := by
  unfold seLin; field_simp; ring

/-- Rescaling the limits (percent to proportion) rescales the linear se. -/
theorem seLin_scale (l u z c : ℝ) : seLin (c * l) (c * u) z = c * seLin l u z := by
  unfold seLin; ring

/-- Rescaling the limits leaves the log-scale se unchanged. -/
theorem seLog_scale {l u c : ℝ} (z : ℝ) (hl : 0 < l) (hu : 0 < u) (hc : 0 < c) :
    seLog (c * l) (c * u) z = seLog l u z := by
  unfold seLog; rw [Real.log_mul hc.ne' hl.ne', Real.log_mul hc.ne' hu.ne']; ring

/-! ### Difference intervals (`indgap()` differences, `healchg()` changes) -/

/-- The combined se of a difference of independent estimates is at least each one's. -/
theorem diffSe_ge (s₁ s₂ : ℝ) : |s₁| ≤ Real.sqrt (s₁ ^ 2 + s₂ ^ 2) :=
  Real.abs_le_sqrt (by nlinarith [sq_nonneg s₂])

/-- `d ± z √(s₁² + s₂²)` contains `d`. -/
theorem diffInt_contains (d s₁ s₂ : ℝ) {z : ℝ} (hz : 0 ≤ z) :
    d - z * Real.sqrt (s₁ ^ 2 + s₂ ^ 2) ≤ d ∧ d ≤ d + z * Real.sqrt (s₁ ^ 2 + s₂ ^ 2) := by
  have := mul_nonneg hz (Real.sqrt_nonneg (s₁ ^ 2 + s₂ ^ 2))
  constructor <;> linarith

/-- A common rescaling of both estimates rescales the difference. -/
theorem diff_scale (c a b : ℝ) : c * a - c * b = c * (a - b) := by ring

/-! ### Ratios on the log scale (`indgap()` ratios, `dualcont()`, `trchlthgap()`) -/

/-- Lower log-normal limit `exp(log ρ - h)` (with `h = z √(σ_g² + σ_r²)`). -/
def logLo (ρ h : ℝ) : ℝ := Real.exp (Real.log ρ - h)

/-- Upper log-normal limit `exp(log ρ + h)`. -/
def logHi (ρ h : ℝ) : ℝ := Real.exp (Real.log ρ + h)

theorem logLo_eq {ρ : ℝ} (h : ℝ) (hρ : 0 < ρ) : logLo ρ h = ρ * Real.exp (-h) := by
  unfold logLo; rw [sub_eq_add_neg, Real.exp_add, Real.exp_log hρ]

theorem logHi_eq {ρ : ℝ} (h : ℝ) (hρ : 0 < ρ) : logHi ρ h = ρ * Real.exp h := by
  unfold logHi; rw [Real.exp_add, Real.exp_log hρ]

/-- Both limits are positive. -/
theorem logInt_pos (ρ h : ℝ) : 0 < logLo ρ h ∧ 0 < logHi ρ h :=
  ⟨Real.exp_pos _, Real.exp_pos _⟩

/-- The log-normal interval of a positive ratio contains it. -/
theorem logInt_contains {ρ h : ℝ} (hρ : 0 < ρ) (hh : 0 ≤ h) : logLo ρ h ≤ ρ ∧ ρ ≤ logHi ρ h := by
  constructor
  · unfold logLo
    calc Real.exp (Real.log ρ - h) ≤ Real.exp (Real.log ρ) := Real.exp_le_exp.mpr (by linarith)
      _ = ρ := Real.exp_log hρ
  · unfold logHi
    calc ρ = Real.exp (Real.log ρ) := (Real.exp_log hρ).symm
      _ ≤ Real.exp (Real.log ρ + h) := Real.exp_le_exp.mpr (by linarith)

/-- Geometric symmetry: `lo · hi = ρ²`. -/
theorem logInt_mul {ρ : ℝ} (h : ℝ) (hρ : 0 < ρ) : logLo ρ h * logHi ρ h = ρ ^ 2 := by
  rw [logLo_eq h hρ, logHi_eq h hρ]
  have : Real.exp (-h) * Real.exp h = 1 := by rw [← Real.exp_add]; simp
  calc ρ * Real.exp (-h) * (ρ * Real.exp h) = ρ ^ 2 * (Real.exp (-h) * Real.exp h) := by ring
    _ = ρ ^ 2 := by rw [this, mul_one]

/-- The interval excludes 1 (a gap is "detected") exactly when `|log ρ| > h`. -/
theorem logInt_contains_one_iff {ρ : ℝ} (h : ℝ) :
    (logLo ρ h ≤ 1 ∧ 1 ≤ logHi ρ h) ↔ |Real.log ρ| ≤ h := by
  unfold logLo logHi
  rw [← Real.exp_zero]
  simp only [Real.exp_le_exp]
  rw [abs_le]; constructor <;> rintro ⟨a, b⟩ <;> constructor <;> linarith

/-- The ratio of two estimates is unchanged by a common rescaling. -/
theorem ratio_scale {c : ℝ} (a b : ℝ) (hc : c ≠ 0) : (c * a) / (c * b) = a / b :=
  mul_div_mul_left a b hc

/-- With a positive reference, the ratio is below one exactly when the difference is negative. -/
theorem ratio_lt_one_iff {a b : ℝ} (hb : 0 < b) : a / b < 1 ↔ a - b < 0 := by
  rw [div_lt_one hb]; constructor <;> intro h <;> linarith

/-- The strength ratio `R = W/D` exceeds one exactly when `W > D`. -/
theorem ratio_gt_one_iff {a b : ℝ} (hb : 0 < b) : 1 < a / b ↔ 0 < a - b := by
  rw [one_lt_div hb]; constructor <;> intro h <;> linarith

/-- Reciprocal of the upper limit is the lower limit for `1/ρ`. -/
theorem inv_logHi (ρ h : ℝ) : (logHi ρ h)⁻¹ = logLo ρ⁻¹ h := by
  unfold logHi logLo; rw [← Real.exp_neg, Real.log_inv]; congr 1; ring

/-- Reciprocal of the lower limit is the upper limit for `1/ρ`. -/
theorem inv_logLo (ρ h : ℝ) : (logLo ρ h)⁻¹ = logHi ρ⁻¹ h := by
  unfold logHi logLo; rw [← Real.exp_neg, Real.log_inv]; congr 1; ring

/-- Any positive interval is carried by `ρ ↦ 1/ρ` onto `[1/hi, 1/lo]`. -/
theorem inv_interval {lo ρ hi : ℝ} (hlo : 0 < lo) (h₁ : lo ≤ ρ) (h₂ : ρ ≤ hi) :
    hi⁻¹ ≤ ρ⁻¹ ∧ ρ⁻¹ ≤ lo⁻¹ :=
  ⟨inv_anti₀ (by linarith) h₂, inv_anti₀ hlo h₁⟩

/-! ### The Call to Action 19 disadvantage ratio (`trchlthgap()`) -/

/-- `T = ρ` when lower is better, `T = 1/ρ` when higher is better. -/
def disratio (lowerBetter : Bool) (yg yr : ℝ) : ℝ := if lowerBetter then yg / yr else (yg / yr)⁻¹

/-- When higher is better, `T = y_r / y_g`: `T = 1.19` for life expectancy means the
reference lives 19% longer. -/
theorem disratio_higher (yg yr : ℝ) : disratio false yg yr = yr / yg := by
  simp [disratio, inv_div]

/-- `T > 1` exactly when the group fares worse: a higher value when lower is better,
a lower value when higher is better. -/
theorem disratio_gt_one_iff (lb : Bool) {yg yr : ℝ} (hg : 0 < yg) (hr : 0 < yr) :
    1 < disratio lb yg yr ↔ (if lb then yr < yg else yg < yr) := by
  cases lb
  · simp only [disratio_higher, Bool.false_eq_true, ite_false]; exact one_lt_div hg
  · simp only [disratio, ite_true]; exact one_lt_div hr

/-- The oriented interval of `trchlthgap()`, `[1/ratioup, 1/ratiolo]`, is the log-normal
interval of `1/ρ`, so `T` sits inside it exactly as `ρ` sits inside its own. -/
theorem disratio_interval {ρ h : ℝ} (hρ : 0 < ρ) (hh : 0 ≤ h) :
    (logHi ρ h)⁻¹ ≤ ρ⁻¹ ∧ ρ⁻¹ ≤ (logLo ρ h)⁻¹ := by
  rw [inv_logHi ρ h, inv_logLo ρ h]; exact logInt_contains (inv_pos.mpr hρ) hh

/-! ### The logit-scale interval (`ordcmp()`) -/

/-- `plogis`. -/
def expit (x : ℝ) : ℝ := 1 / (1 + Real.exp (-x))

/-- `qlogis`. -/
def logit (a : ℝ) : ℝ := Real.log (a / (1 - a))

theorem expit_pos (x : ℝ) : 0 < expit x := by
  unfold expit; have := Real.exp_pos (-x); positivity

theorem expit_lt_one (x : ℝ) : expit x < 1 := by
  unfold expit; have := Real.exp_pos (-x)
  rw [div_lt_one (by linarith)]; linarith

theorem expit_strictMono : StrictMono expit := by
  intro x y hxy
  unfold expit
  have hx := Real.exp_pos (-x); have hy := Real.exp_pos (-y)
  have : Real.exp (-y) < Real.exp (-x) := Real.exp_lt_exp.mpr (by linarith)
  exact one_div_lt_one_div_of_lt (by linarith) (by linarith)

theorem expit_logit {a : ℝ} (h0 : 0 < a) (h1 : a < 1) : expit (logit a) = a := by
  unfold expit logit
  have hb : 0 < 1 - a := by linarith
  rw [Real.exp_neg, Real.exp_log (div_pos h0 hb), inv_div]
  field_simp
  ring

theorem logit_strictMonoOn : StrictMonoOn logit (Set.Ioo 0 1) := by
  intro a ha b hb hab
  unfold logit
  apply Real.log_lt_log (div_pos ha.1 (by linarith [ha.2]))
  rw [div_lt_div_iff₀ (by linarith [ha.2]) (by linarith [hb.2])]
  nlinarith

/-- The logit interval lies inside (0,1) and contains the estimate. -/
theorem logitInt {a h : ℝ} (h0 : 0 < a) (h1 : a < 1) (hh : 0 ≤ h) :
    0 < expit (logit a - h) ∧ expit (logit a - h) ≤ a ∧
      a ≤ expit (logit a + h) ∧ expit (logit a + h) < 1 := by
  refine ⟨expit_pos _, ?_, ?_, expit_lt_one _⟩
  · conv_rhs => rw [← expit_logit h0 h1]
    exact expit_strictMono.monotone (by linarith)
  · conv_lhs => rw [← expit_logit h0 h1]
    exact expit_strictMono.monotone (by linarith)

/-- For a fixed half-width on the logit scale both limits are strictly increasing in the
estimate. -/
theorem logitInt_mono (c : ℝ) :
    StrictMonoOn (fun a => expit (logit a + c)) (Set.Ioo 0 1) := by
  intro a ha b hb hab
  exact expit_strictMono (by linarith [logit_strictMonoOn ha hb hab])

/-- The delta-method factor of `ordcmp()`: `d logit(a)/da = 1/(a(1-a))`, so the logit-scale
se is `√v / (a(1-a))`. -/
theorem hasDerivAt_logit {a : ℝ} (h0 : 0 < a) (h1 : a < 1) :
    HasDerivAt logit (1 / (a * (1 - a))) a := by
  have hb : (1 - a) ≠ 0 := by linarith
  have hq : HasDerivAt (fun x => x / (1 - x)) ((1 * (1 - a) - a * (-1)) / (1 - a) ^ 2) a :=
    (hasDerivAt_id a).div ((hasDerivAt_id a).const_sub 1 |>.congr_deriv (by simp)) hb
  have hl := hq.log (div_ne_zero h0.ne' hb)
  unfold logit
  convert hl using 1
  field_simp
  ring

end IndigenousWellness.Intervals
