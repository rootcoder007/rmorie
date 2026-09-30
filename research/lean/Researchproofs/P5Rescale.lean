/-
  Researchproofs/P5Rescale.lean

  P5, continued: why logistic coefficients cannot be compared across
  nested models (Karlson, Holm & Breen 2012; Weisburd & Britt ch. 4).

  In a latent-index model y* = β x + γ u + ε with ε logistic (variance
  π²/3) and u an independent covariate with variance τ², the coefficient
  that a logit IDENTIFIES is the latent coefficient divided by the
  standard deviation of the total unexplained part. Omitting u — with no
  confounding at all, since u ⊥ x — moves that unexplained variance from
  π²/3 to π²/3 + γ²τ², so the identified coefficient shrinks by

      c = sqrt((π²/3) / (π²/3 + γ²τ²)) ∈ (0, 1]                 (rescale_lt_one)

  and the odds ratio exp(β) moves to exp(cβ). Adding a covariate to a
  logit therefore changes every other coefficient even when it is
  orthogonal to them; the change is rescaling, not confounding, and
  comparing odds ratios across the two models without a variance
  normalisation is not identified (ratio_is_rescaling).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P5

/-- The rescaling factor of an identified logit coefficient when omitted variance `v ≥ 0` joins the error variance `s > 0`. -/
def rescale (s v : ℝ) : ℝ := Real.sqrt (s / (s + v))

theorem rescale_pos (s v : ℝ) (hs : 0 < s) (hv : 0 ≤ v) : 0 < rescale s v := by
  unfold rescale; apply Real.sqrt_pos.2; positivity

theorem rescale_le_one (s v : ℝ) (hs : 0 < s) (hv : 0 ≤ v) : rescale s v ≤ 1 := by
  unfold rescale
  rw [Real.sqrt_le_one]
  rw [div_le_one (by positivity)]; linarith

theorem rescale_lt_one (s v : ℝ) (hs : 0 < s) (hv : 0 < v) : rescale s v < 1 := by
  unfold rescale
  rw [Real.sqrt_lt' (by norm_num : (0:ℝ) < 1), one_pow, div_lt_one (by positivity)]; linarith

theorem rescale_eq_one_iff (s v : ℝ) (hs : 0 < s) (hv : 0 ≤ v) : rescale s v = 1 ↔ v = 0 := by
  unfold rescale
  constructor
  · intro h
    have h2 : s / (s + v) = 1 := by
      have := congrArg (fun t => t ^ 2) h
      simp only [Real.sq_sqrt (by positivity : (0:ℝ) ≤ s / (s + v)), one_pow] at this
      exact this
    rw [div_eq_one_iff_eq (by positivity)] at h2; linarith
  · intro h; subst h; simp [div_self hs.ne']

/-- The identified coefficient in the reduced model equals the full-model one times the rescaling. -/
theorem reduced_coefficient (β s v : ℝ) (hs : 0 < s) (hv : 0 ≤ v) :
    β / Real.sqrt (s + v) = (β / Real.sqrt s) * rescale s v := by
  unfold rescale
  rw [Real.sqrt_div hs.le]
  have h1 : Real.sqrt s ≠ 0 := (Real.sqrt_pos.2 hs).ne'
  have h2 : Real.sqrt (s + v) ≠ 0 := (Real.sqrt_pos.2 (by positivity)).ne'
  field_simp

/-- The odds-ratio "change" between the two models is exp((c − 1) β) even with u ⟂ x: pure rescaling. -/
theorem ratio_is_rescaling (β s v : ℝ) (hs : 0 < s) (hv : 0 ≤ v) :
    Real.exp (β / Real.sqrt (s + v)) / Real.exp (β / Real.sqrt s)
      = Real.exp ((rescale s v - 1) * (β / Real.sqrt s)) := by
  rw [reduced_coefficient β s v hs hv, ← Real.exp_sub]
  congr 1; ring

end Research.P5
