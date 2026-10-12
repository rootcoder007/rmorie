/-
  Researchproofs/P8Instrument.lean

  P8, continued: the nonlinear expected-cost response and the instrument
  condition for one dimension.

  Expected cost of an offence under certainty p, severity s and celerity
  (delay) c with discount rate ρ is p · s · e^{−ρ c} (Becker 1968; Nagin
  2013). Its logarithm is linear in (log p, log s, c):

    * logcost_linear: log(p s e^{−ρc}) = log p + log s − ρ c, so the
      response y = β₀ + β₁ log p + β₂ log s + β₃ c is the expected-cost
      model with (β₁, β₂, β₃) = (β, β, −βρ) for an elasticity β — the
      three dimensions have the SAME coefficient on the log scale up to
      the discount; a design that varies them on the log scale decides
      the elasticities exactly as P8Deterrence's rank condition says.
    * ratio_is_discount: with a common elasticity, β₃/β₁ = −ρ identifies
      the discount rate from the celerity-to-certainty coefficient ratio.

  The instrument condition (Angrist & Krueger 2001): with
  y_i = α + β p_i + u_i on a finite population and an instrument z,

    * wald_identifies: if cov(z, u) = 0 and cov(z, p) ≠ 0 then
      β = cov(z, y) / cov(z, p) — the Wald ratio, with finite covariances
      (no model of the sample beyond the two stated conditions).
    * wald_undefined: cov(z, p) = 0 leaves β unidentified: every β gives
      the same cov(z, y) once u absorbs the difference (relevance is the
      condition, not a convenience).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P8.Instrument

theorem logcost_linear (p s c ρ : ℝ) (hp : 0 < p) (hs : 0 < s) :
    Real.log (p * s * Real.exp (-ρ * c)) = Real.log p + Real.log s - ρ * c := by
  rw [Real.log_mul (by positivity) (Real.exp_pos _).ne', Real.log_mul hp.ne' hs.ne', Real.log_exp]
  ring

theorem ratio_is_discount (β ρ : ℝ) (hβ : β ≠ 0) : (-(β * ρ)) / β = -ρ := by
  field_simp

variable {n : ℕ}

/-- Finite-population covariance. -/
def cov (a b : Fin n → ℝ) : ℝ :=
  (∑ i, a i * b i) / n - ((∑ i, a i) / n) * ((∑ i, b i) / n)

theorem cov_add (a b c : Fin n → ℝ) : cov a (fun i => b i + c i) = cov a b + cov a c := by
  unfold cov
  simp only [mul_add, sum_add_distrib, add_div]
  ring

theorem cov_smul (a b : Fin n → ℝ) (k : ℝ) : cov a (fun i => k * b i) = k * cov a b := by
  unfold cov
  simp only [mul_left_comm _ k, ← mul_sum, mul_div_assoc]
  ring

theorem cov_const (a : Fin n → ℝ) (k : ℝ) (hn : 0 < n) : cov a (fun _ => k) = 0 := by
  unfold cov
  have : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp only [sum_const, card_univ, Fintype.card_fin, nsmul_eq_mul, ← sum_mul]
  field_simp
  ring

/-- The Wald ratio under exclusion (`cov z u = 0`) and relevance (`cov z p ≠ 0`). -/
theorem wald_identifies (z p u : Fin n → ℝ) (α β : ℝ) (hn : 0 < n)
    (hexcl : cov z u = 0) (hrel : cov z p ≠ 0) :
    cov z (fun i => α + β * p i + u i) / cov z p = β := by
  have h : cov z (fun i => α + β * p i + u i) = β * cov z p := by
    have e1 : (fun i => α + β * p i + u i) = fun i => (α + β * p i) + u i := rfl
    rw [e1, cov_add]
    have e2 : (fun i => α + β * p i) = fun i => (fun _ => α) i + (fun i => β * p i) i := rfl
    rw [e2, cov_add, cov_const z α hn, cov_smul, hexcl]
    ring
  rw [h]
  field_simp

/-- Without relevance, two different slopes produce the same instrument covariance. -/
theorem wald_undefined (z p u : Fin n → ℝ) (α β β' : ℝ) (hn : 0 < n)
    (hexcl : cov z u = 0) (hirr : cov z p = 0) :
    cov z (fun i => α + β * p i + u i) = cov z (fun i => α + β' * p i + u i) := by
  have key : ∀ b : ℝ, cov z (fun i => α + b * p i + u i) = 0 := by
    intro b
    have e1 : (fun i => α + b * p i + u i) = fun i => (α + b * p i) + u i := rfl
    rw [e1, cov_add]
    have e2 : (fun i => α + b * p i) = fun i => (fun _ => α) i + (fun i => b * p i) i := rfl
    rw [e2, cov_add, cov_const z α hn, cov_smul, hexcl, hirr]
    ring
  rw [key β, key β']

end Research.P8.Instrument
