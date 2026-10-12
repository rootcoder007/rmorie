/-
  Researchproofs/P7Chernoff.lean

  P7, continued: the Chernoff tail of a Poisson count (library scan C11;
  Grimmett & Stirzaker Theorem 5.11.4, Problem 5.11.5).

  For a random count S whose moment generating function is e^{λ(e^t − 1)} (a
  Poisson(λ), or the sum of independent Poissons), Markov's inequality on
  e^{tS} at t = log(1 + ε) gives
      P(S ≥ (1 + ε)λ) ≤ exp(−λ[(1 + ε) log(1 + ε) − ε])          (`poisson_chernoff`),
  with a strictly positive rate (`rate_pos`): the top share of places under the
  Poisson null decays exponentially in λ. The bound needs the generating
  function; a Cauchy summand has none, and the argument says nothing there.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open MeasureTheory ProbabilityTheory Real

namespace Research.P7.Chernoff

theorem rate_pos (ε : ℝ) (hε : 0 < ε) : 0 < (1 + ε) * log (1 + ε) - ε := by
  have h1 : 0 < 1 + ε := by linarith
  have h2 : log (1 / (1 + ε)) < 1 / (1 + ε) - 1 :=
    Real.log_lt_sub_one_of_pos (by positivity) (by
      intro h; rw [div_eq_one_iff_eq h1.ne'] at h; linarith)
  rw [Real.log_div one_ne_zero h1.ne', Real.log_one] at h2
  have : (1 + ε) * (0 - log (1 + ε)) < (1 + ε) * (1 / (1 + ε) - 1) :=
    mul_lt_mul_of_pos_left h2 h1
  have e : (1 + ε) * (1 / (1 + ε) - 1) = -ε := by field_simp; ring
  linarith

theorem poisson_chernoff {Ω : Type*} [MeasurableSpace Ω] (μ : Measure Ω) [IsFiniteMeasure μ]
    (S : Ω → ℝ) (lam ε : ℝ) (hε : 0 < ε)
    (hmgf : mgf S μ (log (1 + ε)) = exp (lam * (exp (log (1 + ε)) - 1)))
    (hint : Integrable (fun ω => exp (log (1 + ε) * S ω)) μ) :
    (μ {ω | (1 + ε) * lam ≤ S ω}).toReal ≤ exp (-(lam * ((1 + ε) * log (1 + ε) - ε))) := by
  have ht : 0 ≤ log (1 + ε) := Real.log_nonneg (by linarith)
  have h := measure_ge_le_exp_mul_mgf ((1 + ε) * lam) ht hint
  rw [hmgf, Real.exp_log (by linarith), ← Real.exp_add] at h
  refine h.trans (le_of_eq ?_)
  congr 1; ring

end Research.P7.Chernoff
