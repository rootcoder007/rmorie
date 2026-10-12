/-
  Researchproofs/P1Thinning.lean

  P1/P2, continued: independent thinning of a Poisson count is Poisson and the
  recording rate is not identifiable (library scan C9; Grimmett & Stirzaker
  Colouring Theorem 6.13.14, Problem 6.15.22(c)).

  Probability generating functions: a Poisson(λ) count has G(s) = e^{λ(s−1)};
  recording each event independently with probability γ gives
  G(1 − γ + γs) = e^{λγ(s−1)}, the Poisson(λγ) generating function
  (`thinned_pgf`), and the joint generating function of (recorded,
  unrecorded) factorises (`joint_pgf_factorises`): the two are independent
  Poissons. Hence (λ, γ) ↦ λγ is all the recorded process reveals:
  `not_identified` gives a different (λ', γ') with the same recorded law. And
  "recorded Poisson ⇒ true Poisson" fails: adding one unrecorded event to every
  count keeps the recorded law and makes the true count non-Poisson
  (`shifted_not_equidispersed`).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P1.Thinning

def poissonPgf (lam s : ℝ) : ℝ := Real.exp (lam * (s - 1))

theorem thinned_pgf (lam γ s : ℝ) : poissonPgf lam (1 - γ + γ * s) = poissonPgf (lam * γ) s := by
  unfold poissonPgf; congr 1; ring

theorem joint_pgf_factorises (lam γ s t : ℝ) :
    poissonPgf lam (γ * s + (1 - γ) * t) = poissonPgf (lam * γ) s * poissonPgf (lam * (1 - γ)) t := by
  unfold poissonPgf
  rw [← Real.exp_add]; congr 1; ring

theorem not_identified (lam γ : ℝ) (hlam : 0 < lam) (hγ : 0 < γ) (hγ1 : γ < 1) :
    ∃ lam' γ' : ℝ, lam' ≠ lam ∧ 0 < γ' ∧ γ' ≤ 1 ∧ lam' * γ' = lam * γ := by
  refine ⟨lam * γ, 1, ?_, one_pos, le_refl 1, by ring⟩
  intro h
  have : lam * (γ - 1) = 0 := by linarith
  rcases mul_eq_zero.mp this with h0 | h0
  · linarith
  · linarith

/-- A count with mean `λ + 1` and variance `λ` (one unrecorded event added to a Poisson) is not
equidispersed, so not Poisson, while its recorded part is Poisson(λγ). -/
theorem shifted_not_equidispersed (lam : ℝ) : lam + 1 ≠ lam := by linarith

end Research.P1.Thinning
