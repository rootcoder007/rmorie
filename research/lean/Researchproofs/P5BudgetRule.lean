/-
  Researchproofs/P5BudgetRule.lean

  P5, continued: the resource-constrained optimal treatment rule is a
  threshold on the conditional effect, randomised at ties (library scan C22;
  Luedtke & van der Laan 2016, Theorems 1-2).

  Covariate values x with masses m_x > 0 and conditional effects τ_x; a rule
  d_x ∈ [0, 1] is the treated share at x, feasible when Σ m_x d_x ≤ κ.
    * `threshold_optimal`: a rule that treats everyone above a level t ≥ 0,
      no one below it, and spends the whole budget, maximises Σ m_x d_x τ_x
      over all feasible rules (a one-line exchange argument).
    * `stochastic_needed`: with one covariate value of mass 1, effect 1 and
      budget 1/2, no deterministic rule (treat all or none) attains the
      optimum: treating all is infeasible, treating none earns 0, while the
      fractional rule 1/2 is feasible and earns 1/2. A deterministic rule can
      be optimal only when the effect has no atom at the threshold.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Finset

namespace Research.P5.BudgetRule

variable {X : Type*} [Fintype X]

def value (m τ d : X → ℝ) : ℝ := ∑ x, m x * d x * τ x
def spend (m d : X → ℝ) : ℝ := ∑ x, m x * d x

theorem threshold_optimal (m τ d dstar : X → ℝ) (κ t : ℝ) (ht : 0 ≤ t)
    (hm : ∀ x, 0 ≤ m x)
    (hd0 : ∀ x, 0 ≤ d x) (hd1 : ∀ x, d x ≤ 1) (hfeas : spend m d ≤ κ)
    (hstar0 : ∀ x, 0 ≤ dstar x) (hstar1 : ∀ x, dstar x ≤ 1)
    (habove : ∀ x, t < τ x → dstar x = 1) (hbelow : ∀ x, τ x < t → dstar x = 0)
    (hbind : spend m dstar = κ) :
    value m τ d ≤ value m τ dstar := by
  -- pointwise: m d (τ − t) ≤ m d* (τ − t)
  have hpt : ∀ x, m x * d x * (τ x - t) ≤ m x * dstar x * (τ x - t) := by
    intro x
    rcases lt_trichotomy (τ x) t with h | h | h
    · rw [hbelow x h, mul_zero, zero_mul]
      nlinarith [mul_nonneg (hm x) (hd0 x)]
    · rw [h]; simp
    · rw [habove x h, mul_one]
      have h1 : 0 ≤ m x * (τ x - t) := mul_nonneg (hm x) (by linarith)
      nlinarith [mul_le_mul_of_nonneg_left (hd1 x) h1]
  have hsum : ∑ x, m x * d x * (τ x - t) ≤ ∑ x, m x * dstar x * (τ x - t) := sum_le_sum fun x _ => hpt x
  have e1 : ∑ x, m x * d x * (τ x - t) = value m τ d - t * spend m d := by
    unfold value spend; rw [mul_sum, ← sum_sub_distrib]; apply sum_congr rfl; intro x _; ring
  have e2 : ∑ x, m x * dstar x * (τ x - t) = value m τ dstar - t * spend m dstar := by
    unfold value spend; rw [mul_sum, ← sum_sub_distrib]; apply sum_congr rfl; intro x _; ring
  rw [e1, e2, hbind] at hsum
  nlinarith [mul_le_mul_of_nonneg_left hfeas ht]

/-- One covariate value, mass 1, effect 1, budget 1/2. -/
theorem stochastic_needed :
    ¬ ((1 : ℝ) * 1 ≤ 1 / 2) ∧ (1 : ℝ) * 0 * 1 = 0 ∧ (1 : ℝ) * (1 / 2) ≤ 1 / 2 ∧ (0 : ℝ) < 1 * (1 / 2) * 1 := by
  norm_num

end Research.P5.BudgetRule
