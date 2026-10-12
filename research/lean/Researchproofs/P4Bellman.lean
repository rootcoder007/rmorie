/-
  Researchproofs/P4Bellman.lean

  P4/P8, continued: the Bellman operator is a contraction and the myopic
  hot-spot rule is not optimal (library scan C21; Puterman Theorems 6.2.3,
  6.2.5, Proposition 6.2.4).

  Finite states and actions, rewards r(s, a), row-stochastic transitions
  p(s, a, ·), discount γ ∈ [0, 1). The operator
      (T v)(s) = max_a [r(s, a) + γ Σ_{s'} p(s, a, s') v(s')]
  satisfies ‖T v − T w‖_∞ ≤ γ ‖v − w‖_∞ (`bellman_lipschitz`), so it is a
  contraction (`bellman_contracting`) with a unique fixed point, the optimal
  value (`optimal_value_exists_unique`), reached by value iteration.
  `myopic_suboptimal`: a three-state example in which the greedy action
  (reward 1 now, then 1/2 per period) is beaten by the patient action (0 now,
  then 1 per period) whenever γ > 2/3: allocating to the highest predicted
  rate suppresses the observation that would have paid later.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Finset

noncomputable section

namespace Research.P4.Bellman

variable {S A : Type*} [Fintype S] [Fintype A] [Nonempty A]

structure MDP (S A : Type*) [Fintype S] [Fintype A] where
  r : S → A → ℝ
  p : S → A → S → ℝ
  γ : ℝ
  p_nonneg : ∀ s a s', 0 ≤ p s a s'
  p_sum : ∀ s a, ∑ s', p s a s' = 1
  γ_nonneg : 0 ≤ γ
  γ_lt_one : γ < 1

/-- The Bellman optimality operator. -/
def T (M : MDP S A) (v : S → ℝ) (s : S) : ℝ :=
  univ.sup' univ_nonempty fun a => M.r s a + M.γ * ∑ s', M.p s a s' * v s'

theorem abs_sup'_sub_sup'_le {ι : Type*} (s : Finset ι) (hs : s.Nonempty) (f g : ι → ℝ) (c : ℝ)
    (h : ∀ i ∈ s, |f i - g i| ≤ c) : |s.sup' hs f - s.sup' hs g| ≤ c := by
  rw [abs_le]
  constructor
  · have : s.sup' hs g ≤ s.sup' hs f + c := by
      apply sup'_le; intro i hi
      have := abs_le.mp (h i hi)
      calc g i ≤ f i + c := by linarith [this.1]
        _ ≤ s.sup' hs f + c := by linarith [le_sup' f hi]
    linarith
  · have : s.sup' hs f ≤ s.sup' hs g + c := by
      apply sup'_le; intro i hi
      have := abs_le.mp (h i hi)
      calc f i ≤ g i + c := by linarith [this.2]
        _ ≤ s.sup' hs g + c := by linarith [le_sup' g hi]
    linarith

theorem bellman_pointwise (M : MDP S A) (v w : S → ℝ) (s : S) :
    |T M v s - T M w s| ≤ M.γ * dist v w := by
  unfold T
  apply abs_sup'_sub_sup'_le
  intro a _
  have : (M.r s a + M.γ * ∑ s', M.p s a s' * v s') - (M.r s a + M.γ * ∑ s', M.p s a s' * w s')
      = M.γ * ∑ s', M.p s a s' * (v s' - w s') := by
    simp only [mul_sum, mul_sub, sum_sub_distrib]; ring
  rw [this, abs_mul, abs_of_nonneg M.γ_nonneg]
  apply mul_le_mul_of_nonneg_left _ M.γ_nonneg
  calc |∑ s', M.p s a s' * (v s' - w s')| ≤ ∑ s', |M.p s a s' * (v s' - w s')| := abs_sum_le_sum_abs _ _
    _ = ∑ s', M.p s a s' * |v s' - w s'| := by
        apply sum_congr rfl; intro s' _; rw [abs_mul, abs_of_nonneg (M.p_nonneg s a s')]
    _ ≤ ∑ s', M.p s a s' * dist v w := by
        apply sum_le_sum; intro s' _
        apply mul_le_mul_of_nonneg_left _ (M.p_nonneg s a s')
        rw [← Real.dist_eq]; exact dist_le_pi_dist v w s'
    _ = dist v w := by rw [← sum_mul, M.p_sum, one_mul]

theorem bellman_lipschitz (M : MDP S A) (v w : S → ℝ) :
    dist (T M v) (T M w) ≤ M.γ * dist v w := by
  rw [dist_pi_le_iff (mul_nonneg M.γ_nonneg dist_nonneg)]
  intro s
  rw [Real.dist_eq]
  exact bellman_pointwise M v w s

theorem bellman_contracting (M : MDP S A) : ContractingWith ⟨M.γ, M.γ_nonneg⟩ (T M) := by
  refine ⟨?_, LipschitzWith.of_dist_le_mul fun v w => ?_⟩
  · exact_mod_cast M.γ_lt_one
  · exact bellman_lipschitz M v w

theorem optimal_value_exists_unique (M : MDP S A) : ∃! v : S → ℝ, T M v = v := by
  have h := bellman_contracting M
  refine ⟨h.fixedPoint (T M), h.fixedPoint_isFixedPt, fun v hv => h.fixedPoint_unique hv⟩

/-- Policy values in the three-state example: greedy `1 + γ·(1/2)/(1 − γ)`, patient `γ/(1 − γ)`. -/
def greedyValue (γ : ℝ) : ℝ := 1 + γ * (1 / 2) / (1 - γ)
def patientValue (γ : ℝ) : ℝ := γ / (1 - γ)

theorem greedy_value_fixed (γ : ℝ) (hγ : γ < 1) :
    greedyValue γ = 1 + γ * ((1 / 2) / (1 - γ)) ∧ (1 / 2) / (1 - γ) = 1 / 2 + γ * ((1 / 2) / (1 - γ)) := by
  have : 1 - γ ≠ 0 := by linarith
  constructor
  · unfold greedyValue; ring
  · field_simp; ring

theorem patient_value_fixed (γ : ℝ) (hγ : γ < 1) :
    patientValue γ = 0 + γ * (1 / (1 - γ)) ∧ 1 / (1 - γ) = 1 + γ * (1 / (1 - γ)) := by
  have : 1 - γ ≠ 0 := by linarith
  constructor
  · unfold patientValue; ring
  · field_simp; ring

theorem myopic_suboptimal (γ : ℝ) (hγ0 : 0 ≤ γ) (hγ : γ < 1) :
    greedyValue γ < patientValue γ ↔ 2 / 3 < γ := by
  unfold greedyValue patientValue
  have h1 : 0 < 1 - γ := by linarith
  have e : 1 + γ * (1 / 2) / (1 - γ) = (1 - γ / 2) / (1 - γ) := by field_simp; ring
  rw [e, div_lt_div_iff_of_pos_right h1]
  constructor <;> intro h <;> linarith

end Research.P4.Bellman
