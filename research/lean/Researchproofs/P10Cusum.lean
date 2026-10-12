/-
  Researchproofs/P10Cusum.lean

  P10, continued: a restarted stopping rule with false-alarm probability α has
  mean run length bounded below (library scan C23; Lai 1995, Lemma 1).

  Copies τ_k (k = 0, 1, …) of a stopping rule, the k-th started at time k, each
  with P(τ_k < ∞) ≤ α under no change; the detector stops at
  N = inf_k (k + τ_k).
    * `stop_subset_union`: {N < n + 1} ⊆ ⋃_{k ≤ n} {τ_k < ∞}.
    * `stop_prob_le`: P(N ≤ n) ≤ (n + 1) α.
    * `mean_run_length_ge`: with the tail-sum expectation
      E N = Σ_{n ≥ 0} P(N > n), for every J with J α ≤ 1,
      E N ≥ Σ_{n < J} (1 − n α) ≥ J/2 (`partial_sum_ge_half`); taking
      J = ⌊1/α⌋ gives E N ≥ (1/α − 1)/2. Lorden's sharper 1/α constant and the
      CUSUM optimality are not formalised; the restart argument's
      union bound is.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open MeasureTheory Finset ENNReal

noncomputable section

namespace Research.P10.Cusum

variable {Ω : Type*} [MeasurableSpace Ω]

/-- The detector's stopping time from restarted copies. -/
def N (τ : ℕ → Ω → ℕ∞) (ω : Ω) : ℕ∞ := ⨅ k : ℕ, ((k : ℕ∞) + τ k ω)

theorem stop_subset_union (τ : ℕ → Ω → ℕ∞) (n : ℕ) :
    {ω | N τ ω < ((n + 1 : ℕ) : ℕ∞)} ⊆ ⋃ k ∈ range (n + 1), {ω | τ k ω < ⊤} := by
  intro ω hω
  simp only [Set.mem_setOf_eq, N, iInf_lt_iff] at hω
  obtain ⟨k, hk⟩ := hω
  simp only [Set.mem_iUnion, Set.mem_setOf_eq, mem_range, exists_prop]
  refine ⟨k, ?_, ?_⟩
  · have : (k : ℕ∞) ≤ k + τ k ω := le_self_add
    have := lt_of_le_of_lt this hk
    exact_mod_cast this
  · exact lt_of_le_of_lt le_add_self (lt_of_lt_of_le hk le_top)

theorem stop_prob_le (μ : Measure Ω) (τ : ℕ → Ω → ℕ∞) (α : ℝ≥0∞) (n : ℕ)
    (hα : ∀ k, μ {ω | τ k ω < ⊤} ≤ α) :
    μ {ω | N τ ω < ((n + 1 : ℕ) : ℕ∞)} ≤ (n + 1) * α := by
  calc μ {ω | N τ ω < ((n + 1 : ℕ) : ℕ∞)} ≤ μ (⋃ k ∈ range (n + 1), {ω | τ k ω < ⊤}) :=
        measure_mono (stop_subset_union τ n)
    _ ≤ ∑ k ∈ range (n + 1), μ {ω | τ k ω < ⊤} := measure_biUnion_finset_le _ _
    _ ≤ ∑ k ∈ range (n + 1), α := sum_le_sum fun k _ => hα k
    _ = (n + 1) * α := by simp

/-- Tail-sum expectation of an `ℕ∞`-valued time from its survival probabilities `s n = P(N > n)`:
with `s n ≥ 1 − (n + 1)α` in `[0, 1]`, the partial sums bound it below. -/
theorem partial_sum_ge_half (α : ℝ) (J : ℕ) (hα : 0 ≤ α) (hJ : (J : ℝ) * α ≤ 1) :
    (J : ℝ) / 2 ≤ ∑ n ∈ range J, (1 - n * α) := by
  have hsum : ∑ n ∈ range J, (1 - (n : ℝ) * α) = J - α * ∑ n ∈ range J, (n : ℝ) := by
    rw [sum_sub_distrib, sum_const, card_range, nsmul_eq_mul, mul_one, mul_sum]
    congr 1; apply sum_congr rfl; intro n _; ring
  have hgauss : ∀ J : ℕ, ∑ n ∈ range J, (n : ℝ) = J * (J - 1) / 2 := by
    intro J
    induction J with
    | zero => simp
    | succ J ih => rw [sum_range_succ, ih]; push_cast; ring
  rw [hsum, hgauss J]
  have hJ0 : (0 : ℝ) ≤ J := by positivity
  nlinarith

theorem mean_run_length_ge (s : ℕ → ℝ) (α : ℝ) (J : ℕ) (hα : 0 ≤ α) (hJ : (J : ℝ) * α ≤ 1)
    (hs0 : ∀ n, 0 ≤ s n) (hs : ∀ n, 1 - n * α ≤ s n) (hsum : Summable s) :
    (J : ℝ) / 2 ≤ ∑' n, s n := by
  calc (J : ℝ) / 2 ≤ ∑ n ∈ range J, (1 - n * α) := partial_sum_ge_half α J hα hJ
    _ ≤ ∑ n ∈ range J, s n := sum_le_sum fun n _ => hs n
    _ ≤ ∑' n, s n := hsum.sum_le_tsum (range J) (fun n _ => hs0 n)

end Research.P10.Cusum
