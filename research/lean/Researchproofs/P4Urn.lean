/-
  Researchproofs/P4Urn.lean

  P4, the stochastic version: the discovery process as an urn.

  The mean-field recursion of P4Feedback replaces each random discovery
  by its expectation. The stochastic process draws one discovery per
  step: with probability equal to the current share x the discovery is
  in region A and A's count grows by one, otherwise B's does. Two facts
  are proved here on the finite path space (no measure theory is needed,
  the law after n steps is a finite probability vector):

  * urn_step_martingale: the share is a martingale — the expected next
    share given the current counts is the current share (Grimmett &
    Stirzaker, Problem 12.9.14). This holds for any reinforcement a > 0.

  * polya_uniform: from one count each (r₀ = b₀ = 1, a = 1) the number of
    A-discoveries after n draws is exactly uniform on {0, …, n}, so the
    share (1+j)/(n+2) is uniform on its grid. Consequently
    (polya_no_concentration) the probability that the share lies in
    [1/4, 3/4] is at least 1/4 at every horizon n ≥ 2. With equal true
    rates the mean field says the share never moves (naive_step_drift
    with λA = λB); the urn says the share converges to a random limit
    that is uniform on (0, 1). "Equal rates, therefore no runaway" is
    false for the stochastic process: with probability about one quarter
    the department ends up sending at least three quarters of its patrol
    to one of two identical regions, on the strength of early luck alone.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P4.Urn

/-- One-step martingale identity for the share `r / (r + b)` with reinforcement `a`. -/
theorem urn_step_martingale (r b a : ℝ) (hr : 0 < r) (hb : 0 < b) (_ha : 0 ≤ a) :
    (r / (r + b)) * ((r + a) / (r + b + a)) + (1 - r / (r + b)) * (r / (r + b + a)) = r / (r + b) := by
  have h1 : r + b ≠ 0 := by positivity
  have h2 : r + b + a ≠ 0 := by positivity
  field_simp
  ring

/-- Law of the number of A-discoveries after `n` draws of the Pólya urn started at one count each:
`q n j` is the probability of `j` A-discoveries. -/
def q : ℕ → ℕ → ℝ
  | 0, 0 => 1
  | 0, _ + 1 => 0
  | n + 1, 0 => q n 0 * (1 - 1 / ((n : ℝ) + 2))
  | n + 1, j + 1 => q n j * ((1 + (j : ℝ)) / ((n : ℝ) + 2)) + q n (j + 1) * (1 - (1 + ((j : ℝ) + 1)) / ((n : ℝ) + 2))

theorem q_zero_of_gt : ∀ n j, n < j → q n j = 0 := by
  intro n
  induction n with
  | zero =>
    intro j hj
    obtain ⟨k, rfl⟩ : ∃ k, j = k + 1 := ⟨j - 1, by omega⟩
    simp [q]
  | succ n ih =>
    intro j hj
    obtain ⟨k, rfl⟩ : ∃ k, j = k + 1 := ⟨j - 1, by omega⟩
    simp only [q]
    rw [ih k (by omega), ih (k + 1) (by omega)]
    ring

/-- The law is uniform: `q n j = 1 / (n + 1)` for every `j ≤ n`. -/
theorem polya_uniform : ∀ n j, j ≤ n → q n j = 1 / ((n : ℝ) + 1) := by
  intro n
  induction n with
  | zero =>
    intro j hj
    have : j = 0 := by omega
    subst this
    simp [q]
  | succ n ih =>
    intro j hj
    have hn2 : ((n : ℝ) + 2) ≠ 0 := by positivity
    have hn1 : ((n : ℝ) + 1) ≠ 0 := by positivity
    rcases j with _ | k
    · simp only [q]
      rw [ih 0 (Nat.zero_le _)]
      push_cast
      field_simp
      ring
    · simp only [q]
      rw [ih k (by omega)]
      rcases Nat.lt_or_ge n (k + 1) with h | h
      · rw [q_zero_of_gt n (k + 1) h]
        have hk : k = n := by omega
        subst hk
        push_cast
        field_simp
        ring
      · rw [ih (k + 1) h]
        push_cast
        field_simp
        ring

/-- The share after `n` draws when `j` discoveries went to A. -/
def share (n j : ℕ) : ℝ := (1 + (j : ℝ)) / ((n : ℝ) + 2)

/-- Probability that the share lies in `[1/4, 3/4]` after `n` draws. -/
def probMiddle (n : ℕ) : ℝ :=
  ∑ j ∈ (range (n + 1)).filter (fun j => 1 / 4 ≤ share n j ∧ share n j ≤ 3 / 4), q n j

theorem middle_superset (n : ℕ) :
    Icc (n / 4 + 1) (n / 2 + n / 4) ⊆
      (range (n + 1)).filter (fun j => 1 / 4 ≤ share n j ∧ share n j ≤ 3 / 4) := by
  intro j hj
  rw [mem_Icc] at hj
  rw [mem_filter, mem_range]
  refine ⟨by omega, ?_, ?_⟩
  · unfold share
    rw [div_le_div_iff₀ (by norm_num) (by positivity)]
    have : (n : ℝ) + 2 ≤ 4 * (1 + (j : ℝ)) := by
      have h : n + 2 ≤ 4 * (1 + j) := by omega
      exact_mod_cast h
    linarith
  · unfold share
    rw [div_le_div_iff₀ (by positivity) (by norm_num)]
    have : 4 * (1 + (j : ℝ)) ≤ 3 * ((n : ℝ) + 2) := by
      have h : 4 * (1 + j) ≤ 3 * (n + 2) := by omega
      exact_mod_cast h
    linarith

/-- No concentration at the ends: for every horizon `n ≥ 2` the share sits in `[1/4, 3/4]`
with probability at least `1/4`. -/
theorem polya_no_concentration (n : ℕ) (hn : 2 ≤ n) : 1 / 4 ≤ probMiddle n := by
  unfold probMiddle
  set S := (range (n + 1)).filter (fun j => 1 / 4 ≤ share n j ∧ share n j ≤ 3 / 4)
  have hconst : ∀ j ∈ S, q n j = 1 / ((n : ℝ) + 1) := by
    intro j hj
    rw [mem_filter, mem_range] at hj
    exact polya_uniform n j (by omega)
  rw [sum_congr rfl hconst, sum_const, nsmul_eq_mul]
  have hcard : n / 2 ≤ S.card := by
    have h := card_le_card (middle_superset n)
    rw [Nat.card_Icc] at h
    have : n / 2 + n / 4 + 1 - (n / 4 + 1) = n / 2 := by omega
    rw [this] at h
    exact h
  have hcard' : ((n / 2 : ℕ) : ℝ) ≤ (S.card : ℝ) := by exact_mod_cast hcard
  have hhalf : ((n : ℝ) + 1) / 4 ≤ ((n / 2 : ℕ) : ℝ) := by
    have h : n + 1 ≤ 4 * (n / 2) := by omega
    have h' : ((n : ℝ) + 1) ≤ 4 * ((n / 2 : ℕ) : ℝ) := by exact_mod_cast h
    linarith
  have hn1 : (0 : ℝ) < (n : ℝ) + 1 := by positivity
  rw [div_le_iff₀ (by norm_num : (0 : ℝ) < 4)]
  calc (1 : ℝ) = ((n : ℝ) + 1) * (1 / ((n : ℝ) + 1)) := by field_simp
    _ ≤ (4 * ((n / 2 : ℕ) : ℝ)) * (1 / ((n : ℝ) + 1)) := by
        apply mul_le_mul_of_nonneg_right _ (by positivity); linarith
    _ ≤ (4 * (S.card : ℝ)) * (1 / ((n : ℝ) + 1)) := by
        apply mul_le_mul_of_nonneg_right _ (by positivity); linarith
    _ = (S.card : ℝ) * (1 / ((n : ℝ) + 1)) * 4 := by ring

end Research.P4.Urn
