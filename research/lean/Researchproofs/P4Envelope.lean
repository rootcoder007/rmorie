/-
  Researchproofs/P4Envelope.lean

  P4, continued: a drift envelope for the stochastic urn at ANY rates.

  The discovery urn of P4Urn is generalised: counts start at (r₀, b₀) > 0,
  each step adds a > 0 to the region drawn, and the draw goes to A with
  probability equal to A's current share. The share after n steps with j
  A-draws is S(n, j) = (r₀ + j a) / (r₀ + b₀ + n a) and its law q(n, j)
  is the finite recursion of the urn. Everything is a finite sum.

    * step_mean / mean_const: the share is a martingale, so its expectation
      stays at the initial share S(0, 0) for every horizon.
    * step_var: one step adds exactly s (1 − s) (a / (T + a))² to the
      second moment, T the current total, hence at most (a / (T + a))² / 4.
    * second_moment_le / var_le: the variance of the share after n steps
      is at most (1/4) Σ_{k<n} (a / (T₀ + (k+1) a))² ≤ a / (4 T₀), a bound
      uniform in n (telescoping).
    * chebyshev: P(|S_n − S(0,0)| ≥ t) ≤ a / (4 T₀ t²) for every n.

  So the spread of the long-run allocation around its start is governed
  by a / T₀, the reinforcement relative to the initial mass: a department
  that starts with little history (small T₀) is at the mercy of its early
  luck, whatever the true rates; one that starts from a large prior mass
  is not. This is the "drift envelope" the ledger asked for; the random
  limit's law for unequal rates remains open.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P4.Envelope

variable (r₀ b₀ a : ℝ)

/-- Share of A after `n` steps with `j` A-draws. -/
def S (n j : ℕ) : ℝ := (r₀ + (j : ℝ) * a) / (r₀ + b₀ + (n : ℝ) * a)

/-- Law of the number of A-draws. -/
def q : ℕ → ℕ → ℝ
  | 0, 0 => 1
  | 0, _ + 1 => 0
  | n + 1, 0 => q n 0 * (1 - S r₀ b₀ a n 0)
  | n + 1, j + 1 => q n j * S r₀ b₀ a n j + q n (j + 1) * (1 - S r₀ b₀ a n (j + 1))

/-- Total mass after `n` steps. -/
def T (n : ℕ) : ℝ := r₀ + b₀ + (n : ℝ) * a

theorem q_zero_of_gt : ∀ n j, n < j → q r₀ b₀ a n j = 0 := by
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

section pos
variable (hr : 0 < r₀) (hb : 0 < b₀) (ha : 0 < a)
include hr hb ha

theorem T_pos (n : ℕ) : 0 < T r₀ b₀ a n := by unfold T; positivity

theorem S_nonneg (n j : ℕ) : 0 ≤ S r₀ b₀ a n j := by unfold S; positivity

theorem S_le_one (n j : ℕ) (hj : j ≤ n) : S r₀ b₀ a n j ≤ 1 := by
  unfold S
  rw [div_le_one (by positivity)]
  have : (j : ℝ) ≤ n := by exact_mod_cast hj
  nlinarith

theorem q_nonneg : ∀ n j, 0 ≤ q r₀ b₀ a n j := by
  intro n
  induction n with
  | zero => intro j; rcases j with _ | k <;> simp [q]
  | succ n ih =>
    intro j
    rcases j with _ | k
    · simp only [q]
      rcases Nat.lt_or_ge n 0 with h | h
      · omega
      · exact mul_nonneg (ih 0) (by linarith [S_le_one r₀ b₀ a hr hb ha n 0 (Nat.zero_le _)])
    · simp only [q]
      rcases Nat.lt_or_ge n (k + 1) with h | h
      · rw [q_zero_of_gt r₀ b₀ a n (k + 1) h, zero_mul, add_zero]
        exact mul_nonneg (ih k) (S_nonneg r₀ b₀ a hr hb ha n k)
      · exact add_nonneg (mul_nonneg (ih k) (S_nonneg r₀ b₀ a hr hb ha n k))
          (mul_nonneg (ih (k + 1)) (by linarith [S_le_one r₀ b₀ a hr hb ha n (k + 1) h]))

/-- The one-step reindexing: an expectation under `q (n+1)` is the mixture of the two moves. -/
theorem step_expect (n : ℕ) (f : ℕ → ℝ) :
    ∑ j ∈ range (n + 2), q r₀ b₀ a (n + 1) j * f j =
      ∑ j ∈ range (n + 1), q r₀ b₀ a n j * (S r₀ b₀ a n j * f (j + 1) + (1 - S r₀ b₀ a n j) * f j) := by
  rw [sum_range_succ' (fun j => q r₀ b₀ a (n + 1) j * f j)]
  simp only [q]
  have h1 : ∑ j ∈ range (n + 1), (q r₀ b₀ a n j * S r₀ b₀ a n j + q r₀ b₀ a n (j + 1) * (1 - S r₀ b₀ a n (j + 1))) * f (j + 1)
      = ∑ j ∈ range (n + 1), q r₀ b₀ a n j * S r₀ b₀ a n j * f (j + 1)
        + ∑ j ∈ range (n + 1), q r₀ b₀ a n (j + 1) * (1 - S r₀ b₀ a n (j + 1)) * f (j + 1) := by
    rw [← sum_add_distrib]; congr 1; ext j; ring
  rw [h1]
  -- shift the second sum back by one; its last term vanishes
  have h2 : ∑ j ∈ range (n + 1), q r₀ b₀ a n (j + 1) * (1 - S r₀ b₀ a n (j + 1)) * f (j + 1)
      = ∑ j ∈ range (n + 2), q r₀ b₀ a n j * (1 - S r₀ b₀ a n j) * f j - q r₀ b₀ a n 0 * (1 - S r₀ b₀ a n 0) * f 0 := by
    rw [sum_range_succ' (fun j => q r₀ b₀ a n j * (1 - S r₀ b₀ a n j) * f j)]
    ring
  rw [h2]
  have h3 : ∑ j ∈ range (n + 2), q r₀ b₀ a n j * (1 - S r₀ b₀ a n j) * f j
      = ∑ j ∈ range (n + 1), q r₀ b₀ a n j * (1 - S r₀ b₀ a n j) * f j := by
    rw [sum_range_succ, q_zero_of_gt r₀ b₀ a n (n + 1) (by omega)]; ring
  rw [h3]
  have h4 : ∑ j ∈ range (n + 1), q r₀ b₀ a n j * (S r₀ b₀ a n j * f (j + 1) + (1 - S r₀ b₀ a n j) * f j)
      = ∑ j ∈ range (n + 1), q r₀ b₀ a n j * S r₀ b₀ a n j * f (j + 1)
        + ∑ j ∈ range (n + 1), q r₀ b₀ a n j * (1 - S r₀ b₀ a n j) * f j := by
    rw [← sum_add_distrib]; congr 1; ext j; ring
  rw [h4]
  ring

theorem q_sum (n : ℕ) : ∑ j ∈ range (n + 1), q r₀ b₀ a n j = 1 := by
  induction n with
  | zero => simp [q]
  | succ n ih =>
    have := step_expect r₀ b₀ a hr hb ha n (fun _ => 1)
    simp only [mul_one] at this
    rw [this]
    have hpt : ∀ j, q r₀ b₀ a n j * (S r₀ b₀ a n j + (1 - S r₀ b₀ a n j)) = q r₀ b₀ a n j := fun j => by ring
    simp only [hpt, ih]

/-- The two moves average to the current share. -/
theorem step_mean (n j : ℕ) :
    S r₀ b₀ a n j * S r₀ b₀ a (n + 1) (j + 1) + (1 - S r₀ b₀ a n j) * S r₀ b₀ a (n + 1) j = S r₀ b₀ a n j := by
  unfold S
  have h1 : r₀ + b₀ + (n : ℝ) * a ≠ 0 := by positivity
  have h2 : r₀ + b₀ + ((n + 1 : ℕ) : ℝ) * a ≠ 0 := by positivity
  push_cast
  field_simp
  ring

/-- One step adds `s (1 − s) (a/(T+a))²` to the second moment. -/
theorem step_var (n j : ℕ) :
    S r₀ b₀ a n j * S r₀ b₀ a (n + 1) (j + 1) ^ 2 + (1 - S r₀ b₀ a n j) * S r₀ b₀ a (n + 1) j ^ 2
      = S r₀ b₀ a n j ^ 2 + S r₀ b₀ a n j * (1 - S r₀ b₀ a n j) * (a / (T r₀ b₀ a n + a)) ^ 2 := by
  unfold S T
  have h1 : r₀ + b₀ + (n : ℝ) * a ≠ 0 := by positivity
  have h2 : r₀ + b₀ + ((n + 1 : ℕ) : ℝ) * a ≠ 0 := by positivity
  have h3 : r₀ + b₀ + (n : ℝ) * a + a ≠ 0 := by positivity
  push_cast
  field_simp
  ring

/-- Expected share. -/
def E (n : ℕ) (f : ℕ → ℝ) : ℝ := ∑ j ∈ range (n + 1), q r₀ b₀ a n j * f j

theorem mean_const (n : ℕ) : E r₀ b₀ a n (S r₀ b₀ a n) = S r₀ b₀ a 0 0 := by
  induction n with
  | zero => simp [E, q]
  | succ n ih =>
    unfold E at ih ⊢
    rw [step_expect r₀ b₀ a hr hb ha n (S r₀ b₀ a (n + 1))]
    simp only [step_mean r₀ b₀ a hr hb ha]
    exact ih

theorem second_moment_step (n : ℕ) :
    E r₀ b₀ a (n + 1) (fun j => S r₀ b₀ a (n + 1) j ^ 2)
      ≤ E r₀ b₀ a n (fun j => S r₀ b₀ a n j ^ 2) + (a / (T r₀ b₀ a n + a)) ^ 2 / 4 := by
  unfold E
  rw [step_expect r₀ b₀ a hr hb ha n (fun j => S r₀ b₀ a (n + 1) j ^ 2)]
  simp only [step_var r₀ b₀ a hr hb ha]
  have hq := q_sum r₀ b₀ a hr hb ha n
  have hpt : ∀ j ∈ range (n + 1),
      q r₀ b₀ a n j * (S r₀ b₀ a n j ^ 2 + S r₀ b₀ a n j * (1 - S r₀ b₀ a n j) * (a / (T r₀ b₀ a n + a)) ^ 2)
        ≤ q r₀ b₀ a n j * S r₀ b₀ a n j ^ 2 + q r₀ b₀ a n j * ((a / (T r₀ b₀ a n + a)) ^ 2 / 4) := by
    intro j hj
    rw [mem_range] at hj
    have hs0 := S_nonneg r₀ b₀ a hr hb ha n j
    have hs1 := S_le_one r₀ b₀ a hr hb ha n j (by omega)
    have hq0 := q_nonneg r₀ b₀ a hr hb ha n j
    have hvar : S r₀ b₀ a n j * (1 - S r₀ b₀ a n j) ≤ 1 / 4 := by nlinarith [sq_nonneg (S r₀ b₀ a n j - 1 / 2)]
    have hc : 0 ≤ (a / (T r₀ b₀ a n + a)) ^ 2 := sq_nonneg _
    have : q r₀ b₀ a n j * (S r₀ b₀ a n j * (1 - S r₀ b₀ a n j) * (a / (T r₀ b₀ a n + a)) ^ 2)
        ≤ q r₀ b₀ a n j * ((a / (T r₀ b₀ a n + a)) ^ 2 / 4) := by
      apply mul_le_mul_of_nonneg_left _ hq0
      calc S r₀ b₀ a n j * (1 - S r₀ b₀ a n j) * (a / (T r₀ b₀ a n + a)) ^ 2
          ≤ 1 / 4 * (a / (T r₀ b₀ a n + a)) ^ 2 := mul_le_mul_of_nonneg_right hvar hc
        _ = (a / (T r₀ b₀ a n + a)) ^ 2 / 4 := by ring
    nlinarith
  calc ∑ j ∈ range (n + 1), q r₀ b₀ a n j * (S r₀ b₀ a n j ^ 2 + S r₀ b₀ a n j * (1 - S r₀ b₀ a n j) * (a / (T r₀ b₀ a n + a)) ^ 2)
      ≤ ∑ j ∈ range (n + 1), (q r₀ b₀ a n j * S r₀ b₀ a n j ^ 2 + q r₀ b₀ a n j * ((a / (T r₀ b₀ a n + a)) ^ 2 / 4)) :=
        sum_le_sum hpt
    _ = ∑ j ∈ range (n + 1), q r₀ b₀ a n j * S r₀ b₀ a n j ^ 2 + (a / (T r₀ b₀ a n + a)) ^ 2 / 4 := by
        rw [sum_add_distrib, ← sum_mul, hq, one_mul]

/-- The second moment after `n` steps against its start. -/
theorem second_moment_le (n : ℕ) :
    E r₀ b₀ a n (fun j => S r₀ b₀ a n j ^ 2)
      ≤ S r₀ b₀ a 0 0 ^ 2 + ∑ k ∈ range n, (a / (T r₀ b₀ a k + a)) ^ 2 / 4 := by
  induction n with
  | zero => simp [E, q]
  | succ n ih =>
    rw [sum_range_succ]
    have := second_moment_step r₀ b₀ a hr hb ha n
    linarith

/-- The telescoping bound on the sum of squared step sizes. -/
theorem step_sum_le (n : ℕ) :
    ∑ k ∈ range n, (a / (T r₀ b₀ a k + a)) ^ 2 ≤ a / (r₀ + b₀) := by
  have hT0 : 0 < r₀ + b₀ := by linarith
  have key : ∀ k : ℕ, (a / (T r₀ b₀ a k + a)) ^ 2 ≤ a / T r₀ b₀ a k - a / T r₀ b₀ a (k + 1) := by
    intro k
    have hk : 0 < T r₀ b₀ a k := T_pos r₀ b₀ a hr hb ha k
    have hk1 : T r₀ b₀ a (k + 1) = T r₀ b₀ a k + a := by unfold T; push_cast; ring
    rw [hk1]
    have hka : 0 < T r₀ b₀ a k + a := by linarith
    rw [div_sub_div _ _ hk.ne' hka.ne', div_pow, div_le_div_iff₀ (by positivity) (by positivity)]
    nlinarith [mul_pos hk hka, mul_pos ha hk]
  calc ∑ k ∈ range n, (a / (T r₀ b₀ a k + a)) ^ 2
      ≤ ∑ k ∈ range n, (a / T r₀ b₀ a k - a / T r₀ b₀ a (k + 1)) := sum_le_sum fun k _ => key k
    _ = a / T r₀ b₀ a 0 - a / T r₀ b₀ a n := by
        rw [sum_range_sub' (fun k => a / T r₀ b₀ a k)]
    _ ≤ a / (r₀ + b₀) := by
        have : T r₀ b₀ a 0 = r₀ + b₀ := by unfold T; simp
        rw [this]
        have : 0 ≤ a / T r₀ b₀ a n := div_nonneg ha.le (T_pos r₀ b₀ a hr hb ha n).le
        linarith

/-- The variance of the share is at most `a / (4 (r₀ + b₀))`, whatever the horizon. -/
theorem var_le (n : ℕ) :
    E r₀ b₀ a n (fun j => (S r₀ b₀ a n j - S r₀ b₀ a 0 0) ^ 2) ≤ a / (4 * (r₀ + b₀)) := by
  have hm := mean_const r₀ b₀ a hr hb ha n
  have hq := q_sum r₀ b₀ a hr hb ha n
  have expand : E r₀ b₀ a n (fun j => (S r₀ b₀ a n j - S r₀ b₀ a 0 0) ^ 2)
      = E r₀ b₀ a n (fun j => S r₀ b₀ a n j ^ 2) - S r₀ b₀ a 0 0 ^ 2 := by
    unfold E at hm hq ⊢
    have hpt : ∀ j, q r₀ b₀ a n j * (S r₀ b₀ a n j - S r₀ b₀ a 0 0) ^ 2
        = q r₀ b₀ a n j * S r₀ b₀ a n j ^ 2 - 2 * S r₀ b₀ a 0 0 * (q r₀ b₀ a n j * S r₀ b₀ a n j)
          + S r₀ b₀ a 0 0 ^ 2 * q r₀ b₀ a n j := fun j => by ring
    simp only [hpt, sum_add_distrib, sum_sub_distrib, ← mul_sum, hm, hq]
    ring
  rw [expand]
  have h1 := second_moment_le r₀ b₀ a hr hb ha n
  have h2 := step_sum_le r₀ b₀ a hr hb ha n
  have h3 : ∑ k ∈ range n, (a / (T r₀ b₀ a k + a)) ^ 2 / 4 = (∑ k ∈ range n, (a / (T r₀ b₀ a k + a)) ^ 2) / 4 := by
    rw [sum_div]
  rw [h3] at h1
  have : a / (4 * (r₀ + b₀)) = (a / (r₀ + b₀)) / 4 := by rw [div_div, mul_comm]
  rw [this]
  linarith

/-- Chebyshev: the share leaves a band of half-width `t` about its start with probability at most
`a / (4 (r₀ + b₀) t²)`, for every horizon. -/
theorem chebyshev (n : ℕ) (t : ℝ) (ht : 0 < t) :
    ∑ j ∈ (range (n + 1)).filter (fun j => t ≤ |S r₀ b₀ a n j - S r₀ b₀ a 0 0|), q r₀ b₀ a n j
      ≤ a / (4 * (r₀ + b₀) * t ^ 2) := by
  have hvar := var_le r₀ b₀ a hr hb ha n
  unfold E at hvar
  -- Markov: on the bad set t² ≤ (S − S₀)², and q ≥ 0 everywhere
  have hmark : ∑ j ∈ (range (n + 1)).filter (fun j => t ≤ |S r₀ b₀ a n j - S r₀ b₀ a 0 0|), q r₀ b₀ a n j * t ^ 2
      ≤ ∑ j ∈ range (n + 1), q r₀ b₀ a n j * (S r₀ b₀ a n j - S r₀ b₀ a 0 0) ^ 2 := by
    calc ∑ j ∈ (range (n + 1)).filter (fun j => t ≤ |S r₀ b₀ a n j - S r₀ b₀ a 0 0|), q r₀ b₀ a n j * t ^ 2
        ≤ ∑ j ∈ (range (n + 1)).filter (fun j => t ≤ |S r₀ b₀ a n j - S r₀ b₀ a 0 0|),
            q r₀ b₀ a n j * (S r₀ b₀ a n j - S r₀ b₀ a 0 0) ^ 2 := by
          apply sum_le_sum
          intro j hj
          rw [mem_filter] at hj
          apply mul_le_mul_of_nonneg_left _ (q_nonneg r₀ b₀ a hr hb ha n j)
          have := hj.2
          calc t ^ 2 ≤ |S r₀ b₀ a n j - S r₀ b₀ a 0 0| ^ 2 := by
                exact pow_le_pow_left₀ ht.le this 2
            _ = (S r₀ b₀ a n j - S r₀ b₀ a 0 0) ^ 2 := sq_abs _
      _ ≤ ∑ j ∈ range (n + 1), q r₀ b₀ a n j * (S r₀ b₀ a n j - S r₀ b₀ a 0 0) ^ 2 := by
          apply sum_le_sum_of_subset_of_nonneg (filter_subset _ _)
          intro j _ _
          exact mul_nonneg (q_nonneg r₀ b₀ a hr hb ha n j) (sq_nonneg _)
  rw [← sum_mul] at hmark
  have hT0 : 0 < r₀ + b₀ := by linarith
  rw [le_div_iff₀ (by positivity)]
  calc (∑ j ∈ (range (n + 1)).filter (fun j => t ≤ |S r₀ b₀ a n j - S r₀ b₀ a 0 0|), q r₀ b₀ a n j) * (4 * (r₀ + b₀) * t ^ 2)
      = ((∑ j ∈ (range (n + 1)).filter (fun j => t ≤ |S r₀ b₀ a n j - S r₀ b₀ a 0 0|), q r₀ b₀ a n j) * t ^ 2) * (4 * (r₀ + b₀)) := by ring
    _ ≤ (a / (4 * (r₀ + b₀))) * (4 * (r₀ + b₀)) := by
        apply mul_le_mul_of_nonneg_right (le_trans hmark hvar) (by positivity)
    _ = a := by field_simp

end pos

/-- The Pólya start of P4Urn (one count each, reinforcement one): the envelope gives variance ≤ 1/8,
against the exact limiting variance 1/12 of the uniform law. -/
theorem polya_envelope : (1 : ℝ) / (4 * (1 + 1)) = 1 / 8 := by norm_num

end Research.P4.Envelope
