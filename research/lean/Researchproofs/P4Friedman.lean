/-
  Researchproofs/P4Friedman.lean

  P4, continued: the Friedman urn — cross-colour reinforcement pulls the share
  to one half (library scan C2; Grimmett & Stirzaker Exercise 12.3.6).

  Opposite-colour reinforcement: a ball is drawn, and one ball of the OTHER
  colour is added. With b blue and r red (T = b + r):

    * `friedman_step`: (B − R)(B + R − 1) is a martingale — its one-step
      conditional expectation equals its current value.
    * `difference_step`: the difference D = B − R has E[D'] = D (1 − 1/T), so
      `difference_mean` telescopes to E[D_n] = D_0 (T_0 − 1)/(T_0 + n − 1) → 0
      (`difference_mean_tendsto`), and the mean share tends to 1/2
      (`mean_share_tendsto_half`). Same-colour reinforcement (P4Urn,
      P4Envelope) leaves the share a martingale with a random limit; the
      cross-colour rule makes it deterministic. The two urns are the two
      policing feedback rules: reinforce where you found crime, or spread
      effort away from where you looked last.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Filter Topology

namespace Research.P4.Friedman

/-- One Friedman step from `(b, r)`: draw blue with probability `b/(b+r)` and add a red ball,
or draw red and add a blue one. The conditional expectation of `f` after the step. -/
def step (b r : ℝ) (f : ℝ → ℝ → ℝ) : ℝ := b / (b + r) * f b (r + 1) + r / (b + r) * f (b + 1) r

/-- `(B − R)(B + R − 1)` is a martingale. -/
theorem friedman_step (b r : ℝ) (hT : 0 < b + r) :
    step b r (fun b r => (b - r) * (b + r - 1)) = (b - r) * (b + r - 1) := by
  unfold step
  field_simp
  ring

/-- The difference after one step: `E[D'] = D (1 − 1/T)`. -/
theorem difference_step (b r : ℝ) (hT : 0 < b + r) :
    step b r (fun b r => b - r) = (b - r) * (1 - 1 / (b + r)) := by
  unfold step
  field_simp
  ring

/-- The expected difference after `n` steps from total `T₀ > 1`: `D₀ (T₀ − 1)/(T₀ + n − 1)`. -/
def meanDiff (D₀ T₀ : ℝ) (n : ℕ) : ℝ := D₀ * (T₀ - 1) / (T₀ + n - 1)

theorem meanDiff_zero (D₀ T₀ : ℝ) (hT : 1 < T₀) : meanDiff D₀ T₀ 0 = D₀ := by
  unfold meanDiff
  have : T₀ - 1 ≠ 0 := by linarith
  rw [Nat.cast_zero, add_zero]
  field_simp

/-- The recursion `E[D_{n+1}] = E[D_n] (1 − 1/T_n)` with `T_n = T₀ + n` is solved by `meanDiff`. -/
theorem meanDiff_succ (D₀ T₀ : ℝ) (n : ℕ) (hT : 1 < T₀) :
    meanDiff D₀ T₀ (n + 1) = meanDiff D₀ T₀ n * (1 - 1 / (T₀ + n)) := by
  unfold meanDiff
  have h1 : T₀ + n - 1 ≠ 0 := by push_cast; linarith [Nat.cast_nonneg (α := ℝ) n]
  have h2 : T₀ + n ≠ 0 := by linarith [Nat.cast_nonneg (α := ℝ) n]
  have h3 : T₀ + (n + 1 : ℕ) - 1 ≠ 0 := by push_cast; linarith [Nat.cast_nonneg (α := ℝ) n]
  field_simp
  push_cast
  ring

theorem difference_mean_tendsto (D₀ T₀ : ℝ) (hT : 1 < T₀) :
    Tendsto (meanDiff D₀ T₀) atTop (𝓝 0) := by
  unfold meanDiff
  have : (fun n : ℕ => D₀ * (T₀ - 1) / (T₀ + n - 1)) = fun n : ℕ => (D₀ * (T₀ - 1)) * (1 / ((n : ℝ) + (T₀ - 1))) := by
    funext n; ring
  rw [this]
  have hinf : Tendsto (fun n : ℕ => (n : ℝ) + (T₀ - 1)) atTop atTop :=
    tendsto_natCast_atTop_atTop.atTop_add tendsto_const_nhds
  have h := (tendsto_const_nhds (x := D₀ * (T₀ - 1))).div_atTop hinf
  refine h.congr' (Filter.Eventually.of_forall fun n => ?_)
  ring

/-- The mean share `1/2 + E[D_n]/(2 T_n)` tends to one half. -/
theorem mean_share_tendsto_half (D₀ T₀ : ℝ) (hT : 1 < T₀) :
    Tendsto (fun n : ℕ => 1 / 2 + meanDiff D₀ T₀ n / (2 * (T₀ + n))) atTop (𝓝 (1 / 2)) := by
  have hD := difference_mean_tendsto D₀ T₀ hT
  have hbound : ∀ n : ℕ, |meanDiff D₀ T₀ n / (2 * (T₀ + n))| ≤ |meanDiff D₀ T₀ n| := by
    intro n
    rw [abs_div]
    have hpos : 0 < |2 * (T₀ + (n : ℝ))| := by
      rw [abs_pos]; have := Nat.cast_nonneg (α := ℝ) n; intro h; nlinarith
    rw [div_le_iff₀ hpos]
    have : (1 : ℝ) ≤ |2 * (T₀ + (n : ℝ))| := by
      rw [abs_of_pos (by have := Nat.cast_nonneg (α := ℝ) n; linarith)]
      have := Nat.cast_nonneg (α := ℝ) n; linarith
    nlinarith [abs_nonneg (meanDiff D₀ T₀ n)]
  have hsmall : Tendsto (fun n : ℕ => meanDiff D₀ T₀ n / (2 * (T₀ + n))) atTop (𝓝 0) := by
    rw [tendsto_zero_iff_abs_tendsto_zero] at hD ⊢
    exact squeeze_zero (fun n => abs_nonneg _) hbound hD
  have := (tendsto_const_nhds (x := (1 / 2 : ℝ))).add hsmall
  simpa using this

end Research.P4.Friedman
