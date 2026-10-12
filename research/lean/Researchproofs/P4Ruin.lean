/-
  Researchproofs/P4Ruin.lean

  P4, continued: absorption of a finite-capacity allocation — the de Moivre
  martingale and the gambler's-ruin probability (library scan C4; Grimmett &
  Stirzaker Example 12.5.8, Theorems 12.5.1 and 12.5.9).

  A resource allocated one unit at a time between two regions, moving toward
  the first with probability p and the second with probability q = 1 − p,
  stops when one region holds everything (0 or N). With ρ = q/p:

    * `deMoivre_step`: ρ^{S} is a martingale — its one-step conditional
      expectation is itself (`p ρ^{k+1} + q ρ^{k−1} = ρ^k`).
    * `ruin_prob` p_k = (ρ^k − ρ^N)/(1 − ρ^N) solves the harmonic equation
      p_k = p p_{k+1} + q p_{k−1} with p_0 = 1 and p_N = 0
      (`ruin_boundary`, `ruin_harmonic`), the optional-stopping identity
      E[ρ^{S_τ}] = ρ^k written out (`optional_stopping_identity`).
    * `ruin_symmetric`: at p = 1/2 the probability is 1 − k/N, the limit the
      formula does not reach directly (ρ = 1 makes it 0/0), which is why
      optional stopping needs its boundedness condition, not just the
      martingale property.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P4.Ruin

/-- The de Moivre martingale: with `ρ = q/p`, `p ρ^{j+2} + q ρ^{j} = ρ^{j+1}`: one step from
`S = j + 1` leaves `E[ρ^S]` unchanged. -/
theorem deMoivre_step (p : ℝ) (hp : 0 < p) (hp1 : p < 1) (j : ℕ) :
    p * ((1 - p) / p) ^ (j + 2) + (1 - p) * ((1 - p) / p) ^ j = ((1 - p) / p) ^ (j + 1) := by
  have hpne : p ≠ 0 := hp.ne'
  set ρ := (1 - p) / p with hρ
  have hρp : ρ * p = 1 - p := by rw [hρ]; field_simp
  have h2 : p * ρ ^ 2 + (1 - p) = ρ := by linear_combination (ρ - 1) * hρp
  calc p * ρ ^ (j + 2) + (1 - p) * ρ ^ j = ρ ^ j * (p * ρ ^ 2 + (1 - p)) := by ring
    _ = ρ ^ j * ρ := by rw [h2]
    _ = ρ ^ (j + 1) := by ring

/-- The ruin probability from `k` with capacity `N`, `ρ ≠ 1`. -/
def ruinProb (ρ : ℝ) (N k : ℕ) : ℝ := (ρ ^ k - ρ ^ N) / (1 - ρ ^ N)

theorem ruin_boundary (ρ : ℝ) (N : ℕ) (hρ : ρ ^ N ≠ 1) :
    ruinProb ρ N 0 = 1 ∧ ruinProb ρ N N = 0 := by
  unfold ruinProb
  have : 1 - ρ ^ N ≠ 0 := sub_ne_zero.mpr (Ne.symm hρ)
  constructor
  · rw [pow_zero]; exact div_self this
  · simp

/-- The harmonic equation: `p_k = p p_{k+1} + q p_{k−1}` for `0 < k`, with `ρ = q/p`. -/
theorem ruin_harmonic (p : ℝ) (hp : 0 < p) (hp1 : p < 1) (N k : ℕ) (hk : 0 < k)
    (hρ : ((1 - p) / p) ^ N ≠ 1) :
    ruinProb ((1 - p) / p) N k = p * ruinProb ((1 - p) / p) N (k + 1) + (1 - p) * ruinProb ((1 - p) / p) N (k - 1) := by
  unfold ruinProb
  have hq : 0 < 1 - p := by linarith
  have hden : 1 - ((1 - p) / p) ^ N ≠ 0 := sub_ne_zero.mpr (Ne.symm hρ)
  obtain ⟨j, rfl⟩ : ∃ j, k = j + 1 := ⟨k - 1, by omega⟩
  simp only [Nat.add_sub_cancel]
  have hrec := deMoivre_step p hp hp1 j
  set ρ := (1 - p) / p with hρdef
  rw [← mul_div_assoc, ← mul_div_assoc, ← add_div]
  congr 1
  linear_combination (-1 : ℝ) * hrec

/-- Optional stopping written out: `ρ^k = p_k ρ^0 + (1 − p_k) ρ^N`. -/
theorem optional_stopping_identity (ρ : ℝ) (N k : ℕ) (hρ : ρ ^ N ≠ 1) :
    ρ ^ k = ruinProb ρ N k * 1 + (1 - ruinProb ρ N k) * ρ ^ N := by
  unfold ruinProb
  have : 1 - ρ ^ N ≠ 0 := sub_ne_zero.mpr (Ne.symm hρ)
  field_simp
  ring

/-- The symmetric case the formula does not reach: with `p = 1/2` the ruin probability `1 − k/N`
solves the same boundary problem. -/
theorem ruin_symmetric (N k : ℕ) (hN : 0 < N) (hk : 0 < k) (hkN : k ≤ N) :
    (1 : ℝ) - (k : ℝ) / N = (1 / 2) * (1 - ((k + 1 : ℕ) : ℝ) / N) + (1 / 2) * (1 - ((k - 1 : ℕ) : ℝ) / N) := by
  have hN' : (N : ℝ) ≠ 0 := by exact_mod_cast hN.ne'
  obtain ⟨j, rfl⟩ : ∃ j, k = j + 1 := ⟨k - 1, by omega⟩
  simp only [Nat.add_sub_cancel]
  push_cast
  field_simp
  ring

end Research.P4.Ruin
