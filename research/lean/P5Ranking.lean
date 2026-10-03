/-
  Researchproofs/P5Ranking.lean

  P5, continued: ranking defendants by a noisy score (Weisburd & Britt
  ch. 1 on the Baldus proportionality review, where defendants were
  ordered by predicted death-sentence probabilities whose confidence
  intervals covered most of [0, 1]).

  Two people have true scores t₁ < t₂ and the court sees p̂ᵢ = tᵢ + eᵢ.
  If the noise can move each estimate by up to δ, then whenever the true
  gap t₂ − t₁ is below 2δ there is an admissible noise pair that reverses
  the ranking (rank_reversal_exists), and whenever the gap exceeds 2δ no
  admissible noise can (rank_stable_of_gap). So the resolution of a
  ranking is exactly twice the noise half-width: with intervals of
  half-width 0.4 on a probability scale, only pairs whose true
  probabilities differ by more than 0.8 have an identified order — in a
  unit interval, almost none.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

namespace Research.P5

/-- Below the resolution `2δ` the observed order can be reversed by admissible noise. -/
theorem rank_reversal_exists (t₁ t₂ δ : ℝ) (hδ : 0 < δ) (h12 : t₁ < t₂) (hgap : t₂ - t₁ < 2 * δ) :
    ∃ e₁ e₂ : ℝ, |e₁| ≤ δ ∧ |e₂| ≤ δ ∧ t₂ + e₂ < t₁ + e₁ := by
  refine ⟨δ, -δ, by rw [abs_of_pos hδ], by rw [abs_neg, abs_of_pos hδ], ?_⟩
  linarith

/-- Above the resolution the order is identified whatever the noise. -/
theorem rank_stable_of_gap (t₁ t₂ δ e₁ e₂ : ℝ) (hgap : 2 * δ < t₂ - t₁) (h1 : |e₁| ≤ δ) (h2 : |e₂| ≤ δ) :
    t₁ + e₁ < t₂ + e₂ := by
  have := abs_le.1 h1
  have := abs_le.1 h2
  linarith [this.1, this.2]

/-- Scores confined to `[0, 1]` cannot be pairwise separated by a gap above one: with noise
half-width above one half, no two defendants have an identified order. -/
theorem identified_scores_le (g : ℝ) (hg : 1 < g) (t : ℕ → ℝ) (h0 : ∀ i, 0 ≤ t i ∧ t i ≤ 1)
    (hsep : ∀ i j : ℕ, i < j → g ≤ t j - t i) : ∀ i j : ℕ, i < j → False := by
  intro i j hij
  have := hsep i j hij
  have := (h0 i).1
  have := (h0 j).2
  linarith

end Research.P5
