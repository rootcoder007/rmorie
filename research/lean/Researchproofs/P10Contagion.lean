/-
  Researchproofs/P10Contagion.lean

  P10 (new): near-repeat contagion as a branching process.

  In a self-exciting (Hawkes) model of near-repeat victimisation every
  recorded event triggers, on average, n further events (the branching
  ratio n = ∫g). The offspring of one background event form a
  Galton–Watson tree with mean offspring n, so the expected number of
  events in generation k is n^k and the expected cluster size is the
  geometric sum Σ n^k (Grimmett & Stirzaker, Lemma 5.4.2 and Theorem
  5.4.5; D'Orsogna & Perc, "Statistical physics of crime", §on
  self-exciting point processes).

  Proved here:
    * generation_mean: E Z_k = n^k for a branching process with mean
      offspring n (the mean recursion, on expectations).
    * cluster_size_of_lt_one: for 0 ≤ n < 1 the expected cluster size is
      1 / (1 − n), and the mean event rate of the stationary process is
      μ / (1 − n) for a background rate μ.
    * cluster_size_diverges_of_ge_one: for n ≥ 1 the partial sums of the
      cluster size are unbounded — the process has no finite mean and a
      fitted branching ratio at or above one is a model that predicts
      unbounded crime, not a forecast.
    * endogeneity_share: the share of events that are triggered rather
      than background is exactly n — the number a "contagion" story owes
      its reader.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P10

/-- Expected size of generation `k` when every event triggers `n` on average. -/
def generationMean (n : ℝ) (k : ℕ) : ℝ := n ^ k

theorem generation_mean (n : ℝ) (k : ℕ) : generationMean n (k + 1) = n * generationMean n k := by
  unfold generationMean; ring

/-- Expected cluster size (all generations) for a subcritical branching ratio. -/
theorem cluster_size_of_lt_one (n : ℝ) (h0 : 0 ≤ n) (h1 : n < 1) :
    HasSum (fun k : ℕ => generationMean n k) (1 / (1 - n)) := by
  unfold generationMean
  have := hasSum_geometric_of_lt_one h0 h1
  simpa [one_div] using this

/-- Stationary mean rate `μ / (1 − n)` for a background rate `μ`. -/
theorem stationary_rate (μ n : ℝ) (h0 : 0 ≤ n) (h1 : n < 1) :
    HasSum (fun k : ℕ => μ * generationMean n k) (μ / (1 - n)) := by
  have h := (cluster_size_of_lt_one n h0 h1).mul_left μ
  rw [div_eq_mul_one_div]
  exact h

/-- At or above criticality the partial sums of the cluster size are unbounded. -/
theorem cluster_size_diverges_of_ge_one (n : ℝ) (h : 1 ≤ n) (B : ℝ) :
    ∃ K : ℕ, B < ∑ k ∈ range K, generationMean n k := by
  obtain ⟨K, hK⟩ := exists_nat_gt B
  refine ⟨K, lt_of_lt_of_le hK ?_⟩
  have : ∀ k ∈ range K, (1 : ℝ) ≤ generationMean n k := by
    intro k _; unfold generationMean; exact one_le_pow₀ h
  calc (K : ℝ) = ∑ k ∈ range K, (1 : ℝ) := by simp
    _ ≤ ∑ k ∈ range K, generationMean n k := sum_le_sum this

/-- The share of triggered (non-background) events in the stationary process is exactly `n`. -/
theorem endogeneity_share (μ n : ℝ) (hμ : 0 < μ) (h1 : n < 1) :
    (μ / (1 - n) - μ) / (μ / (1 - n)) = n := by
  have h1n : (1 - n) ≠ 0 := by linarith
  field_simp
  ring

end Research.P10
