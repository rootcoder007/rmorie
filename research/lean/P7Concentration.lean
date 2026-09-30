/-
  Researchproofs/P7Concentration.lean

  P7. The law of crime concentration: how much of it is crime-free places?

  The Gini coefficient of a vector of place counts, written through the
  mean absolute difference, splits exactly into the share of places with
  zero crime and the Gini among the places with any crime:

      G(all) = z + (1 − z) · G(positive places),      z = zero share.

  So a headline "50% of crime on 5% of places" is partly a statement
  about how many places have no crime at all, and that share is itself
  what a Poisson null predicts mechanically: with mean count μ per
  place, a place is crime-free with probability exp(−μ), whatever the
  concentration of the underlying rates.

  Proved:
    * gini_zero_decomposition: the exact identity above for finite count
      vectors (any real non-negative values, at least one positive).
    * poisson_zero_prob: P(Poisson(μ) = 0) = exp(−μ), from Mathlib's
      Poisson pmf, so the expected zero share under the equal-rate null
      is exp(−μ) in closed form.

  Lean checks the algebra; whether counts are Poisson, and at what unit
  of place, is a modelling decision the analyst states.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false
set_option linter.deprecated false

noncomputable section
open Finset

namespace Research.P7

/-- Mean absolute difference form of the Gini coefficient for a finite vector
indexed by a `Fintype`: `Σᵢ Σⱼ |xᵢ − xⱼ| / (2 n² μ)` with `μ` the mean. -/
def gini {ι : Type*} [Fintype ι] (x : ι → ℝ) : ℝ :=
  (∑ i, ∑ j, |x i - x j|) / (2 * (Fintype.card ι : ℝ) ^ 2 * ((∑ i, x i) / Fintype.card ι))

/-- Gini restricted to a subset `s`, with `s` as the population. -/
def giniOn {ι : Type*} (s : Finset ι) (x : ι → ℝ) : ℝ :=
  (∑ i ∈ s, ∑ j ∈ s, |x i - x j|) / (2 * (s.card : ℝ) ^ 2 * ((∑ i ∈ s, x i) / s.card))

/-- The zero-share decomposition. `pos` is the set of places with `x > 0`,
`zero` the set with `x = 0`; both are given explicitly so the statement
carries its own bookkeeping. -/
theorem gini_zero_decomposition {ι : Type*} [Fintype ι] [DecidableEq ι] (x : ι → ℝ)
    (hnn : ∀ i, 0 ≤ x i)
    (pos zero : Finset ι) (hpos : ∀ i ∈ pos, 0 < x i) (hzero : ∀ i ∈ zero, x i = 0)
    (hdisj : Disjoint pos zero) (hcover : pos ∪ zero = univ) (hne : pos.Nonempty) :
    gini x = (zero.card : ℝ) / Fintype.card ι
      + (1 - (zero.card : ℝ) / Fintype.card ι) * giniOn pos x := by
  classical
  set n : ℝ := (Fintype.card ι : ℝ) with hn_def
  set m : ℝ := (pos.card : ℝ) with hm_def
  set z : ℝ := (zero.card : ℝ) with hz_def
  have hcard : m + z = n := by
    have := card_union_of_disjoint hdisj
    rw [hcover, card_univ] at this
    simp only [m, z, n]; exact_mod_cast this.symm
  have hm : 0 < m := by simp only [m]; exact_mod_cast hne.card_pos
  have hn : 0 < n := by simp only [n]; exact_mod_cast Fintype.card_pos_iff.mpr ⟨hne.choose⟩
  -- sums over the whole index set split over pos and zero
  have hsum_split : ∀ f : ι → ℝ, ∑ i, f i = ∑ i ∈ pos, f i + ∑ i ∈ zero, f i := by
    intro f; rw [← sum_union hdisj, hcover]
  -- total mass lives on pos
  have hS : ∑ i, x i = ∑ i ∈ pos, x i := by
    rw [hsum_split]; simp [sum_eq_zero (fun i hi => hzero i hi)]
  have hSpos : 0 < ∑ i ∈ pos, x i := sum_pos (fun i hi => hpos i hi) hne
  -- the double sum of |x i - x j| over all pairs
  have hinner : ∀ i, ∑ j, |x i - x j| = ∑ j ∈ pos, |x i - x j| + z * |x i| := by
    intro i
    rw [hsum_split]
    congr 1
    rw [sum_congr rfl (fun j hj => by rw [hzero j hj, sub_zero])]
    simp [z]
  have hdouble : ∑ i, ∑ j, |x i - x j|
      = ∑ i ∈ pos, ∑ j ∈ pos, |x i - x j| + 2 * z * ∑ i ∈ pos, x i := by
    rw [hsum_split (fun i => ∑ j, |x i - x j|)]
    simp_rw [hinner]
    rw [sum_add_distrib, sum_add_distrib]
    have h1 : ∑ i ∈ zero, ∑ j ∈ pos, |x i - x j| = ∑ j ∈ pos, z * |x j| := by
      rw [sum_comm]
      apply sum_congr rfl; intro j _
      rw [sum_congr rfl (fun i hi => by rw [hzero i hi, zero_sub, abs_neg])]
      simp [z]
    have h2 : ∑ i ∈ zero, z * |x i| = 0 := sum_eq_zero (fun i hi => by rw [hzero i hi]; simp)
    have h3 : ∑ i ∈ pos, z * |x i| = z * ∑ i ∈ pos, x i := by
      rw [← mul_sum]; congr 1
      exact sum_congr rfl (fun i hi => abs_of_pos (hpos i hi))
    rw [h1, h2, h3]; ring
  unfold gini giniOn
  rw [hdouble, hS]
  rw [← hn_def, ← hm_def]
  have hn' := hn.ne'
  have hm' := hm.ne'
  have hS' := hSpos.ne'
  rw [show z = n - m by linarith]
  field_simp
  ring

/-- Under a Poisson null with mean `μ`, a place is crime-free with probability `exp (−μ)`. -/
theorem poisson_zero_prob (μ : NNReal) :
    ProbabilityTheory.poissonPMFReal μ 0 = Real.exp (-μ) := by
  unfold ProbabilityTheory.poissonPMFReal
  simp

end Research.P7
