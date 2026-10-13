/-
  Researchproofs/P16Censoring.lean

  P16 (continued): the disposed-cases mean as a bound problem.

  A cohort of n + m cases: n disposed with durations t_i, m still pending
  with elapsed ages a_j — their true durations u_j ≥ a_j are not yet
  observed. The published "mean time to disposition" is the disposed-cases
  mean t̄ = ∑ t / n. The cohort's true mean is (∑ t + ∑ u)/(n + m).

    * true_mean_ge: the true mean is at least L = (∑ t + ∑ a)/(n + m): the
      pending ages are lower bounds on their durations.
    * lower_bound_sub: L − t̄ = (m/(n+m)) (ā − t̄), so the disposed-cases
      mean understates the true mean for certain whenever the pending cases
      are on average older than the disposed mean (disposed_understates),
      and the understatement is at least (m/(n+m))(ā − t̄) (bias_lower).
    * no_upper_bound: without a cap on the pending durations no upper bound
      exists — the true mean can be made arbitrarily large while every
      observed number stays fixed.

  This is the P1 (dark figure) structure again: the unobserved part is
  bounded on one side by what is seen, and unbounded on the other without
  an assumption. A court reporting t̄ alone has reported a lower bound only
  when ā ≥ t̄, and nothing at all otherwise.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P16Censoring

variable {n m : ℕ} (t : Fin n → ℝ) (a u : Fin m → ℝ)

/-- Mean of the disposed cases. -/
def disposedMean : ℝ := (∑ i, t i) / n
/-- True cohort mean, with the pending durations `u`. -/
def trueMean : ℝ := (∑ i, t i + ∑ j, u j) / ((n : ℝ) + m)
/-- The computable lower bound: pending ages in place of durations. -/
def lowerBound : ℝ := (∑ i, t i + ∑ j, a j) / ((n : ℝ) + m)
/-- Mean elapsed age of the pending cases. -/
def pendingAge : ℝ := (∑ j, a j) / m

theorem true_mean_ge (hu : ∀ j, a j ≤ u j) : lowerBound t a ≤ trueMean t u := by
  unfold lowerBound trueMean
  apply div_le_div_of_nonneg_right _ (by positivity)
  have := sum_le_sum (fun j (_ : j ∈ univ) => hu j)
  linarith

theorem lower_bound_sub (hn : 0 < n) (hm : 0 < m) :
    lowerBound t a - disposedMean t = (m / ((n : ℝ) + m)) * (pendingAge a - disposedMean t) := by
  unfold lowerBound disposedMean pendingAge
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hm' : (m : ℝ) ≠ 0 := by exact_mod_cast hm.ne'
  have hnm : (n : ℝ) + m ≠ 0 := by positivity
  field_simp
  ring

/-- The understatement is at least the pending share times the age excess. -/
theorem bias_lower (hn : 0 < n) (hm : 0 < m) (hu : ∀ j, a j ≤ u j) :
    (m / ((n : ℝ) + m)) * (pendingAge a - disposedMean t) ≤ trueMean t u - disposedMean t := by
  have h1 := true_mean_ge t a u hu
  have h2 := lower_bound_sub t a hn hm
  linarith

/-- Pending cases older on average than the disposed mean: the published mean is below the truth. -/
theorem disposed_understates (hn : 0 < n) (hm : 0 < m) (hu : ∀ j, a j ≤ u j)
    (h : disposedMean t ≤ pendingAge a) : disposedMean t ≤ trueMean t u := by
  have := bias_lower t a u hn hm hu
  have hs : 0 ≤ (m / ((n : ℝ) + m)) * (pendingAge a - disposedMean t) :=
    mul_nonneg (by positivity) (by linarith)
  linarith

/-- No upper bound: for every `B` some admissible pending durations push the true mean above `B`. -/
theorem no_upper_bound (hm : 0 < m) (B : ℝ) :
    ∃ u : Fin m → ℝ, (∀ j, a j ≤ u j) ∧ B < trueMean t u := by
  have hm' : (0 : ℝ) < m := by exact_mod_cast hm
  have hnm : (0 : ℝ) < (n : ℝ) + m := by positivity
  set K₀ : ℝ := (B * ((n : ℝ) + m) - ∑ i, t i - ∑ j, a j) / m + 1 with hK₀
  set K : ℝ := max K₀ 0 with hK
  refine ⟨fun j => a j + K, fun j => ?_, ?_⟩
  · have : 0 ≤ K := le_max_right _ _
    linarith
  · unfold trueMean
    rw [lt_div_iff₀ hnm, sum_add_distrib, sum_const, card_univ, Fintype.card_fin, nsmul_eq_mul]
    have hK₀K : K₀ ≤ K := le_max_left _ _
    have hmK : m * K₀ = B * ((n : ℝ) + m) - ∑ i, t i - ∑ j, a j + m := by
      rw [hK₀]; field_simp
    nlinarith [mul_le_mul_of_nonneg_left hK₀K hm'.le]


/-! ### Duration bands

Courts publish the shares of disposed cases in duration bands (under 10 months, 10 to 15, ...)
rather than the durations. Each case in band `k` lasted at least the band's lower edge `f k`, so
the floors bound the disposed mean from below, and the bound is a sum over the published shares;
an open top band has no upper edge, which is `no_upper_bound` again. -/

variable {K : ℕ}

/-- The floors of the cases' bands, averaged, are at most the disposed-cases mean. -/
theorem band_floor_le_mean (b : Fin n → Fin K) (f : Fin K → ℝ) (hf : ∀ i, f (b i) ≤ t i) :
    (∑ i, f (b i)) / n ≤ disposedMean t := by
  unfold disposedMean
  apply div_le_div_of_nonneg_right _ (by positivity)
  exact sum_le_sum (fun i _ => hf i)

/-- The floor average is the share-weighted sum of the floors: the bound in the form courts
publish (the share of band `k` is the count of cases in it over `n`). -/
theorem band_floor_shares (b : Fin n → Fin K) (f : Fin K → ℝ) :
    (∑ i, f (b i)) / n = ∑ k, (((univ.filter (fun i => b i = k)).card : ℝ) / n) * f k := by
  rw [← Finset.sum_fiberwise univ b (fun i => f (b i)), Finset.sum_div]
  refine Finset.sum_congr rfl (fun k _ => ?_)
  have h : ∑ i ∈ univ.filter (fun i => b i = k), f (b i) =
      ∑ _i ∈ univ.filter (fun i => b i = k), f k :=
    Finset.sum_congr rfl (fun i hi => by rw [(Finset.mem_filter.mp hi).2])
  rw [h, Finset.sum_const, nsmul_eq_mul]
  ring

end Research.P16Censoring
