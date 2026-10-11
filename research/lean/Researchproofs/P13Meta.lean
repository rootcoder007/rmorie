/-
  Researchproofs/P13Meta.lean

  P13 (new): pooling evaluations — random effects do not collapse to
  fixed effects in finite samples (library scan A11; Weisburd & Britt,
  Advanced Statistics in Criminology and Criminal Justice, ch. 11;
  DerSimonian & Laird 1986).

  The DerSimonian–Laird between-study variance is a truncation,
  τ̂² = max(0, (Q − (k−1))/c). Two facts that do not depend on any
  distribution for Q:

    * truncation_bias / pos_part_pos: on a finite probability space,
      E[max(X, 0)] = E[X] + E[max(−X, 0)] ≥ E[X], and E[max(X, 0)] > 0 as
      soon as one outcome of positive probability has X > 0. Under
      homogeneity E[Q] = k − 1 gives E[X] = 0 for X = (Q − (k−1))/c, yet
      Q > k − 1 happens with positive probability, so E[τ̂²] > 0: the
      estimator is biased upward exactly when the truth is zero.
    * re_var_ge / re_var_eq_iff: with study variances v_i > 0 and any
      τ² ≥ 0 the random-effects variance 1/∑ 1/(v_i + τ²) is at least the
      fixed-effect variance 1/∑ 1/v_i, with equality iff τ² = 0. So the
      random-effects interval is never narrower, and the two methods
      coincide in a finite sample only when τ̂² happens to be truncated.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P13

/-- A finite probability space: outcomes `Ω`, weights `p ≥ 0` summing to one. -/
structure Prob (Ω : Type*) [Fintype Ω] where
  (p : Ω → ℝ)
  (nonneg : ∀ ω, 0 ≤ p ω)
  (total : ∑ ω, p ω = 1)

variable {Ω : Type*} [Fintype Ω] (P : Prob Ω)

def Prob.E (P : Prob Ω) (X : Ω → ℝ) : ℝ := ∑ ω, P.p ω * X ω

/-- `max(X,0) = X + max(−X,0)` pointwise, so the truncated mean exceeds the mean by `E[max(−X,0)] ≥ 0`. -/
theorem truncation_bias (X : Ω → ℝ) :
    P.E (fun ω => max (X ω) 0) = P.E X + P.E (fun ω => max (-X ω) 0) ∧
      0 ≤ P.E (fun ω => max (-X ω) 0) := by
  constructor
  · unfold Prob.E; rw [← sum_add_distrib]; apply sum_congr rfl; intro ω _
    rw [← mul_add]; congr 1
    show max (X ω) 0 = X ω + max (-X ω) 0
    rcases le_total (X ω) 0 with h | h
    · rw [max_eq_right h, max_eq_left (by linarith)]; ring
    · rw [max_eq_left h, max_eq_right (by linarith)]; ring
  · unfold Prob.E; apply sum_nonneg; intro ω _
    exact mul_nonneg (P.nonneg ω) (le_max_right _ _)

/-- The truncated mean is at least `max(E X, 0)`. -/
theorem pos_part_ge (X : Ω → ℝ) : max (P.E X) 0 ≤ P.E (fun ω => max (X ω) 0) := by
  apply max_le
  · unfold Prob.E; apply sum_le_sum; intro ω _
    exact mul_le_mul_of_nonneg_left (le_max_left _ _) (P.nonneg ω)
  · unfold Prob.E; apply sum_nonneg; intro ω _
    exact mul_nonneg (P.nonneg ω) (le_max_right _ _)

/-- One outcome of positive probability with `X > 0` makes the truncated mean strictly positive:
    this is why `E[τ̂²_DL] > 0` under homogeneity, where `E[X] = 0`. -/
theorem pos_part_pos (X : Ω → ℝ) (ω₀ : Ω) (hp : 0 < P.p ω₀) (hx : 0 < X ω₀) :
    0 < P.E (fun ω => max (X ω) 0) := by
  unfold Prob.E
  have h1 : P.p ω₀ * max (X ω₀) 0 ≤ ∑ ω, P.p ω * max (X ω) 0 :=
    single_le_sum (fun ω _ => mul_nonneg (P.nonneg ω) (le_max_right _ _)) (mem_univ ω₀)
  have h2 : 0 < P.p ω₀ * max (X ω₀) 0 := by
    rw [max_eq_left hx.le]; exact mul_pos hp hx
  linarith

/-- Under homogeneity the mean of `X = (Q − (k−1))/c` is zero while the truncated estimator has a
    strictly positive mean: the DerSimonian–Laird estimator is biased upward exactly when τ² = 0. -/
theorem dl_biased_under_homogeneity (X : Ω → ℝ) (hE : P.E X = 0) (ω₀ : Ω)
    (hp : 0 < P.p ω₀) (hx : 0 < X ω₀) :
    P.E X < P.E (fun ω => max (X ω) 0) := by
  rw [hE]; exact pos_part_pos P X ω₀ hp hx

/-! ### Fixed-effect versus random-effects variance -/

section Variance

variable {ι : Type*} [Fintype ι]

/-- DerSimonian–Laird: `Q`, the scale `c = ∑w − ∑w²/∑w`, and the truncated estimator. -/
def Q (v θ : ι → ℝ) : ℝ :=
  let w := fun i => 1 / v i
  let θFE := (∑ i, w i * θ i) / (∑ i, w i)
  ∑ i, w i * (θ i - θFE) ^ 2

def cDL (v : ι → ℝ) : ℝ :=
  let w := fun i => 1 / v i
  (∑ i, w i) - (∑ i, w i ^ 2) / (∑ i, w i)

def tauDL (v θ : ι → ℝ) : ℝ := max 0 ((Q v θ - (Fintype.card ι - 1)) / cDL v)

theorem tauDL_nonneg (v θ : ι → ℝ) : 0 ≤ tauDL v θ := le_max_left _ _

/-- The estimator is truncated to zero exactly when `Q ≤ k − 1` (for `c > 0`). -/
theorem tauDL_eq_zero_iff (v θ : ι → ℝ) (hc : 0 < cDL v) :
    tauDL v θ = 0 ↔ Q v θ ≤ Fintype.card ι - 1 := by
  unfold tauDL
  constructor
  · intro h
    have : (Q v θ - (Fintype.card ι - 1)) / cDL v ≤ 0 := by
      by_contra hcon; push Not at hcon
      rw [max_eq_right hcon.le] at h; linarith
    have := (div_nonpos_iff).1 this
    rcases this with ⟨_, h2⟩ | ⟨h1, _⟩
    · linarith
    · linarith
  · intro h
    apply max_eq_left
    apply div_nonpos_of_nonpos_of_nonneg (by linarith) hc.le

/-- Fixed-effect and random-effects variances of the pooled estimate. -/
def varFE (v : ι → ℝ) : ℝ := 1 / ∑ i, 1 / v i
def varRE (v : ι → ℝ) (t : ℝ) : ℝ := 1 / ∑ i, 1 / (v i + t)

omit [Fintype ι] in
theorem re_weight_le (v : ι → ℝ) (t : ℝ) (hv : ∀ i, 0 < v i) (ht : 0 ≤ t) (i : ι) :
    1 / (v i + t) ≤ 1 / v i :=
  one_div_le_one_div_of_le (hv i) (by linarith)

/-- The random-effects variance is never below the fixed-effect variance. -/
theorem re_var_ge [Nonempty ι] (v : ι → ℝ) (t : ℝ) (hv : ∀ i, 0 < v i) (ht : 0 ≤ t) :
    varFE v ≤ varRE v t := by
  unfold varFE varRE
  apply one_div_le_one_div_of_le
  · apply sum_pos (fun i _ => one_div_pos.2 (by linarith [hv i])) univ_nonempty
  · exact sum_le_sum (fun i _ => re_weight_le v t hv ht i)

/-- Equality holds only at `τ² = 0`: any heterogeneity widens the interval. -/
theorem re_var_eq_iff [Nonempty ι] (v : ι → ℝ) (t : ℝ) (hv : ∀ i, 0 < v i) (ht : 0 ≤ t) :
    varFE v = varRE v t ↔ t = 0 := by
  constructor
  · intro h
    by_contra hne
    have htpos : 0 < t := lt_of_le_of_ne ht (Ne.symm hne)
    have hlt : ∑ i, 1 / (v i + t) < ∑ i, 1 / v i :=
      sum_lt_sum_of_nonempty univ_nonempty
        (fun i _ => one_div_lt_one_div_of_lt (hv i) (by linarith [hv i]))
    have hpos : 0 < ∑ i, 1 / (v i + t) :=
      sum_pos (fun i _ => one_div_pos.2 (by linarith [hv i])) univ_nonempty
    have : varFE v < varRE v t := by
      unfold varFE varRE
      exact one_div_lt_one_div_of_lt hpos hlt
    linarith
  · rintro rfl; simp [varFE, varRE]

end Variance

end Research.P13
