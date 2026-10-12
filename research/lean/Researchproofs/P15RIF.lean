/-
  Researchproofs/P15RIF.lean

  P15, continued: the recentered influence function (Firpo, Fortin &
  Lemieux 2009) that carries the Oaxaca–Blinder split to quantiles.

  On a finite weighted population the RIF of the mean is the outcome
  itself, and the RIF of the τ-quantile at a stated point q with density
  value f > 0 is q + (τ − 1{y ≤ q}) / f.

    * rif_mean_avg: the weighted mean of the mean's RIF is the mean.
    * rif_quantile_avg: the weighted mean of the quantile's RIF is
      q + (τ − F(q)) / f, with F(q) the weighted share at or below q.
    * rif_quantile_avg_eq_iff: it equals q exactly when F(q) = τ — the
      "recentering" is exact at the sample quantile and off by
      (τ − F(q))/f otherwise, which is what a decomposition on the RIF
      inherits.
    * rif_gap: the gap between two groups' RIF means is the gap of the
      quantile estimates plus (F_B(q_B) − F_A(q_A)) terms, zero when each
      group is evaluated at its own quantile: so the P15 threefold split
      of the RIF means is a split of the quantile gap, not of a proxy.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P15.RIF

variable {Ω : Type*} [Fintype Ω] (w y : Ω → ℝ)

def W : ℝ := ∑ i, w i
/-- Weighted mean of `f`. -/
def mean (f : Ω → ℝ) : ℝ := (∑ i, w i * f i) / W w
/-- Weighted share of outcomes at or below `q`. -/
def F (q : ℝ) : ℝ := (∑ i, w i * (if y i ≤ q then 1 else 0)) / W w
/-- RIF of the mean. -/
def rifMean (i : Ω) : ℝ := y i
/-- RIF of the τ-quantile at the point `q` with density value `f`. -/
def rifQuantile (τ q f : ℝ) (i : Ω) : ℝ := q + (τ - (if y i ≤ q then 1 else 0)) / f

theorem rif_mean_avg : mean w (rifMean y) = mean w y := rfl

theorem rif_quantile_avg (τ q f : ℝ) (hW : W w ≠ 0) (hf : f ≠ 0) :
    mean w (rifQuantile y τ q f) = q + (τ - F w y q) / f := by
  unfold mean rifQuantile F
  have hpt : ∀ i, w i * (q + (τ - (if y i ≤ q then 1 else 0)) / f)
      = (q + τ / f) * w i - (1 / f) * (w i * (if y i ≤ q then 1 else 0)) := fun i => by
    field_simp; ring
  simp only [hpt, sum_sub_distrib, ← mul_sum]
  unfold W at hW ⊢
  field_simp
  ring

theorem rif_quantile_avg_eq_iff (τ q f : ℝ) (hW : W w ≠ 0) (hf : f ≠ 0) :
    mean w (rifQuantile y τ q f) = q ↔ F w y q = τ := by
  rw [rif_quantile_avg w y τ q f hW hf]
  constructor
  · intro h
    have : (τ - F w y q) / f = 0 := by linarith
    rcases div_eq_zero_iff.mp this with h1 | h1
    · linarith
    · exact absurd h1 hf
  · intro h; rw [h, sub_self, zero_div, add_zero]

/-- Two groups, each with its own weights, outcomes and evaluation point. -/
theorem rif_gap {Ω' : Type*} [Fintype Ω'] (w' y' : Ω' → ℝ) (τ qA qB fA fB : ℝ)
    (hW : W w ≠ 0) (hW' : W w' ≠ 0) (hfA : fA ≠ 0) (hfB : fB ≠ 0) :
    mean w (rifQuantile y τ qA fA) - mean w' (rifQuantile y' τ qB fB)
      = (qA - qB) + ((τ - F w y qA) / fA - (τ - F w' y' qB) / fB) := by
  rw [rif_quantile_avg w y τ qA fA hW hfA, rif_quantile_avg w' y' τ qB fB hW' hfB]
  ring

end Research.P15.RIF
