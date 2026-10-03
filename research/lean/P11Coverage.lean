/-
  Researchproofs/P11Coverage.lean

  P11, continued: confidence sets for an identification region (library
  scan B25; Manski §2.7; Imbens & Manski 2004).

  Two things an interval estimate of a partially identified sentencing
  effect has to get right.

  (1) Coverage of the region versus coverage of the point. On a finite
      probability space, if the identification region H contains the true
      parameter θ, then the event "C covers all of H" is contained in the
      event "C covers θ":  P[H ⊆ C] ≤ P[θ ∈ C]  (region_coverage_le). A set
      built to cover the whole region is at least as conservative for the
      parameter; the converse construction (cover the point uniformly over
      the region) is Imbens–Manski's.

  (2) The Imbens–Manski cutoff. With F a strictly increasing CDF bounded by
      one, the coverage of the interval [L̂ − c σ, Û + c σ] for a point at
      one end of a region of (standardised) width Δ is F(c + Δ) − F(−c).
      The cutoff c_α(Δ) solving F(c + Δ) − F(−c) = 1 − α is:
        * non-increasing in Δ (im_cutoff_antitone): a wider region needs a
          smaller cutoff, because the far end of the region protects the near
          end;
        * at most the two-sided cutoff z with F(z) − F(−z) = 1 − α, and at
          least the one-sided one in the sense F(−c) ≤ α (im_cutoff_between);
        * so the naive two-sided interval over-covers whenever Δ > 0
          (two_sided_overcovers).
      Existence of the cutoff (an intermediate-value argument) is what the
      R and Python arms compute by root finding; the theorems are about the
      cutoff once found.
-/
import Mathlib
import Researchproofs.P13Meta

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset Classical

namespace Research.P11

open Research.P13 in
/-- Probability of an event on a finite probability space. -/
def Prob.prob {Ω : Type*} [Fintype Ω] (P : Research.P13.Prob Ω) (E : Ω → Prop) : ℝ :=
  ∑ ω, if E ω then P.p ω else 0

variable {Ω : Type*} [Fintype Ω] (P : Research.P13.Prob Ω)

theorem prob_mono (E F : Ω → Prop) (h : ∀ ω, E ω → F ω) : Prob.prob P E ≤ Prob.prob P F := by
  unfold Prob.prob
  apply sum_le_sum; intro ω _
  by_cases hE : E ω
  · rw [if_pos hE, if_pos (h ω hE)]
  · rw [if_neg hE]
    split_ifs
    · exact P.nonneg ω
    · exact le_rfl

/-- `P[H ⊆ C] ≤ P[θ ∈ C]` whenever `θ ∈ H`. -/
theorem region_coverage_le (C : Ω → Set ℝ) (H : Set ℝ) (θ : ℝ) (hθ : θ ∈ H) :
    Prob.prob P (fun ω => H ⊆ C ω) ≤ Prob.prob P (fun ω => θ ∈ C ω) :=
  prob_mono P _ _ (fun ω hsub => hsub hθ)

/-! ### The Imbens–Manski cutoff -/

section Cutoff

variable (F : ℝ → ℝ) (hF : StrictMono F) (hF1 : ∀ x, F x ≤ 1)

/-- Coverage of the near end of a region of width `Δ` at cutoff `c`. -/
def coverage (c Δ : ℝ) : ℝ := F (c + Δ) - F (-c)

include hF in
theorem coverage_strictMono_c (Δ : ℝ) : StrictMono (fun c => coverage F c Δ) := by
  intro a b hab
  unfold coverage
  have h1 : F (a + Δ) < F (b + Δ) := hF (by linarith)
  have h2 : F (-b) < F (-a) := hF (by linarith)
  linarith

include hF in
theorem coverage_mono_Δ (c : ℝ) : Monotone (fun Δ => coverage F c Δ) := by
  intro a b hab
  unfold coverage
  have : F (c + a) ≤ F (c + b) := hF.monotone (by linarith)
  linarith

include hF in
/-- A wider region needs a smaller cutoff. -/
theorem im_cutoff_antitone (α c₁ c₂ Δ₁ Δ₂ : ℝ) (hΔ : Δ₁ ≤ Δ₂)
    (h₁ : coverage F c₁ Δ₁ = 1 - α) (h₂ : coverage F c₂ Δ₂ = 1 - α) : c₂ ≤ c₁ := by
  by_contra hlt
  push Not at hlt
  have a : coverage F c₁ Δ₂ < coverage F c₂ Δ₂ := coverage_strictMono_c F hF Δ₂ hlt
  have b : coverage F c₁ Δ₁ ≤ coverage F c₁ Δ₂ := coverage_mono_Δ F hF c₁ hΔ
  linarith

include hF hF1 in
/-- The cutoff lies below the two-sided one and above the one-sided one. -/
theorem im_cutoff_between (α c Δ z : ℝ) (hΔ : 0 ≤ Δ) (hc : coverage F c Δ = 1 - α)
    (hz : F z - F (-z) = 1 - α) : c ≤ z ∧ F (-c) ≤ α := by
  constructor
  · by_contra hlt
    push Not at hlt
    have a : F z - F (-z) < F c - F (-c) := by
      have := hF hlt; have := hF (neg_lt_neg hlt); linarith
    have b : F c - F (-c) ≤ coverage F c Δ := by
      unfold coverage; have : F c ≤ F (c + Δ) := hF.monotone (by linarith); linarith
    linarith
  · unfold coverage at hc
    have := hF1 (c + Δ)
    linarith

include hF in
/-- At the two-sided cutoff a region of positive width is over-covered. -/
theorem two_sided_overcovers (α z Δ : ℝ) (hΔ : 0 < Δ) (hz : F z - F (-z) = 1 - α) :
    1 - α < coverage F z Δ := by
  unfold coverage
  have : F z < F (z + Δ) := hF (by linarith)
  linarith

end Cutoff

end Research.P11
