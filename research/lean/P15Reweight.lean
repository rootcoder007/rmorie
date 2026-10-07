/-
  Researchproofs/P15Reweight.lean

  P15 (continued): the DiNardo-Fortin-Lemieux reweighting (DiNardo, Fortin
  & Lemieux 1996; Fortin, Lemieux & Firpo 2011, §4).

  A finite weighted population with a group g ∈ {0, 1}, a discrete
  covariate x and an outcome y. The DFL weight for a group-0 person with
  covariate value v is ψ(v) = m₁(v)/m₀(v), the ratio of the group masses
  at v. Reweighting group 0 by ψ reproduces group 1's covariate
  distribution exactly:

    * reweighting_matches: for every function h of x,
        ∑_{g=0} ψ(x_i) w_i h(x_i) = ∑_{g=1} w_i h(x_i)
      under common support (m₁(v) ≠ 0 → m₀(v) ≠ 0); in particular the
      reweighted mass equals group 1's mass (reweighted_mass).
    * counterfactual_outcome: when group-0 outcomes are a function of x,
      the reweighted outcome mass is ∑_v m₁(v) μ₀(v) — group 1's
      composition evaluated at group 0's structure, the counterfactual the
      aggregate decomposition needs without a linear model.
    * decomposition: the raw gap splits into (group 1 − counterfactual) +
      (counterfactual − group 0), structure and composition.

  What this proves is that the reweighting is exact on the covariate
  distribution; what it does not prove is that the structure within x is
  the "unexplained" part — that is the P15 point again, now without
  linearity.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P15Reweight

/-- A grouped population with a discrete covariate. -/
structure Pop (Ω X : Type*) [Fintype Ω] where
  (w : Ω → ℝ)
  (w_nonneg : ∀ i, 0 ≤ w i)
  (g : Ω → Bool)
  (x : Ω → X)
  (y : Ω → ℝ)

variable {Ω X : Type*} [Fintype Ω] [Fintype X] [DecidableEq X] (P : Pop Ω X)

/-- Mass of group `b` at covariate value `v`. -/
def mass (b : Bool) (v : X) : ℝ := ∑ i ∈ univ.filter (fun i => P.g i = b ∧ P.x i = v), P.w i
/-- The DFL weight. -/
def psi (v : X) : ℝ := mass P true v / mass P false v
/-- Weighted sum of `f` over group `b`. -/
def groupSum (b : Bool) (f : Ω → ℝ) : ℝ := ∑ i ∈ univ.filter (fun i => P.g i = b), P.w i * f i
/-- Weighted sum of `f` over group 0, reweighted by `ψ`. -/
def reweightedSum (f : Ω → ℝ) : ℝ :=
  ∑ i ∈ univ.filter (fun i => P.g i = false), psi P (P.x i) * (P.w i * f i)
/-- Common support. -/
def Support : Prop := ∀ v, mass P true v ≠ 0 → mass P false v ≠ 0

theorem groupSum_fiber (b : Bool) (h : X → ℝ) :
    groupSum P b (fun i => h (P.x i)) = ∑ v, h v * mass P b v := by
  unfold groupSum mass
  rw [← sum_fiberwise (univ.filter (fun i => P.g i = b)) P.x (fun i => P.w i * h (P.x i))]
  apply sum_congr rfl; intro v _
  rw [filter_filter, mul_sum]
  apply sum_congr rfl; intro i hi
  rw [(mem_filter.mp hi).2.2]
  ring

theorem reweightedSum_fiber (h : X → ℝ) :
    reweightedSum P (fun i => h (P.x i)) = ∑ v, psi P v * (h v * mass P false v) := by
  unfold reweightedSum mass
  rw [← sum_fiberwise (univ.filter (fun i => P.g i = false)) P.x
    (fun i => psi P (P.x i) * (P.w i * h (P.x i)))]
  apply sum_congr rfl; intro v _
  simp only [filter_filter, mul_sum]
  apply sum_congr rfl; intro i hi
  rw [(mem_filter.mp hi).2.2]
  ring

/-- Reweighted group 0 has group 1's covariate distribution, exactly. -/
theorem reweighting_matches (hs : Support P) (h : X → ℝ) :
    reweightedSum P (fun i => h (P.x i)) = groupSum P true (fun i => h (P.x i)) := by
  rw [reweightedSum_fiber, groupSum_fiber]
  apply sum_congr rfl; intro v _
  unfold psi
  by_cases h1 : mass P true v = 0
  · rw [h1]; simp
  · have h0 := hs v h1
    field_simp

theorem reweighted_mass (hs : Support P) :
    reweightedSum P (fun _ => 1) = groupSum P true (fun _ => 1) :=
  reweighting_matches P hs (fun _ => 1)

omit [Fintype X] in
theorem psi_nonneg (v : X) : 0 ≤ psi P v :=
  div_nonneg (sum_nonneg fun i _ => P.w_nonneg i) (sum_nonneg fun i _ => P.w_nonneg i)

/-- With group-0 outcomes a function `μ₀` of `x`, the reweighted outcome is group 1's composition at group 0's structure. -/
theorem counterfactual_outcome (hs : Support P) (μ₀ : X → ℝ)
    (hy : ∀ i, P.g i = false → P.y i = μ₀ (P.x i)) :
    reweightedSum P P.y = ∑ v, μ₀ v * mass P true v := by
  have : reweightedSum P P.y = reweightedSum P (fun i => μ₀ (P.x i)) := by
    unfold reweightedSum
    apply sum_congr rfl; intro i hi
    rw [hy i (mem_filter.mp hi).2]
  rw [this, reweighting_matches P hs, groupSum_fiber]

/-- Structure plus composition. -/
theorem decomposition (m₁ m₀ mcf : ℝ) : m₁ - m₀ = (m₁ - mcf) + (mcf - m₀) := by ring

end Research.P15Reweight
