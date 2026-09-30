/-
  Researchproofs/P3Interference.lean

  P3. Policing effects under interference: identification with an
  exposure mapping.

  A finite population of places Ω with weights. Each place has a covariate
  stratum x : Ω → K and, after the treatment assignment of the whole
  population, an exposure level e : Ω → E (for example: 0 = untreated and
  no treated neighbour, 1 = untreated with a treated neighbour, 2 =
  treated). Potential outcomes Y(ℓ) for each exposure level ℓ. The
  observed outcome is Y(e ω).

  Ignorability of the exposure given the stratum: within every stratum
  the mean of Y(ℓ) among places at exposure ℓ equals its mean over the
  whole stratum. Positivity: every exposure level has positive mass in
  every stratum.

  Proved:
    * exposure_adjustment: under those two assumptions the mean potential
      outcome at exposure ℓ, E[Y(ℓ)], equals the stratified average of
      observed outcomes among places at exposure ℓ, i.e. it is identified
      from observables.
    * direct_spillover_decomposition: the contrast E[Y(2)] − E[Y(0)]
      (treated versus untreated-and-isolated) splits exactly into a
      spillover part E[Y(1)] − E[Y(0)] and a direct-beyond-spillover part
      E[Y(2)] − E[Y(1)]; both are identified by the first theorem.
    * misspecified_exposure_bias: if the analyst pools exposure levels 0
      and 1 (ignores spillover) the "untreated" mean is a mixture, and its
      distance from E[Y(0)] is |share of level 1 among the pooled| ×
      |E[Y(1) | pooled] − E[Y(0) | pooled]| — an explicit, reportable
      bias rather than a hidden one.

  Lean checks these identities on a finite population; whether exposure
  ignorability holds for a given deployment is a claim about the world.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P3

/-- A finite population with weights summing to one. -/
structure Pop (Ω : Type*) [Fintype Ω] where
  w : Ω → ℝ
  nonneg : ∀ ω, 0 ≤ w ω
  total : ∑ ω, w ω = 1

namespace Pop

variable {Ω : Type*} [Fintype Ω] [DecidableEq Ω] (P : Pop Ω)

/-- Weighted sum of `f` over a subset. -/
def sumOn (s : Finset Ω) (f : Ω → ℝ) : ℝ := ∑ ω ∈ s, P.w ω * f ω
/-- Mass of a subset. -/
def mass (s : Finset Ω) : ℝ := ∑ ω ∈ s, P.w ω
/-- Population mean of `f`. -/
def mean (f : Ω → ℝ) : ℝ := ∑ ω, P.w ω * f ω
/-- Mean of `f` within a subset of positive mass. -/
def meanOn (s : Finset Ω) (f : Ω → ℝ) : ℝ := P.sumOn s f / P.mass s

theorem mass_nonneg (s : Finset Ω) : 0 ≤ P.mass s := sum_nonneg (fun ω _ => P.nonneg ω)

theorem mean_eq_sum_strata {K : Type*} [Fintype K] [DecidableEq K] (x : Ω → K) (f : Ω → ℝ) :
    P.mean f = ∑ k, P.sumOn (univ.filter (fun ω => x ω = k)) f := by
  unfold mean sumOn
  rw [← sum_biUnion]
  · congr 1
    ext ω; simp
  · intro a _ b _ hab
    simp only [Function.onFun]
    rw [disjoint_left]
    intro ω ha hb
    simp only [mem_filter] at ha hb
    exact hab (ha.2.symm.trans hb.2)

end Pop

/-- The identification model: strata, exposure levels, potential outcomes. -/
structure Model (Ω K E : Type*) [Fintype Ω] [DecidableEq Ω] [Fintype K] [Fintype E] [DecidableEq K] [DecidableEq E] where
  P : Pop Ω
  x : Ω → K
  e : Ω → E
  Y : E → Ω → ℝ

namespace Model

variable {Ω K E : Type*} [Fintype Ω] [DecidableEq Ω] [Fintype K] [Fintype E] [DecidableEq K] [DecidableEq E]
variable (M : Model Ω K E)

/-- Places in stratum `k`. -/
def stratum (k : K) : Finset Ω := univ.filter (fun ω => M.x ω = k)
/-- Places in stratum `k` at exposure `ℓ`. -/
def cell (k : K) (ℓ : E) : Finset Ω := univ.filter (fun ω => M.x ω = k ∧ M.e ω = ℓ)
/-- Observed outcome. -/
def Yobs (ω : Ω) : ℝ := M.Y (M.e ω) ω

/-- Positivity: every cell has positive mass. -/
def Positivity : Prop := ∀ k ℓ, 0 < M.P.mass (M.cell k ℓ)

/-- Exposure ignorability given the stratum: within a stratum, the mean of the
potential outcome `Y(ℓ)` among places at exposure `ℓ` equals its mean over the
whole stratum. -/
def Ignorable : Prop :=
  ∀ k ℓ, M.P.meanOn (M.cell k ℓ) (M.Y ℓ) = M.P.meanOn (M.stratum k) (M.Y ℓ)

/-- Observed outcomes on a cell are the potential outcome at that exposure. -/
theorem sumOn_cell_obs (k : K) (ℓ : E) :
    M.P.sumOn (M.cell k ℓ) M.Yobs = M.P.sumOn (M.cell k ℓ) (M.Y ℓ) := by
  unfold Pop.sumOn Yobs
  apply sum_congr rfl
  intro ω hω
  simp only [cell, mem_filter] at hω
  rw [hω.2.2]

/-- The stratified estimand built from observables only. -/
def adjusted (ℓ : E) : ℝ :=
  ∑ k, M.P.mass (M.stratum k) * M.P.meanOn (M.cell k ℓ) M.Yobs

/-- Exposure adjustment: under positivity and ignorability the mean potential
outcome at exposure `ℓ` is identified by the stratified observed means. -/
theorem exposure_adjustment (hpos : M.Positivity) (hign : M.Ignorable) (ℓ : E) :
    M.P.mean (M.Y ℓ) = M.adjusted ℓ := by
  unfold adjusted
  rw [M.P.mean_eq_sum_strata M.x]
  apply sum_congr rfl
  intro k _
  have hcell := hpos k ℓ
  have hstr : 0 < M.P.mass (M.stratum k) := by
    have hsub : M.cell k ℓ ⊆ M.stratum k := by
      intro ω hω; simp only [cell, stratum, mem_filter] at hω ⊢; exact ⟨hω.1, hω.2.1⟩
    exact lt_of_lt_of_le hcell (sum_le_sum_of_subset_of_nonneg hsub (fun ω _ _ => M.P.nonneg ω))
  have h := hign k ℓ
  unfold Pop.meanOn at h ⊢
  rw [M.sumOn_cell_obs, h]
  change M.P.sumOn (M.stratum k) (M.Y ℓ) = M.P.mass (M.stratum k) * (M.P.sumOn (M.stratum k) (M.Y ℓ) / M.P.mass (M.stratum k))
  field_simp

/-- Direct versus spillover: the treated-versus-isolated contrast splits into
the spillover contrast and the direct-beyond-spillover contrast. -/
theorem direct_spillover_decomposition (ℓ0 ℓ1 ℓ2 : E) :
    M.P.mean (M.Y ℓ2) - M.P.mean (M.Y ℓ0)
      = (M.P.mean (M.Y ℓ1) - M.P.mean (M.Y ℓ0)) + (M.P.mean (M.Y ℓ2) - M.P.mean (M.Y ℓ1)) := by
  ring

/-- Pooling two exposure levels: the pooled mean is a mixture of the two
level means with weights equal to their mass shares. -/
theorem pooled_mean_mixture (s t : Finset Ω) (hdisj : Disjoint s t) (f : Ω → ℝ)
    (hs : 0 < M.P.mass s) (ht : 0 < M.P.mass t) :
    M.P.meanOn (s ∪ t) f
      = (M.P.mass s / (M.P.mass s + M.P.mass t)) * M.P.meanOn s f
        + (M.P.mass t / (M.P.mass s + M.P.mass t)) * M.P.meanOn t f := by
  unfold Pop.meanOn Pop.sumOn Pop.mass
  rw [sum_union hdisj, sum_union hdisj]
  have hs' : (∑ ω ∈ s, M.P.w ω) ≠ 0 := by unfold Pop.mass at hs; exact hs.ne'
  have ht' : (∑ ω ∈ t, M.P.w ω) ≠ 0 := by unfold Pop.mass at ht; exact ht.ne'
  have hst : (∑ ω ∈ s, M.P.w ω) + (∑ ω ∈ t, M.P.w ω) ≠ 0 := by
    unfold Pop.mass at hs ht; linarith
  field_simp

/-- The bias of pooling: the pooled mean differs from the level-`s` mean by the
mass share of `t` times the gap between the two level means. -/
theorem misspecified_exposure_bias (s t : Finset Ω) (hdisj : Disjoint s t) (f : Ω → ℝ)
    (hs : 0 < M.P.mass s) (ht : 0 < M.P.mass t) :
    M.P.meanOn (s ∪ t) f - M.P.meanOn s f
      = (M.P.mass t / (M.P.mass s + M.P.mass t)) * (M.P.meanOn t f - M.P.meanOn s f) := by
  rw [M.pooled_mean_mixture s t hdisj f hs ht]
  have hst : M.P.mass s + M.P.mass t ≠ 0 := by linarith
  field_simp
  ring

end Model

end Research.P3
