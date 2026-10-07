/-
  Researchproofs/P6AgeCrime.lean

  P6. The age–crime curve: what an aggregate curve can and cannot say.

  Individuals belong to latent types g with offending-rate curves
  λ_g : Age → ℝ and population shares π_g. The aggregate curve is the
  mixture  A(a) = Σ_g π_g λ_g(a).

  Proved:
    * aggregate_not_identifying: the aggregate curve of ANY mixture is
      also the aggregate curve of the one-type population whose single
      curve is the mixture itself. So the number of types and their
      curves are not identified from the aggregate curve alone; only
      panel (within-person) data can separate them. A finite-grid,
      fully explicit counterexample rather than an appeal to intuition.
    * invariance_sufficient: if every type's curve is the same across two
      populations (cohorts, countries) and the shares agree, the
      aggregate curves agree. The converse fails (invariance_not_necessary
      exhibits two populations with different type curves and the same
      aggregate), which is exactly why an invariant aggregate curve is
      not evidence for the invariance thesis about individuals.

  Lean checks these on finite age grids and finite type sets; the
  empirical question of which types exist is untouched.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P6

/-- A mixture population: finitely many types with shares and rate curves on a finite age grid. -/
structure Mixture (Age G : Type*) [Fintype G] where
  π : G → ℝ
  lam : G → Age → ℝ

namespace Mixture

variable {Age G : Type*} [Fintype G]

/-- The aggregate age–crime curve. -/
def aggregate (M : Mixture Age G) (a : Age) : ℝ := ∑ g, M.π g * M.lam g a

end Mixture

/-- The one-type population carrying a given curve. -/
def single (Age : Type*) (c : Age → ℝ) : Mixture Age Unit := ⟨fun _ => 1, fun _ => c⟩

theorem single_aggregate (Age : Type*) (c : Age → ℝ) :
    (single Age c).aggregate = c := by
  funext a; simp [Mixture.aggregate, single]

/-- Non-identification: every mixture's aggregate curve is produced by a
one-type population, so the aggregate does not determine the number of
types or their curves. -/
theorem aggregate_not_identifying {Age G : Type*} [Fintype G] (M : Mixture Age G) :
    ∃ N : Mixture Age Unit, N.aggregate = M.aggregate :=
  ⟨single Age M.aggregate, single_aggregate Age M.aggregate⟩

/-- Sufficient condition for aggregate invariance across two populations. -/
theorem invariance_sufficient {Age G : Type*} [Fintype G] (M N : Mixture Age G)
    (hπ : M.π = N.π) (hlam : M.lam = N.lam) : M.aggregate = N.aggregate := by
  funext a; simp [Mixture.aggregate, hπ, hlam]

/-- Invariance of the aggregate is not necessary: two populations with
different type curves (a two-type mixture and its one-type average) share
an aggregate. Stated on a two-type population over `Bool`. -/
theorem invariance_not_necessary (Age : Type*) [Inhabited Age] (early late : Age → ℝ)
    (hne : early ≠ late) :
    let M : Mixture Age Bool := ⟨fun _ => 1 / 2, fun b => if b then early else late⟩
    let N : Mixture Age Bool := ⟨fun _ => 1 / 2, fun _ a => (early a + late a) / 2⟩
    M.aggregate = N.aggregate ∧ M.lam ≠ N.lam := by
  intro M N
  constructor
  · funext a
    simp only [Mixture.aggregate, M, N, Fintype.sum_bool]
    simp; ring
  · intro h
    have h1 : M.lam true = N.lam true := by rw [h]
    have h2 : M.lam false = N.lam false := by rw [h]
    simp only [M, N] at h1 h2
    have : early = late := by
      funext a
      have e1 := congrFun h1 a
      have e2 := congrFun h2 a
      simp only [if_true, if_false] at e1 e2
      linarith
    exact hne this

end Research.P6
