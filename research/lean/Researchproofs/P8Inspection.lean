/-
  Researchproofs/P8Inspection.lean

  P8, continued: the criminal–citizen–inspector game has a cyclic phase with
  no stable interior equilibrium (library scans A18 and C24; D'Orsogna &
  Perc Sec. IV, Eq. 6).

  Replicator dynamics on the simplex for a rock–paper–scissors payoff with
  win a > 0 and loss −a (zero-sum, each strategy beats the next):
      ẋ_i = x_i ((A x)_i − xᵀA x).
    * `payoff_zero_sum`: xᵀA x = 0 on the whole simplex.
    * `interior_fixed_point`: the centroid (1/3, 1/3, 1/3) is a rest point.
    * `conserved`: V = x₁ x₂ x₃ is a first integral — its time derivative
      along the flow is identically zero — so orbits lie on the level
      curves of V around the centroid: the interior rest point is a centre
      (closed orbits), not an attractor. More inspection does not
      monotonically reduce crime; the strategies cycle.
  Everything is polynomial algebra in (x₁, x₂, x₃, a).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

namespace Research.P8.Inspection

/-- The payoff `(A x)` of rock–paper–scissors with stake `a`: `1` beats `2`, `2` beats `3`, `3` beats `1`. -/
def Ax (a x1 x2 x3 : ℝ) : ℝ × ℝ × ℝ := (a * (x2 - x3), a * (x3 - x1), a * (x1 - x2))

/-- `xᵀ A x` vanishes identically (zero-sum, antisymmetric payoff). -/
theorem payoff_zero_sum (a x1 x2 x3 : ℝ) :
    x1 * (Ax a x1 x2 x3).1 + x2 * (Ax a x1 x2 x3).2.1 + x3 * (Ax a x1 x2 x3).2.2 = 0 := by
  unfold Ax; ring

/-- The replicator vector field. -/
def field (a x1 x2 x3 : ℝ) : ℝ × ℝ × ℝ :=
  (x1 * (Ax a x1 x2 x3).1, x2 * (Ax a x1 x2 x3).2.1, x3 * (Ax a x1 x2 x3).2.2)

theorem interior_fixed_point (a : ℝ) : field a (1 / 3) (1 / 3) (1 / 3) = (0, 0, 0) := by
  unfold field Ax; norm_num

/-- `V = x₁ x₂ x₃` is conserved: its derivative along the field, `∂V·f`, is zero. -/
theorem conserved (a x1 x2 x3 : ℝ) :
    (x2 * x3) * (field a x1 x2 x3).1 + (x1 * x3) * (field a x1 x2 x3).2.1 + (x1 * x2) * (field a x1 x2 x3).2.2 = 0 := by
  unfold field Ax; ring

/-- The simplex is invariant: the field's components sum to zero. -/
theorem simplex_invariant (a x1 x2 x3 : ℝ) :
    (field a x1 x2 x3).1 + (field a x1 x2 x3).2.1 + (field a x1 x2 x3).2.2 = 0 := by
  unfold field Ax; ring

/-- Away from the centroid the field is not zero (for `a ≠ 0` and an interior point not at the
centroid), so the centroid is the only interior rest point. -/
theorem only_interior_rest (a x1 x2 x3 : ℝ) (ha : a ≠ 0) (h1 : 0 < x1) (h2 : 0 < x2) (h3 : 0 < x3)
    (hfix : field a x1 x2 x3 = (0, 0, 0)) : x1 = x2 ∧ x2 = x3 := by
  unfold field Ax at hfix
  simp only [Prod.mk.injEq] at hfix
  obtain ⟨e1, e2, _⟩ := hfix
  have : x2 - x3 = 0 := by
    rcases mul_eq_zero.mp e1 with h | h
    · exact absurd h h1.ne'
    · rcases mul_eq_zero.mp h with h' | h'
      · exact absurd h' ha
      · exact h'
  have : x3 - x1 = 0 := by
    rcases mul_eq_zero.mp e2 with h | h
    · exact absurd h h2.ne'
    · rcases mul_eq_zero.mp h with h' | h'
      · exact absurd h' ha
      · exact h'
  constructor <;> linarith

end Research.P8.Inspection
