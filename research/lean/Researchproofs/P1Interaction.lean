/-
  Researchproofs/P1Interaction.lean

  P1, continued: the three-source count under one interaction constraint
  (Fienberg 1972; Bishop, Fienberg & Holland ch. 6).

  P1ThreeList showed that the saturated log-linear model on the 2×2×2
  table of list memberships leaves the never-listed cell free. The
  conventional identifying restriction is "no three-way interaction":
  u₁₂₃ = 0. With expected cell counts written from the log-linear
  parameters,

    * missing_cell_identified: under u₁₂₃ = 0 the missing cell is the
      closed form
          m₀₀₀ = m₁₀₀ m₀₁₀ m₀₀₁ m₁₁₁ / (m₁₁₀ m₁₀₁ m₀₁₁),
      a function of the seven observed cells only.
    * missing_cell_with_interaction: with u₁₂₃ = t the same ratio equals
      m₀₀₀ e^t, so the observed seven cells determine m₀₀₀ exactly up to
      the factor e^{−t}: the dark figure moves by the interaction the
      analyst assumes away, and nothing in the data pins it.
    * total_identified: hence N = (observed total) + m₀₀₀ under u₁₂₃ = 0.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P1.Interaction

/-- Expected cell count of the log-linear model: `a b c ∈ {0,1}` are the list indicators. -/
def m (u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t : ℝ) (a b c : ℝ) : ℝ :=
  Real.exp (u + u₁ * a + u₂ * b + u₃ * c + u₁₂ * (a * b) + u₁₃ * (a * c) + u₂₃ * (b * c) + t * (a * b * c))

theorem missing_cell_with_interaction (u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t : ℝ) :
    m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 1 0 0 * m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 0 1 0 *
      m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 0 0 1 * m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 1 1 1 /
      (m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 1 1 0 * m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 1 0 1 *
        m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 0 1 1)
      = m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 0 0 0 * Real.exp t := by
  unfold m
  simp only [mul_one, mul_zero, add_zero]
  rw [div_eq_iff (by positivity)]
  simp only [← Real.exp_add]
  congr 1
  ring

/-- Under no three-way interaction the never-listed cell is a closed form of the seven observed cells. -/
theorem missing_cell_identified (u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ : ℝ) :
    m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 0 0 0 =
      m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 1 0 0 * m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 0 1 0 *
        m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 0 0 1 * m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 1 1 1 /
        (m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 1 1 0 * m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 1 0 1 *
          m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 0 1 1) := by
  rw [missing_cell_with_interaction, Real.exp_zero, mul_one]

/-- The population total under the no-interaction restriction. -/
theorem total_identified (u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ : ℝ) (observed : ℝ)
    (_hobs : observed = m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 1 0 0 + m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 0 1 0 +
      m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 0 0 1 + m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 1 1 0 +
      m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 1 0 1 + m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 0 1 1 +
      m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 1 1 1) :
    observed + m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 0 0 0 =
      observed + m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 1 0 0 * m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 0 1 0 *
        m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 0 0 1 * m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 1 1 1 /
        (m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 1 1 0 * m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 1 0 1 *
          m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ 0 0 1 1) := by
  rw [missing_cell_identified]

/-- Two interaction values give two totals from the same observed cells: the data do not choose. -/
theorem interaction_not_identified (u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t : ℝ) (ht : t ≠ 0) :
    m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 0 0 0 ≠
      m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 1 0 0 * m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 0 1 0 *
        m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 0 0 1 * m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 1 1 1 /
        (m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 1 1 0 * m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 1 0 1 *
          m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 0 1 1) := by
  rw [missing_cell_with_interaction]
  intro h
  have hpos : 0 < m u u₁ u₂ u₃ u₁₂ u₁₃ u₂₃ t 0 0 0 := by unfold m; positivity
  have : Real.exp t = 1 := by
    have := h
    field_simp at this
    nlinarith [hpos]
  exact ht (Real.exp_eq_one_iff t |>.mp this)

end Research.P1.Interaction
