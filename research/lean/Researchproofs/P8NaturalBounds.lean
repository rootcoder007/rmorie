/-
  Researchproofs/P8NaturalBounds.lean

  P8, continued: instrumental-variable natural bounds (library scan B11;
  Hernán & Robins Technical Point 16.2; Pearl §8.2.4).

  Binary instrument Z, treatment A, outcome Y in [0, 1], exclusion restriction.
  Observed: p₁₁ = P(A = 1 | Z = 1), p₁₀ = P(A = 1 | Z = 0), m₁ = E[Y | A = 1, Z = 1],
  m₀ = E[Y | A = 0, Z = 0]. Unobserved: q₁ = E[Y(1) | A = 0, Z = 1] and
  q₀ = E[Y(0) | A = 1, Z = 0], each in [0, 1]. Then
      E[Y(1)] = m₁ p₁₁ + q₁ (1 − p₁₁),   E[Y(0)] = m₀ (1 − p₁₀) + q₀ p₁₀,
  so the average effect lies in an interval (`ate_mem`) whose ends are attained
  (`bounds_attained`) and whose width is exactly
      P(A = 0 | Z = 1) + P(A = 1 | Z = 0)                     (`width`),
  zero under perfect compliance (`perfect_compliance_point`). Balke and Pearl's
  sharper linear-programming bounds, which can collapse to a point at fifty
  percent noncompliance, are not formalised here; the natural bounds contain
  them (the natural model places no restriction on the response types).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

namespace Research.P8.NaturalBounds

def ate (p11 p10 m1 m0 q1 q0 : ℝ) : ℝ :=
  (m1 * p11 + q1 * (1 - p11)) - (m0 * (1 - p10) + q0 * p10)
def lower (p11 p10 m1 m0 : ℝ) : ℝ := m1 * p11 - m0 * (1 - p10) - p10
def upper (p11 p10 m1 m0 : ℝ) : ℝ := m1 * p11 + (1 - p11) - m0 * (1 - p10)

theorem ate_mem (p11 p10 m1 m0 q1 q0 : ℝ) (hp11 : p11 ≤ 1) (hp10 : 0 ≤ p10)
    (hq1 : 0 ≤ q1) (hq1' : q1 ≤ 1) (hq0 : 0 ≤ q0) (hq0' : q0 ≤ 1) :
    lower p11 p10 m1 m0 ≤ ate p11 p10 m1 m0 q1 q0 ∧ ate p11 p10 m1 m0 q1 q0 ≤ upper p11 p10 m1 m0 := by
  unfold lower upper ate
  constructor <;> nlinarith

theorem bounds_attained (p11 p10 m1 m0 : ℝ) :
    ate p11 p10 m1 m0 0 1 = lower p11 p10 m1 m0 ∧ ate p11 p10 m1 m0 1 0 = upper p11 p10 m1 m0 := by
  unfold ate lower upper; constructor <;> ring

theorem width (p11 p10 m1 m0 : ℝ) :
    upper p11 p10 m1 m0 - lower p11 p10 m1 m0 = (1 - p11) + p10 := by
  unfold upper lower; ring

theorem perfect_compliance_point (m1 m0 : ℝ) :
    upper 1 0 m1 m0 - lower 1 0 m1 m0 = 0 ∧ upper 1 0 m1 m0 = m1 - m0 := by
  unfold upper lower; constructor <;> ring

/-- Fifty percent noncompliance in both arms gives natural bounds of width one: nothing. -/
theorem half_noncompliance_width (m1 m0 : ℝ) :
    upper (1 / 2) (1 / 2) m1 m0 - lower (1 / 2) (1 / 2) m1 m0 = 1 := by
  rw [width]; norm_num

end Research.P8.NaturalBounds
