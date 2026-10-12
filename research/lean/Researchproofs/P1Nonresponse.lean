/-
  Researchproofs/P1Nonresponse.lean

  P1, continued: nonresponse and the width of what a proportion can be
  (library scan B10; Lohr §8.1; Manski ch. 2 eq. 2.4; a 93 percent dark figure).

  A proportion p = r p_obs + (1 − r) p_miss, with r the share that responds
  (or reports) and p_miss ∈ [0, 1] unknown. Its identification region is
  [r p_obs, r p_obs + 1 − r] (`region_lower`, `region_upper`, both attained:
  `region_sharp`), of width exactly 1 − r (`region_width`). With r = 0.106
  (Mexico's reporting rate) the region has width 0.894: the recorded share
  says almost nothing about the whole.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

namespace Research.P1.Nonresponse

/-- The proportion when the missing part has value `q`. -/
def prop (r p_obs q : ℝ) : ℝ := r * p_obs + (1 - r) * q

theorem region_lower (r p_obs q : ℝ) (hr : 0 ≤ r) (hr1 : r ≤ 1) (hq : 0 ≤ q) :
    r * p_obs ≤ prop r p_obs q := by
  unfold prop; nlinarith

theorem region_upper (r p_obs q : ℝ) (hr : 0 ≤ r) (hr1 : r ≤ 1) (hq1 : q ≤ 1) :
    prop r p_obs q ≤ r * p_obs + (1 - r) := by
  unfold prop; nlinarith

/-- Both ends are attained (`q = 0`, `q = 1`). -/
theorem region_sharp (r p_obs : ℝ) :
    prop r p_obs 0 = r * p_obs ∧ prop r p_obs 1 = r * p_obs + (1 - r) := by
  unfold prop; constructor <;> ring

theorem region_width (r p_obs : ℝ) : (r * p_obs + (1 - r)) - r * p_obs = 1 - r := by ring

end Research.P1.Nonresponse
