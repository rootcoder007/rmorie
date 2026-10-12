/-
  Researchproofs/P2Misclassification.lean

  P2, continued: nondifferential misclassification of a binary exposure
  attenuates the risk difference by sensitivity + specificity − 1; an ordinal
  exposure can flip its sign (library scan B14; Hernán & Robins, Fine Point
  9.1, Technical Point 9.1).

  With true binary exposure A, outcome risks r1 = P(Y | A = 1), r0 = P(Y | A =
  0), prevalence π, sensitivity se and specificity sp, the measured risks are
  mixtures and the measured risk difference is
      RD* = (se + sp − 1) · RD · π(1 − π) / (d1 d0)             (`binary_attenuation`),
  with d1, d0 the measured-group shares, so when se + sp ≥ 1 the measured
  difference has the sign of the true one or is zero (`binary_sign`).
  For a three-level exposure with risks (0, 1/2, 1) and a non-monotone
  misclassification that sends levels 0 and 2 to the measured "high" group
  and level 1 to the measured "low" group, the measured contrast is
  1/2 − 1/2 = 0 while the true top-vs-bottom contrast is 1
  (`ordinal_witness`); with the groups arranged the other way the measured
  contrast is negative (`ordinal_reversal`). The binary theorem is a binary
  theorem.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P2.Misclassification

/-- The measured risk difference under nondifferential misclassification of a binary
exposure, written out from the two mixtures: outcome mass over group share in each
measured group. -/
def measuredRD (r1 r0 π se sp : ℝ) : ℝ :=
  (π * se * r1 + (1 - π) * (1 - sp) * r0) / (π * se + (1 - π) * (1 - sp))
    - (π * (1 - se) * r1 + (1 - π) * sp * r0) / (π * (1 - se) + (1 - π) * sp)

/-- `RD* = (se + sp − 1) RD π(1 − π) / (d1 d0)` when both measured groups are non-empty. -/
theorem binary_attenuation (r1 r0 π se sp : ℝ) (hd1 : π * se + (1 - π) * (1 - sp) ≠ 0)
    (hd0 : π * (1 - se) + (1 - π) * sp ≠ 0) :
    measuredRD r1 r0 π se sp
      = (se + sp - 1) * (r1 - r0) * (π * (1 - π)) / ((π * se + (1 - π) * (1 - sp)) * (π * (1 - se) + (1 - π) * sp)) := by
  unfold measuredRD
  field_simp
  ring

/-- With `se + sp ≥ 1` the measured difference has the sign of the true one, or is zero. -/
theorem binary_sign (r1 r0 π se sp : ℝ) (hπ : 0 < π) (hπ1 : π < 1)
    (hvalid : 1 ≤ se + sp) (hd1 : 0 < π * se + (1 - π) * (1 - sp))
    (hd0 : 0 < π * (1 - se) + (1 - π) * sp) (hrd : 0 < r1 - r0) :
    0 ≤ measuredRD r1 r0 π se sp := by
  rw [binary_attenuation r1 r0 π se sp hd1.ne' hd0.ne']
  have h1 : 0 ≤ se + sp - 1 := by linarith
  have h2 : 0 ≤ π * (1 - π) := by nlinarith
  exact div_nonneg (mul_nonneg (mul_nonneg h1 hrd.le) h2) (mul_pos hd1 hd0).le

/-- Three true levels with risks `0, 1/2, 1`; the measured "high" group holds levels 0 and 2
(equal weight, mean risk `1/2`), the measured "low" group holds level 1 (risk `1/2`): the
measured contrast is `0` while the true top-vs-bottom contrast is `1`. -/
theorem ordinal_witness : ((0 : ℝ) + 1) / 2 - 1 / 2 = 0 ∧ (1 : ℝ) - 0 = 1 := by norm_num

/-- Level 2 alone in the measured "low" group (risk `1`), levels 0 and 1 in "high" (mean risk
`1/4`): the measured contrast `1/4 − 1` is negative while the true contrast is positive. -/
theorem ordinal_reversal : ((0 : ℝ) + 1 / 2) / 2 - 1 < 0 ∧ (0 : ℝ) < 1 - 0 := by norm_num

end Research.P2.Misclassification
