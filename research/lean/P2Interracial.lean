/-
  Researchproofs/P2Interracial.lean

  P2, continued: interracial crime rates with race-of-offender
  denominators (Stolzenberg, Eitle & D'Alessio 2006; Blau's random-mixing
  null).

  With population shares p_A, p_B and N people, the random-mixing null
  says the expected number of A-on-B offences is k · p_A · p_B · N for a
  constant k (each of the p_A N potential offenders meets a B victim with
  probability p_B). Divided by the A population p_A N, the "A-on-B rate
  per A" is k · p_B — a linear function of percent B with no behavioural
  content (rate_per_offender_group). Regressing that rate on percent B
  therefore recovers a positive coefficient under the null
  (null_slope_positive), and the ratio of the two dyadic rates per
  offender group is p_B / p_A (dyad_ratio): "Black-on-White exceeds
  White-on-Black per capita" is what exposure alone predicts whenever
  p_B < p_A. The correct null denominator is the pair-exposure
  p_A · p_B · N (pair_exposure_rate_constant).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P2

/-- Expected A-on-B offences under random mixing. -/
def mixing (k pA pB N : ℝ) : ℝ := k * pA * pB * N

theorem rate_per_offender_group (k pA pB N : ℝ) (hA : 0 < pA) (hN : 0 < N) :
    mixing k pA pB N / (pA * N) = k * pB := by
  unfold mixing; field_simp

theorem null_slope_positive (k pA N pB₁ pB₂ : ℝ) (hk : 0 < k) (hA : 0 < pA) (hN : 0 < N) (h : pB₁ < pB₂) :
    mixing k pA pB₁ N / (pA * N) < mixing k pA pB₂ N / (pA * N) := by
  rw [rate_per_offender_group k pA pB₁ N hA hN, rate_per_offender_group k pA pB₂ N hA hN]
  nlinarith

/-- A-on-B per A divided by B-on-A per B equals `pB / pA`: pure exposure. -/
theorem dyad_ratio (k pA pB N : ℝ) (hk : 0 < k) (hA : 0 < pA) (hB : 0 < pB) (hN : 0 < N) :
    (mixing k pA pB N / (pA * N)) / (mixing k pB pA N / (pB * N)) = pB / pA := by
  rw [rate_per_offender_group k pA pB N hA hN, rate_per_offender_group k pB pA N hB hN]
  field_simp

/-- Per pair-exposure the null rate is the constant `k`, for every composition. -/
theorem pair_exposure_rate_constant (k pA pB N : ℝ) (hA : 0 < pA) (hB : 0 < pB) (hN : 0 < N) :
    mixing k pA pB N / (pA * pB * N) = k := by
  unfold mixing; field_simp

end Research.P2
