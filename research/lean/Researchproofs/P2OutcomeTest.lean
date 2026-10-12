/-
  Researchproofs/P2OutcomeTest.lean

  P2/P5, continued: equal hit rates with unequal stop rates are consistent with
  bias or with none (library scan A24; TPS "From Impact to Action";
  Bowling & Phillips).

  In a threshold model each person carries a guilt probability q and is
  stopped when q exceeds the group's threshold; the hit rate is the mean of q
  among the stopped.
    * `infra_marginality`: two groups with different signal laws and
      different thresholds (0.5 and 0.2) have the same hit rate (0.9) and
      different stop rates (1/2 and 1): equal hit rates, unequal thresholds.
    * `equal_thresholds_unequal_hits`: two groups with the same threshold and
      different signal laws have different hit rates: the test flags no bias
      as bias.
    * `uniform_hit_rate`, `uniform_equal_hits_equal_thresholds`: when both
      groups' q is uniform on [0, 1] (one signal law, the single-crossing
      case), the hit rate is (1 + t)/2, strictly increasing in t, so equal hit
      rates do certify equal thresholds. The outcome test is a test only under
      that condition on the signal, which the data do not carry.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P2.OutcomeTest

/-- Hit rate and stop rate of a two-type group `(q₁, w₁), (q₂, w₂)` at threshold `t`. -/
def stopped (q w t : ℝ) : ℝ := if t < q then w else 0
def hits (q w t : ℝ) : ℝ := if t < q then w * q else 0

theorem infra_marginality :
    let stopA := stopped 0.3 (1 / 2) 0.5 + stopped 0.9 (1 / 2) 0.5
    let hitA := (hits 0.3 (1 / 2) 0.5 + hits 0.9 (1 / 2) 0.5) / stopA
    let stopB := stopped 0.9 1 0.2
    let hitB := hits 0.9 1 0.2 / stopB
    hitA = hitB ∧ stopA ≠ stopB ∧ (0.5 : ℝ) ≠ 0.2 := by
  simp only [stopped, hits]
  norm_num

theorem equal_thresholds_unequal_hits :
    let hitA := (hits 0.3 (1 / 2) 0.5 + hits 0.9 (1 / 2) 0.5) / (stopped 0.3 (1 / 2) 0.5 + stopped 0.9 (1 / 2) 0.5)
    let hitB := (hits 0.6 (1 / 2) 0.5 + hits 0.9 (1 / 2) 0.5) / (stopped 0.6 (1 / 2) 0.5 + stopped 0.9 (1 / 2) 0.5)
    hitA ≠ hitB := by
  simp only [stopped, hits]
  norm_num

/-- The hit rate of a uniform signal at threshold `t ∈ [0, 1)`: `E[q | q > t] = (1 + t)/2`. -/
def uniformHitRate (t : ℝ) : ℝ := (∫ q in t..1, q) / (1 - t)

theorem uniform_hit_rate (t : ℝ) (ht : t < 1) : uniformHitRate t = (1 + t) / 2 := by
  unfold uniformHitRate
  rw [integral_id]
  have : (1 : ℝ) - t ≠ 0 := by linarith
  field_simp
  ring

theorem uniform_equal_hits_equal_thresholds (t s : ℝ) (ht : t < 1) (hs : s < 1)
    (h : uniformHitRate t = uniformHitRate s) : t = s := by
  rw [uniform_hit_rate t ht, uniform_hit_rate s hs] at h
  linarith

end Research.P2.OutcomeTest
