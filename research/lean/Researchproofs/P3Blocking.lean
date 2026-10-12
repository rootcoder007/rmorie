/-
  Researchproofs/P3Blocking.lean

  P3, continued: blocking does not always increase power (library scan A8;
  Weisburd & Britt ch. 9 pp. 389-390, 400-401).

  n pairs, effect δ, unit standard deviation σ, within-pair correlation ρ < 1,
  and the two critical values c_p (paired test, n − 1 degrees of freedom) and
  c_u (two-sample test, 2n − 2 degrees of freedom), with c_u < c_p. In the
  usual normal approximation the power deviates are
      z_p = k/√(1 − ρ) − c_p,   z_u = k − c_u,   k = δ√(n/2)/σ.
  The paired design has more power iff ρ exceeds the crossover
      ρ* = 1 − (1 + (c_p − c_u)/k)^{−2}                     (`paired_better_iff`),
  and ρ* > 0 (`crossover_pos`): for a small positive within-pair correlation
  the unblocked design has more power, because the pairing spends degrees of
  freedom it does not earn back.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P3.Blocking

def zPaired (k ρ cp : ℝ) : ℝ := k / Real.sqrt (1 - ρ) - cp
def zUnpaired (k cu : ℝ) : ℝ := k - cu
def crossover (k cp cu : ℝ) : ℝ := 1 - 1 / (1 + (cp - cu) / k) ^ 2

theorem crossover_pos (k cp cu : ℝ) (hk : 0 < k) (hc : cu < cp) : 0 < crossover k cp cu := by
  unfold crossover
  have h1 : 1 < 1 + (cp - cu) / k := by
    have : 0 < (cp - cu) / k := div_pos (by linarith) hk
    linarith
  have h2 : 1 / (1 + (cp - cu) / k) ^ 2 < 1 := by
    rw [div_lt_one (by positivity)]
    nlinarith
  linarith

theorem paired_better_iff (k ρ cp cu : ℝ) (hk : 0 < k) (hc : cu < cp) (hρ : ρ < 1) :
    zUnpaired k cu < zPaired k ρ cp ↔ crossover k cp cu < ρ := by
  unfold zUnpaired zPaired crossover
  have hs : 0 < Real.sqrt (1 - ρ) := Real.sqrt_pos.mpr (by linarith)
  have hq : 0 < 1 + (cp - cu) / k := by
    have : 0 < (cp - cu) / k := div_pos (by linarith) hk
    linarith
  -- k - cu < k / s - cp  ↔  k (1 + (cp - cu)/k) < k / s  ↔  s < 1 / (1 + d/k)  ↔  1 - ρ < (1 + d/k)^{-2}
  have step1 : (k - cu < k / Real.sqrt (1 - ρ) - cp) ↔ (1 + (cp - cu) / k) * Real.sqrt (1 - ρ) < 1 := by
    constructor
    · intro h
      have h' : k * (1 + (cp - cu) / k) < k / Real.sqrt (1 - ρ) := by
        have : k * (1 + (cp - cu) / k) = k + (cp - cu) := by field_simp
        linarith
      rw [lt_div_iff₀ hs] at h'
      nlinarith
    · intro h
      have h' : k * (1 + (cp - cu) / k) < k / Real.sqrt (1 - ρ) := by
        rw [lt_div_iff₀ hs]; nlinarith
      have : k * (1 + (cp - cu) / k) = k + (cp - cu) := by field_simp
      linarith
  rw [step1]
  have hq' : 0 < 1 / (1 + (cp - cu) / k) := by positivity
  have step2 : (1 + (cp - cu) / k) * Real.sqrt (1 - ρ) < 1 ↔ Real.sqrt (1 - ρ) < 1 / (1 + (cp - cu) / k) := by
    rw [lt_div_iff₀ hq, mul_comm]
  rw [step2, Real.sqrt_lt' hq']
  have e : (1 / (1 + (cp - cu) / k)) ^ 2 = 1 / (1 + (cp - cu) / k) ^ 2 := by rw [div_pow, one_pow]
  rw [e]
  constructor <;> intro h <;> linarith

end Research.P3.Blocking
