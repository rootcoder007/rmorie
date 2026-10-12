/-
  Researchproofs/P10ExtremalIndex.lean

  P10, continued: the extremal index is the reciprocal mean cluster size
  (library scan C17; Coles, Theorem 5.2, eq. 5.4, Definition 5.1).

  A block-cluster process: n blocks of m days each; a block starts a cluster
  with probability p_n = τ/(nm), and then every day of the block exceeds the
  level. Each day exceeds with marginal probability p_n, exactly as in an
  independent sequence with the same marginal. Over the nm days
      P(no exceedance) = (1 − p_n)^n → e^{−τ/m}          (`cluster_tendsto`),
  while the independent comparison gives
      (1 − p_n)^{nm} → e^{−τ}                             (`iid_tendsto`),
  and e^{−τ/m} = (e^{−τ})^{1/m} (`extremal_index`): the limit law of the
  maximum is G^θ with θ = 1/m, the reciprocal of the mean cluster size. The
  runs estimator of θ on daily counts therefore estimates 1/(mean run of
  exceedances), and a Hawkes cluster size is its reciprocal.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Filter Topology Real

noncomputable section

namespace Research.P10.ExtremalIndex

def clusterSurvival (τ : ℝ) (m n : ℕ) : ℝ := (1 - τ / (n * m)) ^ n
def iidSurvival (τ : ℝ) (m n : ℕ) : ℝ := (1 - τ / (n * m)) ^ (n * m)

theorem cluster_tendsto (τ : ℝ) (m : ℕ) (hm : 0 < m) :
    Tendsto (clusterSurvival τ m) atTop (𝓝 (exp (-τ / m))) := by
  have h := Real.tendsto_one_add_div_pow_exp (-τ / m)
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop 0] with n hn
  unfold clusterSurvival
  congr 1
  have hm' : (m : ℝ) ≠ 0 := by exact_mod_cast hm.ne'
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  field_simp
  ring

theorem iid_tendsto (τ : ℝ) (m : ℕ) (hm : 0 < m) :
    Tendsto (iidSurvival τ m) atTop (𝓝 (exp (-τ))) := by
  have h := Real.tendsto_one_add_div_pow_exp (-τ)
  have hmul : Tendsto (fun n : ℕ => n * m) atTop atTop := by
    refine tendsto_atTop_atTop.mpr fun b => ⟨b, fun n hn => ?_⟩
    calc b ≤ n := hn
      _ ≤ n * m := Nat.le_mul_of_pos_right n hm
  have h2 := h.comp hmul
  refine h2.congr' ?_
  filter_upwards [eventually_gt_atTop 0] with n hn
  unfold iidSurvival
  simp only [Function.comp]
  push_cast
  congr 1
  ring

theorem extremal_index (τ : ℝ) (m : ℕ) (hm : 0 < m) : (exp (-τ / m)) ^ m = exp (-τ) := by
  rw [← Real.exp_nat_mul]
  congr 1
  have hm' : (m : ℝ) ≠ 0 := by exact_mod_cast hm.ne'
  field_simp

end Research.P10.ExtremalIndex
