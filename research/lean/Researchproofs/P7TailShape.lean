/-
  Researchproofs/P7TailShape.lean

  P7, continued: the tail shape separates a power law from a mixed-Poisson
  place distribution (library scan C18; Coles Theorems 3.1.1 and 4.1).

  The generalised Pareto shape of a count law is read from the tail ratio
  S(2k)/S(k): a light (exponential-type) tail has ratio → 0 (shape ξ = 0), a
  Zipf(α) tail has ratio → 2^{−α} (shape ξ = 1/α).
    * `geometric_ratio_tendsto_zero`: for the geometric tail q^k the ratio is
      q^k → 0.
    * `power_ratio_const`: for the power tail k^{−α} the ratio is the constant
      2^{−α}.
    * `nb_pmf_ratio_tendsto_zero`: for the negative binomial with integer
      shape r (a Gamma-mixed Poisson), pmf(2k)/pmf(k) ≤ (2k + r)^{r−1} q^k → 0:
      the mixed-Poisson tail is light, however heterogeneous the places.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Filter Topology

noncomputable section

namespace Research.P7.TailShape

theorem geometric_ratio_tendsto_zero (q : ℝ) (hq : 0 ≤ q) (hq1 : q < 1) :
    Tendsto (fun k : ℕ => q ^ (2 * k) / q ^ k) atTop (𝓝 0) := by
  have h : ∀ k : ℕ, q ^ (2 * k) / q ^ k = q ^ k ∨ q = 0 := by
    intro k
    by_cases hq0 : q = 0
    · right; exact hq0
    · left; rw [pow_mul, sq, mul_pow, mul_div_assoc, div_self (pow_ne_zero k hq0), mul_one]
  by_cases hq0 : q = 0
  · subst hq0
    refine tendsto_const_nhds.congr' ?_
    filter_upwards [eventually_gt_atTop 0] with k hk
    simp [zero_pow hk.ne', zero_pow (by omega : 2 * k ≠ 0)]
  · have : (fun k : ℕ => q ^ (2 * k) / q ^ k) = fun k => q ^ k := by
      funext k
      rcases h k with h' | h'
      · exact h'
      · exact absurd h' hq0
    rw [this]
    exact tendsto_pow_atTop_nhds_zero_of_lt_one hq hq1

theorem power_ratio_const (α : ℝ) (k : ℝ) (hk : 0 < k) :
    (2 * k) ^ (-α) / k ^ (-α) = 2 ^ (-α) := by
  rw [Real.mul_rpow (by norm_num) hk.le, mul_div_assoc, div_self (Real.rpow_pos_of_pos hk _).ne', mul_one]

/-- The negative binomial pmf with integer shape `r`: `C(k + r − 1, k) p^r q^k`. -/
def nbPmf (r : ℕ) (p q : ℝ) (k : ℕ) : ℝ := ((k + r - 1).choose k : ℝ) * p ^ r * q ^ k

theorem nb_pmf_ratio_le (r : ℕ) (hr : 1 ≤ r) (p q : ℝ) (hp : 0 < p) (hq : 0 < q) (k : ℕ) :
    nbPmf r p q (2 * k) / nbPmf r p q k ≤ (2 * k + r : ℝ) ^ (r - 1) * q ^ k := by
  unfold nbPmf
  have hc1 : (1 : ℝ) ≤ ((k + r - 1).choose k : ℝ) := by
    exact_mod_cast Nat.succ_le_of_lt (Nat.choose_pos (by omega))
  have hc2 : ((2 * k + r - 1).choose (2 * k) : ℝ) ≤ (2 * k + r : ℝ) ^ (r - 1) := by
    have hsymm : (2 * k + r - 1).choose (2 * k) = (2 * k + r - 1).choose (r - 1) := by
      rw [← Nat.choose_symm (by omega : 2 * k ≤ 2 * k + r - 1)]; congr 1; omega
    rw [hsymm]
    calc ((2 * k + r - 1).choose (r - 1) : ℝ)
        ≤ ((2 * k + r - 1 : ℕ) : ℝ) ^ (r - 1) / ((r - 1).factorial : ℝ) := by
          have h := Nat.choose_le_pow_div (α := ℝ) (r - 1) (2 * k + r - 1)
          simpa using h
      _ ≤ ((2 * k + r - 1 : ℕ) : ℝ) ^ (r - 1) := by
          apply div_le_self (by positivity)
          exact_mod_cast Nat.one_le_iff_ne_zero.mpr (Nat.factorial_ne_zero _)
      _ ≤ (2 * k + r : ℝ) ^ (r - 1) := by
          apply pow_le_pow_left₀ (by positivity)
          have : ((2 * k + r - 1 : ℕ) : ℝ) ≤ ((2 * k + r : ℕ) : ℝ) := by exact_mod_cast Nat.sub_le _ _
          push_cast at this; linarith
  have hpos : 0 < ((k + r - 1).choose k : ℝ) * p ^ r * q ^ k := by positivity
  rw [div_le_iff₀ hpos]
  have e : (2 * k + r - 1 : ℕ) = 2 * k + r - 1 := rfl
  calc ((2 * k + r - 1).choose (2 * k) : ℝ) * p ^ r * q ^ (2 * k)
      ≤ (2 * k + r : ℝ) ^ (r - 1) * p ^ r * q ^ (2 * k) := by gcongr
    _ = (2 * k + r : ℝ) ^ (r - 1) * q ^ k * (1 * p ^ r * q ^ k) := by ring
    _ ≤ (2 * k + r : ℝ) ^ (r - 1) * q ^ k * (((k + r - 1).choose k : ℝ) * p ^ r * q ^ k) := by gcongr

theorem nb_pmf_ratio_tendsto_zero (r : ℕ) (hr : 1 ≤ r) (p q : ℝ) (hp : 0 < p) (hq : 0 < q) (hq1 : q < 1) :
    Tendsto (fun k : ℕ => nbPmf r p q (2 * k) / nbPmf r p q k) atTop (𝓝 0) := by
  -- squeeze between 0 and (2k + r)^(r-1) q^k ≤ (3k)^(r-1) q^k for k ≥ r
  have hupper : Tendsto (fun k : ℕ => (3 : ℝ) ^ (r - 1) * ((k : ℝ) ^ (r - 1) * q ^ k)) atTop (𝓝 0) := by
    have := tendsto_pow_const_mul_const_pow_of_abs_lt_one (r - 1) (by rw [abs_of_pos hq]; exact hq1 : |q| < 1)
    simpa using this.const_mul ((3 : ℝ) ^ (r - 1))
  refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds hupper ?_ ?_
  · filter_upwards with k
    unfold nbPmf; positivity
  · filter_upwards [eventually_ge_atTop r] with k hk
    refine (nb_pmf_ratio_le r hr p q hp hq k).trans ?_
    rw [← mul_assoc, ← mul_pow]
    apply mul_le_mul_of_nonneg_right _ (pow_nonneg hq.le k)
    apply pow_le_pow_left₀ (by positivity)
    have : (r : ℝ) ≤ k := by exact_mod_cast hk
    linarith

end Research.P7.TailShape
