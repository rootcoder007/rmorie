/-
  IndigenousWellness/Ordinal.lean

  Whole rating distributions (Methods, "Whole rating distributions", the equations for `A`
  and for `Var(Â)`). R function certified: `ordcmp()`.

  Shares `p_k`, `q_k` over ordered categories `0, …, K-1`; `below q k = Q_{k-1}`; the
  Mann–Whitney probability `A = ∑ p_k (Q_{k-1} + q_k/2)`; the reference placement
  `G₁(k) = P_{>k} + p_k/2`.

    * `mw_add_swap`: A(p,q) + A(q,p) = (∑p)(∑q), so for normalised shares A(q,p) = 1 - A(p,q),
      A(p,p) = 1/2, and Cliff's δ = 2A - 1 is antisymmetric.
    * `mw_nonneg`, `mw_le_one`, `cliff_bounds`: 0 ≤ A ≤ 1, -1 ≤ δ ≤ 1.
    * `placement_ref`: ∑ q_k G₁(k) = A, so both placements have mean A and the two terms of
      the U-statistic variance, ∑ p G₀² - A² and ∑ q G₁² - A², are variances (`ordvar_nonneg`).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace IndigenousWellness.Ordinal

/-- `Q_{k-1}`: the share strictly below category `k`. -/
def below (q : ℕ → ℝ) (k : ℕ) : ℝ := ∑ j ∈ range k, q j

/-- `P_{>k}` over `K` categories: the share strictly above category `k`. -/
def above (K : ℕ) (p : ℕ → ℝ) (k : ℕ) : ℝ := ∑ j ∈ Ico (k + 1) K, p j

/-- The Mann–Whitney probability `A = ∑_k p_k (Q_{k-1} + q_k/2)` (`probbet` of `ordcmp()`). -/
def mw (K : ℕ) (p q : ℕ → ℝ) : ℝ := ∑ k ∈ range K, p k * (below q k + q k / 2)

/-- `A(p,q) + A(q,p) = (∑ p)(∑ q)`. -/
theorem mw_add_swap (K : ℕ) (p q : ℕ → ℝ) :
    mw K p q + mw K q p = (∑ k ∈ range K, p k) * (∑ k ∈ range K, q k) := by
  induction K with
  | zero => simp [mw]
  | succ K ih =>
    simp only [mw, below, sum_range_succ] at ih ⊢
    linear_combination ih

theorem mw_swap {K : ℕ} {p q : ℕ → ℝ} (hp : ∑ k ∈ range K, p k = 1) (hq : ∑ k ∈ range K, q k = 1) :
    mw K q p = 1 - mw K p q := by
  have := mw_add_swap K p q; rw [hp, hq] at this; linarith

/-- A group compared with itself: `A = 1/2`, `δ = 0`. -/
theorem mw_self {K : ℕ} {p : ℕ → ℝ} (hp : ∑ k ∈ range K, p k = 1) : mw K p p = 1 / 2 := by
  have := mw_add_swap K p p; rw [hp] at this; linarith

theorem mw_nonneg {K : ℕ} {p q : ℕ → ℝ} (hp : ∀ k, 0 ≤ p k) (hq : ∀ k, 0 ≤ q k) : 0 ≤ mw K p q := by
  unfold mw below
  exact sum_nonneg fun k _ => mul_nonneg (hp k)
    (add_nonneg (sum_nonneg fun j _ => hq j) (by linarith [hq k]))

theorem mw_le_one {K : ℕ} {p q : ℕ → ℝ} (hp0 : ∀ k, 0 ≤ p k) (hq0 : ∀ k, 0 ≤ q k)
    (hp : ∑ k ∈ range K, p k = 1) (hq : ∑ k ∈ range K, q k = 1) : mw K p q ≤ 1 := by
  rw [← sub_nonneg, ← mw_swap hp hq]; exact mw_nonneg hq0 hp0

/-- Cliff's `δ = 2A - 1` lies in `[-1, 1]` and changes sign when the groups are swapped. -/
theorem cliff_bounds {K : ℕ} {p q : ℕ → ℝ} (hp0 : ∀ k, 0 ≤ p k) (hq0 : ∀ k, 0 ≤ q k)
    (hp : ∑ k ∈ range K, p k = 1) (hq : ∑ k ∈ range K, q k = 1) :
    -1 ≤ 2 * mw K p q - 1 ∧ 2 * mw K p q - 1 ≤ 1 ∧ 2 * mw K q p - 1 = -(2 * mw K p q - 1) := by
  have h0 := mw_nonneg (K := K) hp0 hq0
  have h1 := mw_le_one hp0 hq0 hp hq
  refine ⟨by linarith, by linarith, ?_⟩
  rw [mw_swap hp hq]; ring

/-- The reference placement `G₁(k) = P_{>k} + p_k/2` has `q`-mean `A`. -/
theorem placement_ref (K : ℕ) (p q : ℕ → ℝ) :
    ∑ k ∈ range K, q k * (above K p k + p k / 2) = mw K p q := by
  have key : ∀ k ∈ range K, q k * (above K p k + p k / 2) =
      (∑ j ∈ range K, p j) * q k - q k * (below p k + p k / 2) := by
    intro k hk
    have hk' : k + 1 ≤ K := mem_range.mp hk
    have hs := sum_range_add_sum_Ico p hk'
    rw [sum_range_succ] at hs
    unfold above below
    rw [← hs]; ring
  rw [sum_congr rfl key, sum_sub_distrib, ← mul_sum]
  have := mw_add_swap K p q
  unfold mw at this ⊢
  linarith

/-- A weighted variance is nonnegative: `∑ p g² - (∑ p g)² ≥ 0` for shares summing to one. -/
theorem var_nonneg {K : ℕ} {p : ℕ → ℝ} (g : ℕ → ℝ) (hp0 : ∀ k, 0 ≤ p k)
    (hp : ∑ k ∈ range K, p k = 1) :
    0 ≤ ∑ k ∈ range K, p k * g k ^ 2 - (∑ k ∈ range K, p k * g k) ^ 2 := by
  set m := ∑ k ∈ range K, p k * g k
  have e : ∑ k ∈ range K, p k * (g k - m) ^ 2 = ∑ k ∈ range K, p k * g k ^ 2 - m ^ 2 := by
    have : ∀ k ∈ range K, p k * (g k - m) ^ 2 = p k * g k ^ 2 - 2 * m * (p k * g k) + m ^ 2 * p k := by
      intro k _; ring
    rw [sum_congr rfl this, sum_add_distrib, sum_sub_distrib, ← mul_sum, ← mul_sum, hp]; ring
  rw [← e]; exact sum_nonneg fun k _ => mul_nonneg (hp0 k) (sq_nonneg _)

/-- The U-statistic variance of `ordcmp()`,
`(∑ p G₀² - A²)/n₁ + (∑ q G₁² - A²)/n₀`, is nonnegative for positive effective sizes. -/
theorem ordvar_nonneg {K : ℕ} {p q : ℕ → ℝ} {n₁ n₀ : ℝ} (hp0 : ∀ k, 0 ≤ p k) (hq0 : ∀ k, 0 ≤ q k)
    (hp : ∑ k ∈ range K, p k = 1) (hq : ∑ k ∈ range K, q k = 1) (hn₁ : 0 < n₁) (hn₀ : 0 < n₀) :
    0 ≤ (∑ k ∈ range K, p k * (below q k + q k / 2) ^ 2 - mw K p q ^ 2) / n₁ +
      (∑ k ∈ range K, q k * (above K p k + p k / 2) ^ 2 - mw K p q ^ 2) / n₀ := by
  have h1 := var_nonneg (fun k => below q k + q k / 2) hp0 hp
  have h2 := var_nonneg (fun k => above K p k + p k / 2) hq0 hq
  rw [placement_ref] at h2
  exact add_nonneg (div_nonneg h1 hn₁.le) (div_nonneg h2 hn₀.le)

end IndigenousWellness.Ordinal
