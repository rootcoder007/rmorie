/-
  IndigenousWellness/Balance.lean

  Holistic balance (Methods, "Holistic balance", Definition and the two displayed equations;
  "The four directions"). R functions certified: `holbal()`, `mhcont()` (balance across the
  four directions).

  For domain weights `w ≥ 0`, `∑ w = 1` and levels `p > 0`:
  `L = ∑ w p`, `G = exp(∑ w log p)`, `B = G / L = 1 - 𝒜₁`.

    * `gm_eq_prod`: `exp(∑ w log p) = ∏ p^w` (the weighted geometric mean).
    * `bal_pos`, `bal_le_one`: `0 < B ≤ 1` (weighted AM-GM).
    * `bal_eq_one_iff`: `B = 1` iff every domain of positive weight is at the level `L`;
      a domain of weight zero is free (the paper's "every domain" needs "of positive weight").
    * `bal_scale`: `B(c p) = B(p)` for `c > 0` (scale invariance).
    * `bal_example`: the paper's example (80, 80, 20, 20 against 50 everywhere) has the same
      level 1/2 and balance 4/5, not 0.4 (0.4 is the geometric mean).
    * `bal_not_concave`: `B` is not concave on `(0,1)²` with equal weights, so the paper's
      (and the code comment's) "B is concave in the p_k" is false.
    * `balci_bounds`: the bias-shifted, clamped interval of `holbal()` satisfies
      `0 ≤ B_lo ≤ B̂ ≤ B_hi ≤ 1` whatever the Monte Carlo quantiles are.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset Real

namespace IndigenousWellness.Balance

variable {ι : Type*} [Fintype ι]

/-- The level `L = ∑ w p`. -/
def level (w p : ι → ℝ) : ℝ := ∑ i, w i * p i

/-- The weighted geometric mean `exp(∑ w log p)`. -/
def gm (w p : ι → ℝ) : ℝ := Real.exp (∑ i, w i * Real.log (p i))

/-- Holistic balance `B = exp(∑ w log p) / ∑ w p`. -/
def bal (w p : ι → ℝ) : ℝ := gm w p / level w p

theorem gm_eq_prod (w : ι → ℝ) {p : ι → ℝ} (hp : ∀ i, 0 < p i) : gm w p = ∏ i, p i ^ w i := by
  unfold gm; rw [Real.exp_sum]
  refine prod_congr rfl fun i _ => ?_
  rw [Real.rpow_def_of_pos (hp i), mul_comm]

theorem gm_pos (w p : ι → ℝ) : 0 < gm w p := Real.exp_pos _

/-- Weighted AM-GM: `G ≤ L`. -/
theorem gm_le_level {w p : ι → ℝ} (hw : ∀ i, 0 ≤ w i) (hw1 : ∑ i, w i = 1) (hp : ∀ i, 0 < p i) :
    gm w p ≤ level w p := by
  rw [gm_eq_prod w hp]
  exact Real.geom_mean_le_arith_mean_weighted univ w p (fun i _ => hw i) hw1 (fun i _ => (hp i).le)

theorem level_pos {w p : ι → ℝ} (hw : ∀ i, 0 ≤ w i) (hw1 : ∑ i, w i = 1) (hp : ∀ i, 0 < p i) :
    0 < level w p := lt_of_lt_of_le (gm_pos w p) (gm_le_level hw hw1 hp)

theorem bal_pos {w p : ι → ℝ} (hw : ∀ i, 0 ≤ w i) (hw1 : ∑ i, w i = 1) (hp : ∀ i, 0 < p i) :
    0 < bal w p := div_pos (gm_pos w p) (level_pos hw hw1 hp)

theorem bal_le_one {w p : ι → ℝ} (hw : ∀ i, 0 ≤ w i) (hw1 : ∑ i, w i = 1) (hp : ∀ i, 0 < p i) :
    bal w p ≤ 1 := by
  unfold bal; rw [div_le_one (level_pos hw hw1 hp)]; exact gm_le_level hw hw1 hp

/-- Atkinson's index `𝒜₁ = 1 - B` lies in `[0, 1)`. -/
theorem atkinson_bounds {w p : ι → ℝ} (hw : ∀ i, 0 ≤ w i) (hw1 : ∑ i, w i = 1) (hp : ∀ i, 0 < p i) :
    0 ≤ 1 - bal w p ∧ 1 - bal w p < 1 := by
  have := bal_pos hw hw1 hp; have := bal_le_one hw hw1 hp; constructor <;> linarith

/-- `B = 1` exactly when every domain of positive weight is at the level `L`. -/
theorem bal_eq_one_iff {w p : ι → ℝ} (hw : ∀ i, 0 ≤ w i) (hw1 : ∑ i, w i = 1) (hp : ∀ i, 0 < p i) :
    bal w p = 1 ↔ ∀ j, w j ≠ 0 → p j = level w p := by
  unfold bal
  rw [div_eq_one_iff_eq (level_pos hw hw1 hp).ne', gm_eq_prod w hp]
  have := Real.geom_mean_eq_arith_mean_weighted_iff_of_nonneg' univ w p (fun i _ => hw i) hw1
    (fun i _ => (hp i).le)
  simpa [level] using this

/-- Equivalently: `B = 1` iff all domains of positive weight are at one common level. -/
theorem bal_eq_one_iff_equal {w p : ι → ℝ} (hw : ∀ i, 0 ≤ w i) (hw1 : ∑ i, w i = 1)
    (hp : ∀ i, 0 < p i) :
    bal w p = 1 ↔ ∀ i j, w i ≠ 0 → w j ≠ 0 → p i = p j := by
  rw [bal_eq_one_iff hw hw1 hp]
  constructor
  · intro h i j hi hj; rw [h i hi, h j hj]
  · intro h j hj
    -- the level is a weighted mean of values all equal to `p j` on the support
    unfold level
    have : ∀ i ∈ univ, w i * p i = w i * p j := by
      intro i _
      by_cases hi : w i = 0
      · simp [hi]
      · rw [h i j hi hj]
    rw [sum_congr rfl this, ← sum_mul, hw1, one_mul]

/-- Scale invariance: `B(c p) = B(p)` for `c > 0`. -/
theorem bal_scale {w p : ι → ℝ} {c : ℝ} (hw1 : ∑ i, w i = 1) (hp : ∀ i, 0 < p i) (hc : 0 < c) :
    bal w (fun i => c * p i) = bal w p := by
  unfold bal gm level
  have hlog : ∀ i ∈ univ, w i * Real.log (c * p i) = Real.log c * w i + w i * Real.log (p i) := by
    intro i _; rw [Real.log_mul hc.ne' (hp i).ne']; ring
  rw [sum_congr rfl hlog, sum_add_distrib, ← mul_sum, hw1, mul_one, Real.exp_add, Real.exp_log hc]
  have hl : ∑ i, w i * (c * p i) = c * ∑ i, w i * p i := by
    rw [mul_sum]; exact sum_congr rfl fun i _ => by ring
  rw [hl, mul_div_mul_left _ _ hc.ne']

/-- The level is between the smallest and the largest domain. -/
theorem level_between {w p : ι → ℝ} {a b : ℝ} (hw : ∀ i, 0 ≤ w i) (hw1 : ∑ i, w i = 1)
    (ha : ∀ i, a ≤ p i) (hb : ∀ i, p i ≤ b) : a ≤ level w p ∧ level w p ≤ b := by
  unfold level
  constructor
  · calc a = ∑ i, w i * a := by rw [← sum_mul, hw1, one_mul]
      _ ≤ ∑ i, w i * p i := sum_le_sum fun i _ => mul_le_mul_of_nonneg_left (ha i) (hw i)
  · calc ∑ i, w i * p i ≤ ∑ i, w i * b := sum_le_sum fun i _ => mul_le_mul_of_nonneg_left (hb i) (hw i)
      _ = b := by rw [← sum_mul, hw1, one_mul]

/-! ### The paper's worked example -/

/-- Equal weights on four domains. -/
def w4 : Fin 4 → ℝ := fun _ => 1 / 4

/-- 80% in two domains, 20% in two others. -/
def pEx : Fin 4 → ℝ := ![4 / 5, 4 / 5, 1 / 5, 1 / 5]

/-- The example has level 1/2 (as 50% everywhere) and balance 4/5: the paper's "0.4" is the
geometric mean, not the balance. -/
theorem bal_example : level w4 pEx = 1 / 2 ∧ gm w4 pEx = 2 / 5 ∧ bal w4 pEx = 4 / 5 := by
  have hl : level w4 pEx = 1 / 2 := by
    simp [level, w4, pEx, Fin.sum_univ_four]; norm_num
  have hs : ∑ i, w4 i * Real.log (pEx i) = Real.log (2 / 5) := by
    simp only [w4, pEx, Fin.sum_univ_four]
    simp
    have h : Real.log (4 / 25) = Real.log (4 / 5) - Real.log 5 := by
      rw [show (4 / 25 : ℝ) = (4 / 5) / 5 by norm_num, Real.log_div (by norm_num) (by norm_num)]
    have h2 : Real.log (4 / 25) = 2 * Real.log (2 / 5) := by
      rw [show (4 / 25 : ℝ) = (2 / 5) ^ 2 by norm_num, Real.log_pow]; norm_num
    linarith
  have hg : gm w4 pEx = 2 / 5 := by
    unfold gm; rw [hs, Real.exp_log (by norm_num)]
  refine ⟨hl, hg, ?_⟩
  unfold bal; rw [hg, hl]; norm_num

/-- Equal levels everywhere give balance one. -/
theorem bal_const {w : ι → ℝ} {c : ℝ} (hw : ∀ i, 0 ≤ w i) (hw1 : ∑ i, w i = 1) (hc : 0 < c) :
    bal w (fun _ => c) = 1 :=
  (bal_eq_one_iff_equal hw hw1 (fun _ => hc)).mpr fun _ _ _ _ => rfl

/-! ### `B` is not concave -/

/-- Equal weights on two domains. -/
def w2 : Fin 2 → ℝ := fun _ => 1 / 2

theorem bal2 {a b : ℝ} (ha : 0 < a) (hb : 0 < b) {g : ℝ} (hg : 0 < g) (hab : a * b = g ^ 2) :
    bal w2 ![a, b] = g / ((a + b) / 2) := by
  unfold bal gm level w2
  simp only [Fin.sum_univ_two, Matrix.cons_val_zero, Matrix.cons_val_one]
  have : 1 / 2 * Real.log a + 1 / 2 * Real.log b = Real.log g := by
    have h1 : Real.log (a * b) = Real.log a + Real.log b := Real.log_mul ha.ne' hb.ne'
    have h2 : Real.log (g ^ 2) = 2 * Real.log g := by rw [Real.log_pow]; norm_num
    rw [hab] at h1; linarith
  rw [this, Real.exp_log hg]; congr 1; ring

/-- `B` is not concave on `(0,1)²`: at `x = (61/3800, 61/3800)` (`B = 1`) and
`y = (9/10, 1/10)` (`B = 3/5`) the midpoint has `B = 1239/1961 < 4/5`. So the argument
"since B is concave, noise pulls the draws below B" does not hold as stated. -/
theorem bal_not_concave :
    ¬ ConcaveOn ℝ (Set.univ.pi fun _ : Fin 2 => Set.Ioo (0 : ℝ) 1) (bal w2) := by
  intro h
  set x : Fin 2 → ℝ := ![61 / 3800, 61 / 3800]
  set y : Fin 2 → ℝ := ![9 / 10, 1 / 10]
  have hx : x ∈ Set.univ.pi fun _ : Fin 2 => Set.Ioo (0 : ℝ) 1 := by
    intro i _; fin_cases i <;> simp [x] <;> norm_num
  have hy : y ∈ Set.univ.pi fun _ : Fin 2 => Set.Ioo (0 : ℝ) 1 := by
    intro i _; fin_cases i <;> simp [y] <;> norm_num
  have hc := h.2 hx hy (show (0 : ℝ) ≤ 1 / 2 by norm_num) (show (0 : ℝ) ≤ 1 / 2 by norm_num)
    (by norm_num)
  have hmid : (1 / 2 : ℝ) • x + (1 / 2 : ℝ) • y = ![3481 / 7600, 441 / 7600] := by
    ext i; fin_cases i <;> simp [x, y] <;> norm_num
  have bx : bal w2 x = 1 := by
    rw [bal2 (g := 61 / 3800) (by norm_num) (by norm_num) (by norm_num) (by norm_num)]; norm_num
  have by' : bal w2 y = 3 / 5 := by
    rw [bal2 (g := 3 / 10) (by norm_num) (by norm_num) (by norm_num) (by norm_num)]; norm_num
  have bm : bal w2 ![3481 / 7600, 441 / 7600] = 1239 / 1961 := by
    rw [bal2 (g := 1239 / 7600) (by norm_num) (by norm_num) (by norm_num) (by norm_num)]; norm_num
  rw [hmid, bx, by', bm] at hc
  norm_num at hc

/-! ### The interval of `holbal()` -/

/-- `B_lo = min(B̂, max(0, q_lo - b̂))`, `B_hi = max(B̂, min(1, q_hi - b̂))`: for any quantiles and
bias, `0 ≤ B_lo ≤ B̂ ≤ B_hi ≤ 1` once `B̂ ∈ [0,1]`. -/
theorem balci_bounds {B qlo qhi b : ℝ} (h0 : 0 ≤ B) (h1 : B ≤ 1) :
    0 ≤ min B (max 0 (qlo - b)) ∧ min B (max 0 (qlo - b)) ≤ B ∧
      B ≤ max B (min 1 (qhi - b)) ∧ max B (min 1 (qhi - b)) ≤ 1 := by
  refine ⟨le_min h0 (le_max_left _ _), min_le_left _ _, le_max_left _ _, max_le h1 (min_le_left _ _)⟩

/-- `mhcont()`: a direction's level is the mean of its `m` proxies with se `√(∑ s_j²)/m`,
which is the se of a mean of independent proxies: `(√∑ s²/m)² = ∑ (s/m)²`. -/
theorem direction_se {m : ℕ} (s : Fin m → ℝ) :
    (Real.sqrt (∑ j, s j ^ 2) / m) ^ 2 = ∑ j, (s j / m) ^ 2 := by
  rw [div_pow, Real.sq_sqrt (sum_nonneg fun j _ => sq_nonneg _), sum_div]
  exact sum_congr rfl fun j _ => by rw [div_pow]

end IndigenousWellness.Balance
