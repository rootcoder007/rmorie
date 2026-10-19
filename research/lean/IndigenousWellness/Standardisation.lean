/-
  IndigenousWellness/Standardisation.lean

  Age standardisation (Methods, "Age standardisation") and census profile shares (Methods,
  "Census profile shares", the displayed variance). R functions certified: `stdprev()`,
  `agestd()` (Fay–Feuer gamma interval), `profshr()` (rounding variance and Wilson interval),
  `cnsstd()` (standard weights).

    * `std_between`, `std_const`: a directly standardised rate `∑ w_i p_i` (weights ≥ 0
      summing to one) lies between the smallest and largest stratum rate and equals the
      common rate when the strata agree.
    * `std_own_is_crude`: standardising to the group's own population gives the crude rate.
    * `std_se_le`: the standardised se `√(∑ w² s²)` is at most the largest stratum se.
    * `stdprev_cut`: the cut interval `[max(0, y - zse), min(scale, y + zse)]` contains `y`.
    * `fayfeuer_moments`: the scaled chi-square `c·χ²_k` with `c = v/(2y)`, `k = 2y²/v` has
      mean `y` and variance `v` (the gamma matching behind Fay–Feuer); the same for the upper
      limit's `(y + w_M, v + w_M²)`.
    * `fayfeuer_single`: with one age group the parameters are Garwood's: shape `2d` and
      `2(d+1)`, scale `1/(2n)`.
    * `roundvar_avg`, `roundvar_five`: unbiased random rounding to multiples of `b` has
      variance `r(b - r)` for remainder `r`, averaging `(b² - 1)/6` over remainders; `4` for
      `b = 5`.
    * `share_delta`: the delta-method variance of `a/N` with independent rounding errors of
      variance `c` on both counts is `c(1 + p²)/N²`.
    * `wilson_bounds`: the Wilson interval lies in `[0, 1]` and contains `p`.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset Real

namespace IndigenousWellness.Standardisation

variable {ι : Type*} [Fintype ι]

/-- The directly standardised value `∑ w_i p_i`. -/
def stdz (w p : ι → ℝ) : ℝ := ∑ i, w i * p i

theorem std_between {w p : ι → ℝ} {a b : ℝ} (hw : ∀ i, 0 ≤ w i) (hw1 : ∑ i, w i = 1)
    (ha : ∀ i, a ≤ p i) (hb : ∀ i, p i ≤ b) : a ≤ stdz w p ∧ stdz w p ≤ b := by
  unfold stdz
  constructor
  · calc a = ∑ i, w i * a := by rw [← sum_mul, hw1, one_mul]
      _ ≤ ∑ i, w i * p i := sum_le_sum fun i _ => mul_le_mul_of_nonneg_left (ha i) (hw i)
  · calc ∑ i, w i * p i ≤ ∑ i, w i * b := sum_le_sum fun i _ => mul_le_mul_of_nonneg_left (hb i) (hw i)
      _ = b := by rw [← sum_mul, hw1, one_mul]

theorem std_const {w : ι → ℝ} (r : ℝ) (hw1 : ∑ i, w i = 1) : stdz w (fun _ => r) = r := by
  unfold stdz; rw [← sum_mul, hw1, one_mul]

/-- Weights from a standard population `std ≥ 0` (`cnsstd()`): `std_i / ∑ std` sum to one. -/
theorem stdweights_sum {std : ι → ℝ} (hs : 0 < ∑ i, std i) : ∑ i, std i / ∑ j, std j = 1 := by
  rw [← sum_div, div_self hs.ne']

/-- Standardising to the group's own population gives its crude rate `∑ d / ∑ n`. -/
theorem std_own_is_crude {d n : ι → ℝ} (hn : ∀ i, 0 < n i) :
    stdz (fun i => n i / ∑ j, n j) (fun i => d i / n i) = (∑ i, d i) / ∑ j, n j := by
  unfold stdz; rw [sum_div]
  exact sum_congr rfl fun i _ => by
    rw [div_mul_div_comm, mul_comm (n i) (d i), mul_div_mul_right _ _ (hn i).ne']

/-- The standardised se is at most the largest stratum se. -/
theorem std_se_le {w s : ι → ℝ} {M : ℝ} (hw : ∀ i, 0 ≤ w i) (hw1 : ∑ i, w i = 1)
    (hM : 0 ≤ M) (hs : ∀ i, |s i| ≤ M) : Real.sqrt (∑ i, w i ^ 2 * s i ^ 2) ≤ M := by
  rw [Real.sqrt_le_left hM]
  have hw' : ∀ i, w i ≤ 1 := fun i => by
    rw [← hw1]; exact single_le_sum (fun j _ => hw j) (mem_univ i)
  calc ∑ i, w i ^ 2 * s i ^ 2 ≤ ∑ i, w i * M ^ 2 := by
        refine sum_le_sum fun i _ => ?_
        have h1 : w i ^ 2 ≤ w i := by nlinarith [hw i, hw' i]
        have h2 : s i ^ 2 ≤ M ^ 2 := by
          have := abs_le.mp (hs i); nlinarith [abs_nonneg (s i), sq_abs (s i)]
        calc w i ^ 2 * s i ^ 2 ≤ w i * s i ^ 2 := mul_le_mul_of_nonneg_right h1 (sq_nonneg _)
          _ ≤ w i * M ^ 2 := mul_le_mul_of_nonneg_left h2 (hw i)
    _ = M ^ 2 := by rw [← sum_mul, hw1, one_mul]

/-- `stdprev()`'s interval, cut at `0` and at `scale`, contains the estimate. -/
theorem stdprev_cut {y h scale : ℝ} (hy0 : 0 ≤ y) (hy1 : y ≤ scale) (hh : 0 ≤ h) :
    max 0 (y - h) ≤ y ∧ y ≤ min scale (y + h) :=
  ⟨max_le hy0 (by linarith), le_min hy1 (by linarith)⟩

/-! ### Fay–Feuer (`agestd()`) -/

/-- `c χ²_k` has mean `c k` and variance `2 c² k`; with `c = v/(2y)` and `k = 2y²/v` these are
`y` and `v`. -/
theorem fayfeuer_moments {y v : ℝ} (hy : 0 < y) (hv : 0 < v) :
    v / (2 * y) * (2 * y ^ 2 / v) = y ∧ 2 * (v / (2 * y)) ^ 2 * (2 * y ^ 2 / v) = v := by
  constructor <;> field_simp

/-- The upper limit uses `(y + w_M, v + w_M²)`, matched the same way. -/
theorem fayfeuer_upper_moments {y v wM : ℝ} (hy : 0 ≤ y) (hv : 0 ≤ v) (hw : 0 < wM) :
    (v + wM ^ 2) / (2 * (y + wM)) * (2 * (y + wM) ^ 2 / (v + wM ^ 2)) = y + wM ∧
      2 * ((v + wM ^ 2) / (2 * (y + wM))) ^ 2 * (2 * (y + wM) ^ 2 / (v + wM ^ 2)) = v + wM ^ 2 := by
  have h1 : 0 < y + wM := by linarith
  have h2 : 0 < v + wM ^ 2 := by positivity
  exact fayfeuer_moments h1 h2

/-- The standardised rate and its variance from counts: `y = ∑ w d/n`, `v = ∑ (w/n)² d ≥ 0`,
and each `w_i/n_i ≤ w_M`. -/
theorem dsr_var_nonneg {w d n : ι → ℝ} (hd : ∀ i, 0 ≤ d i) : 0 ≤ ∑ i, (w i / n i) ^ 2 * d i :=
  sum_nonneg fun i _ => mul_nonneg (sq_nonneg _) (hd i)

/-- One age group (`w = 1`): `y = d/n`, `v = d/n²`, `w_M = 1/n`. Then the lower limit's gamma
has shape `2d` and scale `1/(2n)`, and the upper one shape `2(d+1)` and scale `1/(2n)`: the
exact Garwood limits `χ²_{2d}(α/2)/(2n)` and `χ²_{2(d+1)}(1-α/2)/(2n)`. -/
theorem fayfeuer_single {d n : ℝ} (hd : 0 < d) (hn : 0 < n) :
    2 * (d / n) ^ 2 / (d / n ^ 2) = 2 * d ∧ (d / n ^ 2) / (2 * (d / n)) = 1 / (2 * n) ∧
      2 * (d / n + 1 / n) ^ 2 / (d / n ^ 2 + (1 / n) ^ 2) = 2 * (d + 1) ∧
      (d / n ^ 2 + (1 / n) ^ 2) / (2 * (d / n + 1 / n)) = 1 / (2 * n) := by
  have hd1 : d + 1 ≠ 0 := by linarith
  refine ⟨?_, ?_, ?_, ?_⟩ <;> field_simp

/-! ### Random rounding (`profshr()`) -/

/-- Unbiased random rounding of `b k + r` to `b k + b` with probability `r/b`, else `b k`:
mean `b k + r`, variance `r (b - r)`. -/
theorem round_unbiased {b k r : ℝ} (hb : 0 < b) :
    (r / b) * (b * k + b) + (1 - r / b) * (b * k) = b * k + r ∧
      (r / b) * (b - r) ^ 2 + (1 - r / b) * r ^ 2 = r * (b - r) := by
  constructor <;> field_simp <;> ring

theorem sum_range_id_real (b : ℕ) : ∑ r ∈ range b, (r : ℝ) = b * (b - 1) / 2 := by
  induction b with
  | zero => simp
  | succ n ih => rw [sum_range_succ, ih]; push_cast; ring

theorem sum_range_sq_real (b : ℕ) : ∑ r ∈ range b, (r : ℝ) ^ 2 = (b - 1) * b * (2 * b - 1) / 6 := by
  induction b with
  | zero => simp
  | succ n ih => rw [sum_range_succ, ih]; push_cast; ring

/-- Averaged over the remainders `0, …, b-1`, the rounding variance is `(b² - 1)/6`. -/
theorem roundvar_avg {b : ℕ} (hb : 0 < b) :
    (∑ r ∈ range b, (r : ℝ) * (b - r)) / b = ((b : ℝ) ^ 2 - 1) / 6 := by
  have : ∑ r ∈ range b, (r : ℝ) * (b - r) = b * ∑ r ∈ range b, (r : ℝ) - ∑ r ∈ range b, (r : ℝ) ^ 2 := by
    rw [mul_sum, ← sum_sub_distrib]; exact sum_congr rfl fun r _ => by ring
  rw [this, sum_range_id_real, sum_range_sq_real]
  have : (b : ℝ) ≠ 0 := by exact_mod_cast hb.ne'
  field_simp; ring

/-- `c = 4` for multiples of `5`, as the paper says. -/
theorem roundvar_five : (∑ r ∈ range 5, (r : ℝ) * (5 - r)) / 5 = 4 := by
  norm_num [sum_range_succ]

/-- The rounding term of `profshr()`: with gradient `(1/N, -a/N²)` of `a/N` and independent
rounding errors of variance `c` on `a` and on `N`, the variance is `c (1 + p²)/N²`, `p = a/N`. -/
theorem share_delta {a N c : ℝ} (hN : N ≠ 0) :
    (1 / N) ^ 2 * c + (-a / N ^ 2) ^ 2 * c = c * (1 + (a / N) ^ 2) / N ^ 2 := by
  field_simp

/-! ### The Wilson interval (`profshr()`) -/

/-- Wilson centre and half-width at `k = z²/n`. -/
def wcentre (p k : ℝ) : ℝ := (p + k / 2) / (1 + k)
def whalf (p k : ℝ) : ℝ := Real.sqrt (k * (p * (1 - p)) + k ^ 2 / 4) / (1 + k)

/-- `profshr()` writes the half-width as `z/(1 + z²/n) √(p(1-p)/n + z²/(4n²))`; with
`k = z²/n` it is `whalf p k`. -/
theorem whalf_eq {p z n : ℝ} (hz : 0 ≤ z) (hn : 0 < n) :
    z / (1 + z ^ 2 / n) * Real.sqrt (p * (1 - p) / n + z ^ 2 / (4 * n ^ 2)) = whalf p (z ^ 2 / n) := by
  unfold whalf
  have hk : 0 < 1 + z ^ 2 / n := by positivity
  rw [div_mul_eq_mul_div]
  congr 1
  rw [show z = Real.sqrt (z ^ 2) from (Real.sqrt_sq hz).symm, ← Real.sqrt_mul (sq_nonneg _)]
  congr 1
  rw [Real.sq_sqrt (sq_nonneg z)]
  field_simp

/-- For `p ∈ [0,1]` and `k ≥ 0`, the Wilson interval lies in `[0,1]` and contains `p`. -/
theorem wilson_bounds {p k : ℝ} (hp0 : 0 ≤ p) (hp1 : p ≤ 1) (hk : 0 ≤ k) :
    0 ≤ wcentre p k - whalf p k ∧ wcentre p k - whalf p k ≤ p ∧
      p ≤ wcentre p k + whalf p k ∧ wcentre p k + whalf p k ≤ 1 := by
  unfold wcentre whalf
  have h1k : 0 < 1 + k := by linarith
  have hrad : 0 ≤ k * (p * (1 - p)) + k ^ 2 / 4 := by
    have : 0 ≤ p * (1 - p) := mul_nonneg hp0 (by linarith); positivity
  set S := Real.sqrt (k * (p * (1 - p)) + k ^ 2 / 4) with hS
  have hS0 : 0 ≤ S := Real.sqrt_nonneg _
  have hS2 : S ^ 2 = k * (p * (1 - p)) + k ^ 2 / 4 := Real.sq_sqrt hrad
  -- lower ≥ 0: S ≤ p + k/2, since (p + k/2)² - S² = p²(1 + k)
  have hlo : S ≤ p + k / 2 := by
    rw [hS, Real.sqrt_le_left (by linarith)]; nlinarith [mul_nonneg hk (sq_nonneg p)]
  -- upper ≤ 1: S ≤ 1 - p + k/2, since (1 - p + k/2)² - S² = (1-p)²(1 + k)
  have hhi : S ≤ 1 - p + k / 2 := by
    rw [hS, Real.sqrt_le_left (by linarith)]; nlinarith [mul_nonneg hk (sq_nonneg (1 - p))]
  -- containment: |k(p - 1/2)| ≤ S
  have hc : (k * (p - 1 / 2)) ^ 2 ≤ S ^ 2 := by
    rw [hS2]; nlinarith [mul_nonneg hk (mul_nonneg hp0 (by linarith : (0:ℝ) ≤ 1 - p))]
  have hc' : |k * (p - 1 / 2)| ≤ S := by
    have := Real.abs_le_sqrt hc; rwa [Real.sqrt_sq hS0] at this
  have hc'' := abs_le.mp hc'
  refine ⟨?_, ?_, ?_, ?_⟩
  · rw [← sub_div]; exact div_nonneg (by linarith) h1k.le
  · rw [← sub_div, div_le_iff₀ h1k]; nlinarith
  · rw [← add_div, le_div_iff₀ h1k]; nlinarith
  · rw [← add_div, div_le_one h1k]; linarith

end IndigenousWellness.Standardisation
