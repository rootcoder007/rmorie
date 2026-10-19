/-
  IndigenousWellness/Association.lean

  Connection across places (Methods, "Connection across places"). R function certified:
  `connassoc()`: Spearman's ρ as the Pearson correlation of the ranks, with an exact
  permutation p-value over all `n!` orderings for `n ≤ 8`.

    * `corr_abs_le_one`: a Pearson correlation (so Spearman's ρ, with or without ties) lies in
      `[-1, 1]` (Cauchy–Schwarz).
    * `corr_self`: `ρ = 1` for identical rankings with positive spread.
    * `perm_mean`, `perm_ssq`: permuting one ranking leaves its mean and spread unchanged, so
      across the permutation distribution only the cross-product `∑ (x - x̄)(y_σ - ȳ)` moves.
    * `exact_p_bounds`: the exact p-value `#{σ : |ρ_σ| ≥ |ρ|}/n!` lies in `[1/n!, 1]` (the
      observed ordering is one of the orderings counted).
    * `mc_p_bounds`: the Monte Carlo value `(b+1)/(R+1)` lies in `(0, 1]` for `0 ≤ b ≤ R`.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset Real

namespace IndigenousWellness.Association

variable {ι : Type*} [Fintype ι]

/-- The mean. -/
def mean (x : ι → ℝ) : ℝ := (∑ i, x i) / Fintype.card ι

/-- Centred cross-product. -/
def cross (x y : ι → ℝ) : ℝ := ∑ i, (x i - mean x) * (y i - mean y)

/-- Pearson correlation; applied to ranks (`stats::cor(rank(x), rank(y))`) it is Spearman's ρ. -/
def corr (x y : ι → ℝ) : ℝ := cross x y / Real.sqrt (cross x x * cross y y)

theorem corr_abs_le_one (x y : ι → ℝ) : |corr x y| ≤ 1 := by
  unfold corr
  have hcs : cross x y ^ 2 ≤ cross x x * cross y y := by
    unfold cross
    have := sum_mul_sq_le_sq_mul_sq univ (fun i => x i - mean x) (fun i => y i - mean y)
    simpa [sq] using this
  rw [abs_div, abs_of_nonneg (Real.sqrt_nonneg _)]
  rcases eq_or_lt_of_le (Real.sqrt_nonneg (cross x x * cross y y)) with h | h
  · rw [← h, div_zero]; norm_num
  · rw [div_le_one h]; exact Real.abs_le_sqrt hcs

theorem corr_self {x : ι → ℝ} (h : 0 < cross x x) : corr x x = 1 := by
  unfold corr
  rw [Real.sqrt_mul_self h.le, div_self h.ne']

/-- Permuting a ranking leaves its mean unchanged. -/
theorem perm_mean (σ : Equiv.Perm ι) (y : ι → ℝ) : mean (y ∘ σ) = mean y := by
  unfold mean; congr 1; exact Equiv.sum_comp σ y

/-- … and its spread. -/
theorem perm_ssq (σ : Equiv.Perm ι) (y : ι → ℝ) : cross (y ∘ σ) (y ∘ σ) = cross y y := by
  unfold cross; rw [perm_mean]
  exact Equiv.sum_comp σ (fun i => (y i - mean y) * (y i - mean y))

/-- So every permuted correlation shares one denominator. -/
theorem perm_corr (σ : Equiv.Perm ι) (x y : ι → ℝ) :
    corr x (y ∘ σ) = cross x (y ∘ σ) / Real.sqrt (cross x x * cross y y) := by
  unfold corr; rw [perm_ssq]

variable [DecidableEq ι]

/-- The exact two-sided permutation p-value of `connassoc()`. -/
def exactP (x y : ι → ℝ) : ℝ :=
  ((univ.filter fun σ : Equiv.Perm ι => |corr x y| ≤ |corr x (y ∘ σ)|).card : ℝ) /
    (Fintype.card ι).factorial

theorem exact_p_bounds (x y : ι → ℝ) :
    1 / ((Fintype.card ι).factorial : ℝ) ≤ exactP x y ∧ exactP x y ≤ 1 := by
  unfold exactP
  have hf : (0 : ℝ) < (Fintype.card ι).factorial := by exact_mod_cast Nat.factorial_pos _
  have hmem : (1 : Equiv.Perm ι) ∈ univ.filter fun σ : Equiv.Perm ι => |corr x y| ≤ |corr x (y ∘ σ)| := by
    simp
  have hcard_le : (univ.filter fun σ : Equiv.Perm ι => |corr x y| ≤ |corr x (y ∘ σ)|).card ≤
      (Fintype.card ι).factorial := by
    rw [← Fintype.card_perm]; exact card_filter_le _ _
  have hcard_ge : 1 ≤ (univ.filter fun σ : Equiv.Perm ι => |corr x y| ≤ |corr x (y ∘ σ)|).card :=
    card_pos.mpr ⟨1, hmem⟩
  constructor
  · apply div_le_div_of_nonneg_right _ hf.le; exact_mod_cast hcard_ge
  · rw [div_le_one hf]; exact_mod_cast hcard_le

/-- The Monte Carlo p-value `(b + 1)/(R + 1)` is in `(0, 1]` for `0 ≤ b ≤ R`. -/
theorem mc_p_bounds {b R : ℕ} (h : b ≤ R) :
    0 < ((b : ℝ) + 1) / (R + 1) ∧ ((b : ℝ) + 1) / (R + 1) ≤ 1 := by
  have : (b : ℝ) ≤ R := by exact_mod_cast h
  constructor
  · positivity
  · rw [div_le_one (by positivity)]; linarith

end IndigenousWellness.Association
