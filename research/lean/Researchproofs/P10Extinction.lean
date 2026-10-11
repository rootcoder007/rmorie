/-
  Researchproofs/P10Extinction.lean

  P10, continued: does a near-repeat chain die out? The extinction
  probability of a Galton–Watson process with finite offspring support is
  the smallest fixed point of the generating function (Harris 1963,
  Theorem I.6.1; Grimmett & Stirzaker §5.4).

  Offspring law p_0, …, p_K (p_k ≥ 0, ∑ p_k = 1); generating function
  f(s) = ∑ p_k s^k. On [0, 1]:

    * pgf_mono, pgf_one, pgf_nonneg: f is non-decreasing, f(1) = 1, f ≥ 0.
    * iter_mono, iter_le_fixed: the iterates s_0 = 0, s_{n+1} = f(s_n) are
      non-decreasing and bounded above by every fixed point q ∈ [0, 1].
    * iter_tendsto, extinction_fixed, extinction_le_fixed: they converge; the
      limit is a fixed point and lies below every fixed point in [0, 1] — it
      is the extinction probability.
    * one_sub_pgf_le: 1 − f(s) ≤ m (1 − s) with m = ∑ k p_k the mean offspring.
    * subcritical_only_fixed_point_one: m < 1 forces q = 1 — the chain dies
      out with probability one.
    * one_sub_pgf_ge: 1 − f(s) ≥ (1 − s) f'(s) with f'(s) = ∑ k p_k s^{k−1}.
    * supercritical_extinction_lt_one: m > 1 gives a fixed point below one,
      hence extinction probability strictly below one.

  Reading for P10: a fitted branching ratio n = m below one says every
  cluster dies and the process is a sum of finite clusters; above one the
  model predicts an everlasting chain with positive probability, which is a
  statement about the model, not a forecast.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset Filter Topology

namespace Research.P10

/-- A finite offspring law on `{0, …, K}`. -/
structure Offspring where
  (K : ℕ)
  (p : ℕ → ℝ)
  (nonneg : ∀ k, 0 ≤ p k)
  (total : ∑ k ∈ range (K + 1), p k = 1)

variable (O : Offspring)

/-- The probability generating function. -/
def pgf (s : ℝ) : ℝ := ∑ k ∈ range (O.K + 1), O.p k * s ^ k
/-- Mean offspring `m = ∑ k p_k`. -/
def meanOff : ℝ := ∑ k ∈ range (O.K + 1), k * O.p k
/-- `f'(s) = ∑ k p_k s^(k−1)`. -/
def pgf' (s : ℝ) : ℝ := ∑ k ∈ range (O.K + 1), k * O.p k * s ^ (k - 1)

theorem pgf_one : pgf O 1 = 1 := by
  unfold pgf; simp [O.total]

theorem pgf_nonneg (s : ℝ) (hs : 0 ≤ s) : 0 ≤ pgf O s :=
  sum_nonneg (fun k _ => mul_nonneg (O.nonneg k) (pow_nonneg hs k))

theorem pgf_mono (a b : ℝ) (ha : 0 ≤ a) (hab : a ≤ b) : pgf O a ≤ pgf O b := by
  unfold pgf; apply sum_le_sum; intro k _
  exact mul_le_mul_of_nonneg_left (pow_le_pow_left₀ ha hab k) (O.nonneg k)

theorem pgf_le_one (s : ℝ) (hs0 : 0 ≤ s) (hs1 : s ≤ 1) : pgf O s ≤ 1 := by
  rw [← pgf_one O]; exact pgf_mono O s 1 hs0 hs1

theorem pgf_continuous : Continuous (pgf O) := by
  unfold pgf; fun_prop

/-- The iterates `s_0 = 0`, `s_{n+1} = f(s_n)`. -/
def iter : ℕ → ℝ
  | 0 => 0
  | n + 1 => pgf O (iter n)

theorem iter_nonneg (n : ℕ) : 0 ≤ iter O n := by
  induction n with
  | zero => simp [iter]
  | succ n ih => simp only [iter]; exact pgf_nonneg O _ ih

theorem iter_le_one (n : ℕ) : iter O n ≤ 1 := by
  induction n with
  | zero => simp [iter]
  | succ n ih => simp only [iter]; exact pgf_le_one O _ (iter_nonneg O n) ih

theorem iter_mono : Monotone (iter O) := by
  apply monotone_nat_of_le_succ
  intro n
  induction n with
  | zero => simp only [iter]; exact pgf_nonneg O 0 le_rfl
  | succ n ih => simp only [iter] at ih ⊢; exact pgf_mono O _ _ (iter_nonneg O n) ih

/-- Every fixed point in `[0, 1]` bounds the iterates. -/
theorem iter_le_fixed (q : ℝ) (hq0 : 0 ≤ q) (hfix : pgf O q = q) (n : ℕ) : iter O n ≤ q := by
  induction n with
  | zero => simpa [iter] using hq0
  | succ n ih => simp only [iter]; rw [← hfix]; exact pgf_mono O _ _ (iter_nonneg O n) ih

/-- The extinction probability: the limit of the iterates. -/
def extinction : ℝ := ⨆ n, iter O n

theorem iter_tendsto : Tendsto (iter O) atTop (𝓝 (extinction O)) :=
  tendsto_atTop_ciSup (iter_mono O) ⟨1, by rintro _ ⟨n, rfl⟩; exact iter_le_one O n⟩

theorem extinction_nonneg : 0 ≤ extinction O :=
  le_ciSup_of_le ⟨1, by rintro _ ⟨n, rfl⟩; exact iter_le_one O n⟩ 0 (iter_nonneg O 0)

theorem extinction_le_one : extinction O ≤ 1 :=
  ciSup_le (fun n => iter_le_one O n)

/-- The limit is a fixed point of `f`. -/
theorem extinction_fixed : pgf O (extinction O) = extinction O := by
  have h1 : Tendsto (fun n => pgf O (iter O n)) atTop (𝓝 (pgf O (extinction O))) :=
    ((pgf_continuous O).tendsto _).comp (iter_tendsto O)
  have h2 : Tendsto (fun n => iter O (n + 1)) atTop (𝓝 (extinction O)) :=
    (iter_tendsto O).comp (tendsto_add_atTop_nat 1)
  have : (fun n => pgf O (iter O n)) = fun n => iter O (n + 1) := by funext n; rfl
  rw [this] at h1
  exact tendsto_nhds_unique h1 h2

/-- The limit lies below every fixed point in `[0, 1]`: it is the smallest one. -/
theorem extinction_le_fixed (q : ℝ) (hq0 : 0 ≤ q) (hfix : pgf O q = q) : extinction O ≤ q :=
  ciSup_le (fun n => iter_le_fixed O q hq0 hfix n)

/-! ### Subcritical: the mean decides -/

theorem one_sub_pow_le (s : ℝ) (hs0 : 0 ≤ s) (hs1 : s ≤ 1) (k : ℕ) : 1 - s ^ k ≤ k * (1 - s) := by
  induction k with
  | zero => simp
  | succ k ih =>
    have hpow : s ^ k ≤ 1 := pow_le_one₀ hs0 hs1
    have e : s ^ (k + 1) = s ^ k * s := pow_succ s k
    have h2 : s ^ k * (1 - s) ≤ 1 * (1 - s) := mul_le_mul_of_nonneg_right hpow (sub_nonneg.2 hs1)
    push_cast
    nlinarith [ih, e, h2]

/-- `1 − f(s) ≤ m (1 − s)` on `[0, 1]`. -/
theorem one_sub_pgf_le (s : ℝ) (hs0 : 0 ≤ s) (hs1 : s ≤ 1) : 1 - pgf O s ≤ meanOff O * (1 - s) := by
  have h : 1 - pgf O s = ∑ k ∈ range (O.K + 1), O.p k * (1 - s ^ k) := by
    have e : ∑ k ∈ range (O.K + 1), O.p k * (1 - s ^ k) =
        (∑ k ∈ range (O.K + 1), O.p k) - ∑ k ∈ range (O.K + 1), O.p k * s ^ k := by
      rw [← sum_sub_distrib]; apply sum_congr rfl; intro k _; ring
    rw [e, O.total]; rfl
  rw [h]; unfold meanOff; rw [sum_mul]
  apply sum_le_sum; intro k _
  have := one_sub_pow_le s hs0 hs1 k
  have hp := O.nonneg k
  nlinarith [mul_le_mul_of_nonneg_left this hp]

/-- Subcritical (`m < 1`): the only fixed point in `[0, 1]` is `1`, so extinction is certain. -/
theorem subcritical_only_fixed_point_one (hm : meanOff O < 1) (q : ℝ) (hq0 : 0 ≤ q) (hq1 : q ≤ 1)
    (hfix : pgf O q = q) : q = 1 := by
  by_contra hne
  have hlt : q < 1 := lt_of_le_of_ne hq1 hne
  have h := one_sub_pgf_le O q hq0 hq1
  rw [hfix] at h
  have : 0 < 1 - q := by linarith
  nlinarith

theorem subcritical_extinction_one (hm : meanOff O < 1) : extinction O = 1 :=
  subcritical_only_fixed_point_one O hm _ (extinction_nonneg O) (extinction_le_one O) (extinction_fixed O)

/-! ### Supercritical: a fixed point below one -/

theorem one_sub_pow_ge (s : ℝ) (hs0 : 0 ≤ s) (hs1 : s ≤ 1) (k : ℕ) : k * s ^ (k - 1) * (1 - s) ≤ 1 - s ^ k := by
  induction k with
  | zero => simp
  | succ k ih =>
    rcases k with _ | k
    · simp
    · have hpow : s ^ (k + 1) ≤ s ^ k := pow_le_pow_of_le_one hs0 hs1 (Nat.le_succ k)
      simp only [Nat.add_sub_cancel] at ih ⊢
      push_cast at ih ⊢
      have e1 : s ^ (k + 1 + 1) = s ^ (k + 1) * s := pow_succ s (k + 1)
      have h1 : 0 ≤ ((k : ℝ) + 1) * (1 - s) := mul_nonneg (by positivity) (sub_nonneg.2 hs1)
      have h2 : ((k : ℝ) + 1) * (1 - s) * s ^ (k + 1) ≤ ((k : ℝ) + 1) * (1 - s) * s ^ k :=
        mul_le_mul_of_nonneg_left hpow h1
      have h3 : 0 ≤ s ^ (k + 1) * (1 - s) := mul_nonneg (pow_nonneg hs0 _) (sub_nonneg.2 hs1)
      nlinarith [ih, e1, h2, h3]

/-- `1 − f(s) ≥ (1 − s) f'(s)` on `[0, 1]`. -/
theorem one_sub_pgf_ge (s : ℝ) (hs0 : 0 ≤ s) (hs1 : s ≤ 1) : (1 - s) * pgf' O s ≤ 1 - pgf O s := by
  have h : 1 - pgf O s = ∑ k ∈ range (O.K + 1), O.p k * (1 - s ^ k) := by
    have e : ∑ k ∈ range (O.K + 1), O.p k * (1 - s ^ k) =
        (∑ k ∈ range (O.K + 1), O.p k) - ∑ k ∈ range (O.K + 1), O.p k * s ^ k := by
      rw [← sum_sub_distrib]; apply sum_congr rfl; intro k _; ring
    rw [e, O.total]; rfl
  rw [h]; unfold pgf'; rw [mul_sum]
  apply sum_le_sum; intro k _
  have := one_sub_pow_ge s hs0 hs1 k
  have hp := O.nonneg k
  nlinarith [mul_le_mul_of_nonneg_left this hp]

theorem pgf'_one : pgf' O 1 = meanOff O := by
  unfold pgf' meanOff; apply sum_congr rfl; intro k _; simp

theorem pgf'_continuous : Continuous (pgf' O) := by
  unfold pgf'; fun_prop

/-- Supercritical (`m > 1`): some `s < 1` has `f(s) < s`, so a fixed point lies in `[0, s]`
    and the extinction probability is strictly below one. -/
theorem supercritical_extinction_lt_one (hm : 1 < meanOff O) : extinction O < 1 := by
  -- f' > 1 on a neighbourhood of 1
  have hcont : ContinuousAt (pgf' O) 1 := (pgf'_continuous O).continuousAt
  have hev : ∀ᶠ s in 𝓝 (1 : ℝ), 1 < pgf' O s := by
    rw [← pgf'_one O] at hm
    exact hcont.eventually (lt_mem_nhds hm)
  obtain ⟨δ, hδ, hball⟩ := Metric.eventually_nhds_iff.1 hev
  set s₀ : ℝ := max 0 (1 - δ / 2) with hs₀
  have hs₀0 : 0 ≤ s₀ := le_max_left _ _
  have hs₀1 : s₀ < 1 := by
    rw [hs₀]; apply max_lt one_pos; linarith
  have hnear : dist s₀ 1 < δ := by
    rw [Real.dist_eq, abs_sub_lt_iff]; constructor <;> [linarith; (rw [hs₀]; have := le_max_right 0 (1 - δ / 2); linarith)]
  have hd : 1 < pgf' O s₀ := hball hnear
  -- hence f(s₀) < s₀
  have hlt : pgf O s₀ < s₀ := by
    have h := one_sub_pgf_ge O s₀ hs₀0 hs₀1.le
    have : 0 < 1 - s₀ := by linarith
    nlinarith
  -- intermediate value on g(s) = f(s) − s between 0 and s₀
  have hg : ContinuousOn (fun s => pgf O s - s) (Set.Icc 0 s₀) :=
    ((pgf_continuous O).sub continuous_id).continuousOn
  have h0 : 0 ≤ pgf O 0 - 0 := by simpa using pgf_nonneg O 0 le_rfl
  have hs : pgf O s₀ - s₀ ≤ 0 := by linarith
  obtain ⟨q, hqmem, hq⟩ := intermediate_value_Icc' hs₀0 hg ⟨hs, h0⟩
  have hfix : pgf O q = q := by linarith [hq]
  have := extinction_le_fixed O q hqmem.1 hfix
  linarith [hqmem.2]

end Research.P10
