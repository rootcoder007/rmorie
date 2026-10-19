/-
  IndigenousWellness/Indices.lean

  Indices built from published figures. R functions certified: `cwbscr()` and `cnscwb()`
  (Methods, "Community Well-Being from public figures"), `commidx()` ("Community-weighted
  protective and risk factors"), `frmaud()` ("Wellness beside distress", the equation for
  `F`), `dstntst()` ("A test before pooling", the equation for `Q` and `I²`).

    * `cwbinc_*`: the income component `100 (log y - log a)/(log b - log a)` is `0` at `a`,
      `100` at `b`, strictly increasing, in `[0,100]` on `[a,b]`; the clamp of `cwbscr()` keeps
      it in `[0,100]` for every income.
    * `cwb_between`, `cwb_bounds`: the index (mean of four components) lies between its
      smallest and largest component, so in `[0,100]` when they are; the education, housing and
      labour components are in `[0,100]` when their shares are.
    * `commidx_abs_le`, `commidx_between`: `|C| ≤ max |p_i|`; with all weights protective `C`
      is a weighted mean of the indicators.
    * `framing_bounds`, `framing_overall_between`: `F ∈ [0,1]`, `F = 1` iff no wellness
      indicator, and the overall share lies between the smallest and largest group share.
    * `cochran_*`: `Q ≥ 0`, `Q = ∑ w y² - (∑ w y)²/∑ w`, `Q = 0` iff all groups equal the
      pooled value, the pooled value is a weighted mean, `0 ≤ I² < 1`; the paper's
      `Q = 11.97` on 2 df gives `I² ∈ (0.83, 0.84)`.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset Real

namespace IndigenousWellness.Indices

/-! ### Community Well-Being -/

/-- The income component for per-capita income `y` between bounds `a < b`. -/
def cwbinc (a b y : ℝ) : ℝ := 100 * (Real.log y - Real.log a) / (Real.log b - Real.log a)

theorem cwbinc_at_a (a b : ℝ) : cwbinc a b a = 0 := by simp [cwbinc]

theorem cwbinc_at_b {a b : ℝ} (ha : 0 < a) (hab : a < b) : cwbinc a b b = 100 := by
  unfold cwbinc
  have : 0 < Real.log b - Real.log a := by linarith [Real.log_lt_log ha hab]
  field_simp

theorem cwbinc_strictMonoOn {a b : ℝ} (ha : 0 < a) (hab : a < b) :
    StrictMonoOn (cwbinc a b) (Set.Ioi 0) := by
  intro x hx y hy hxy
  unfold cwbinc
  have hd : 0 < Real.log b - Real.log a := by linarith [Real.log_lt_log ha hab]
  have := Real.log_lt_log hx hxy
  apply div_lt_div_of_pos_right _ hd
  linarith

theorem cwbinc_bounds {a b y : ℝ} (ha : 0 < a) (hab : a < b) (hay : a ≤ y) (hyb : y ≤ b) :
    0 ≤ cwbinc a b y ∧ cwbinc a b y ≤ 100 := by
  unfold cwbinc
  have hd : 0 < Real.log b - Real.log a := by linarith [Real.log_lt_log ha hab]
  have h1 := Real.log_le_log ha hay
  have h2 := Real.log_le_log (by linarith) hyb
  constructor
  · apply div_nonneg _ hd.le; linarith
  · rw [div_le_iff₀ hd]; linarith

/-- `cwbscr()` clamps: `pmin(100, pmax(0, ·))` is in `[0,100]` for any income. -/
theorem clamp_bounds (x : ℝ) : 0 ≤ min 100 (max 0 x) ∧ min 100 (max 0 x) ≤ 100 :=
  ⟨le_min (by norm_num) (le_max_left _ _), min_le_left _ _⟩

/-- The CWB score, the mean of four components. -/
def cwb (i e h l : ℝ) : ℝ := (i + e + h + l) / 4

theorem cwb_between {i e h l : ℝ} :
    min (min i e) (min h l) ≤ cwb i e h l ∧ cwb i e h l ≤ max (max i e) (max h l) := by
  unfold cwb
  constructor
  · have := min_le_left (min i e) (min h l); have := min_le_right (min i e) (min h l)
    have := min_le_left i e; have := min_le_right i e; have := min_le_left h l; have := min_le_right h l
    linarith
  · have := le_max_left (max i e) (max h l); have := le_max_right (max i e) (max h l)
    have := le_max_left i e; have := le_max_right i e; have := le_max_left h l; have := le_max_right h l
    linarith

theorem cwb_bounds {i e h l : ℝ} (hi : 0 ≤ i ∧ i ≤ 100) (he : 0 ≤ e ∧ e ≤ 100)
    (hh : 0 ≤ h ∧ h ≤ 100) (hl : 0 ≤ l ∧ l ≤ 100) : 0 ≤ cwb i e h l ∧ cwb i e h l ≤ 100 := by
  unfold cwb; constructor <;> linarith [hi.1, hi.2, he.1, he.2, hh.1, hh.2, hl.1, hl.2]

/-- Education `2/3 h + 1/3 u`, housing and labour (means of two shares) are in `[0,100]`. -/
theorem components_bounds {s t : ℝ} (hs : 0 ≤ s ∧ s ≤ 100) (ht : 0 ≤ t ∧ t ≤ 100) :
    (0 ≤ 2 / 3 * s + 1 / 3 * t ∧ 2 / 3 * s + 1 / 3 * t ≤ 100) ∧
      (0 ≤ (s + t) / 2 ∧ (s + t) / 2 ≤ 100) := by
  refine ⟨⟨?_, ?_⟩, ?_, ?_⟩ <;> linarith [hs.1, hs.2, ht.1, ht.2]

/-! ### The community-weighted index -/

variable {ι : Type*} [Fintype ι]

/-- `C = ∑ w p / ∑ |w|`. -/
def commidx (w p : ι → ℝ) : ℝ := (∑ i, w i * p i) / ∑ i, |w i|

/-- `|C| ≤ M` when every indicator has `|p_i| ≤ M`: risk weights pull `C` down, never past `-M`. -/
theorem commidx_abs_le {w p : ι → ℝ} {M : ℝ} (hw : 0 < ∑ i, |w i|) (hp : ∀ i, |p i| ≤ M) :
    |commidx w p| ≤ M := by
  unfold commidx
  rw [abs_div, abs_of_pos hw, div_le_iff₀ hw]
  calc |∑ i, w i * p i| ≤ ∑ i, |w i * p i| := abs_sum_le_sum_abs _ _
    _ = ∑ i, |w i| * |p i| := sum_congr rfl fun i _ => abs_mul _ _
    _ ≤ ∑ i, |w i| * M := sum_le_sum fun i _ => mul_le_mul_of_nonneg_left (hp i) (abs_nonneg _)
    _ = M * ∑ i, |w i| := by rw [← sum_mul, mul_comm]

/-- With only protective (nonnegative) weights, `C` is a weighted mean of the indicators. -/
theorem commidx_between {w p : ι → ℝ} {a b : ℝ} (hw0 : ∀ i, 0 ≤ w i) (hw : 0 < ∑ i, |w i|)
    (ha : ∀ i, a ≤ p i) (hb : ∀ i, p i ≤ b) : a ≤ commidx w p ∧ commidx w p ≤ b := by
  unfold commidx
  have habs : ∀ i, |w i| = w i := fun i => abs_of_nonneg (hw0 i)
  simp only [habs] at hw ⊢
  constructor
  · rw [le_div_iff₀ hw, mul_sum]
    exact sum_le_sum fun i _ => by rw [mul_comm]; exact mul_le_mul_of_nonneg_left (ha i) (hw0 i)
  · rw [div_le_iff₀ hw, mul_sum]
    exact sum_le_sum fun i _ => by rw [mul_comm b]; exact mul_le_mul_of_nonneg_left (hb i) (hw0 i)

/-! ### The framing audit -/

/-- `F = |𝒟| / (|𝒲| + |𝒟|)`. -/
def framing (well dist : ℝ) : ℝ := dist / (well + dist)

theorem framing_bounds {well dist : ℝ} (hw : 0 ≤ well) (hd : 0 ≤ dist) (hn : 0 < well + dist) :
    0 ≤ framing well dist ∧ framing well dist ≤ 1 ∧ (framing well dist = 1 ↔ well = 0) := by
  unfold framing
  refine ⟨div_nonneg hd hn.le, (div_le_one hn).mpr (by linarith), ?_⟩
  rw [div_eq_one_iff_eq hn.ne']; constructor <;> intro h <;> linarith

omit [Fintype ι] in
/-- The overall share `∑ d / ∑ (w + d)` lies between the smallest and largest group share
(a mediant). -/
theorem framing_overall_between {s : Finset ι} {d n : ι → ℝ} {a b : ℝ} (hn : ∀ g ∈ s, 0 < n g)
    (hs : s.Nonempty) (ha : ∀ g ∈ s, a ≤ d g / n g) (hb : ∀ g ∈ s, d g / n g ≤ b) :
    a ≤ (∑ g ∈ s, d g) / ∑ g ∈ s, n g ∧ (∑ g ∈ s, d g) / ∑ g ∈ s, n g ≤ b := by
  have hN : 0 < ∑ g ∈ s, n g := sum_pos hn hs
  constructor
  · rw [le_div_iff₀ hN, mul_sum]
    exact sum_le_sum fun g hg => by
      have := ha g hg; rw [le_div_iff₀ (hn g hg)] at this; linarith
  · rw [div_le_iff₀ hN, mul_sum]
    exact sum_le_sum fun g hg => by
      have := hb g hg; rw [div_le_iff₀ (hn g hg)] at this; linarith

/-! ### Cochran's Q and I² (`dstntst()`) -/

/-- The pooled value `∑ w y / ∑ w`. -/
def pooled (w y : ι → ℝ) : ℝ := (∑ i, w i * y i) / ∑ i, w i

/-- `Q = ∑ w (y - ȳ_w)²`. -/
def cochranQ (w y : ι → ℝ) : ℝ := ∑ i, w i * (y i - pooled w y) ^ 2

theorem cochran_nonneg {w y : ι → ℝ} (hw : ∀ i, 0 ≤ w i) : 0 ≤ cochranQ w y :=
  sum_nonneg fun i _ => mul_nonneg (hw i) (sq_nonneg _)

/-- The computational form `Q = ∑ w y² - (∑ w y)²/∑ w`. -/
theorem cochran_eq {w y : ι → ℝ} (hW : ∑ i, w i ≠ 0) :
    cochranQ w y = ∑ i, w i * y i ^ 2 - (∑ i, w i * y i) ^ 2 / ∑ i, w i := by
  unfold cochranQ pooled
  set m := (∑ i, w i * y i) / ∑ i, w i
  have : ∀ i ∈ univ, w i * (y i - m) ^ 2 = w i * y i ^ 2 - 2 * m * (w i * y i) + m ^ 2 * w i := by
    intro i _; ring
  rw [sum_congr rfl this, sum_add_distrib, sum_sub_distrib, ← mul_sum, ← mul_sum]
  simp only [m]; field_simp; ring

/-- `Q = 0` exactly when every group (of positive weight) equals the pooled value. -/
theorem cochran_eq_zero_iff {w y : ι → ℝ} (hw : ∀ i, 0 < w i) :
    cochranQ w y = 0 ↔ ∀ i, y i = pooled w y := by
  unfold cochranQ
  rw [sum_eq_zero_iff_of_nonneg fun i _ => mul_nonneg (hw i).le (sq_nonneg _)]
  constructor
  · intro h i
    have := h i (mem_univ i)
    rcases mul_eq_zero.mp this with h1 | h1
    · exact absurd h1 (hw i).ne'
    · have := pow_eq_zero_iff (n := 2) (by norm_num) |>.mp h1; linarith
  · intro h i _; rw [h i]; ring

/-- The pooled value is between the smallest and largest group estimate. -/
theorem pooled_between {w y : ι → ℝ} {a b : ℝ} (hw : ∀ i, 0 ≤ w i) (hW : 0 < ∑ i, w i)
    (ha : ∀ i, a ≤ y i) (hb : ∀ i, y i ≤ b) : a ≤ pooled w y ∧ pooled w y ≤ b := by
  unfold pooled
  constructor
  · rw [le_div_iff₀ hW, mul_sum]
    exact sum_le_sum fun i _ => by rw [mul_comm]; exact mul_le_mul_of_nonneg_left (ha i) (hw i)
  · rw [div_le_iff₀ hW, mul_sum]
    exact sum_le_sum fun i _ => by rw [mul_comm b]; exact mul_le_mul_of_nonneg_left (hb i) (hw i)

/-- `I² = max(0, (Q - df)/Q)` lies in `[0, 1)` for `Q > 0` and `df > 0`. -/
theorem i2_bounds {Q df : ℝ} (hQ : 0 < Q) (hdf : 0 < df) :
    0 ≤ max 0 ((Q - df) / Q) ∧ max 0 ((Q - df) / Q) < 1 := by
  refine ⟨le_max_left _ _, max_lt (by norm_num) ?_⟩
  rw [div_lt_one hQ]; linarith

/-- The paper's figures: `Q = 11.97` on `2` df gives `I² ≈ 0.833` ("83%"). -/
theorem i2_paper : 0.83 < max 0 ((11.97 - 2) / 11.97 : ℝ) ∧ max 0 ((11.97 - 2) / 11.97 : ℝ) < 0.84 := by
  rw [max_eq_right (by norm_num)]; constructor <;> norm_num

end IndigenousWellness.Indices
