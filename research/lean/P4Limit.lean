/-
  Researchproofs/P4Limit.lean

  P4, continued: the naive mean-field recursion runs away completely.

  With λA > λB and any positive initial counts, the share of patrol sent
  to A converges to 1. The ratio λA / λB plays no role: a region whose true
  rate is higher by any margin ends up with the whole patrol. This is the
  deterministic (expected-count) version of the Ensign et al. runaway
  theorem; the stochastic urn version is a later target.

  Proof: the share is increasing and bounded by 1, so it converges to some
  L ≤ 1. If L < 1 then 1 − x_n ≥ 1 − L for all n, and the one-step drift
  is at least K / (c₀ + (n+1)Λ) with K > 0 and Λ = λA + λB, because the
  total count grows by at most Λ per step. Those increments are not
  summable (harmonic comparison), yet their partial sums are bounded by
  1 − x₀; contradiction.
-/
import Mathlib
import Researchproofs.P4Feedback

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Filter Topology

namespace Research.P4

/-- The naive recursion iterated `n` times from the initial counts. -/
def naiveIter (P : Params) : ℕ → ℝ × ℝ
  | 0 => (P.cA0, P.cB0)
  | n + 1 => naiveStep P (naiveIter P n).1 (naiveIter P n).2

/-- Share after `n` naive steps. -/
def naiveShare (P : Params) (n : ℕ) : ℝ := share (naiveIter P n).1 (naiveIter P n).2

/-- Total count after `n` naive steps. -/
def naiveTotal (P : Params) (n : ℕ) : ℝ := (naiveIter P n).1 + (naiveIter P n).2

theorem naiveIter_pos (P : Params) (n : ℕ) :
    0 < (naiveIter P n).1 ∧ 0 < (naiveIter P n).2 := by
  induction n with
  | zero => exact ⟨P.cA0_pos, P.cB0_pos⟩
  | succ n ih =>
    obtain ⟨hA, hB⟩ := ih
    have hc : 0 < (naiveIter P n).1 + (naiveIter P n).2 := by positivity
    have hs0 : 0 < share (naiveIter P n).1 (naiveIter P n).2 := by unfold share; positivity
    have hs1 : share (naiveIter P n).1 (naiveIter P n).2 < 1 := by
      unfold share; rw [div_lt_one hc]; linarith
    have hlA := P.lamA_pos
    have hlB := P.lamB_pos
    simp only [naiveIter, naiveStep]
    constructor
    · positivity
    · have : 0 < 1 - share (naiveIter P n).1 (naiveIter P n).2 := by linarith
      positivity

theorem naiveShare_pos (P : Params) (n : ℕ) : 0 < naiveShare P n := by
  obtain ⟨hA, hB⟩ := naiveIter_pos P n
  unfold naiveShare share; positivity

theorem naiveShare_lt_one (P : Params) (n : ℕ) : naiveShare P n < 1 := by
  obtain ⟨hA, hB⟩ := naiveIter_pos P n
  unfold naiveShare share
  rw [div_lt_one (by positivity)]; linarith

/-- The share is strictly increasing when λA > λB. -/
theorem naiveShare_strictMono (P : Params) (hlam : P.lamB < P.lamA) :
    StrictMono (naiveShare P) := by
  apply strictMono_nat_of_lt_succ
  intro n
  obtain ⟨hA, hB⟩ := naiveIter_pos P n
  unfold naiveShare
  change share (naiveIter P n).1 (naiveIter P n).2 < share (naiveIter P (n+1)).1 (naiveIter P (n+1)).2
  simp only [naiveIter]
  exact naive_share_increasing P _ _ hA hB hlam

/-- The total count grows by at most λA + λB per step. -/
theorem naiveTotal_succ_le (P : Params) (n : ℕ) :
    naiveTotal P (n + 1) ≤ naiveTotal P n + (P.lamA + P.lamB) := by
  obtain ⟨hA, hB⟩ := naiveIter_pos P n
  have hs0 := naiveShare_pos P n
  have hs1 := naiveShare_lt_one P n
  unfold naiveShare at hs0 hs1
  have hlA := P.lamA_pos
  have hlB := P.lamB_pos
  unfold naiveTotal
  simp only [naiveIter, naiveStep]
  nlinarith [mul_pos hlA hs0, mul_pos hlB (sub_pos.mpr hs1)]

theorem naiveTotal_le (P : Params) (n : ℕ) :
    naiveTotal P n ≤ naiveTotal P 0 + n * (P.lamA + P.lamB) := by
  induction n with
  | zero => simp
  | succ n ih =>
    have := naiveTotal_succ_le P n
    push_cast
    linarith

/-- Lower bound on the one-step gain when the share stays below `1 - ε`. -/
theorem naiveShare_gain (P : Params) (hlam : P.lamB < P.lamA) (n : ℕ) (ε : ℝ)
    (hε : ε ≤ 1 - naiveShare P n) :
    naiveShare P 0 * ε * (P.lamA - P.lamB) / (naiveTotal P 0 + (n + 1) * (P.lamA + P.lamB))
      ≤ naiveShare P (n + 1) - naiveShare P n := by
  obtain ⟨hA, hB⟩ := naiveIter_pos P n
  have hd := naive_step_drift P (naiveIter P n).1 (naiveIter P n).2 hA hB
  have hstep : naiveShare P (n + 1) - naiveShare P n
      = share (naiveIter P n).1 (naiveIter P n).2 * (1 - share (naiveIter P n).1 (naiveIter P n).2)
        * (P.lamA - P.lamB)
        / ((naiveIter P n).1 + (naiveIter P n).2 + P.lamA * share (naiveIter P n).1 (naiveIter P n).2
          + P.lamB * (1 - share (naiveIter P n).1 (naiveIter P n).2)) := by
    unfold naiveShare; simp only [naiveIter]; exact hd
  rw [hstep]
  have hx0 : naiveShare P 0 ≤ share (naiveIter P n).1 (naiveIter P n).2 :=
    (naiveShare_strictMono P hlam).monotone (Nat.zero_le n)
  have hxpos := naiveShare_pos P 0
  have hδ : 0 < P.lamA - P.lamB := by linarith
  have hs1 := naiveShare_lt_one P n
  unfold naiveShare at hs1 hx0
  have hden_pos := naive_den_pos P _ _ hA hB
  -- denominator bound: c_n + λA x + λB (1-x) ≤ c_n + Λ ≤ c_0 + (n+1) Λ
  have hden_le : (naiveIter P n).1 + (naiveIter P n).2
      + P.lamA * share (naiveIter P n).1 (naiveIter P n).2
      + P.lamB * (1 - share (naiveIter P n).1 (naiveIter P n).2)
      ≤ naiveTotal P 0 + (n + 1) * (P.lamA + P.lamB) := by
    have h1 := naiveTotal_le P n
    unfold naiveTotal at h1
    have hs0 : 0 < share (naiveIter P n).1 (naiveIter P n).2 := by unfold share; positivity
    have hlA := P.lamA_pos
    have hlB := P.lamB_pos
    have e1 : P.lamA * share (naiveIter P n).1 (naiveIter P n).2 ≤ P.lamA := by
      have := mul_le_mul_of_nonneg_left hs1.le hlA.le; simpa using this
    have e2 : P.lamB * (1 - share (naiveIter P n).1 (naiveIter P n).2) ≤ P.lamB := by
      have h : 1 - share (naiveIter P n).1 (naiveIter P n).2 ≤ 1 := by linarith
      have := mul_le_mul_of_nonneg_left h hlB.le; simpa using this
    have e3 : ((n : ℝ) + 1) * (P.lamA + P.lamB) = n * (P.lamA + P.lamB) + (P.lamA + P.lamB) := by ring
    unfold naiveTotal
    linarith
  have hnum : naiveShare P 0 * ε * (P.lamA - P.lamB)
      ≤ share (naiveIter P n).1 (naiveIter P n).2
        * (1 - share (naiveIter P n).1 (naiveIter P n).2) * (P.lamA - P.lamB) := by
    have hε' : ε ≤ 1 - share (naiveIter P n).1 (naiveIter P n).2 := by
      unfold naiveShare at hε; exact hε
    have : naiveShare P 0 * ε ≤ share (naiveIter P n).1 (naiveIter P n).2
        * (1 - share (naiveIter P n).1 (naiveIter P n).2) := by
      by_cases hεpos : 0 ≤ ε
      · exact mul_le_mul hx0 hε' hεpos (by unfold share; positivity)
      · push Not at hεpos
        have : naiveShare P 0 * ε < 0 := mul_neg_of_pos_of_neg hxpos hεpos
        have : 0 ≤ share (naiveIter P n).1 (naiveIter P n).2
            * (1 - share (naiveIter P n).1 (naiveIter P n).2) := by
          apply mul_nonneg (by unfold share; positivity); linarith
        linarith
    exact mul_le_mul_of_nonneg_right this hδ.le
  have hnum_nonneg : 0 ≤ share (naiveIter P n).1 (naiveIter P n).2
      * (1 - share (naiveIter P n).1 (naiveIter P n).2) * (P.lamA - P.lamB) := by
    apply mul_nonneg (mul_nonneg (by unfold share; positivity) (by linarith)) hδ.le
  calc naiveShare P 0 * ε * (P.lamA - P.lamB) / (naiveTotal P 0 + (n + 1) * (P.lamA + P.lamB))
      ≤ share (naiveIter P n).1 (naiveIter P n).2
          * (1 - share (naiveIter P n).1 (naiveIter P n).2) * (P.lamA - P.lamB)
          / (naiveTotal P 0 + (n + 1) * (P.lamA + P.lamB)) := by
        apply div_le_div_of_nonneg_right hnum
        have h0 : naiveTotal P 0 = P.cA0 + P.cB0 := rfl
        rw [h0]
        have := P.cA0_pos; have := P.cB0_pos; have := P.lamA_pos; have := P.lamB_pos
        positivity
    _ ≤ _ := by
        apply div_le_div_of_nonneg_left hnum_nonneg hden_pos hden_le

/-- The harmonic-type series `1 / (a + (n+1) b)` with `a ≥ 0`, `b > 0` is not summable. -/
theorem not_summable_shifted_harmonic (a b : ℝ) (ha : 0 ≤ a) (hb : 0 < b) :
    ¬ Summable (fun n : ℕ => 1 / (a + (n + 1) * b)) := by
  intro hs
  have hcomp : ∀ n : ℕ, 1 / ((n : ℝ) + 1) ≤ (a + b) * (1 / (a + (n + 1) * b)) := by
    intro n
    have hn : (0 : ℝ) ≤ n := by positivity
    have hd1 : (0 : ℝ) < (n : ℝ) + 1 := by positivity
    have hd2 : 0 < a + (n + 1) * b := by positivity
    rw [← div_eq_mul_one_div]
    rw [div_le_div_iff₀ hd1 hd2]
    nlinarith
  have h1 : Summable (fun n : ℕ => 1 / ((n : ℝ) + 1)) :=
    Summable.of_nonneg_of_le (fun n => by positivity) hcomp (hs.mul_left (a + b))
  have h2 : Summable (fun n : ℕ => 1 / (n : ℝ)) := by
    rw [← summable_nat_add_iff 1]
    refine h1.congr (fun n => ?_)
    push_cast; ring
  exact Real.not_summable_one_div_natCast h2

/-- Runaway: with λA > λB the naive share converges to 1 whatever the initial counts. -/
theorem naiveShare_tendsto_one (P : Params) (hlam : P.lamB < P.lamA) :
    Tendsto (naiveShare P) atTop (𝓝 1) := by
  have hmono := (naiveShare_strictMono P hlam).monotone
  have hbdd : BddAbove (Set.range (naiveShare P)) :=
    ⟨1, by rintro _ ⟨n, rfl⟩; exact (naiveShare_lt_one P n).le⟩
  have hlim := tendsto_atTop_ciSup hmono hbdd
  have hL1 : (⨆ n, naiveShare P n) ≤ 1 := ciSup_le (fun n => (naiveShare_lt_one P n).le)
  rcases hL1.lt_or_eq with hlt | heq
  · exfalso
    have hεpos : 0 < 1 - (⨆ n, naiveShare P n) := by linarith
    have hle : ∀ n, 1 - (⨆ n, naiveShare P n) ≤ 1 - naiveShare P n := fun n => by
      have := le_ciSup hbdd n; linarith
    have hδ : 0 < P.lamA - P.lamB := by linarith
    have hK : 0 < naiveShare P 0 * (1 - (⨆ n, naiveShare P n)) * (P.lamA - P.lamB) := by
      have := naiveShare_pos P 0; positivity
    have hΛ : 0 < P.lamA + P.lamB := by have := P.lamA_pos; have := P.lamB_pos; linarith
    have hc0 : 0 ≤ naiveTotal P 0 := by
      change 0 ≤ P.cA0 + P.cB0
      have := P.cA0_pos; have := P.cB0_pos; linarith
    have hgain : ∀ n, naiveShare P 0 * (1 - (⨆ n, naiveShare P n)) * (P.lamA - P.lamB)
        / (naiveTotal P 0 + (n + 1) * (P.lamA + P.lamB))
        ≤ naiveShare P (n + 1) - naiveShare P n :=
      fun n => naiveShare_gain P hlam n _ (hle n)
    have hpartial : ∀ N, ∑ n ∈ Finset.range N,
        naiveShare P 0 * (1 - (⨆ n, naiveShare P n)) * (P.lamA - P.lamB)
          / (naiveTotal P 0 + (n + 1) * (P.lamA + P.lamB)) ≤ 1 := by
      intro N
      calc ∑ n ∈ Finset.range N, naiveShare P 0 * (1 - (⨆ n, naiveShare P n)) * (P.lamA - P.lamB)
              / (naiveTotal P 0 + (n + 1) * (P.lamA + P.lamB))
          ≤ ∑ n ∈ Finset.range N, (naiveShare P (n + 1) - naiveShare P n) :=
            Finset.sum_le_sum (fun n _ => hgain n)
        _ = naiveShare P N - naiveShare P 0 := Finset.sum_range_sub (naiveShare P) N
        _ ≤ 1 := by have := naiveShare_lt_one P N; have := naiveShare_pos P 0; linarith
    have hsum : Summable (fun n : ℕ => naiveShare P 0 * (1 - (⨆ n, naiveShare P n)) * (P.lamA - P.lamB)
        / (naiveTotal P 0 + (n + 1) * (P.lamA + P.lamB))) :=
      summable_of_sum_range_le (fun n => by positivity) hpartial
    have hsum' : Summable (fun n : ℕ => 1 / (naiveTotal P 0 + (n + 1) * (P.lamA + P.lamB))) := by
      have hK' := hK.ne'
      have h0 := (naiveShare_pos P 0).ne'
      have hε' := hεpos.ne'
      have hδ' := hδ.ne'
      refine (hsum.mul_left (1 / (naiveShare P 0 * (1 - (⨆ n, naiveShare P n)) * (P.lamA - P.lamB)))).congr (fun n => ?_)
      field_simp
    exact not_summable_shifted_harmonic _ _ hc0 hΛ hsum'
  · rw [heq] at hlim; exact hlim

end Research.P4
