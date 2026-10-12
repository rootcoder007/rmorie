/-
  Researchproofs/P5Overlap.lean

  P5, continued: Albert & Anderson's converse. When the data overlap — no
  direction d ≠ 0 leaves every margin s_i x_i·d non-negative — the logistic
  log-likelihood attains its maximum: a finite estimate exists.

  Overlap is stated as: for every direction d ≠ 0 some observation has a
  strictly negative margin. On the unit sphere the smallest margin is a
  continuous function, negative everywhere, so it has a negative maximum
  −δ (sphere_margin_neg). Along any b of norm r the smallest margin is
  then at most −δ r, the log-likelihood at most ll1(−δ r), which is below
  −δ r (loglik_le_of_norm, loglik_small_far). The closed ball on which
  ℓ ≥ ℓ(0) can hold is compact, ℓ attains its maximum there, and outside
  it ℓ < ℓ(0) (mle_exists).

  Together with P5Separation: a maximum-likelihood estimate exists if and
  only if the data are not quasi-completely separated.
-/
import Mathlib
import Researchproofs.P5Separation

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset Filter Topology

namespace Research.P5

variable {n p : Type*} [Fintype n] [Fintype p]

/-- Overlap: every non-zero direction gives some observation a negative margin. -/
def Overlap (D : Logit n p) : Prop := ∀ d : p → ℝ, d ≠ 0 → ∃ i, D.s i * dot (D.x i) d < 0

theorem dot_continuous (x : p → ℝ) : Continuous fun b : p → ℝ => dot x b := by
  unfold dot
  exact continuous_finsetSum _ fun j _ => continuous_const.mul (continuous_apply j)

theorem dot_smul (x b : p → ℝ) (r : ℝ) : dot x (r • b) = r * dot x b := by
  unfold dot
  simp only [Pi.smul_apply, smul_eq_mul, mul_sum]
  exact sum_congr rfl fun j _ => by ring

/-- The smallest margin over the observations. -/
def minMargin [Nonempty n] (D : Logit n p) (b : p → ℝ) : ℝ :=
  (univ : Finset n).inf' univ_nonempty fun i => D.s i * dot (D.x i) b

theorem minMargin_continuous [Nonempty n] (D : Logit n p) : Continuous (minMargin D) := by
  unfold minMargin
  have h := Continuous.finset_inf' (univ_nonempty : (univ : Finset n).Nonempty)
    fun i (_ : i ∈ univ) => (continuous_const.mul (dot_continuous (D.x i)) :
      Continuous fun b : p → ℝ => D.s i * dot (D.x i) b)
  convert h using 1
  funext b
  simp only [Finset.inf'_apply, Pi.mul_apply]

theorem minMargin_le [Nonempty n] (D : Logit n p) (b : p → ℝ) (i : n) :
    minMargin D b ≤ D.s i * dot (D.x i) b :=
  inf'_le _ (mem_univ i)

theorem minMargin_attained [Nonempty n] (D : Logit n p) (b : p → ℝ) :
    ∃ i, minMargin D b = D.s i * dot (D.x i) b := by
  obtain ⟨i, _, hi⟩ := exists_mem_eq_inf' (univ_nonempty : (univ : Finset n).Nonempty)
    (fun i => D.s i * dot (D.x i) b)
  exact ⟨i, hi⟩

/-- The log-likelihood is at most `ll1` of the smallest margin (the other terms are negative). -/
theorem loglik_le_ll1_min [Nonempty n] (D : Logit n p) (b : p → ℝ) : D.loglik b ≤ ll1 (minMargin D b) := by
  classical
  obtain ⟨i₀, hmin⟩ := minMargin_attained D b
  unfold Logit.loglik
  rw [hmin, ← sum_erase_add univ (fun i => ll1 (D.s i * dot (D.x i) b)) (mem_univ i₀)]
  have hle : ∑ i ∈ univ.erase i₀, ll1 (D.s i * dot (D.x i) b) ≤ 0 := sum_nonpos fun i _ => (ll1_neg _).le
  linarith

/-- On the unit sphere the smallest margin has a negative maximum under overlap. -/
theorem sphere_margin_neg [Nonempty n] [Nonempty p] (D : Logit n p) (hov : Overlap D) :
    ∃ δ : ℝ, 0 < δ ∧ ∀ d : p → ℝ, ‖d‖ = 1 → minMargin D d ≤ -δ := by
  classical
  have hcomp : IsCompact (Metric.sphere (0 : p → ℝ) 1) := isCompact_sphere 0 1
  have hne : (Metric.sphere (0 : p → ℝ) 1).Nonempty := by
    obtain ⟨j⟩ := ‹Nonempty p›
    refine ⟨Pi.single j (1 : ℝ), ?_⟩
    rw [mem_sphere_zero_iff_norm, Pi.norm_single, norm_one]
  obtain ⟨d₀, hd₀, hmax⟩ := hcomp.exists_isMaxOn hne (minMargin_continuous D).continuousOn
  rw [mem_sphere_zero_iff_norm] at hd₀
  have hd₀ne : d₀ ≠ 0 := by
    intro h; rw [h, norm_zero] at hd₀; exact zero_ne_one hd₀
  obtain ⟨i, hi⟩ := hov d₀ hd₀ne
  refine ⟨-(minMargin D d₀), ?_, ?_⟩
  · have := minMargin_le D d₀ i
    linarith
  · intro d hd
    rw [neg_neg]
    exact hmax (mem_sphere_zero_iff_norm.mpr hd)

theorem loglik_le_of_norm [Nonempty n] (D : Logit n p) (δ : ℝ)
    (hδ : ∀ d : p → ℝ, ‖d‖ = 1 → minMargin D d ≤ -δ) (b : p → ℝ) (hb : b ≠ 0) :
    D.loglik b ≤ ll1 (-(δ * ‖b‖)) := by
  have hnorm : 0 < ‖b‖ := norm_pos_iff.mpr hb
  set u := (‖b‖)⁻¹ • b with hu
  have hu1 : ‖u‖ = 1 := by
    rw [hu, norm_smul, norm_inv, norm_norm, inv_mul_cancel₀ hnorm.ne']
  have hbu : b = ‖b‖ • u := by
    rw [hu, smul_smul, mul_inv_cancel₀ hnorm.ne', one_smul]
  obtain ⟨i, hi⟩ := minMargin_attained D u
  have hmargin : D.s i * dot (D.x i) b = ‖b‖ * (D.s i * dot (D.x i) u) := by
    have h : D.s i * dot (D.x i) (‖b‖ • u) = ‖b‖ * (D.s i * dot (D.x i) u) := by
      rw [dot_smul]; ring
    rwa [← hbu] at h
  have hu_le : D.s i * dot (D.x i) u ≤ -δ := by rw [← hi]; exact hδ u hu1
  calc D.loglik b ≤ ll1 (minMargin D b) := loglik_le_ll1_min D b
    _ ≤ ll1 (D.s i * dot (D.x i) b) := ll1_strictMono.monotone (minMargin_le D b i)
    _ ≤ ll1 (-(δ * ‖b‖)) := by
        apply ll1_strictMono.monotone
        rw [hmargin]
        nlinarith

theorem ll1_le_neg (v : ℝ) : ll1 (-v) ≤ -v := by
  unfold ll1
  rw [neg_neg, neg_le_neg_iff]
  calc v = Real.log (Real.exp v) := (Real.log_exp v).symm
    _ ≤ Real.log (1 + Real.exp v) :=
        Real.log_le_log (Real.exp_pos _) (by linarith [Real.exp_pos v])

/-- Far from the origin the log-likelihood is below any level. -/
theorem loglik_small_far [Nonempty n] (D : Logit n p) (δ : ℝ) (hδpos : 0 < δ)
    (hδ : ∀ d : p → ℝ, ‖d‖ = 1 → minMargin D d ≤ -δ) (M : ℝ) :
    ∃ R : ℝ, 0 < R ∧ ∀ b : p → ℝ, R < ‖b‖ → D.loglik b < M := by
  refine ⟨max 1 ((1 - M) / δ), lt_of_lt_of_le one_pos (le_max_left _ _), ?_⟩
  intro b hb
  have hb0 : b ≠ 0 := by
    intro h; rw [h, norm_zero] at hb; linarith [le_max_left 1 ((1 - M) / δ)]
  have h1 := loglik_le_of_norm D δ hδ b hb0
  have hv : (1 - M) / δ < ‖b‖ := lt_of_le_of_lt (le_max_right _ _) hb
  have hv' : 1 - M < δ * ‖b‖ := by rw [div_lt_iff₀ hδpos, mul_comm] at hv; exact hv
  have hll := ll1_le_neg (δ * ‖b‖)
  linarith

theorem ll1_continuous : Continuous ll1 := by
  unfold ll1
  have hpos : ∀ u : ℝ, 1 + Real.exp (-u) ∈ ({0}ᶜ : Set ℝ) := by
    intro u
    simp only [Set.mem_compl_singleton_iff]
    positivity
  exact (Real.continuousOn_log.comp_continuous (by fun_prop) hpos).neg

theorem loglik_continuous (D : Logit n p) : Continuous D.loglik := by
  unfold Logit.loglik
  exact continuous_finsetSum _ fun i _ =>
    ll1_continuous.comp (continuous_const.mul (dot_continuous (D.x i)))

/-- Albert–Anderson, the converse: under overlap the log-likelihood has a maximiser. -/
theorem mle_exists [Nonempty n] [Nonempty p] (D : Logit n p) (hov : Overlap D) :
    ∃ b : p → ℝ, ∀ b' : p → ℝ, D.loglik b' ≤ D.loglik b := by
  obtain ⟨δ, hδpos, hδ⟩ := sphere_margin_neg D hov
  obtain ⟨R, hR, hfar⟩ := loglik_small_far D δ hδpos hδ (D.loglik 0)
  have hball : IsCompact (Metric.closedBall (0 : p → ℝ) R) := isCompact_closedBall 0 R
  have hne : (Metric.closedBall (0 : p → ℝ) R).Nonempty := ⟨0, by simp [hR.le]⟩
  obtain ⟨b, _, hbmax⟩ := hball.exists_isMaxOn hne (loglik_continuous D).continuousOn
  refine ⟨b, fun b' => ?_⟩
  by_cases hb' : b' ∈ Metric.closedBall (0 : p → ℝ) R
  · exact hbmax hb'
  · have : R < ‖b'‖ := by
      rw [Metric.mem_closedBall, dist_zero_right] at hb'
      exact lt_of_not_ge hb'
    have h0 : D.loglik 0 ≤ D.loglik b := hbmax (by simp [hR.le])
    exact (hfar b' this).le.trans h0

end Research.P5
