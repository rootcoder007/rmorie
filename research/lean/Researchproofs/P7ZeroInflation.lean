/-
  Researchproofs/P7ZeroInflation.lean

  P7, continued: the zero share with the mean cannot identify zero inflation
  (library scan A6; Weisburd & Britt ch. 6 p. 255).

  A zero-inflated Poisson ZIP(w, λ) has mean μ = (1 − w)λ and zero probability
  p₀ = w + (1 − w)e^{−λ}. By strict convexity of exp, e^{−μ} < p₀ < 1
  (`zip_zero_gt_poisson`, `zip_zero_lt_one`). The negative binomial with mean
  μ and dispersion θ has zero probability (1 + μ/θ)^{−θ}, continuous in θ,
  tending to 1 as θ → 0⁺ and to e^{−μ} as θ → ∞ (`nb_zero_tendsto_zero`,
  `nb_zero_tendsto_atTop`); by the intermediate value theorem some θ matches
  p₀ (`nb_matches_zip`). So an NB with the same mean and the same zero share
  exists for every ZIP: "NB does not model zero-inflated data" cannot be read
  off the zero share; the two laws differ elsewhere in the pmf.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Real Filter Topology Set

noncomputable section

namespace Research.P7.ZeroInflation

def zipMean (w lam : ℝ) : ℝ := (1 - w) * lam
def zipZero (w lam : ℝ) : ℝ := w + (1 - w) * exp (-lam)
/-- NB zero probability at mean `μ` and dispersion `θ`. -/
def nbZero (μ θ : ℝ) : ℝ := (1 + μ / θ) ^ (-θ)

theorem zip_zero_gt_poisson (w lam : ℝ) (hw : 0 < w) (hw1 : w < 1) (hlam : 0 < lam) :
    exp (-zipMean w lam) < zipZero w lam := by
  have h := strictConvexOn_exp.2 (mem_univ (0 : ℝ)) (mem_univ (-lam)) (by linarith) hw
    (by linarith : (0 : ℝ) < 1 - w) (by ring)
  simp only [smul_eq_mul, mul_zero, zero_add, exp_zero, mul_one] at h
  unfold zipMean zipZero
  have : (1 - w) * -lam = -((1 - w) * lam) := by ring
  rw [this] at h
  exact h

theorem zip_zero_lt_one (w lam : ℝ) (hw1 : w < 1) (hlam : 0 < lam) : zipZero w lam < 1 := by
  unfold zipZero
  have h1 : exp (-lam) < 1 := by
    have := Real.exp_lt_exp.mpr (by linarith : -lam < 0)
    rwa [Real.exp_zero] at this
  nlinarith

theorem nbZero_eq (μ θ : ℝ) (hθ : 0 < θ) (hμ : 0 < μ) :
    nbZero μ θ = exp (log (1 + μ / θ) * -θ) := by
  unfold nbZero
  rw [Real.rpow_def_of_pos (by positivity)]

theorem nb_zero_tendsto_atTop (μ : ℝ) (hμ : 0 < μ) :
    Tendsto (nbZero μ) atTop (𝓝 (exp (-μ))) := by
  have h := Real.tendsto_one_add_div_rpow_exp μ
  have h2 : Tendsto (fun x : ℝ => ((1 + μ / x) ^ x)⁻¹) atTop (𝓝 (exp μ)⁻¹) :=
    h.inv₀ (exp_pos μ).ne'
  rw [← Real.exp_neg] at h2
  refine h2.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with x hx
  unfold nbZero
  rw [Real.rpow_neg (by positivity)]

theorem nb_zero_tendsto_zero (μ : ℝ) (hμ : 0 < μ) :
    Tendsto (nbZero μ) (𝓝[>] 0) (𝓝 1) := by
  -- log(1 + μ/θ) * (-θ) = θ log θ - θ log (θ + μ) → 0
  have hlog : Tendsto (fun θ : ℝ => log θ * θ) (𝓝[>] 0) (𝓝 0) := by
    have := tendsto_log_mul_rpow_nhdsGT_zero (one_pos : (0 : ℝ) < 1)
    refine this.congr' ?_
    filter_upwards [self_mem_nhdsWithin] with θ _
    rw [Real.rpow_one]
  have hθ : Tendsto (fun θ : ℝ => θ) (𝓝[>] 0) (𝓝 0) := tendsto_nhdsWithin_of_tendsto_nhds tendsto_id
  have hsum : Tendsto (fun θ : ℝ => log (θ + μ)) (𝓝[>] 0) (𝓝 (log μ)) := by
    have hc : ContinuousAt (fun θ : ℝ => log (θ + μ)) 0 := by
      apply ContinuousAt.log
      · exact (continuous_id.add continuous_const).continuousAt
      · simp; exact hμ.ne'
    have := hc.tendsto
    simp only [zero_add] at this
    exact this.mono_left nhdsWithin_le_nhds
  have hprod : Tendsto (fun θ : ℝ => θ * log (θ + μ)) (𝓝[>] 0) (𝓝 0) := by
    have := hθ.mul hsum
    simpa using this
  have hexp : Tendsto (fun θ : ℝ => log θ * θ - θ * log (θ + μ)) (𝓝[>] 0) (𝓝 0) := by
    have := hlog.sub hprod
    simpa using this
  have hfin : Tendsto (fun θ : ℝ => exp (log θ * θ - θ * log (θ + μ))) (𝓝[>] 0) (𝓝 1) := by
    have := (Real.continuous_exp.tendsto 0).comp hexp
    rw [Real.exp_zero] at this
    exact this
  refine hfin.congr' ?_
  filter_upwards [self_mem_nhdsWithin] with θ hθ0
  have hθp : 0 < θ := hθ0
  rw [nbZero_eq μ θ hθp hμ]
  congr 1
  have : 1 + μ / θ = (θ + μ) / θ := by field_simp
  rw [this, Real.log_div (by positivity) hθp.ne']
  ring

theorem nbZero_continuousOn (μ a b : ℝ) (hμ : 0 < μ) (ha : 0 < a) :
    ContinuousOn (nbZero μ) (Icc a b) := by
  unfold nbZero
  apply ContinuousOn.rpow
  · apply continuousOn_const.add
    apply continuousOn_const.div continuousOn_id
    intro x hx; exact (lt_of_lt_of_le ha hx.1).ne'
  · exact continuousOn_id.neg
  · intro x hx
    left
    have : 0 < x := lt_of_lt_of_le ha hx.1
    positivity

/-- For every ZIP there is a negative binomial with the same mean and the same zero share. -/
theorem nb_matches_zip (w lam : ℝ) (hw : 0 < w) (hw1 : w < 1) (hlam : 0 < lam) :
    ∃ θ : ℝ, 0 < θ ∧ nbZero (zipMean w lam) θ = zipZero w lam := by
  set μ := zipMean w lam with hμdef
  have hμ : 0 < μ := by unfold zipMean at hμdef; rw [hμdef]; nlinarith
  set p0 := zipZero w lam with hp0
  have hlow : exp (-μ) < p0 := zip_zero_gt_poisson w lam hw hw1 hlam
  have hhigh : p0 < 1 := zip_zero_lt_one w lam hw1 hlam
  -- a large positive θ with nbZero below p0
  obtain ⟨b, hb, hb0⟩ := (((nb_zero_tendsto_atTop μ hμ).eventually (gt_mem_nhds hlow)).and (eventually_gt_atTop (0 : ℝ))).exists
  -- a small positive θ (below b) with nbZero above p0
  have hev : ∀ᶠ θ in 𝓝[>] (0 : ℝ), p0 < nbZero μ θ ∧ 0 < θ ∧ θ < b := by
    have h1 := (nb_zero_tendsto_zero μ hμ).eventually (lt_mem_nhds hhigh)
    have h2 : ∀ᶠ θ in 𝓝[>] (0 : ℝ), 0 < θ := self_mem_nhdsWithin
    have h3 : ∀ᶠ θ in 𝓝[>] (0 : ℝ), θ < b :=
      eventually_nhdsWithin_of_eventually_nhds (eventually_lt_nhds hb0)
    filter_upwards [h1, h2, h3] with θ ha hb' hc
    exact ⟨ha, hb', hc⟩
  obtain ⟨a, ha, ha0, hab⟩ := hev.exists
  have hcont := nbZero_continuousOn μ a b hμ ha0
  have hmem : p0 ∈ Icc (nbZero μ b) (nbZero μ a) := ⟨hb.le, ha.le⟩
  obtain ⟨θ, hθ, hθv⟩ := intermediate_value_Icc' hab.le hcont hmem
  exact ⟨θ, lt_of_lt_of_le ha0 hθ.1, hθv⟩

end Research.P7.ZeroInflation
