/-
  Researchproofs/P5DesignSensitivity.lean

  P5/P8, continued: design sensitivity (library scan B17; Fogarty, Handbook
  ch. 25 §25.4.1; Weisburd & Britt ch. 10).

  Along any sequence of matched studies in which the statistic T_n satisfies
  T_n/n → μ and the worst-case critical value at sensitivity Γ satisfies
  c_n/n → μ_Γ, the test rejects for all large n when μ > μ_Γ
  (`eventually_reject`) and fails to reject for all large n when μ < μ_Γ
  (`eventually_accept`): the limiting power is 1 or 0 and depends only on the
  comparison of μ with μ_Γ. The design sensitivity Γ̃ is the root of μ = μ_Γ;
  when Γ ↦ μ_Γ is continuous and strictly increasing the root exists and is
  unique (`design_sensitivity_exists_unique`).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Filter Topology Set

namespace Research.P5.DesignSensitivity

theorem eventually_reject (t c : ℕ → ℝ) (μ μΓ : ℝ)
    (ht : Tendsto (fun n : ℕ => t n / n) atTop (𝓝 μ))
    (hc : Tendsto (fun n : ℕ => c n / n) atTop (𝓝 μΓ)) (h : μΓ < μ) :
    ∀ᶠ n in atTop, c n < t n := by
  have hd : Tendsto (fun n : ℕ => t n / n - c n / n) atTop (𝓝 (μ - μΓ)) := ht.sub hc
  have hev := hd.eventually (lt_mem_nhds (by linarith : (0 : ℝ) < μ - μΓ))
  filter_upwards [hev, eventually_ge_atTop 1] with n hn hn1
  have hnpos : (0 : ℝ) < n := by exact_mod_cast hn1
  rw [← sub_div] at hn
  have := (div_pos_iff_of_pos_right hnpos).mp hn
  linarith

theorem eventually_accept (t c : ℕ → ℝ) (μ μΓ : ℝ)
    (ht : Tendsto (fun n : ℕ => t n / n) atTop (𝓝 μ))
    (hc : Tendsto (fun n : ℕ => c n / n) atTop (𝓝 μΓ)) (h : μ < μΓ) :
    ∀ᶠ n in atTop, t n < c n := by
  have := eventually_reject c t μΓ μ hc ht h
  exact this

/-- The design sensitivity: the unique `Γ` in `[1, G]` with `μ_Γ = μ`. -/
theorem design_sensitivity_exists_unique (g : ℝ → ℝ) (μ G : ℝ) (hG : 1 ≤ G)
    (hmono : StrictMono g) (hcont : ContinuousOn g (Icc 1 G))
    (h1 : g 1 < μ) (h2 : μ < g G) :
    ∃! Γ, Γ ∈ Icc 1 G ∧ g Γ = μ := by
  obtain ⟨Γ, hΓ, hgΓ⟩ := intermediate_value_Icc hG hcont ⟨h1.le, h2.le⟩
  refine ⟨Γ, ⟨hΓ, hgΓ⟩, ?_⟩
  rintro Γ' ⟨_, hgΓ'⟩
  exact hmono.injective (hgΓ'.trans hgΓ.symm)

end Research.P5.DesignSensitivity
