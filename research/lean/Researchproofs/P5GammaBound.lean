/-
  Researchproofs/P5GammaBound.lean

  P5/P8, continued: a Rosenbaum Γ-bound is not a test of ignorability
  (library scan A7; Weisburd & Britt ch. 10 pp. 431-433).

  In a matched-pairs sign test with n discordant pairs and T pairs in which the
  treated unit is the case, the sensitivity model lets the probability that
  the treated unit is the case range over [1/(1 + Γ), Γ/(1 + Γ)]; the
  worst-case expectation of T is n Γ/(1 + Γ) (`worstMean`).
    * `worst_mean_mono`: the worst case is non-decreasing in Γ, so the
      worst-case evidence only weakens as Γ grows.
    * `gamma_star`: Γ* = T/(n − T) is the value at which the worst-case mean
      equals the observed T (`worst_mean_at_gamma_star`); above it the
      observed count is no longer above the worst-case expectation
      (`gamma_star_kills`), and it is finite for every T < n.
    * `gamma_star_table_only`: Γ* is a function of (T, n) alone; two
      situations with the same discordant table and different hidden biases
      have the same Γ*. The analysis bounds what a hidden bias of size Γ could
      do; it cannot say whether one is present.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P5.Gamma

def worstMean (n Γ : ℝ) : ℝ := n * Γ / (1 + Γ)

theorem worst_mean_mono (n Γ Γ' : ℝ) (hn : 0 ≤ n) (hΓ : 0 ≤ Γ) (hΓ' : Γ ≤ Γ') :
    worstMean n Γ ≤ worstMean n Γ' := by
  unfold worstMean
  rw [div_le_div_iff₀ (by linarith) (by linarith)]
  nlinarith

def gammaStar (T n : ℝ) : ℝ := T / (n - T)

theorem worst_mean_at_gamma_star (T n : ℝ) (hT : 0 ≤ T) (hTn : T < n) :
    worstMean n (gammaStar T n) = T := by
  unfold worstMean gammaStar
  have h : n - T ≠ 0 := by linarith
  have hn : n ≠ 0 := by linarith
  have hpos : 0 < 1 + T / (n - T) := by
    have := div_nonneg hT (by linarith : (0 : ℝ) ≤ n - T)
    linarith
  rw [div_eq_iff hpos.ne']
  field_simp
  ring

theorem gamma_star_kills (T n Γ : ℝ) (hT : 0 ≤ T) (hTn : T < n) (hΓ : gammaStar T n ≤ Γ) :
    T ≤ worstMean n Γ := by
  have h0 : 0 ≤ gammaStar T n := by unfold gammaStar; apply div_nonneg hT; linarith
  calc T = worstMean n (gammaStar T n) := (worst_mean_at_gamma_star T n hT hTn).symm
    _ ≤ worstMean n Γ := worst_mean_mono n _ Γ (by linarith) h0 hΓ

/-- A matched-pairs situation: the observed discordant table and a hidden bias. -/
structure Situation where
  T : ℝ
  n : ℝ
  hidden_bias : ℝ

def gammaStarOf (s : Situation) : ℝ := gammaStar s.T s.n

theorem gamma_star_table_only (s s' : Situation) (hT : s.T = s'.T) (hn : s.n = s'.n) :
    gammaStarOf s = gammaStarOf s' := by
  unfold gammaStarOf gammaStar; rw [hT, hn]

/-- Below Γ* the observed count exceeds the worst-case expectation: the evidence survives. -/
theorem below_gamma_star (T n Γ : ℝ) (hT : 0 < T) (hTn : T < n) (hΓ0 : 0 ≤ Γ) (hΓ : Γ < gammaStar T n) :
    worstMean n Γ < T := by
  unfold worstMean
  unfold gammaStar at hΓ
  rw [div_lt_iff₀ (by linarith)]
  rw [lt_div_iff₀ (by linarith)] at hΓ
  nlinarith

end Research.P5.Gamma
