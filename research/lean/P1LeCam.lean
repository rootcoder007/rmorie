/-
  Researchproofs/P1LeCam.lean

  P1 (continued): Le Cam's two-point lower bound on a finite sample space
  (Le Cam 1973; Yu 1997 "Assouad, Fano and Le Cam"; Tsybakov 2009, §2.3).

  Two laws P, Q on a finite space and two parameter values θ_P, θ_Q at
  distance δ. For ANY estimator T,

      E_P |T − θ_P| + E_Q |T − θ_Q| ≥ δ (1 − TV(P, Q))          (two_point)

  so the worse of the two risks is at least δ (1 − TV)/2 (minimax). The
  proof is three lines of arithmetic on each point of the space: the
  triangle inequality |θ_P − θ_Q| ≤ |T − θ_P| + |T − θ_Q| and the identity
  ∑ min(p, q) = 1 − TV (sum_min).

  Reading for the dark figure: when two reporting mechanisms produce
  recorded-crime laws P and Q that are close in total variation but
  correspond to true rates δ apart, no estimator — however clever — has
  worst-case error below δ(1 − TV)/2. The bound is the quantitative form
  of P1's non-identification: the data can only separate what they
  distinguish.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P1LeCam

variable {Ω : Type*} [Fintype Ω]

/-- A probability law on a finite space. -/
structure Law (Ω : Type*) [Fintype Ω] where
  (p : Ω → ℝ)
  (nonneg : ∀ i, 0 ≤ p i)
  (total : ∑ i, p i = 1)

/-- Total variation distance. -/
def TV (p q : Ω → ℝ) : ℝ := (∑ i, |p i - q i|) / 2

theorem min_eq_half (a b : ℝ) : min a b = (a + b - |a - b|) / 2 := by
  rcases le_total a b with h | h
  · rw [min_eq_left h, abs_of_nonpos (by linarith)]; ring
  · rw [min_eq_right h, abs_of_nonneg (by linarith)]; ring

theorem sum_min (P Q : Law Ω) : ∑ i, min (P.p i) (Q.p i) = 1 - TV P.p Q.p := by
  simp only [min_eq_half]
  rw [← sum_div, sum_sub_distrib, sum_add_distrib, P.total, Q.total]
  unfold TV; ring

theorem tv_nonneg (p q : Ω → ℝ) : 0 ≤ TV p q :=
  div_nonneg (sum_nonneg fun i _ => abs_nonneg _) (by norm_num)

theorem tv_le_one (P Q : Law Ω) : TV P.p Q.p ≤ 1 := by
  unfold TV
  rw [div_le_iff₀ (by norm_num : (0 : ℝ) < 2)]
  calc ∑ i, |P.p i - Q.p i| ≤ ∑ i, (P.p i + Q.p i) := by
        apply sum_le_sum; intro i _
        have := P.nonneg i; have := Q.nonneg i
        rw [abs_sub_le_iff]; constructor <;> linarith
    _ = 1 * 2 := by rw [sum_add_distrib, P.total, Q.total]; ring

/-- Le Cam's two-point inequality. -/
theorem two_point (P Q : Law Ω) (T : Ω → ℝ) (θP θQ : ℝ) :
    |θP - θQ| * (1 - TV P.p Q.p) ≤ ∑ i, P.p i * |T i - θP| + ∑ i, Q.p i * |T i - θQ| := by
  rw [← sum_min P Q, mul_sum, ← sum_add_distrib]
  apply sum_le_sum; intro i _
  have hm1 : min (P.p i) (Q.p i) ≤ P.p i := min_le_left _ _
  have hm2 : min (P.p i) (Q.p i) ≤ Q.p i := min_le_right _ _
  have hm0 : 0 ≤ min (P.p i) (Q.p i) := le_min (P.nonneg i) (Q.nonneg i)
  have htri : |θP - θQ| ≤ |T i - θP| + |T i - θQ| := by
    have := abs_sub_le θP (T i) θQ
    rwa [abs_sub_comm θP (T i)] at this
  have ha := abs_nonneg (T i - θP)
  have hb := abs_nonneg (T i - θQ)
  have e1 := mul_nonneg (sub_nonneg.mpr hm1) ha
  have e2 := mul_nonneg (sub_nonneg.mpr hm2) hb
  nlinarith [mul_le_mul_of_nonneg_right htri hm0]

/-- The minimax form: the worse risk is at least `δ (1 − TV)/2`. -/
theorem minimax (P Q : Law Ω) (T : Ω → ℝ) (θP θQ : ℝ) :
    |θP - θQ| * (1 - TV P.p Q.p) / 2 ≤ max (∑ i, P.p i * |T i - θP|) (∑ i, Q.p i * |T i - θQ|) := by
  have h := two_point P Q T θP θQ
  have h1 := le_max_left (∑ i, P.p i * |T i - θP|) (∑ i, Q.p i * |T i - θQ|)
  have h2 := le_max_right (∑ i, P.p i * |T i - θP|) (∑ i, Q.p i * |T i - θQ|)
  linarith

end Research.P1LeCam
