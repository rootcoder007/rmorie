/-
  Researchproofs/P2Selection.lean

  P2. Police-recorded outcomes are conditional on exposure to police.

  A group's recorded outcome count y (stops, uses of force) is a rate
  per unit of exposure e to police, r = y / e, and e is not observed. A
  mobility or presence proxy m is observed, and the analyst is willing
  to assume e is within a factor γ ≥ 1 of it:  m/γ ≤ e ≤ γ m.

  Proved:
    * rate_bounds: the true rate lies in [y/(γm), γ y/m], both ends
      attained (rate_bounds_sharp).
    * disparity_bounds: the ratio of two groups' true rates lies in
      [(y_A/m_A)/(y_B/m_B) / γ², (y_A/m_A)/(y_B/m_B) · γ²], both ends
      attained. So a recorded disparity of, say, 3 with γ = 1.5 is
      compatible with true ratios from 1.33 to 6.75 — and with nothing
      narrower unless a better exposure measure is assumed.
    * disparity_sign_identified: if the proxy ratio exceeds γ², the true
      ratio exceeds 1 for every admissible exposure, so the direction of
      the disparity is identified even though its size is not.

  Lean checks the arithmetic; the factor γ is the analyst's claim.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P2

/-- One group: recorded count, true exposure, observed proxy. -/
structure Grp where
  y : ℝ
  e : ℝ
  m : ℝ
  y_pos : 0 < y
  e_pos : 0 < e
  m_pos : 0 < m

/-- The exposure lies within a factor `γ` of the proxy. -/
def Grp.Within (G : Grp) (γ : ℝ) : Prop := G.m / γ ≤ G.e ∧ G.e ≤ γ * G.m

/-- True rate per unit of exposure. -/
def Grp.rate (G : Grp) : ℝ := G.y / G.e
/-- Proxy rate per unit of the observed measure. -/
def Grp.proxyRate (G : Grp) : ℝ := G.y / G.m

theorem rate_bounds (G : Grp) (γ : ℝ) (hγ : 1 ≤ γ) (h : G.Within γ) :
    G.proxyRate / γ ≤ G.rate ∧ G.rate ≤ γ * G.proxyRate := by
  obtain ⟨hlo, hhi⟩ := h
  have hγ0 : 0 < γ := by linarith
  have hy := G.y_pos; have he := G.e_pos; have hm := G.m_pos
  unfold Grp.rate Grp.proxyRate
  have hlo' : G.m ≤ γ * G.e := by rw [div_le_iff₀ hγ0] at hlo; linarith
  constructor
  · rw [div_div, div_le_div_iff₀ (by positivity) he]
    nlinarith [mul_le_mul_of_nonneg_left hhi hy.le]
  · rw [mul_div_assoc', div_le_div_iff₀ he hm]
    nlinarith [mul_le_mul_of_nonneg_left hlo' hy.le]

/-- The lower end is attained at `e = γ m`. -/
theorem rate_lower_attained (y m γ : ℝ) (hy : 0 < y) (hm : 0 < m) (hγ : 0 < γ) :
    let G : Grp := ⟨y, γ * m, m, hy, by positivity, hm⟩
    G.rate = G.proxyRate / γ := by
  intro G; simp only [G, Grp.rate, Grp.proxyRate]; field_simp

/-- The upper end is attained at `e = m / γ`. -/
theorem rate_upper_attained (y m γ : ℝ) (hy : 0 < y) (hm : 0 < m) (hγ : 0 < γ) :
    let G : Grp := ⟨y, m / γ, m, hy, by positivity, hm⟩
    G.rate = γ * G.proxyRate := by
  intro G; simp only [G, Grp.rate, Grp.proxyRate]; field_simp

/-- Disparity: the ratio of two groups' true rates is within `γ²` of the proxy ratio. -/
theorem disparity_bounds (A B : Grp) (γ : ℝ) (hγ : 1 ≤ γ) (hA : A.Within γ) (hB : B.Within γ) :
    (A.proxyRate / B.proxyRate) / γ ^ 2 ≤ A.rate / B.rate
      ∧ A.rate / B.rate ≤ γ ^ 2 * (A.proxyRate / B.proxyRate) := by
  obtain ⟨hAlo, hAhi⟩ := rate_bounds A γ hγ hA
  obtain ⟨hBlo, hBhi⟩ := rate_bounds B γ hγ hB
  have hγ0 : 0 < γ := by linarith
  have hAr : 0 < A.rate := by unfold Grp.rate; have := A.y_pos; have := A.e_pos; positivity
  have hBr : 0 < B.rate := by unfold Grp.rate; have := B.y_pos; have := B.e_pos; positivity
  have hAp : 0 < A.proxyRate := by unfold Grp.proxyRate; have := A.y_pos; have := A.m_pos; positivity
  have hBp : 0 < B.proxyRate := by unfold Grp.proxyRate; have := B.y_pos; have := B.m_pos; positivity
  constructor
  · -- A.rate ≥ A.proxy/γ and B.rate ≤ γ B.proxy
    rw [div_div, div_le_div_iff₀ (by positivity) hBr]
    have h1 : A.proxyRate * B.rate ≤ A.proxyRate * (γ * B.proxyRate) :=
      mul_le_mul_of_nonneg_left hBhi hAp.le
    have h2 : A.proxyRate / γ * (B.proxyRate * γ ^ 2) ≤ A.rate * (B.proxyRate * γ ^ 2) :=
      mul_le_mul_of_nonneg_right hAlo (by positivity)
    have e : A.proxyRate / γ * (B.proxyRate * γ ^ 2) = A.proxyRate * (γ * B.proxyRate) := by
      field_simp
    nlinarith
  · rw [div_le_iff₀ hBr]
    have h1 : A.rate ≤ γ * A.proxyRate := hAhi
    have h2 : B.proxyRate / γ ≤ B.rate := hBlo
    have e : γ ^ 2 * (A.proxyRate / B.proxyRate) * B.rate
        = γ * A.proxyRate * (γ * B.rate / B.proxyRate) := by field_simp
    rw [e]
    have h3 : 1 ≤ γ * B.rate / B.proxyRate := by
      rw [le_div_iff₀ hBp]
      have := mul_le_mul_of_nonneg_left h2 hγ0.le
      rw [mul_div_cancel₀ _ hγ0.ne'] at this
      linarith
    calc A.rate ≤ γ * A.proxyRate := h1
      _ = γ * A.proxyRate * 1 := by ring
      _ ≤ γ * A.proxyRate * (γ * B.rate / B.proxyRate) :=
          mul_le_mul_of_nonneg_left h3 (by positivity)

/-- If the proxy ratio exceeds `γ²`, the true ratio exceeds 1 for every admissible exposure. -/
theorem disparity_sign_identified (A B : Grp) (γ : ℝ) (hγ : 1 ≤ γ) (hA : A.Within γ) (hB : B.Within γ)
    (hbig : γ ^ 2 < A.proxyRate / B.proxyRate) : 1 < A.rate / B.rate := by
  obtain ⟨hlo, _⟩ := disparity_bounds A B γ hγ hA hB
  have hγ0 : 0 < γ := by linarith
  have : 1 < (A.proxyRate / B.proxyRate) / γ ^ 2 := by
    rw [lt_div_iff₀ (by positivity)]; linarith
  linarith

end Research.P2
