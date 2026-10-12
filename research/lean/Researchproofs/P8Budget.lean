/-
  Researchproofs/P8Budget.lean

  P8, continued: stick versus carrot under a fixed budget has an interior
  optimum (library scan A19; D'Orsogna & Perc p. 11; Berenji et al.).

  A budget C is split into punishment spend C − θ and rehabilitation θ. With a
  concave rehabilitation response g and a concave deterrence response d, the
  objective f(θ) = g(θ) + d(C − θ) is concave on [0, C] (`concave_sum`); it
  attains a maximum (`exists_max`); and when some interior θ beats both
  endpoints the maximiser is interior (`interior_optimum`). When the deterrence
  response is bounded by B and the rehabilitation gain at some θ₁ exceeds B,
  the all-stick corner is beaten (`rehab_beats_bound`): a bounded deterrence
  response cannot make the all-punishment corner optimal against a
  rehabilitation response that keeps growing.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Set

namespace Research.P8.Budget

def objective (g d : ℝ → ℝ) (C θ : ℝ) : ℝ := g θ + d (C - θ)

theorem concave_sum (g d : ℝ → ℝ) (C : ℝ) (hC : 0 ≤ C)
    (hg : ConcaveOn ℝ (Icc 0 C) g) (hd : ConcaveOn ℝ (Icc 0 C) d) :
    ConcaveOn ℝ (Icc 0 C) (objective g d C) := by
  refine ⟨convex_Icc 0 C, ?_⟩
  intro x hx y hy a b ha hb hab
  unfold objective
  have hx' : C - x ∈ Icc 0 C := ⟨by linarith [hx.2], by linarith [hx.1]⟩
  have hy' : C - y ∈ Icc 0 C := ⟨by linarith [hy.2], by linarith [hy.1]⟩
  have h1 := hg.2 hx hy ha hb hab
  have h2 := hd.2 hx' hy' ha hb hab
  simp only [smul_eq_mul] at h1 h2 ⊢
  have : C - (a * x + b * y) = a * (C - x) + b * (C - y) := by
    have : a * C + b * C = C := by rw [← add_mul, hab, one_mul]
    linarith
  rw [this]
  linarith

theorem exists_max (g d : ℝ → ℝ) (C : ℝ) (hC : 0 ≤ C) (hf : ContinuousOn (objective g d C) (Icc 0 C)) :
    ∃ θ ∈ Icc 0 C, ∀ θ' ∈ Icc 0 C, objective g d C θ' ≤ objective g d C θ := by
  obtain ⟨θ, hθ, hmax⟩ := isCompact_Icc.exists_isMaxOn (nonempty_Icc.mpr hC) hf
  exact ⟨θ, hθ, fun θ' hθ' => hmax hθ'⟩

theorem interior_optimum (g d : ℝ → ℝ) (C θ : ℝ) (hθ : θ ∈ Icc 0 C)
    (hmax : ∀ θ' ∈ Icc 0 C, objective g d C θ' ≤ objective g d C θ)
    (θ₁ θ₂ : ℝ) (h₁ : θ₁ ∈ Icc 0 C) (h₂ : θ₂ ∈ Icc 0 C)
    (hbeat0 : objective g d C 0 < objective g d C θ₁) (hbeatC : objective g d C C < objective g d C θ₂) :
    0 < θ ∧ θ < C := by
  constructor
  · rcases hθ.1.lt_or_eq with h | h
    · exact h
    · exfalso; rw [← h] at hmax; linarith [hmax θ₁ h₁]
  · rcases hθ.2.lt_or_eq with h | h
    · exact h
    · exfalso; rw [h] at hmax; linarith [hmax θ₂ h₂]

/-- A bounded deterrence response (range within `B`) loses to a rehabilitation gain above `B`. -/
theorem rehab_beats_bound (g d : ℝ → ℝ) (C θ₁ B : ℝ)
    (hdB : ∀ s t, s ∈ Icc 0 C → t ∈ Icc 0 C → d s - d t ≤ B)
    (h₁ : θ₁ ∈ Icc 0 C) (hC : C ∈ Icc 0 C) (hgain : B < g θ₁ - g 0) :
    objective g d C 0 < objective g d C θ₁ := by
  unfold objective
  have := hdB (C - 0) (C - θ₁) (by simpa using hC) ⟨by linarith [h₁.2], by linarith [h₁.1]⟩
  linarith

end Research.P8.Budget
