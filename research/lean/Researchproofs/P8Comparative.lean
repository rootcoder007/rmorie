/-
  Researchproofs/P8Comparative.lean

  P8, continued: the direction of deterrence needs no convexity.

  Cooter & Ulen (ch. 12) derive "more certainty, less crime" from a
  first-order condition on a concave benefit y and a convex expected
  penalty p·f, and the library scan flagged the derivation as needing
  those shapes. It does not. If the offender picks an offending level x
  from ANY set to maximise y(x) − p·f(x) with f strictly increasing, then
  any optimum at a higher certainty p₂ is at most any optimum at a lower
  certainty p₁ (Topkis: the objective has decreasing differences in
  (x, p)). No concavity, no differentiability, no interior solution.
  The same argument gives the severity direction (severity_monotone).

  What convexity buys is uniqueness and smoothness of x*(p), not its
  direction. Aggregate crime over heterogeneous offenders is then a sum
  of non-increasing functions, hence non-increasing — the identification
  problem of P8Deterrence is about separating p from f in data, not
  about the sign.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

namespace Research.P8

/-- `x` is optimal at certainty `p` for benefit `y`, sanction `f`, over a choice set `S`. -/
def Optimal (S : Set ℝ) (y f : ℝ → ℝ) (p x : ℝ) : Prop :=
  x ∈ S ∧ ∀ x' ∈ S, y x' - p * f x' ≤ y x - p * f x

/-- Certainty: a higher `p` never raises the optimal offending level. -/
theorem certainty_monotone (S : Set ℝ) (y f : ℝ → ℝ) (hf : StrictMono f)
    (p₁ p₂ x₁ x₂ : ℝ) (hp : p₁ < p₂) (h₁ : Optimal S y f p₁ x₁) (h₂ : Optimal S y f p₂ x₂) :
    x₂ ≤ x₁ := by
  have a := h₁.2 x₂ h₂.1   -- y x₂ − p₁ f x₂ ≤ y x₁ − p₁ f x₁
  have b := h₂.2 x₁ h₁.1   -- y x₁ − p₂ f x₁ ≤ y x₂ − p₂ f x₂
  have key : (p₂ - p₁) * (f x₂ - f x₁) ≤ 0 := by nlinarith
  have hpos : 0 < p₂ - p₁ := by linarith
  have : f x₂ ≤ f x₁ := by nlinarith
  exact hf.le_iff_le.1 this

/-- Severity: scaling the sanction up (`f₂ ≥ f₁` pointwise with a larger increment at larger `x`,
i.e. `f₂ − f₁` non-decreasing) never raises the optimum either. -/
theorem severity_monotone (S : Set ℝ) (y f₁ f₂ : ℝ → ℝ) (p : ℝ) (hp : 0 < p)
    (hdiff : StrictMono (fun x => f₂ x - f₁ x))
    (x₁ x₂ : ℝ) (h₁ : Optimal S y f₁ p x₁) (h₂ : Optimal S y f₂ p x₂) :
    x₂ ≤ x₁ := by
  have a := h₁.2 x₂ h₂.1
  have b := h₂.2 x₁ h₁.1
  have : p * ((f₂ x₂ - f₁ x₂) - (f₂ x₁ - f₁ x₁)) ≤ 0 := by nlinarith
  have : (f₂ x₂ - f₁ x₂) ≤ (f₂ x₁ - f₁ x₁) := by nlinarith
  exact hdiff.le_iff_le.1 this

/-- Aggregate offending over finitely many offenders, each optimal, is non-increasing in `p`. -/
theorem aggregate_monotone {ι : Type*} [Fintype ι] (S : ι → Set ℝ) (y : ι → ℝ → ℝ) (f : ι → ℝ → ℝ)
    (hf : ∀ i, StrictMono (f i)) (p₁ p₂ : ℝ) (hp : p₁ < p₂) (x₁ x₂ : ι → ℝ)
    (h₁ : ∀ i, Optimal (S i) (y i) (f i) p₁ (x₁ i)) (h₂ : ∀ i, Optimal (S i) (y i) (f i) p₂ (x₂ i)) :
    ∑ i, x₂ i ≤ ∑ i, x₁ i :=
  Finset.sum_le_sum (fun i _ => certainty_monotone (S i) (y i) (f i) (hf i) p₁ p₂ _ _ hp (h₁ i) (h₂ i))

end Research.P8
