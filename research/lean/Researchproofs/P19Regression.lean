/-
  Researchproofs/P19Regression.lean

  P19 (new): regression to the mean at selected hot spots — crime falls at
  the places chosen for being high, with no treatment at all (Galton 1886;
  Campbell & Stanley 1963; the hot-spots evaluation literature since
  Sherman & Weisburd 1995 uses randomisation to escape it).

  Each place has two period counts x₁, x₂. A hot-spot programme selects the
  places with x₁ > c and reports the change x₂ − x₁ on them. On a finite
  weighted population whose two periods are exchangeable — there is a
  weight-preserving bijection σ with x₁(σ i) = x₂ i and x₂(σ i) = x₁ i — the
  selected places' mean change is non-positive:

      ∑_{x₁ > c} w (x₂ − x₁) ≤ 0                         (selected_change_nonpos)

  because the selected mass under x₂ equals the selected mass under x₁
  (exchange_mass), the cross term reindexes (exchange_cross), and the
  indicator difference times x₁ dominates the same difference times c
  (indicator_bound). The quantity reported by a before/after comparison on
  selected hot spots is therefore negative under the null of no change;
  it measures selection, and a treatment effect is identified only
  against a control group selected the same way (why the trials randomise
  within the selected set).

  Not proved: the size of the fall — that depends on the noise
  distribution, which the data estimate.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P19

/-- Places with weights and two period counts, with an exchange `σ` swapping the periods. -/
structure Places (Ω : Type*) [Fintype Ω] where
  (w : Ω → ℝ)
  (w_nonneg : ∀ i, 0 ≤ w i)
  (x₁ x₂ : Ω → ℝ)
  (σ : Equiv.Perm Ω)
  (w_σ : ∀ i, w (σ i) = w i)
  (x₁_σ : ∀ i, x₁ (σ i) = x₂ i)
  (x₂_σ : ∀ i, x₂ (σ i) = x₁ i)

variable {Ω : Type*} [Fintype Ω] (P : Places Ω) (c : ℝ)

/-- Indicator of selection on the first period. -/
def sel₁ (i : Ω) : ℝ := if c < P.x₁ i then 1 else 0
def sel₂ (i : Ω) : ℝ := if c < P.x₂ i then 1 else 0

/-- Reindexing along `σ`: a sum of any function of `(w, x₁, x₂)` equals the sum with the periods swapped. -/
theorem reindex (f : ℝ → ℝ → ℝ → ℝ) :
    ∑ i, f (P.w i) (P.x₁ i) (P.x₂ i) = ∑ i, f (P.w i) (P.x₂ i) (P.x₁ i) := by
  rw [← Equiv.sum_comp P.σ (fun i => f (P.w i) (P.x₁ i) (P.x₂ i))]
  apply sum_congr rfl; intro i _
  rw [P.w_σ, P.x₁_σ, P.x₂_σ]

/-- The selected mass is the same whichever period selects. -/
theorem exchange_mass : ∑ i, P.w i * sel₁ P c i = ∑ i, P.w i * sel₂ P c i := by
  have := reindex P (fun w a b => w * (if c < a then 1 else 0))
  simpa [sel₁, sel₂] using this

/-- The cross term reindexes: `∑ w x₂ 1{x₁ > c} = ∑ w x₁ 1{x₂ > c}`. -/
theorem exchange_cross : ∑ i, P.w i * (P.x₂ i * sel₁ P c i) = ∑ i, P.w i * (P.x₁ i * sel₂ P c i) := by
  have := reindex P (fun w a b => w * (b * (if c < a then 1 else 0)))
  simpa [sel₁, sel₂] using this

/-- `x₁ (1{x₁>c} − 1{x₂>c}) ≥ c (1{x₁>c} − 1{x₂>c})` pointwise. -/
theorem indicator_bound (i : Ω) :
    c * (sel₁ P c i - sel₂ P c i) ≤ P.x₁ i * (sel₁ P c i - sel₂ P c i) := by
  unfold sel₁ sel₂
  by_cases h1 : c < P.x₁ i <;> by_cases h2 : c < P.x₂ i <;> simp [h1, h2] <;> linarith

/-- Under exchangeable periods the selected places' mean change is non-positive. -/
theorem selected_change_nonpos : ∑ i, P.w i * ((P.x₂ i - P.x₁ i) * sel₁ P c i) ≤ 0 := by
  have hcross := exchange_cross P c
  have hmass := exchange_mass P c
  have hpt : ∀ i, P.w i * (c * (sel₁ P c i - sel₂ P c i)) ≤ P.w i * (P.x₁ i * (sel₁ P c i - sel₂ P c i)) :=
    fun i => mul_le_mul_of_nonneg_left (indicator_bound P c i) (P.w_nonneg i)
  have hsum := sum_le_sum (fun i (_ : i ∈ univ) => hpt i)
  -- ∑ w x₁ sel₁ − ∑ w x₁ sel₂ ≥ c (∑ w sel₁ − ∑ w sel₂) = 0
  have e1 : ∑ i, P.w i * (c * (sel₁ P c i - sel₂ P c i)) = c * (∑ i, P.w i * sel₁ P c i - ∑ i, P.w i * sel₂ P c i) := by
    rw [← sum_sub_distrib, mul_sum]; apply sum_congr rfl; intro i _; ring
  have e2 : ∑ i, P.w i * (P.x₁ i * (sel₁ P c i - sel₂ P c i)) = ∑ i, P.w i * (P.x₁ i * sel₁ P c i) - ∑ i, P.w i * (P.x₁ i * sel₂ P c i) := by
    rw [← sum_sub_distrib]; apply sum_congr rfl; intro i _; ring
  have e3 : ∑ i, P.w i * ((P.x₂ i - P.x₁ i) * sel₁ P c i) = ∑ i, P.w i * (P.x₂ i * sel₁ P c i) - ∑ i, P.w i * (P.x₁ i * sel₁ P c i) := by
    rw [← sum_sub_distrib]; apply sum_congr rfl; intro i _; ring
  rw [e1, hmass, sub_self, mul_zero] at hsum
  rw [e2] at hsum
  rw [e3, hcross]
  linarith

/-- The mirror statement for places selected for being low: their change is non-negative. -/
theorem low_selected_change_nonneg :
    0 ≤ ∑ i, P.w i * ((P.x₂ i - P.x₁ i) * (if P.x₁ i < c then 1 else 0)) := by
  -- apply the theorem to the negated counts
  let Q : Places Ω := ⟨P.w, P.w_nonneg, fun i => -P.x₁ i, fun i => -P.x₂ i, P.σ, P.w_σ,
    fun i => by simp [P.x₁_σ], fun i => by simp [P.x₂_σ]⟩
  have h := selected_change_nonpos Q (-c)
  have e : ∑ i, Q.w i * ((Q.x₂ i - Q.x₁ i) * sel₁ Q (-c) i) =
      -∑ i, P.w i * ((P.x₂ i - P.x₁ i) * (if P.x₁ i < c then 1 else 0)) := by
    rw [← sum_neg_distrib]; apply sum_congr rfl; intro i _
    simp only [sel₁, Q]
    have : (-c < -P.x₁ i) ↔ (P.x₁ i < c) := neg_lt_neg_iff
    simp only [this]; ring
  linarith

end Research.P19
