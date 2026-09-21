/-
  Researchproofs/P1Hierarchy.lean

  P1, continued: the hierarchy rule (Eterno, Verma & Silverman, How
  Countries Count Crime, ch. 15; FBI UCR summary reporting).

  Under the hierarchy rule an incident with several offences is counted
  once, as its most serious offence; an offence-based system (NIBRS,
  Canada's UCR2) counts every offence. With N incidents, k_i ≥ 1 offences
  in incident i and k_i ≤ K:

      N ≤ Σ_i k_i ≤ K · N,          (offence_count_bounds)
      Σ_i k_i = N · (1 + mean extra offences per incident),   (offence_count_eq)

  and the per-category split of the offence count is not a function of
  the incident count (category_not_identified: two incident tables with
  the same hierarchy counts and different offence counts). A jurisdiction
  that switches from incident to offence counting will show a rise in
  recorded crime with no change in crime.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P1

variable {ι : Type*} [Fintype ι]

/-- With `k i ≥ 1` offences per incident, bounded by `K`. -/
theorem offence_count_bounds (k : ι → ℕ) (K : ℕ) (h1 : ∀ i, 1 ≤ k i) (hK : ∀ i, k i ≤ K) :
    Fintype.card ι ≤ ∑ i, k i ∧ ∑ i, k i ≤ K * Fintype.card ι := by
  constructor
  · calc Fintype.card ι = ∑ _i : ι, 1 := by simp
      _ ≤ ∑ i, k i := sum_le_sum (fun i _ => h1 i)
  · calc ∑ i, k i ≤ ∑ _i : ι, K := sum_le_sum (fun i _ => hK i)
      _ = K * Fintype.card ι := by simp [mul_comm]

/-- Offence count = incidents × (1 + mean extra offences). -/
theorem offence_count_eq (k : ι → ℕ) (h1 : ∀ i, 1 ≤ k i) (hne : 0 < Fintype.card ι) :
    (∑ i, (k i : ℝ)) = (Fintype.card ι : ℝ) * (1 + (∑ i, ((k i : ℝ) - 1)) / (Fintype.card ι : ℝ)) := by
  have hN : (Fintype.card ι : ℝ) ≠ 0 := by positivity
  rw [sum_sub_distrib, sum_const, card_univ, nsmul_eq_mul, mul_one]
  field_simp
  ring

/-- Two incident tables with identical hierarchy (incident) counts and different offence counts:
the offence total is not a function of the incident total. -/
theorem category_not_identified :
    ∃ (k₁ k₂ : Fin 2 → ℕ), (∀ i, 1 ≤ k₁ i) ∧ (∀ i, 1 ≤ k₂ i) ∧ ∑ i, k₁ i ≠ ∑ i, k₂ i := by
  refine ⟨fun _ => 1, fun _ => 3, fun _ => le_rfl, fun _ => by norm_num, ?_⟩
  simp [Fin.sum_univ_two]

end Research.P1
