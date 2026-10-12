/-
  Researchproofs/P3Ring.lean

  P3, continued: the ring mis-specification bound. The Horvitz-Thompson estimator of
  P3Spillover is design-unbiased for the estimand written with the d-ring exposure map.
  If the true outcome also moves when the (d+1)-ring is treated, by an amount γ_i for
  unit i, the d-ring estimand differs from the true one by the average of γ_i over the
  units whose outer ring is exposed. In expectation over the design that is
  (1/N) Σ_i γ_i π_i with π_i the (d+1)-ring exposure share of unit i, and in absolute value
  at most G · (1/N) Σ_i π_i when every |γ_i| ≤ G. The bias of the d-ring analysis is
  therefore bounded by the outer ring's exposure share, which the design computes
  (morie_spillover_exposure_probs with d+1), times the largest outer-ring effect.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false
set_option linter.unusedSectionVars false

noncomputable section

namespace Research.P3.Ring

variable {n Z : Type*} [Fintype n] [Fintype Z]

/-- The outer-ring indicator as a real number. -/
def ind (e : Z → n → Prop) [∀ z i, Decidable (e z i)] (z : Z) (i : n) : ℝ :=
  if e z i then 1 else 0

/-- The d-ring estimand at assignment z: the mean of the d-ring outcomes. -/
def tauD (yD : Z → n → ℝ) (z : Z) : ℝ := (∑ i, yD z i) / Fintype.card n

/-- The true estimand: the d-ring outcome plus γ_i when the outer ring of i is exposed. -/
def tauTrue (yD : Z → n → ℝ) (γ : n → ℝ) (e : Z → n → Prop) [∀ z i, Decidable (e z i)] (z : Z) : ℝ :=
  (∑ i, (yD z i + γ i * ind e z i)) / Fintype.card n

/-- The outer-ring exposure share of unit i under the design p. -/
def share (p : Z → ℝ) (e : Z → n → Prop) [∀ z i, Decidable (e z i)] (i : n) : ℝ :=
  ∑ z, p z * ind e z i

section
variable (yD : Z → n → ℝ) (γ : n → ℝ) (e : Z → n → Prop) [∀ z i, Decidable (e z i)]

theorem ind_nonneg (z : Z) (i : n) : 0 ≤ ind e z i := by
  unfold ind; split_ifs <;> norm_num

theorem ind_le_one (z : Z) (i : n) : ind e z i ≤ 1 := by
  unfold ind; split_ifs <;> norm_num

/-- At every assignment the two estimands differ by the mean of γ over the units whose outer ring is exposed. -/
theorem gap_eq (z : Z) :
    tauTrue yD γ e z - tauD yD z = (∑ i, γ i * ind e z i) / Fintype.card n := by
  unfold tauTrue tauD
  rw [Finset.sum_add_distrib, add_div, add_sub_cancel_left]

/-- With no outer-ring effect the estimands coincide. -/
theorem gap_zero (hγ : ∀ i, γ i = 0) (z : Z) : tauTrue yD γ e z = tauD yD z := by
  have := gap_eq yD γ e z
  simp [hγ] at this
  linarith

/-- In expectation over a design p (weights summing to one) the gap is the γ-weighted average of the
outer-ring exposure shares. -/
theorem expected_gap_eq (p : Z → ℝ) :
    ∑ z, p z * (tauTrue yD γ e z - tauD yD z) = (∑ i, γ i * share p e i) / Fintype.card n := by
  simp_rw [gap_eq]
  have h : ∀ z, p z * ((∑ i, γ i * ind e z i) / Fintype.card n) =
      (∑ i, γ i * (p z * ind e z i)) / Fintype.card n := by
    intro z
    rw [mul_div_assoc', Finset.mul_sum]
    congr 1
    exact Finset.sum_congr rfl fun i _ => by ring
  simp_rw [h]
  rw [← Finset.sum_div, Finset.sum_comm]
  congr 1
  refine Finset.sum_congr rfl fun i _ => ?_
  rw [share, Finset.mul_sum]

/-- The ring mis-specification bound: |E gap| ≤ G · (mean outer-ring exposure share) when |γ_i| ≤ G. -/
theorem expected_gap_le (p : Z → ℝ) (hp : ∀ z, 0 ≤ p z) (G : ℝ) (hG : ∀ i, |γ i| ≤ G) :
    |∑ z, p z * (tauTrue yD γ e z - tauD yD z)| ≤ G * ((∑ i, share p e i) / Fintype.card n) := by
  rw [expected_gap_eq]
  have hshare : ∀ i, 0 ≤ share p e i := fun i =>
    Finset.sum_nonneg fun z _ => mul_nonneg (hp z) (ind_nonneg e z i)
  have hN : (0 : ℝ) ≤ Fintype.card n := Nat.cast_nonneg _
  rw [abs_div, abs_of_nonneg hN, mul_div_assoc']
  apply div_le_div_of_nonneg_right _ hN
  calc |∑ i, γ i * share p e i| ≤ ∑ i, |γ i * share p e i| := Finset.abs_sum_le_sum_abs _ _
    _ = ∑ i, |γ i| * share p e i := by
        refine Finset.sum_congr rfl fun i _ => ?_
        rw [abs_mul, abs_of_nonneg (hshare i)]
    _ ≤ ∑ i, G * share p e i := Finset.sum_le_sum fun i _ => mul_le_mul_of_nonneg_right (hG i) (hshare i)
    _ = G * ∑ i, share p e i := by rw [Finset.mul_sum]

end

end Research.P3.Ring
