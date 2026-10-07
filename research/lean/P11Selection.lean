/-
  Researchproofs/P11Selection.lean

  P11, continued: monotone treatment selection (Manski & Pepper 2000;
  Manski, Identification for Prediction and Decision, §9.3).

  Judges do not sentence at random: whoever received the harsher sentence
  b may differ in latent risk from whoever received a. Monotone treatment
  selection (MTS) states the direction of that difference: for each
  potential outcome y(t), its mean among the people sentenced to b is at
  least its mean among the people sentenced to a (the harsher group is the
  riskier group under every sentence). On a finite weighted population with
  binary outcomes and two sentences:

    * mts_mean_b_le: E[y(b)] ≤ E[y | z=b]   (the observed mean of the b group)
    * mts_mean_a_ge: E[y(a)] ≥ E[y | z=a]
    * mts_ate_le_naive: E[y(b)] − E[y(a)] ≤ E[y | z=b] − E[y | z=a]: the
      naive comparison OVERSTATES the effect of the harsher sentence; MTS
      alone gives an upper bound, never a sign.
    * mts_lower bounds: E[y(b)] ≥ P(z=b) E[y|z=b], E[y(a)] ≤ P(z=a) E[y|z=a] + P(z=b),
      so the contrast is at least P(z=b) E[y|z=b] − P(z=a) E[y|z=a] − P(z=b).
    * mtr_mts_bounds: with monotone treatment response as well, the contrast
      lies in [0, naive difference]: a sign from MTR, an upper bound from MTS
      (Manski & Pepper's joint assumption).

  Builds on P11Bounds (the population) and P11Monotone (Consistent, MTR).
-/
import Mathlib
import Researchproofs.P11Monotone

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P11.Pop

variable {Ω : Type*} [Fintype Ω] (P : Pop Ω)

/-- Mass of the group sentenced to `b` (`z = true`) and to `a`. -/
def massB : ℝ := ∑ i, if P.z i = true then P.w i else 0
def massA : ℝ := ∑ i, if P.z i = false then P.w i else 0
/-- Weighted total of `f` over a sentence group. -/
def groupSum (f : Ω → ℝ) (b : Bool) : ℝ := ∑ i, if P.z i = b then P.w i * f i else 0
/-- Conditional mean of `f` in a sentence group. -/
def cmean (f : Ω → ℝ) (b : Bool) : ℝ := groupSum P f b / (if b then massB P else massA P)

/-- Monotone treatment selection: under each sentence, the `b` group's mean is at least the `a` group's. -/
def MTS (ya yb : Ω → ℝ) : Prop := cmean P ya false ≤ cmean P ya true ∧ cmean P yb false ≤ cmean P yb true

theorem mean_split (f : Ω → ℝ) : P.mean f = groupSum P f true + groupSum P f false := by
  unfold mean groupSum; rw [← sum_add_distrib]; apply sum_congr rfl; intro i _
  cases P.z i <;> simp

theorem groupSum_true (f : Ω → ℝ) (hB : 0 < massB P) : groupSum P f true = massB P * cmean P f true := by
  unfold cmean; simp only [ite_true]; field_simp

theorem groupSum_false (f : Ω → ℝ) (hA : 0 < massA P) : groupSum P f false = massA P * cmean P f false := by
  unfold cmean; simp only [Bool.false_eq_true, ite_false]; field_simp

theorem mass_total : massB P + massA P = 1 := by
  unfold massB massA; rw [← sum_add_distrib, ← P.w_total]; apply sum_congr rfl; intro i _
  cases P.z i <;> simp

/-- Consistency pins the observed group mean: `cmean yb true = cmean y true`, `cmean ya false = cmean y false`. -/
theorem cmean_consistent (ya yb : Ω → ℝ) (hc : P.Consistent ya yb) :
    cmean P yb true = cmean P P.y true ∧ cmean P ya false = cmean P P.y false := by
  constructor
  · unfold cmean groupSum; congr 1; apply sum_congr rfl; intro i _
    by_cases h : P.z i = true
    · rw [if_pos h, if_pos h, hc.2 i h]
    · rw [if_neg h, if_neg h]
  · unfold cmean groupSum; congr 1; apply sum_congr rfl; intro i _
    by_cases h : P.z i = false
    · rw [if_pos h, if_pos h, hc.1 i h]
    · rw [if_neg h, if_neg h]

section Bounds

variable (ya yb : Ω → ℝ) (hc : P.Consistent ya yb) (hmts : MTS P ya yb)
  (hA : 0 < massA P) (hB : 0 < massB P)
include hc hmts hA hB

/-- `E[y(b)] ≤ E[y | z = b]`. -/
theorem mts_mean_b_le : P.mean yb ≤ cmean P P.y true := by
  have hsplit := mean_split P yb
  have hb := groupSum_true P yb hB
  have ha := groupSum_false P yb hA
  have hcons := (cmean_consistent P ya yb hc).1
  have hle : cmean P yb false ≤ cmean P yb true := hmts.2
  have hprod : massA P * cmean P yb false ≤ massA P * cmean P yb true :=
    mul_le_mul_of_nonneg_left hle hA.le
  have hone : massB P * cmean P yb true + massA P * cmean P yb true = cmean P yb true := by
    rw [← add_mul, mass_total, one_mul]
  rw [hsplit, hb, ha, ← hcons]
  linarith

/-- `E[y(a)] ≥ E[y | z = a]`. -/
theorem mts_mean_a_ge : cmean P P.y false ≤ P.mean ya := by
  have hsplit := mean_split P ya
  have hb := groupSum_true P ya hB
  have ha := groupSum_false P ya hA
  have hcons := (cmean_consistent P ya yb hc).2
  have hle : cmean P ya false ≤ cmean P ya true := hmts.1
  have hprod : massB P * cmean P ya false ≤ massB P * cmean P ya true :=
    mul_le_mul_of_nonneg_left hle hB.le
  have hone : massB P * cmean P ya false + massA P * cmean P ya false = cmean P ya false := by
    rw [← add_mul, mass_total, one_mul]
  rw [hsplit, hb, ha, ← hcons]
  linarith

/-- Under MTS the naive comparison overstates the effect of the harsher sentence. -/
theorem mts_ate_le_naive : P.mean yb - P.mean ya ≤ cmean P P.y true - cmean P P.y false := by
  have h1 := mts_mean_b_le P ya yb hc hmts hA hB
  have h2 := mts_mean_a_ge P ya yb hc hmts hA hB
  linarith

/-- MTR and MTS together: the contrast lies in `[0, naive difference]`. -/
theorem mtr_mts_bounds (hmtr : MTR ya yb) :
    0 ≤ P.mean yb - P.mean ya ∧ P.mean yb - P.mean ya ≤ cmean P P.y true - cmean P P.y false :=
  ⟨mtr_lower P ya yb hmtr, mts_ate_le_naive P ya yb hc hmts hA hB⟩

end Bounds

end Research.P11.Pop
