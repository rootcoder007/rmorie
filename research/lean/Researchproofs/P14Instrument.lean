/-
  Researchproofs/P14Instrument.lean

  P14 (new): judge-leniency designs — what an instrument identifies.

  Pretrial detention, incarceration and sentence length are studied with
  the leniency of a randomly assigned judge as an instrument (Kling 2006;
  Dobbie, Goldin & Yang 2018; Aizer & Doyle 2015). The estimand the Wald
  ratio delivers is a local average treatment effect (Imbens & Angrist
  1994; Angrist, Imbens & Rubin 1996). On a finite weighted population with
  a binary instrument z, potential treatment d(z) and potential outcome
  y(d), each person is one of four types: complier (d(0)=0, d(1)=1),
  always-taker, never-taker, defier (d(0)=1, d(1)=0).

    * itt_decomposition: E[y(d(1))] − E[y(d(0))] = ∑_compliers w (y(1) − y(0))
      − ∑_defiers w (y(1) − y(0)); always- and never-takers contribute nothing.
    * first_stage_decomposition: E[d(1)] − E[d(0)] = P(complier) − P(defier).
    * late_identification: with no defiers and a positive first stage, the
      Wald ratio equals the mean treatment effect among compliers — and only
      among them.
    * wald_with_defiers: without monotonicity the ratio is a weighted
      difference of the complier and defier effects over P(c) − P(d), so its
      sign can be wrong even when every individual effect is positive
      (defiers_can_flip gives a witness).

  What this says about a judge design: a "harsh" judge who detains some
  defendants a lenient one would release (compliers) and releases some a
  lenient one would detain (defiers) does not identify an average effect
  with a known sign; the monotonicity assumption is what the reader must
  judge, and the estimand is the compliers' effect, not the population's.
  Independence (random assignment) is what makes the population quantities
  on the left the ones the data estimate; the identities here are exact on
  the population.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P14

/-- A finite weighted population with potential treatments `d z` and potential outcomes `y t`. -/
structure Pop (Ω : Type*) [Fintype Ω] where
  (w : Ω → ℝ)
  (w_nonneg : ∀ i, 0 ≤ w i)
  (w_total : ∑ i, w i = 1)
  (d : Bool → Ω → Bool)
  (y : Bool → Ω → ℝ)

variable {Ω : Type*} [Fintype Ω] (P : Pop Ω)

def Pop.complier (i : Ω) : Prop := P.d false i = false ∧ P.d true i = true
def Pop.defier (i : Ω) : Prop := P.d false i = true ∧ P.d true i = false
def Pop.alwaysTaker (i : Ω) : Prop := P.d false i = true ∧ P.d true i = true
def Pop.neverTaker (i : Ω) : Prop := P.d false i = false ∧ P.d true i = false

/-- The outcome a person shows when the instrument is `z`. -/
def Pop.yz (z : Bool) (i : Ω) : ℝ := P.y (P.d z i) i
def Pop.mean (f : Ω → ℝ) : ℝ := ∑ i, P.w i * f i
/-- Individual treatment effect `y(1) − y(0)`. -/
def Pop.effect (i : Ω) : ℝ := P.y true i - P.y false i
/-- Mass and effect-weighted mass of a type. -/
def Pop.mass (T : Ω → Prop) [DecidablePred T] : ℝ := ∑ i, if T i then P.w i else 0
def Pop.effectMass (T : Ω → Prop) [DecidablePred T] : ℝ := ∑ i, if T i then P.w i * P.effect i else 0

open Classical in
/-- Intention-to-treat on the outcome: compliers add their effect, defiers subtract it. -/
theorem itt_decomposition :
    P.mean (P.yz true) - P.mean (P.yz false) = P.effectMass P.complier - P.effectMass P.defier := by
  unfold Pop.mean Pop.effectMass
  rw [← sum_sub_distrib, ← sum_sub_distrib]
  apply sum_congr rfl; intro i _
  unfold Pop.yz Pop.complier Pop.defier Pop.effect
  rcases Bool.eq_false_or_eq_true (P.d false i) with h0 | h0 <;>
    rcases Bool.eq_false_or_eq_true (P.d true i) with h1 | h1 <;> simp [h0, h1] <;> ring

open Classical in
/-- First stage: `E[d(1)] − E[d(0)] = P(complier) − P(defier)`. -/
theorem first_stage_decomposition :
    P.mean (fun i => if P.d true i then 1 else 0) - P.mean (fun i => if P.d false i then 1 else 0) =
      P.mass P.complier - P.mass P.defier := by
  unfold Pop.mean Pop.mass
  rw [← sum_sub_distrib, ← sum_sub_distrib]
  apply sum_congr rfl; intro i _
  unfold Pop.complier Pop.defier
  rcases Bool.eq_false_or_eq_true (P.d false i) with h0 | h0 <;>
    rcases Bool.eq_false_or_eq_true (P.d true i) with h1 | h1 <;> simp [h0, h1]

open Classical in
/-- The Wald ratio. -/
def Pop.wald : ℝ :=
  (P.mean (P.yz true) - P.mean (P.yz false)) /
    (P.mean (fun i => if P.d true i then 1 else 0) - P.mean (fun i => if P.d false i then 1 else 0))

open Classical in
/-- Mean effect among a type with positive mass. -/
def Pop.typeEffect (T : Ω → Prop) [DecidablePred T] : ℝ := P.effectMass T / P.mass T

open Classical in
theorem effectMass_eq (T : Ω → Prop) [DecidablePred T] (h : 0 < P.mass T) :
    P.effectMass T = P.mass T * P.typeEffect T := by
  unfold Pop.typeEffect; field_simp

open Classical in
/-- Monotonicity (no defiers) and a positive complier mass: the Wald ratio is the compliers' mean effect. -/
theorem late_identification (hmono : ∀ i, ¬ P.defier i) (_hc : 0 < P.mass P.complier) :
    P.wald = P.typeEffect P.complier := by
  have hd : P.mass P.defier = 0 := by
    unfold Pop.mass; apply sum_eq_zero; intro i _; simp [hmono i]
  have hde : P.effectMass P.defier = 0 := by
    unfold Pop.effectMass; apply sum_eq_zero; intro i _; simp [hmono i]
  unfold Pop.wald
  rw [itt_decomposition, first_stage_decomposition, hd, hde, sub_zero, sub_zero]
  unfold Pop.typeEffect
  rfl

open Classical in
/-- Without monotonicity: a weighted difference of the two types' effects. -/
theorem wald_with_defiers (hc : 0 < P.mass P.complier) (hdf : 0 < P.mass P.defier)
    (hne : P.mass P.complier ≠ P.mass P.defier) :
    P.wald = (P.mass P.complier * P.typeEffect P.complier - P.mass P.defier * P.typeEffect P.defier) /
      (P.mass P.complier - P.mass P.defier) := by
  unfold Pop.wald
  rw [itt_decomposition, first_stage_decomposition, effectMass_eq P _ hc, effectMass_eq P _ hdf]

/-- Witness: two people, a complier with effect `1` (mass 0.6) and a defier with effect `4`
    (mass 0.4). Every individual effect is positive, yet the Wald ratio is
    `(0.6·1 − 0.4·4)/(0.6 − 0.4) = −5`. -/
theorem defiers_can_flip :
    ((0.6 : ℝ) * 1 - 0.4 * 4) / (0.6 - 0.4) < 0 ∧ (0 : ℝ) < 1 ∧ (0 : ℝ) < 4 := by
  norm_num

end Research.P14
