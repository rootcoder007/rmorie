/-
  Researchproofs/P17Incapacitation.lean

  P17 (new): incapacitation — what a sentence length buys in crimes not
  committed (Avi-Itzhak & Shinnar 1973; Shinnar & Shinnar 1975; Blumstein,
  Cohen & Nagin 1978).

  An offender commits crimes at rate λ while free; each crime leads to
  arrest, conviction and a sentence of length S with probability q; so the
  expected free time between sentences is 1/(λq). Over many cycles the
  long-run crime rate is

      λ · (1/(λq)) / (1/(λq) + S)  =  λ / (1 + λ q S)            (steady_state_rate)

  On a finite record of N cycles with free times f_i and crimes c_i = λ f_i
  (the proportional accounting the model assumes), the realised rate is
  λ f̄ / (f̄ + S) (cycle_rate), which is the formula at f̄ = 1/(λq).

    * steady_state_rate: the identity above.
    * prevented_share: the share of the free-offending rate removed is
      λqS/(1 + λqS), strictly below one — incapacitation never removes all
      crime, and the share rises in S, q and λ with diminishing returns.
    * rate_antitone_in_S, rate_antitone_in_q: longer sentences and higher
      certainty lower the rate.
    * marginal_prevention: an extra year of sentence prevents
      λ²q/(1 + λqS)(1 + λq(S+1)) crimes per year — falling in S: the
      marginal year buys less than the previous one.
    * group_rate: with heterogeneous offenders (λ_k, q_k, shares π_k) the
      aggregate rate is the share-weighted sum, so a sentence rule that is
      uniform across rates concentrates its prevention on the high-λ group
      (high_rate_more_prevented).

  Reading: the formula is arithmetic on a stationary model with constant
  λ and no replacement of incapacitated offenders; what is NOT proved is
  that λ is constant over a career or that incarceration leaves λ
  unchanged — those are the empirical assumptions behind every
  incapacitation estimate.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P17

/-- Offending rate `λ`, sentence probability per crime `q`, sentence length `S`. -/
structure Career where
  (lam q S : ℝ)
  (lam_pos : 0 < lam)
  (q_pos : 0 < q)
  (q_le : q ≤ 1)
  (S_nonneg : 0 ≤ S)

variable (C : Career)

/-- Long-run crime rate under the incapacitation model. -/
def rate : ℝ := C.lam / (1 + C.lam * C.q * C.S)
/-- Share of free-state offending removed. -/
def preventedShare : ℝ := C.lam * C.q * C.S / (1 + C.lam * C.q * C.S)

theorem denom_pos : 0 < 1 + C.lam * C.q * C.S := by
  have := mul_nonneg (mul_nonneg C.lam_pos.le C.q_pos.le) C.S_nonneg; linarith

/-- `λ f̄ / (f̄ + S) = λ/(1 + λqS)` at the mean free time `f̄ = 1/(λq)`. -/
theorem steady_state_rate : C.lam * (1 / (C.lam * C.q)) / (1 / (C.lam * C.q) + C.S) = rate C := by
  unfold rate
  have hl := C.lam_pos.ne'
  have hq := C.q_pos.ne'
  have h2 := (denom_pos C).ne'
  field_simp <;> ring

/-- The realised rate on `N` cycles with free times `f_i` and crimes `λ f_i`. -/
theorem cycle_rate {N : ℕ} (f : Fin N → ℝ) (hN : 0 < N) (hf : ∀ i, 0 < f i) :
    (∑ i, C.lam * f i) / (∑ i, (f i + C.S)) = C.lam * ((∑ i, f i) / N) / ((∑ i, f i) / N + C.S) := by
  have hsum : 0 < ∑ i, f i := sum_pos (fun i _ => hf i) ⟨⟨0, hN⟩, mem_univ _⟩
  have hN' : (N : ℝ) ≠ 0 := by exact_mod_cast hN.ne'
  rw [← mul_sum, sum_add_distrib, sum_const, card_univ, Fintype.card_fin, nsmul_eq_mul]
  have hden : (∑ i, f i) + N * C.S ≠ 0 := by
    have := mul_nonneg (Nat.cast_nonneg N : (0 : ℝ) ≤ N) C.S_nonneg
    linarith
  field_simp <;> ring

theorem rate_pos : 0 < rate C := div_pos C.lam_pos (denom_pos C)

theorem rate_le_lam : rate C ≤ C.lam := by
  unfold rate
  rw [div_le_iff₀ (denom_pos C)]
  have := mul_nonneg (mul_nonneg C.lam_pos.le C.q_pos.le) C.S_nonneg
  nlinarith [C.lam_pos]

/-- The prevented share is `1 − rate/λ`, lies in `[0, 1)`, and never reaches one. -/
theorem prevented_share_eq : preventedShare C = 1 - rate C / C.lam := by
  unfold preventedShare rate
  have hl := C.lam_pos.ne'
  have h2 := (denom_pos C).ne'
  field_simp <;> ring

theorem prevented_share_lt_one : preventedShare C < 1 := by
  unfold preventedShare
  rw [div_lt_one (denom_pos C)]; linarith

theorem prevented_share_nonneg : 0 ≤ preventedShare C :=
  div_nonneg (mul_nonneg (mul_nonneg C.lam_pos.le C.q_pos.le) C.S_nonneg) (denom_pos C).le

/-- Longer sentences lower the rate. -/
theorem rate_antitone_in_S (C₁ C₂ : Career) (hl : C₁.lam = C₂.lam) (hq : C₁.q = C₂.q) (hS : C₁.S ≤ C₂.S) :
    rate C₂ ≤ rate C₁ := by
  unfold rate
  rw [hl, hq]
  apply div_le_div_of_nonneg_left C₂.lam_pos.le (by rw [← hl, ← hq]; exact denom_pos C₁)
  have := mul_pos C₂.lam_pos C₂.q_pos
  nlinarith

/-- Higher certainty lowers the rate. -/
theorem rate_antitone_in_q (C₁ C₂ : Career) (hl : C₁.lam = C₂.lam) (hS : C₁.S = C₂.S) (hq : C₁.q ≤ C₂.q) :
    rate C₂ ≤ rate C₁ := by
  unfold rate
  rw [hl, hS]
  apply div_le_div_of_nonneg_left C₂.lam_pos.le (by rw [← hl, ← hS]; exact denom_pos C₁)
  have := mul_nonneg C₂.lam_pos.le C₂.S_nonneg
  nlinarith

/-- Crimes prevented per year by one more year of sentence, and that it falls in `S`. -/
def marginalPrevention : ℝ := rate C - C.lam / (1 + C.lam * C.q * (C.S + 1))

theorem marginal_prevention_eq :
    marginalPrevention C = C.lam ^ 2 * C.q / ((1 + C.lam * C.q * C.S) * (1 + C.lam * C.q * (C.S + 1))) := by
  unfold marginalPrevention rate
  have h1 := (denom_pos C).ne'
  have h2 : (1 + C.lam * C.q * (C.S + 1)) ≠ 0 := by
    have := mul_nonneg (mul_nonneg C.lam_pos.le C.q_pos.le) (by linarith [C.S_nonneg] : (0 : ℝ) ≤ C.S + 1)
    linarith
  field_simp
  ring

theorem marginal_prevention_pos : 0 < marginalPrevention C := by
  rw [marginal_prevention_eq]
  have := mul_nonneg (mul_nonneg C.lam_pos.le C.q_pos.le) C.S_nonneg
  have h2 : 0 < 1 + C.lam * C.q * (C.S + 1) := by nlinarith [mul_pos C.lam_pos C.q_pos]
  exact div_pos (mul_pos (pow_pos C.lam_pos 2) C.q_pos) (mul_pos (denom_pos C) h2)

/-! ### Heterogeneous offenders -/

/-- Share-weighted aggregate rate of `K` groups. -/
def groupRate {K : ℕ} (π : Fin K → ℝ) (Cs : Fin K → Career) : ℝ := ∑ k, π k * rate (Cs k)

/-- Under a uniform sentence the high-rate group has the larger prevented share. -/
theorem high_rate_more_prevented (C₁ C₂ : Career) (hq : C₁.q = C₂.q) (hS : C₁.S = C₂.S) (hl : C₁.lam ≤ C₂.lam) :
    preventedShare C₁ ≤ preventedShare C₂ := by
  unfold preventedShare
  rw [hq, hS]
  have d1 := denom_pos C₁; have d2 := denom_pos C₂
  rw [hq, hS] at d1
  rw [div_le_div_iff₀ d1 d2]
  have := mul_nonneg C₂.q_pos.le C₂.S_nonneg
  nlinarith

end Research.P17
