/-
  Researchproofs/P4Mitigation.lean

  P4, continued: public reports mitigate the feedback loop but cannot
  remove it — with an explicit cap.

  With a reporting share ρ ∈ [0, 1], each step's expected increments are
      incA = (1−ρ) λA x + ρ λA,      incB = (1−ρ) λB (1−x) + ρ λB,
  because a publicly reported crime arrives from a region in proportion
  to the true rates regardless of where the patrol is. The next share is
  a weighted average of the current share x and the increment share
  F(x) = incA / (incA + incB), and F(x) never exceeds

      x̄ = λA / (λA + ρ λB).

  Hence (rho_cap) the share never exceeds max(x₀, x̄): with ρ = 0 the cap
  is 1 (no mitigation, the runaway of P4Limit), with ρ = 1 it is the
  true-rate proportion λA/(λA+λB) (full correction), and in between the
  loop is capped strictly below 1 but strictly above the truth. This is
  the quantitative form of "reports mitigate but do not remove".
-/
import Mathlib
import Researchproofs.P4Feedback

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P4

/-- Increments with a reporting share `ρ`. -/
def incA (P : Params) (ρ x : ℝ) : ℝ := (1 - ρ) * P.lamA * x + ρ * P.lamA
def incB (P : Params) (ρ x : ℝ) : ℝ := (1 - ρ) * P.lamB * (1 - x) + ρ * P.lamB

/-- One step of the recursion with reporting share `ρ`. -/
def rhoStep (P : Params) (ρ : ℝ) (cA cB : ℝ) : ℝ × ℝ :=
  (cA + incA P ρ (share cA cB), cB + incB P ρ (share cA cB))

/-- The cap on the share to A. -/
def cap (P : Params) (ρ : ℝ) : ℝ := P.lamA / (P.lamA + ρ * P.lamB)

theorem incA_nonneg (P : Params) (ρ x : ℝ) (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1) (hx0 : 0 ≤ x) :
    0 ≤ incA P ρ x := by
  unfold incA
  have hA := P.lamA_pos
  have hr : 0 ≤ 1 - ρ := by linarith
  have h1 : 0 ≤ (1 - ρ) * P.lamA * x := mul_nonneg (mul_nonneg hr hA.le) hx0
  have h2 : 0 ≤ ρ * P.lamA := mul_nonneg hρ0 hA.le
  linarith

theorem incB_nonneg (P : Params) (ρ x : ℝ) (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1) (hx1 : x ≤ 1) :
    0 ≤ incB P ρ x := by
  unfold incB
  have hB := P.lamB_pos
  have hr : 0 ≤ 1 - ρ := by linarith
  have hx : 0 ≤ 1 - x := by linarith
  have h1 : 0 ≤ (1 - ρ) * P.lamB * (1 - x) := mul_nonneg (mul_nonneg hr hB.le) hx
  have h2 : 0 ≤ ρ * P.lamB := mul_nonneg hρ0 hB.le
  linarith

theorem incA_pos (P : Params) (ρ x : ℝ) (hρ0 : 0 ≤ ρ) (hx0 : 0 < x) (hx1 : x ≤ 1) :
    0 < incA P ρ x := by
  unfold incA
  have hA := P.lamA_pos
  have hx : 0 ≤ 1 - x := by linarith
  have key : 0 < x + ρ * (1 - x) := by nlinarith [mul_nonneg hρ0 hx]
  have : (1 - ρ) * P.lamA * x + ρ * P.lamA = P.lamA * (x + ρ * (1 - x)) := by ring
  rw [this]; exact mul_pos hA key

/-- The increment share is at most the cap. -/
theorem inc_share_le_cap (P : Params) (ρ x : ℝ) (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1) (_hx0 : 0 ≤ x) (hx1 : x ≤ 1) :
    incA P ρ x * (P.lamA + ρ * P.lamB) ≤ P.lamA * (incA P ρ x + incB P ρ x) := by
  unfold incA incB
  have hA := P.lamA_pos; have hB := P.lamB_pos
  have h1 : 0 ≤ 1 - ρ := by linarith
  have h2 : 0 ≤ 1 - x := by linarith
  nlinarith [mul_nonneg (mul_nonneg h1 hρ0) h2, mul_nonneg h1 h2, mul_pos hA hB]

/-- The share after one step never exceeds `max x cap` when `x ≤ 1`. -/
theorem rhoStep_share_le (P : Params) (ρ : ℝ) (cA cB : ℝ) (hA : 0 < cA) (hB : 0 < cB)
    (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1) :
    share (rhoStep P ρ cA cB).1 (rhoStep P ρ cA cB).2 ≤ max (share cA cB) (cap P ρ) := by
  have hc : 0 < cA + cB := by positivity
  have hx0 : 0 < share cA cB := by unfold share; positivity
  have hx1 : share cA cB < 1 := by unfold share; rw [div_lt_one hc]; linarith
  have hnext : share (rhoStep P ρ cA cB).1 (rhoStep P ρ cA cB).2 =
      (cA + incA P ρ (share cA cB)) / ((cA + incA P ρ (share cA cB)) + (cB + incB P ρ (share cA cB))) := rfl
  set x := share cA cB with hx
  set IA := incA P ρ x
  set IB := incB P ρ x
  have hIA : 0 < IA := incA_pos P ρ x hρ0 hx0 hx1.le
  have hIB : 0 ≤ IB := incB_nonneg P ρ x hρ0 hρ1 hx1.le
  have hcap := inc_share_le_cap P ρ x hρ0 hρ1 hx0.le hx1.le
  have hden : 0 < P.lamA + ρ * P.lamB := by have := P.lamA_pos; have := P.lamB_pos; positivity
  rw [hnext]
  set M := max x (cap P ρ)
  have hxM : x ≤ M := le_max_left _ _
  have hcM : cap P ρ ≤ M := le_max_right _ _
  -- cA ≤ M (cA + cB) since x = cA/(cA+cB) ≤ M
  have h1 : cA ≤ M * (cA + cB) := by
    have : x * (cA + cB) = cA := by rw [hx]; unfold share; exact div_mul_cancel₀ cA hc.ne'
    nlinarith [mul_le_mul_of_nonneg_right hxM hc.le]
  -- IA ≤ M (IA + IB) since IA/(IA+IB) ≤ cap ≤ M
  have h2 : IA ≤ M * (IA + IB) := by
    have hcapM : IA ≤ cap P ρ * (IA + IB) := by
      unfold cap
      rw [div_mul_eq_mul_div, le_div_iff₀ hden]
      linarith
    have : cap P ρ * (IA + IB) ≤ M * (IA + IB) := mul_le_mul_of_nonneg_right hcM (by linarith)
    linarith
  rw [div_le_iff₀ (by linarith)]
  linarith

/-- Iterating the ρ-recursion from the initial counts. -/
def rhoIter (P : Params) (ρ : ℝ) : ℕ → ℝ × ℝ
  | 0 => (P.cA0, P.cB0)
  | n + 1 => rhoStep P ρ (rhoIter P ρ n).1 (rhoIter P ρ n).2

theorem rhoIter_pos (P : Params) (ρ : ℝ) (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1) (n : ℕ) :
    0 < (rhoIter P ρ n).1 ∧ 0 < (rhoIter P ρ n).2 := by
  induction n with
  | zero => exact ⟨P.cA0_pos, P.cB0_pos⟩
  | succ n ih =>
    obtain ⟨hA, hB⟩ := ih
    have hc : 0 < (rhoIter P ρ n).1 + (rhoIter P ρ n).2 := by positivity
    have hx0 : 0 < share (rhoIter P ρ n).1 (rhoIter P ρ n).2 := by unfold share; positivity
    have hx1 : share (rhoIter P ρ n).1 (rhoIter P ρ n).2 < 1 := by
      unfold share; rw [div_lt_one hc]; linarith
    simp only [rhoIter, rhoStep]
    constructor
    · have := incA_nonneg P ρ _ hρ0 hρ1 hx0.le; linarith
    · have := incB_nonneg P ρ _ hρ0 hρ1 hx1.le; linarith

/-- The cap holds forever: the share never exceeds `max x₀ cap`. -/
theorem rho_cap (P : Params) (ρ : ℝ) (hρ0 : 0 ≤ ρ) (hρ1 : ρ ≤ 1) (n : ℕ) :
    share (rhoIter P ρ n).1 (rhoIter P ρ n).2 ≤ max (share P.cA0 P.cB0) (cap P ρ) := by
  induction n with
  | zero => exact le_max_left _ _
  | succ n ih =>
    obtain ⟨hA, hB⟩ := rhoIter_pos P ρ hρ0 hρ1 n
    have h := rhoStep_share_le P ρ _ _ hA hB hρ0 hρ1
    simp only [rhoIter]
    calc share (rhoStep P ρ (rhoIter P ρ n).1 (rhoIter P ρ n).2).1 (rhoStep P ρ (rhoIter P ρ n).1 (rhoIter P ρ n).2).2
        ≤ max (share (rhoIter P ρ n).1 (rhoIter P ρ n).2) (cap P ρ) := h
      _ ≤ max (share P.cA0 P.cB0) (cap P ρ) := max_le (le_trans ih le_rfl) (le_max_right _ _)

/-- With full reporting the cap is the true-rate proportion; with none it is 1. -/
theorem cap_one (P : Params) : cap P 1 = P.lamA / (P.lamA + P.lamB) := by unfold cap; ring_nf
theorem cap_zero (P : Params) : cap P 0 = 1 := by
  unfold cap; rw [zero_mul, add_zero]; exact div_self P.lamA_pos.ne'

end Research.P4
