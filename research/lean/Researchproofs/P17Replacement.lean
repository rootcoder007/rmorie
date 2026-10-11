/-
  Researchproofs/P17Replacement.lean

  P17 (continued): replacement and desistance — the two assumptions
  incapacitation estimates rest on, stated as arithmetic.

  Replacement (markets for crime; Reuter 1983; Cook 1986): a share r of
  the crimes an incarcerated offender would have committed is committed by
  someone else. The share of free-state offending actually removed is then
  (1 − r) times the P17 prevented share:
    * replaced_le, replaced_antitone, replaced_full: replacement only
      lowers the prevented share, monotonically, to zero at r = 1.

  Desistance (Blumstein, Cohen, Roth & Visher 1986; Laub & Sampson 2003):
  the offending rate is a non-increasing function λ(t) of career age. A
  sentence of S periods served from career age t₀ prevents ∑_{s<S} λ(t₀+s)
  crimes:
    * prevented_le_const: at most S·λ(t₀) — the constant-rate formula with
      the rate at entry is an upper bound, never an underestimate;
    * prevented_ge_const: at least S·λ(t₀+S);
    * later_sentence_prevents_less: the same sentence served later in a
      career prevents fewer crimes;
    * constant_rate: with a constant λ the sum is S·λ (the P17 model).

  So every constant-λ incapacitation estimate from an entry-age rate is an
  upper bound under desistance, and an overestimate again under replacement
  — the two biases point the same way.
-/
import Mathlib
import Researchproofs.P17Incapacitation

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset Research.P17

namespace Research.P17Replacement

/-! ### Replacement -/

/-- Prevented share net of a replacement fraction `r`. -/
def replacedShare (C : Career) (r : ℝ) : ℝ := (1 - r) * preventedShare C

theorem replaced_le (C : Career) (r : ℝ) (hr : 0 ≤ r) : replacedShare C r ≤ preventedShare C := by
  unfold replacedShare
  exact mul_le_of_le_one_left (prevented_share_nonneg C) (by linarith)

theorem replaced_antitone (C : Career) {r₁ r₂ : ℝ} (h : r₁ ≤ r₂) :
    replacedShare C r₂ ≤ replacedShare C r₁ :=
  mul_le_mul_of_nonneg_right (by linarith) (prevented_share_nonneg C)

theorem replaced_full (C : Career) : replacedShare C 1 = 0 := by simp [replacedShare]

theorem replaced_nonneg (C : Career) (r : ℝ) (hr : r ≤ 1) : 0 ≤ replacedShare C r :=
  mul_nonneg (by linarith) (prevented_share_nonneg C)

/-! ### Desistance -/

/-- Crimes prevented by `S` periods served from career age `t₀` under the rate path `lam`. -/
def prevented (lam : ℕ → ℝ) (t₀ S : ℕ) : ℝ := ∑ s ∈ range S, lam (t₀ + s)

theorem prevented_le_const (lam : ℕ → ℝ) (hanti : Antitone lam) (t₀ S : ℕ) :
    prevented lam t₀ S ≤ S * lam t₀ := by
  unfold prevented
  calc ∑ s ∈ range S, lam (t₀ + s) ≤ ∑ s ∈ range S, lam t₀ :=
        sum_le_sum fun s _ => hanti (Nat.le_add_right t₀ s)
    _ = S * lam t₀ := by simp [sum_const, card_range, nsmul_eq_mul]

theorem prevented_ge_const (lam : ℕ → ℝ) (hanti : Antitone lam) (t₀ S : ℕ) :
    S * lam (t₀ + S) ≤ prevented lam t₀ S := by
  unfold prevented
  calc (S : ℝ) * lam (t₀ + S) = ∑ s ∈ range S, lam (t₀ + S) := by
        simp [sum_const, card_range, nsmul_eq_mul]
    _ ≤ ∑ s ∈ range S, lam (t₀ + s) := by
        apply sum_le_sum; intro s hs
        exact hanti (Nat.add_le_add_left (mem_range.mp hs).le t₀)

theorem later_sentence_prevents_less (lam : ℕ → ℝ) (hanti : Antitone lam) {t₀ t₁ : ℕ}
    (h : t₀ ≤ t₁) (S : ℕ) : prevented lam t₁ S ≤ prevented lam t₀ S := by
  unfold prevented
  exact sum_le_sum fun s _ => hanti (Nat.add_le_add_right h s)

theorem constant_rate (c : ℝ) (t₀ S : ℕ) : prevented (fun _ => c) t₀ S = S * c := by
  simp [prevented, sum_const, card_range, nsmul_eq_mul]

/-- Both corrections together: desistance and replacement. -/
def preventedNet (lam : ℕ → ℝ) (t₀ S : ℕ) (r : ℝ) : ℝ := (1 - r) * prevented lam t₀ S

theorem prevented_net_le (lam : ℕ → ℝ) (hanti : Antitone lam) (hnn : ∀ t, 0 ≤ lam t)
    (t₀ S : ℕ) (r : ℝ) (hr0 : 0 ≤ r) (hr1 : r ≤ 1) :
    preventedNet lam t₀ S r ≤ S * lam t₀ := by
  unfold preventedNet
  have h1 := prevented_le_const lam hanti t₀ S
  have h0 : 0 ≤ prevented lam t₀ S := sum_nonneg fun s _ => hnn _
  calc (1 - r) * prevented lam t₀ S ≤ 1 * prevented lam t₀ S :=
        mul_le_mul_of_nonneg_right (by linarith) h0
    _ ≤ S * lam t₀ := by linarith

end Research.P17Replacement
