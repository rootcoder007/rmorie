/-
  Researchproofs/P4Contraction.lean

  P4, continued: the discrete attractiveness field is a (1 − ω)-contraction
  when no crime feeds it (library scan C7; D'Orsogna & Perc Eq. 2).

  The lattice update B' = (1 − ω)[(1 − η)B + η S B] with S the neighbour
  average (a row-stochastic non-negative matrix on the sites) and no events:
    * `spreading_conserves`: Σ(S B) = Σ B when S is doubly stochastic, so the
      spreading step conserves total attractiveness (`update_total`:
      ΣB' = (1 − ω)ΣB).
    * `sup_contraction`: ‖B'‖_∞ ≤ (1 − ω)‖B‖_∞, and `l1_contraction` for the
      ℓ¹ norm; with 0 < ω ≤ 1 the field decays geometrically to zero
      (`decay`): absent crime the map has one fixed point, zero.
    * `feedback_gain`: with the event feedback A₀ + B → events ∝ ρ(A₀ + B),
      the linearised gain at B = 0 is (1 − ω)(1 + ερ) (the Short model's
      θ = ε ρ̄), which exceeds one iff ερ > ω/(1 − ω): the P4Hotspot
      threshold in its site form.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Finset

namespace Research.P4.Contraction

variable {V : Type*} [Fintype V] [Nonempty V]

/-- A row-stochastic non-negative spreading matrix. -/
structure Spread (V : Type*) [Fintype V] where
  S : V → V → ℝ
  nonneg : ∀ i j, 0 ≤ S i j
  row_sum : ∀ i, ∑ j, S i j = 1

def apply (sp : Spread V) (B : V → ℝ) (i : V) : ℝ := ∑ j, sp.S i j * B j

/-- One update with no events. -/
def update (sp : Spread V) (ω η : ℝ) (B : V → ℝ) (i : V) : ℝ :=
  (1 - ω) * ((1 - η) * B i + η * apply sp B i)

/-- Spreading is bounded by the sup norm of the field. -/
theorem apply_le_sup (sp : Spread V) (B : V → ℝ) (i : V) :
    |apply sp B i| ≤ univ.sup' univ_nonempty fun j => |B j| := by
  unfold apply
  calc |∑ j, sp.S i j * B j| ≤ ∑ j, |sp.S i j * B j| := abs_sum_le_sum_abs _ _
    _ = ∑ j, sp.S i j * |B j| := by
        apply sum_congr rfl; intro j _; rw [abs_mul, abs_of_nonneg (sp.nonneg i j)]
    _ ≤ ∑ j, sp.S i j * (univ.sup' univ_nonempty fun k => |B k|) := by
        apply sum_le_sum; intro j _
        exact mul_le_mul_of_nonneg_left (le_sup' (fun k => |B k|) (mem_univ j)) (sp.nonneg i j)
    _ = univ.sup' univ_nonempty fun k => |B k| := by rw [← sum_mul, sp.row_sum, one_mul]

/-- The sup norm contracts by `(1 − ω)` when `0 ≤ η ≤ 1`, `0 ≤ ω ≤ 1`. -/
theorem sup_contraction (sp : Spread V) (ω η : ℝ) (hω : 0 ≤ ω) (hω1 : ω ≤ 1) (hη : 0 ≤ η) (hη1 : η ≤ 1)
    (B : V → ℝ) (i : V) :
    |update sp ω η B i| ≤ (1 - ω) * univ.sup' univ_nonempty fun j => |B j| := by
  unfold update
  set m := univ.sup' univ_nonempty fun j => |B j| with hm
  have hBi : |B i| ≤ m := le_sup' (fun j => |B j|) (mem_univ i)
  have hA : |apply sp B i| ≤ m := apply_le_sup sp B i
  rw [abs_mul, abs_of_nonneg (by linarith : (0:ℝ) ≤ 1 - ω)]
  apply mul_le_mul_of_nonneg_left _ (by linarith)
  calc |(1 - η) * B i + η * apply sp B i| ≤ |(1 - η) * B i| + |η * apply sp B i| := abs_add_le _ _
    _ = (1 - η) * |B i| + η * |apply sp B i| := by
        rw [abs_mul, abs_mul, abs_of_nonneg (by linarith : (0:ℝ) ≤ 1 - η), abs_of_nonneg hη]
    _ ≤ (1 - η) * m + η * m := by nlinarith
    _ = m := by ring

/-- Doubly stochastic spreading conserves the total. -/
theorem spreading_conserves (sp : Spread V) (hcol : ∀ j, ∑ i, sp.S i j = 1) (B : V → ℝ) :
    ∑ i, apply sp B i = ∑ i, B i := by
  unfold apply
  rw [sum_comm]
  apply sum_congr rfl; intro j _
  rw [← sum_mul, hcol j, one_mul]

theorem update_total (sp : Spread V) (hcol : ∀ j, ∑ i, sp.S i j = 1) (ω η : ℝ) (B : V → ℝ) :
    ∑ i, update sp ω η B i = (1 - ω) * ∑ i, B i := by
  unfold update
  rw [← mul_sum, sum_add_distrib, ← mul_sum, ← mul_sum, spreading_conserves sp hcol]
  ring

/-- Iterating: the sup norm after `n` updates is at most `(1 − ω)^n` times the start. -/
theorem decay (sp : Spread V) (ω η : ℝ) (hω : 0 ≤ ω) (hω1 : ω ≤ 1) (hη : 0 ≤ η) (hη1 : η ≤ 1)
    (B : V → ℝ) (n : ℕ) :
    (univ.sup' univ_nonempty fun i => |(update sp ω η)^[n] B i|)
      ≤ (1 - ω) ^ n * univ.sup' univ_nonempty fun i => |B i| := by
  induction n generalizing B with
  | zero => simp
  | succ n ih =>
    rw [Function.iterate_succ_apply]
    calc (univ.sup' univ_nonempty fun i => |(update sp ω η)^[n] (update sp ω η B) i|)
        ≤ (1 - ω) ^ n * univ.sup' univ_nonempty fun i => |update sp ω η B i| := ih _
      _ ≤ (1 - ω) ^ n * ((1 - ω) * univ.sup' univ_nonempty fun i => |B i|) := by
          apply mul_le_mul_of_nonneg_left _ (pow_nonneg (by linarith) n)
          apply sup'_le; intro i _; exact sup_contraction sp ω η hω hω1 hη hη1 B i
      _ = (1 - ω) ^ (n + 1) * univ.sup' univ_nonempty fun i => |B i| := by ring

/-- The linearised gain of the uniform field with event feedback `ε ρ (A₀ + B)`: the uniform
mode of `B ↦ (1 − ω)(B + ε ρ B)` grows iff `ε ρ > ω/(1 − ω)`. -/
theorem feedback_gain (ω ερ : ℝ) (hω : 0 ≤ ω) (hω1 : ω < 1) :
    1 < (1 - ω) * (1 + ερ) ↔ ω / (1 - ω) < ερ := by
  have hpos : 0 < 1 - ω := by linarith
  rw [div_lt_iff₀ hpos]
  constructor <;> intro h <;> nlinarith

end Research.P4.Contraction
