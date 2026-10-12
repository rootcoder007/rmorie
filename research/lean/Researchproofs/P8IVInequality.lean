/-
  Researchproofs/P8IVInequality.lean

  P8, continued: the instrumental inequality (library scan B12; Pearl,
  Causality 2e §8.4, eqs. 8.22-8.23).

  In the instrumental-variable model with finite instrument Z, treatment X
  and outcome Y and a latent U with Z ⊥ U, the observed law is
      P(y, x | z) = Σ_u P(u) P(x | z, u) P(y | x, u).
  Then for every x, Σ_y max_z P(y, x | z) ≤ 1 (`instrumental_inequality`):
  the test Pearl states is a consequence of the model, so a table that
  violates it refutes the instrument. The proof is three lines of finite
  sums: max_z P(y, x | z) ≤ Σ_u P(u) P(y | x, u) max_z P(x | z, u), and
  summing over y and bounding the maximum by one.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Finset

namespace Research.P8.IVInequality

variable {Z X Y U : Type*} [Fintype Z] [Fintype X] [Fintype Y] [Fintype U] [Nonempty Z]

/-- The instrumental-variable model on finite types. -/
structure IVModel (Z X Y U : Type*) [Fintype Z] [Fintype X] [Fintype Y] [Fintype U] where
  pu : U → ℝ
  px : Z → U → X → ℝ      -- P(x | z, u)
  py : X → U → Y → ℝ      -- P(y | x, u)
  pu_nonneg : ∀ u, 0 ≤ pu u
  pu_sum : ∑ u, pu u = 1
  px_nonneg : ∀ z u x, 0 ≤ px z u x
  px_le_one : ∀ z u x, px z u x ≤ 1
  py_nonneg : ∀ x u y, 0 ≤ py x u y
  py_sum : ∀ x u, ∑ y, py x u y = 1

/-- The observed law `P(y, x | z)`. -/
def obs (m : IVModel Z X Y U) (z : Z) (x : X) (y : Y) : ℝ := ∑ u, m.pu u * m.px z u x * m.py x u y

/-- `P(y, x | z) ≤ Σ_u P(u) P(y | x, u) · sup_z P(x | z, u)`, so the bound holds for the max over z. -/
theorem obs_le (m : IVModel Z X Y U) (z : Z) (x : X) (y : Y) :
    obs m z x y ≤ ∑ u, m.pu u * m.py x u y * (univ.sup' univ_nonempty fun z' => m.px z' u x) := by
  unfold obs
  apply sum_le_sum
  intro u _
  have h1 : m.px z u x ≤ univ.sup' univ_nonempty fun z' => m.px z' u x := le_sup' (fun z' => m.px z' u x) (mem_univ z)
  have h2 : 0 ≤ m.pu u * m.py x u y := mul_nonneg (m.pu_nonneg u) (m.py_nonneg x u y)
  nlinarith [h1, h2]

/-- The instrumental inequality: `Σ_y max_z P(y, x | z) ≤ 1` for every `x`. -/
theorem instrumental_inequality (m : IVModel Z X Y U) (x : X) :
    ∑ y, (univ.sup' univ_nonempty fun z => obs m z x y) ≤ 1 := by
  have hterm : ∀ y, (univ.sup' univ_nonempty fun z => obs m z x y)
      ≤ ∑ u, m.pu u * m.py x u y * (univ.sup' univ_nonempty fun z' => m.px z' u x) := by
    intro y
    apply sup'_le
    intro z _
    exact obs_le m z x y
  calc ∑ y, (univ.sup' univ_nonempty fun z => obs m z x y)
      ≤ ∑ y, ∑ u, m.pu u * m.py x u y * (univ.sup' univ_nonempty fun z' => m.px z' u x) :=
        sum_le_sum fun y _ => hterm y
    _ = ∑ u, m.pu u * (univ.sup' univ_nonempty fun z' => m.px z' u x) * ∑ y, m.py x u y := by
        rw [sum_comm]; apply sum_congr rfl; intro u _; rw [mul_sum]; apply sum_congr rfl; intro y _; ring
    _ = ∑ u, m.pu u * (univ.sup' univ_nonempty fun z' => m.px z' u x) := by
        apply sum_congr rfl; intro u _; rw [m.py_sum x u, mul_one]
    _ ≤ ∑ u, m.pu u * 1 := by
        apply sum_le_sum; intro u _
        apply mul_le_mul_of_nonneg_left _ (m.pu_nonneg u)
        apply sup'_le; intro z _; exact m.px_le_one z u x
    _ = 1 := by simp [m.pu_sum]

end Research.P8.IVInequality
