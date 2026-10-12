/-
  Researchproofs/P7MoranTrend.lean

  P7, continued: Moran's I detects a deterministic trend as autocorrelation
  (library scan B19; Schabenberger & Gotway §1.3.2.2, eq. 1.16).

    * `trend_witness`: on the four-node path with the linear trend
      y = (1, 2, 3, 4) and no noise at all, I = 1/3, not the null expectation
      −1/(n − 1) = −1/3: mean nonstationarity alone moves Moran's I.
    * `quad_sum_eq_trace`: for error draws e_k with second-moment matrix
      S = Σ_k e_k e_kᵀ, Σ_k e_kᵀ A e_k = tr(A S).
    * `residual_moran_ratio`: with S = c·I and residuals M e for a symmetric
      idempotent M, the ratio of the summed numerator to the summed
      denominator of Moran's I on residuals is tr(W M)/tr(M), the eq. 1.16
      form with tr(M) = n − p.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Matrix Finset

namespace Research.P7.MoranTrend

theorem trend_witness :
    let z : Fin 4 → ℝ := ![-3 / 2, -1 / 2, 1 / 2, 3 / 2]
    let num := 2 * (z 0 * z 1 + z 1 * z 2 + z 2 * z 3)
    let den := z 0 ^ 2 + z 1 ^ 2 + z 2 ^ 2 + z 3 ^ 2
    (4 / 6 : ℝ) * num / den = 1 / 3 ∧ (1 / 3 : ℝ) ≠ -1 / (4 - 1) := by
  simp only [Matrix.cons_val_zero, Matrix.cons_val_one, Matrix.head_cons, Matrix.cons_val_two,
    Matrix.cons_val_three, Matrix.tail_cons]
  norm_num

variable {n : Type*} [Fintype n] [DecidableEq n] {K : Type*} [Fintype K]

/-- `eᵀ A e = tr(A · e eᵀ)`. -/
theorem quad_eq_trace (A : Matrix n n ℝ) (e : n → ℝ) :
    e ⬝ᵥ (A *ᵥ e) = trace (A * vecMulVec e e) := by
  simp only [dotProduct, mulVec, trace, Matrix.diag, mul_apply, vecMulVec_apply]
  apply sum_congr rfl; intro i _
  rw [mul_sum]
  apply sum_congr rfl; intro j _
  ring

theorem quad_sum_eq_trace (A : Matrix n n ℝ) (e : K → n → ℝ) :
    ∑ k, e k ⬝ᵥ (A *ᵥ e k) = trace (A * ∑ k, vecMulVec (e k) (e k)) := by
  rw [mul_sum, trace_sum]
  apply sum_congr rfl; intro k _
  exact quad_eq_trace A (e k)

/-- With `Σ_k e_k e_kᵀ = c I`, residuals `M e_k`, `M` symmetric idempotent:
numerator sum `c tr(W M)`, denominator sum `c tr(M)`. -/
theorem residual_moran_ratio (W M : Matrix n n ℝ) (e : K → n → ℝ) (c : ℝ)
    (hS : ∑ k, vecMulVec (e k) (e k) = c • (1 : Matrix n n ℝ))
    (hsym : Mᵀ = M) (hidem : M * M = M) :
    ∑ k, (M *ᵥ e k) ⬝ᵥ (W *ᵥ (M *ᵥ e k)) = c * trace (W * M) ∧
    ∑ k, (M *ᵥ e k) ⬝ᵥ (M *ᵥ e k) = c * trace M := by
  have key : ∀ A : Matrix n n ℝ, ∑ k, (M *ᵥ e k) ⬝ᵥ (A *ᵥ (M *ᵥ e k)) = c * trace (Mᵀ * A * M) := by
    intro A
    have : ∀ k, (M *ᵥ e k) ⬝ᵥ (A *ᵥ (M *ᵥ e k)) = e k ⬝ᵥ ((Mᵀ * A * M) *ᵥ e k) := by
      intro k
      rw [← mulVec_mulVec, ← mulVec_mulVec, dotProduct_mulVec (e k) Mᵀ, vecMul_transpose]
    simp_rw [this]
    rw [quad_sum_eq_trace, hS, Matrix.mul_smul, Matrix.mul_one, trace_smul, smul_eq_mul]
  constructor
  · rw [key W, hsym, trace_mul_comm, ← Matrix.mul_assoc, hidem, trace_mul_comm]
  · have := key 1
    rw [Matrix.mul_one, hsym, hidem] at this
    simpa [one_mulVec] using this

end Research.P7.MoranTrend
