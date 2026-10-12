/-
  Researchproofs/P4Martingale.lean

  P4, continued: the stochastic urn's share converges almost surely (the
  "martingale version" the ledger left open after P4Urn and P4Envelope).

  P4Envelope proved on the exact finite path space that the share S_n of a
  generalised Pólya urn is a martingale (step_mean) with values in [0, 1].
  What was missing is the limit statement, which needs the measure-theoretic
  martingale convergence theorem. This file supplies it in the generality the
  urn needs: on any probability space with a filtration ℱ, a process S that is
  a martingale for ℱ and takes values in [0, 1]

    * is uniformly integrable (`uniformIntegrable_of_unit`),
    * converges almost surely (`share_converges`, Doob's theorem through
      Mathlib's `Submartingale.ae_tendsto_limitProcess`),
    * converges in L¹ (`share_converges_L1`),
    * is at every time the conditional expectation of its limit
      (`share_eq_condExp_limit`), so the limit is a random variable S_∞ with
      E S_∞ = E S_n for every n (`limit_mean`): the long-run allocation is
      random, with the starting share as its mean, not the mean-field fixed
      point.

  The hypotheses are exactly what P4Envelope's finite model establishes for
  the urn share, so the two files together are the stochastic P4: the share
  is a bounded martingale, hence converges a.s. and in mean, and its limit's
  spread is governed by the envelope a/(4 T₀). The law of the limit at
  unequal rates (a Rubin-type theorem) remains open.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open MeasureTheory Filter Topology

namespace Research.P4.Martingale

variable {Ω : Type*} {m0 : MeasurableSpace Ω} {μ : Measure Ω} {ℱ : Filtration ℕ m0}

/-- A process with values in `[0, 1]` has norm at most one everywhere. -/
theorem norm_le_one (S : ℕ → Ω → ℝ) (hb : ∀ n ω, 0 ≤ S n ω ∧ S n ω ≤ 1) (n : ℕ) (ω : Ω) :
    ‖S n ω‖ ≤ 1 := by
  rw [Real.norm_eq_abs, abs_le]
  constructor <;> linarith [(hb n ω).1, (hb n ω).2]

/-- A `[0, 1]`-valued process has `L¹` norm at most one on a probability space. -/
theorem eLpNorm_one_le_one [IsProbabilityMeasure μ] (S : ℕ → Ω → ℝ) (hm : ∀ n, StronglyMeasurable (S n))
    (hb : ∀ n ω, 0 ≤ S n ω ∧ S n ω ≤ 1) (n : ℕ) : eLpNorm (S n) 1 μ ≤ (1 : NNReal) := by
  have h := eLpNorm_le_of_ae_bound (μ := μ) (p := 1) (hm n).aestronglyMeasurable
    (Filter.Eventually.of_forall fun ω => norm_le_one S hb n ω)
  simpa using h

/-- A `[0, 1]`-valued process is uniformly integrable: above the threshold 2 there is nothing. -/
theorem uniformIntegrable_of_unit [IsProbabilityMeasure μ] (S : ℕ → Ω → ℝ)
    (hm : ∀ n, StronglyMeasurable (S n)) (hb : ∀ n ω, 0 ≤ S n ω ∧ S n ω ≤ 1) :
    UniformIntegrable S 1 μ := by
  refine uniformIntegrable_of' le_rfl (by simp) hm ?_
  intro ε _
  refine ⟨2, fun n => ?_⟩
  have hempty : {x | (2 : NNReal) ≤ ‖S n x‖₊} = ∅ := by
    ext x
    simp only [Set.mem_setOf_eq, Set.mem_empty_iff_false, iff_false, not_le]
    have h1 : (‖S n x‖₊ : ℝ) ≤ 1 := by simpa [coe_nnnorm] using norm_le_one S hb n x
    exact_mod_cast (show (‖S n x‖₊ : ℝ) < 2 by linarith)
  rw [hempty, Set.indicator_empty]
  simp

/-- **Almost sure convergence of the share.** A `[0, 1]`-valued martingale converges almost
surely to its limit process. -/
theorem share_converges [IsProbabilityMeasure μ] (S : ℕ → Ω → ℝ) (hS : Martingale S ℱ μ)
    (hb : ∀ n ω, 0 ≤ S n ω ∧ S n ω ≤ 1) :
    ∀ᵐ ω ∂μ, Tendsto (fun n => S n ω) atTop (𝓝 (ℱ.limitProcess S μ ω)) :=
  hS.submartingale.ae_tendsto_limitProcess (R := 1)
    (eLpNorm_one_le_one S (fun n => (hS.stronglyMeasurable n).mono (ℱ.le n)) hb)

/-- The same limit in `L¹`. -/
theorem share_converges_L1 [IsProbabilityMeasure μ] (S : ℕ → Ω → ℝ) (hS : Martingale S ℱ μ)
    (hb : ∀ n ω, 0 ≤ S n ω ∧ S n ω ≤ 1) :
    Tendsto (fun n => eLpNorm (S n - ℱ.limitProcess S μ) 1 μ) atTop (𝓝 0) :=
  hS.submartingale.tendsto_eLpNorm_one_limitProcess
    (uniformIntegrable_of_unit S (fun n => (hS.stronglyMeasurable n).mono (ℱ.le n)) hb)

/-- Each `S n` is the conditional expectation of the limit given `ℱ n`: the limit is a genuine
random variable whose expectation the share tracks. -/
theorem share_eq_condExp_limit [IsProbabilityMeasure μ] (S : ℕ → Ω → ℝ) (hS : Martingale S ℱ μ)
    (hb : ∀ n ω, 0 ≤ S n ω ∧ S n ω ≤ 1) (n : ℕ) :
    S n =ᵐ[μ] μ[ℱ.limitProcess S μ | ℱ n] :=
  hS.ae_eq_condExp_limitProcess
    (uniformIntegrable_of_unit S (fun n => (hS.stronglyMeasurable n).mono (ℱ.le n)) hb) n

/-- The mean of the limit is the mean of the share at every time (and so the starting share). -/
theorem limit_mean [IsProbabilityMeasure μ] (S : ℕ → Ω → ℝ) (hS : Martingale S ℱ μ)
    (hb : ∀ n ω, 0 ≤ S n ω ∧ S n ω ≤ 1) (n : ℕ) :
    ∫ ω, ℱ.limitProcess S μ ω ∂μ = ∫ ω, S n ω ∂μ := by
  have h := share_eq_condExp_limit S hS hb n
  rw [integral_congr_ae h, integral_condExp (ℱ.le n)]

end Research.P4.Martingale
