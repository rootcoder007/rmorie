/-
  Researchproofs/P16Backlog.lean

  P16 (new): court backlog — Little's law on a finite docket (Little 1961;
  the finite-horizon sample-path form in Stidham 1974).

  n cases arrive at times a_i and leave at times d_i with a_i ≤ d_i, all
  inside the window [0, T]. The number of pending cases at time t is
  N(t) = #{i : a_i ≤ t < d_i}. Then, with no probability at all,

      ∫_0^T N(t) dt = ∑_i (d_i − a_i)                       (occupancy_integral)

  so the time-average pending load L = (1/T) ∫ N equals λ W̄ with λ = n/T the
  arrival rate and W̄ the mean time to disposition (little). Conversely a
  court that reports a mean disposition time and a filing rate has reported
  its average backlog, whether or not it publishes one (little_backlog); and
  a backlog target L* with filing rate λ fixes the mean disposition time that
  achieves it, W̄ = L*/λ (little_target).

  The identity is about sample paths; the modelling assumption enters when
  the window is cut (cases still pending at T) or when W̄ is estimated from
  disposed cases only — that estimate is biased downward by truncation,
  which is a P1-type dark-figure problem, not a queueing one.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open MeasureTheory Finset

namespace Research.P16

/-- A finite docket: arrival and disposition times inside `[0, T]`. -/
structure Docket (n : ℕ) where
  (T : ℝ)
  (a d : Fin n → ℝ)
  (T_pos : 0 < T)
  (a_nonneg : ∀ i, 0 ≤ a i)
  (a_le_d : ∀ i, a i ≤ d i)
  (d_le_T : ∀ i, d i ≤ T)

variable {n : ℕ} (D : Docket n)

/-- Pending cases at time `t`. -/
def pending (t : ℝ) : ℝ := ∑ i, Set.indicator (Set.Ico (D.a i) (D.d i)) (fun _ => (1 : ℝ)) t
/-- Mean time to disposition. -/
def meanWait : ℝ := (∑ i, (D.d i - D.a i)) / n
/-- Arrival rate over the window. -/
def rate : ℝ := n / D.T
/-- Time-average pending load. -/
def load : ℝ := (∫ t in Set.Icc 0 D.T, pending D t) / D.T

theorem indicator_integral (i : Fin n) :
    ∫ t in Set.Icc 0 D.T, Set.indicator (Set.Ico (D.a i) (D.d i)) (fun _ => (1 : ℝ)) t = D.d i - D.a i := by
  have hsub : Set.Ico (D.a i) (D.d i) ⊆ Set.Icc 0 D.T := by
    intro t ht; exact ⟨(D.a_nonneg i).trans ht.1, ht.2.le.trans (D.d_le_T i)⟩
  rw [integral_indicator measurableSet_Ico, Measure.restrict_restrict measurableSet_Ico,
    Set.inter_eq_self_of_subset_left hsub, setIntegral_const]
  show (volume (Set.Ico (D.a i) (D.d i))).toReal • (1 : ℝ) = D.d i - D.a i
  rw [Real.volume_Ico, ENNReal.toReal_ofReal (sub_nonneg.2 (D.a_le_d i)), smul_eq_mul, mul_one]

/-- `∫_0^T N(t) dt = ∑ (d_i − a_i)`. -/
theorem occupancy_integral : ∫ t in Set.Icc 0 D.T, pending D t = ∑ i, (D.d i - D.a i) := by
  unfold pending
  rw [integral_finsetSum]
  · exact sum_congr rfl (fun i _ => indicator_integral D i)
  · intro i _
    have hfin : volume (Set.Icc (0 : ℝ) D.T) ≠ ⊤ := by rw [Real.volume_Icc]; exact ENNReal.ofReal_ne_top
    exact (integrableOn_const hfin).indicator measurableSet_Ico

/-- Little's law on the docket: `L = λ W̄`. -/
theorem little (hn : 0 < n) : load D = rate D * meanWait D := by
  unfold load rate meanWait
  rw [occupancy_integral]
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  field_simp

/-- A reported mean disposition time and filing rate determine the average backlog. -/
theorem little_backlog (hn : 0 < n) : (∫ t in Set.Icc 0 D.T, pending D t) = D.T * (rate D * meanWait D) := by
  have := little D hn
  unfold load at this
  have hT : D.T ≠ 0 := D.T_pos.ne'
  rw [← this]; field_simp

/-- A backlog target fixes the mean disposition time that achieves it. -/
theorem little_target (hn : 0 < n) (Lstar : ℝ) (h : load D = Lstar) : meanWait D = Lstar / rate D := by
  rw [← h, little D hn]
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have : rate D ≠ 0 := by unfold rate; exact div_ne_zero hn'.ne' D.T_pos.ne'
  field_simp

end Research.P16
