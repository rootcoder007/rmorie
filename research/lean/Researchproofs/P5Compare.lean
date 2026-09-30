/-
  Researchproofs/P5Compare.lean

  P5, continued: which group comparisons survive label noise?

  With the sharp intervals of P5Fairness for two groups' true base rates,
  a comparison is decidable exactly when the intervals do not overlap:

    * compare_decided: if A's upper bound is below B's lower bound, then
      p_A < p_B for every admissible pair of noise rates.
    * compare_undecided: if the intervals overlap, there are admissible
      noise pairs under which p_A < p_B and others under which p_A > p_B
      — so no amount of data settles the order without narrower noise
      boxes. Built from the attained-bound theorems, so it is sharpness
      turned into a decision rule.

  The recorded rates are the inputs; the noise boxes are the analyst's.
-/
import Mathlib
import Researchproofs.P5Fairness

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P5

/-- Lower bound of the identified interval from a recorded rate. -/
def lowerBound (pobs αb : ℝ) : ℝ := (pobs - αb) / (1 - αb)
/-- Upper bound of the identified interval from a recorded rate. -/
def upperBound (pobs βb : ℝ) : ℝ := pobs / (1 - βb)

/-- Decided comparison: disjoint intervals order the true rates for every admissible noise. -/
theorem compare_decided (pA pB αA βA αB βB αb βb : ℝ)
    (hαA0 : 0 ≤ αA) (hαAb : αA ≤ αb) (hβA0 : 0 ≤ βA) (hβAb : βA ≤ βb)
    (hαB0 : 0 ≤ αB) (hαBb : αB ≤ αb) (hβB0 : 0 ≤ βB) (hβBb : βB ≤ βb)
    (hsum : αb + βb < 1) (hA0 : 0 ≤ pA) (hA1 : pA ≤ 1) (hB0 : 0 ≤ pB) (hB1 : pB ≤ 1)
    (hgap : upperBound (observedRate pA αA βA) βb < lowerBound (observedRate pB αB βB) αb) :
    pA < pB := by
  obtain ⟨_, hAhi⟩ := true_base_rate_bounds pA αA βA αb βb hαA0 hαAb hβA0 hβAb hsum hA0 hA1
  obtain ⟨hBlo, _⟩ := true_base_rate_bounds pB αB βB αb βb hαB0 hαBb hβB0 hβBb hsum hB0 hB1
  unfold upperBound lowerBound at hgap
  linarith

/-- Undecided comparison: overlapping intervals admit noise pairs in either
order. Stated as an existence: given recorded rates `qA`, `qB` whose
intervals overlap, there are true rates and admissible noise pairs
reproducing those recorded rates with `pA < pB`, and others with `pA > pB`. -/
theorem compare_undecided (qA qB αb βb : ℝ) (hαb : 0 ≤ αb) (hβb : 0 ≤ βb) (hsum : αb + βb < 1)
    (hqA : αb ≤ qA) (hqA1 : qA ≤ 1 - βb) (hqB : αb ≤ qB) (hqB1 : qB ≤ 1 - βb)
    (hover1 : lowerBound qA αb < upperBound qB βb) (hover2 : lowerBound qB αb < upperBound qA βb) :
    (∃ pA pB : ℝ, observedRate pA αb 0 = qA ∧ observedRate pB 0 βb = qB ∧ pA < pB)
    ∧ (∃ pA pB : ℝ, observedRate pA 0 βb = qA ∧ observedRate pB αb 0 = qB ∧ pB < pA) := by
  have h1 : 1 - αb ≠ 0 := by linarith
  have h2 : 1 - βb ≠ 0 := by linarith
  constructor
  · refine ⟨lowerBound qA αb, upperBound qB βb, ?_, ?_, hover1⟩
    · unfold lowerBound observedRate; field_simp; ring
    · unfold upperBound observedRate; field_simp; ring
  · refine ⟨upperBound qA βb, lowerBound qB αb, ?_, ?_, hover2⟩
    · unfold upperBound observedRate; field_simp; ring
    · unfold lowerBound observedRate; field_simp; ring

end Research.P5
