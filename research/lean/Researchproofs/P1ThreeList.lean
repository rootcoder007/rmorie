/-
  Researchproofs/P1ThreeList.lean

  P1, continued: what two or three lists can and cannot tell you.

  * petersen_ge_floor / chapman_ge_floor: the Lincoln–Petersen and
    Chapman estimators never fall below the logical floor
    n₁ + n₂ − m (the number of distinct units actually seen), because
    n₁n₂/m − (n₁ + n₂ − m) = (n₁ − m)(n₂ − m)/m ≥ 0 and the Chapman
    analogue equals (n₁ − m)(n₂ − m)/(m + 1) (Lohr, Sampling: Design and
    Analysis, §13.1). A Wald interval can and does cross the floor; the
    estimators themselves cannot.

  * three_list_saturated_fits: with three lists, the saturated
    log-linear model (eight parameters, including the three-way
    interaction) reproduces ANY positive table of eight cells exactly.
    Hence (missing_cell_unconstrained) the unobserved cell — the count
    of units on no list — can be set to any positive value while the
    seven observed cells are matched exactly. The three-way interaction
    is never identified from the observed cells, and the population size
    N = observed + missing ranges over (observed, ∞). What identifies N is
    the analyst's decision to set that interaction to zero (Lohr §13.2:
    "in none of these models can we test the hypothesis that the missing
    cell follows the model").
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P1

/-- Lincoln–Petersen minus the floor is a non-negative quantity. -/
theorem petersen_ge_floor (n₁ n₂ m : ℝ) (hm : 0 < m) (h1 : m ≤ n₁) (h2 : m ≤ n₂) :
    n₁ + n₂ - m ≤ n₁ * n₂ / m := by
  have key : n₁ * n₂ / m - (n₁ + n₂ - m) = (n₁ - m) * (n₂ - m) / m := by
    field_simp; ring
  have : 0 ≤ (n₁ - m) * (n₂ - m) / m := by
    apply div_nonneg _ hm.le
    exact mul_nonneg (by linarith) (by linarith)
  linarith

/-- Chapman's estimator (n₁+1)(n₂+1)/(m+1) − 1 is also at least the floor. -/
theorem chapman_ge_floor (n₁ n₂ m : ℝ) (hm : 0 ≤ m) (h1 : m ≤ n₁) (h2 : m ≤ n₂) :
    n₁ + n₂ - m ≤ (n₁ + 1) * (n₂ + 1) / (m + 1) - 1 := by
  have hm1 : 0 < m + 1 := by linarith
  have key : (n₁ + 1) * (n₂ + 1) / (m + 1) - 1 - (n₁ + n₂ - m) = (n₁ - m) * (n₂ - m) / (m + 1) := by
    field_simp; ring
  have : 0 ≤ (n₁ - m) * (n₂ - m) / (m + 1) := by
    apply div_nonneg _ hm1.le
    exact mul_nonneg (by linarith) (by linarith)
  linarith

/-- Saturated log-linear parameters for three lists. -/
structure LogLinear3 where
  (u0 u1 u2 u3 u12 u13 u23 u123 : ℝ)

/-- Expected cell count at membership pattern `(a, b, c)`, each 0 or 1 (as reals). -/
def LogLinear3.cell (u : LogLinear3) (a b c : ℝ) : ℝ :=
  Real.exp (u.u0 + a * u.u1 + b * u.u2 + c * u.u3 + a * b * u.u12 + a * c * u.u13 + b * c * u.u23
    + a * b * c * u.u123)

/-- The parameters that reproduce a positive table `m` exactly (its Walsh–Hadamard transform). -/
def LogLinear3.ofTable (m : ℝ → ℝ → ℝ → ℝ) : LogLinear3 where
  u0 := Real.log (m 0 0 0)
  u1 := Real.log (m 1 0 0) - Real.log (m 0 0 0)
  u2 := Real.log (m 0 1 0) - Real.log (m 0 0 0)
  u3 := Real.log (m 0 0 1) - Real.log (m 0 0 0)
  u12 := Real.log (m 1 1 0) - Real.log (m 1 0 0) - Real.log (m 0 1 0) + Real.log (m 0 0 0)
  u13 := Real.log (m 1 0 1) - Real.log (m 1 0 0) - Real.log (m 0 0 1) + Real.log (m 0 0 0)
  u23 := Real.log (m 0 1 1) - Real.log (m 0 1 0) - Real.log (m 0 0 1) + Real.log (m 0 0 0)
  u123 := Real.log (m 1 1 1) - Real.log (m 1 1 0) - Real.log (m 1 0 1) - Real.log (m 0 1 1)
    + Real.log (m 1 0 0) + Real.log (m 0 1 0) + Real.log (m 0 0 1) - Real.log (m 0 0 0)

/-- The saturated model fits every positive eight-cell table exactly. -/
theorem three_list_saturated_fits (m : ℝ → ℝ → ℝ → ℝ)
    (hpos : ∀ a b c, (a = 0 ∨ a = 1) → (b = 0 ∨ b = 1) → (c = 0 ∨ c = 1) → 0 < m a b c) :
    ∀ a b c, (a = 0 ∨ a = 1) → (b = 0 ∨ b = 1) → (c = 0 ∨ c = 1) →
      (LogLinear3.ofTable m).cell a b c = m a b c := by
  intro a b c ha hb hc
  have p000 := hpos 0 0 0 (Or.inl rfl) (Or.inl rfl) (Or.inl rfl)
  have p100 := hpos 1 0 0 (Or.inr rfl) (Or.inl rfl) (Or.inl rfl)
  have p010 := hpos 0 1 0 (Or.inl rfl) (Or.inr rfl) (Or.inl rfl)
  have p001 := hpos 0 0 1 (Or.inl rfl) (Or.inl rfl) (Or.inr rfl)
  have p110 := hpos 1 1 0 (Or.inr rfl) (Or.inr rfl) (Or.inl rfl)
  have p101 := hpos 1 0 1 (Or.inr rfl) (Or.inl rfl) (Or.inr rfl)
  have p011 := hpos 0 1 1 (Or.inl rfl) (Or.inr rfl) (Or.inr rfl)
  have p111 := hpos 1 1 1 (Or.inr rfl) (Or.inr rfl) (Or.inr rfl)
  dsimp only [LogLinear3.cell, LogLinear3.ofTable]
  rcases ha with rfl | rfl <;> rcases hb with rfl | rfl <;> rcases hc with rfl | rfl
  all_goals first
    | (convert Real.exp_log p000 using 2 <;> ring1)
    | (convert Real.exp_log p100 using 2 <;> ring1)
    | (convert Real.exp_log p010 using 2 <;> ring1)
    | (convert Real.exp_log p001 using 2 <;> ring1)
    | (convert Real.exp_log p110 using 2 <;> ring1)
    | (convert Real.exp_log p101 using 2 <;> ring1)
    | (convert Real.exp_log p011 using 2 <;> ring1)
    | (convert Real.exp_log p111 using 2 <;> ring1)

/-- The unobserved cell is unconstrained: for any positive value `x` there is a saturated model
matching the seven observed cells and putting `x` in the missing cell. -/
theorem missing_cell_unconstrained (obs : ℝ → ℝ → ℝ → ℝ)
    (hobs : ∀ a b c, (a = 0 ∨ a = 1) → (b = 0 ∨ b = 1) → (c = 0 ∨ c = 1) → ¬(a = 0 ∧ b = 0 ∧ c = 0) → 0 < obs a b c)
    (x : ℝ) (hx : 0 < x) :
    ∃ u : LogLinear3, u.cell 0 0 0 = x ∧
      ∀ a b c, (a = 0 ∨ a = 1) → (b = 0 ∨ b = 1) → (c = 0 ∨ c = 1) → ¬(a = 0 ∧ b = 0 ∧ c = 0) →
        u.cell a b c = obs a b c := by
  let m : ℝ → ℝ → ℝ → ℝ := fun a b c => if a = 0 ∧ b = 0 ∧ c = 0 then x else obs a b c
  have hpos : ∀ a b c, (a = 0 ∨ a = 1) → (b = 0 ∨ b = 1) → (c = 0 ∨ c = 1) → 0 < m a b c := by
    intro a b c ha hb hc
    by_cases h : a = 0 ∧ b = 0 ∧ c = 0
    · simp [m, h, hx]
    · simp only [m, if_neg h]; exact hobs a b c ha hb hc h
  refine ⟨LogLinear3.ofTable m, ?_, ?_⟩
  · rw [three_list_saturated_fits m hpos 0 0 0 (Or.inl rfl) (Or.inl rfl) (Or.inl rfl)]
    simp [m]
  · intro a b c ha hb hc hne
    rw [three_list_saturated_fits m hpos a b c ha hb hc]
    simp only [m, if_neg hne]

end Research.P1
