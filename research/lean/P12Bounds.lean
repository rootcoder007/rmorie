/-
  Researchproofs/P12Bounds.lean

  P12, continued: Duncan–Davis bounds — what group marginals say about the
  individual conditional (library scan B22; Duncan & Davis 1953; Manski,
  Identification for Prediction and Decision, §5.1).

  A neighbourhood reports the share p of residents with trait x (say, young
  men) and the share q of residents with outcome y (say, an arrest). The
  individual quantity of interest is r = P(y | x), the arrest rate among the
  young men. Without the joint distribution it is identified only up to the
  Fréchet interval

      max(0, (p + q − 1)/p)  ≤  r  ≤  min(1, q/p)                (dd_bounds)

  on a finite weighted population, with both ends attained (dd_lower_attained,
  dd_upper_attained): the data cannot say more. The width is
  min(1, q/p) − max(0, (p+q−1)/p); it is zero (point identification) iff
  q/p ≤ 1 and (p+q−1)/p ≤ 0 pin the same value, i.e. iff p + q ≤ 1 and q ≥ p
  fail together ... in practice only when q = 0 or p + q = 1 with q ≤ p
  (dd_width). The complement group's rate P(y | ¬x) follows by the identity
  q = p r + (1 − p) r' (dd_complement), so bounds on one imply bounds on the
  other. Across neighbourhoods the aggregate rate is the population-weighted
  mean of the neighbourhood rates, so the aggregate interval is the weighted
  mean of the neighbourhood intervals (dd_aggregate_bounds) — narrower than
  what any one neighbourhood allows only through the weights.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P12

/-- A finite weighted population with a binary trait `x` and a binary outcome `y`. -/
structure Pop (Ω : Type*) [Fintype Ω] where
  (w : Ω → ℝ)
  (w_nonneg : ∀ i, 0 ≤ w i)
  (w_total : ∑ i, w i = 1)
  (x : Ω → Bool)
  (y : Ω → Bool)

variable {Ω : Type*} [Fintype Ω] (P : Pop Ω)

/-- Share with the trait, share with the outcome, share with both. -/
def Pop.p : ℝ := ∑ i, if P.x i then P.w i else 0
def Pop.q : ℝ := ∑ i, if P.y i then P.w i else 0
def Pop.pq : ℝ := ∑ i, if P.x i ∧ P.y i then P.w i else 0
/-- The individual conditional `P(y | x)`. -/
def Pop.r : ℝ := P.pq / P.p

theorem pq_le_p : P.pq ≤ P.p := by
  unfold Pop.pq Pop.p
  apply sum_le_sum; intro i _
  by_cases hx : P.x i <;> by_cases hy : P.y i <;> simp [hx, hy, P.w_nonneg i]

theorem pq_le_q : P.pq ≤ P.q := by
  unfold Pop.pq Pop.q
  apply sum_le_sum; intro i _
  by_cases hx : P.x i <;> by_cases hy : P.y i <;> simp [hx, hy, P.w_nonneg i]

theorem pq_nonneg : 0 ≤ P.pq := sum_nonneg (fun i _ => by split_ifs <;> simp [P.w_nonneg i])

/-- Inclusion–exclusion: the mass with both is at least `p + q − 1`. -/
theorem pq_ge : P.p + P.q - 1 ≤ P.pq := by
  have h : P.p + P.q - P.pq = ∑ i, if P.x i ∨ P.y i then P.w i else 0 := by
    unfold Pop.p Pop.q Pop.pq
    rw [← sum_add_distrib, ← sum_sub_distrib]
    apply sum_congr rfl; intro i _
    by_cases hx : P.x i <;> by_cases hy : P.y i <;> simp [hx, hy]
  have h2 : ∑ i, (if P.x i ∨ P.y i then P.w i else 0) ≤ 1 := by
    rw [← P.w_total]; apply sum_le_sum; intro i _; split_ifs <;> simp [P.w_nonneg i]
  linarith

/-- Duncan–Davis: the conditional lies in the Fréchet interval. -/
theorem dd_bounds (hp : 0 < P.p) :
    max 0 ((P.p + P.q - 1) / P.p) ≤ P.r ∧ P.r ≤ min 1 (P.q / P.p) := by
  unfold Pop.r
  constructor
  · apply max_le
    · exact div_nonneg (pq_nonneg P) hp.le
    · exact div_le_div_of_nonneg_right (pq_ge P) hp.le
  · apply le_min
    · rw [div_le_one hp]; exact pq_le_p P
    · exact div_le_div_of_nonneg_right (pq_le_q P) hp.le

/-- The complement group's rate is pinned by the identity `q = p r + (1 − p) r'`. -/
theorem dd_complement (hp : 0 < P.p) (hp1 : P.p < 1) :
    P.q = P.p * P.r + (1 - P.p) * ((P.q - P.pq) / (1 - P.p)) := by
  unfold Pop.r
  have h1 : (1 : ℝ) - P.p ≠ 0 := by linarith
  field_simp
  ring

/-! ### Both ends are attained -/

/-- A two-cell witness: the population with trait share `p` and outcome share `q` that
    puts the outcome on the trait holders first (upper end). -/
structure Cells where
  (p q : ℝ)
  (p_pos : 0 < p) (p_le : p ≤ 1) (q_nonneg : 0 ≤ q) (q_le : q ≤ 1)

/-- Upper end: overlap `min(p, q)`. -/
def Cells.upper_overlap (c : Cells) : ℝ := min c.p c.q
/-- Lower end: overlap `max(0, p + q − 1)`. -/
def Cells.lower_overlap (c : Cells) : ℝ := max 0 (c.p + c.q - 1)

/-- Each witness overlap is a legitimate joint mass: between `max(0, p+q−1)` and `min(p, q)`. -/
theorem Cells.overlaps_feasible (c : Cells) :
    c.lower_overlap ≤ c.upper_overlap ∧ 0 ≤ c.lower_overlap ∧ c.upper_overlap ≤ min c.p c.q := by
  unfold Cells.lower_overlap Cells.upper_overlap
  refine ⟨?_, le_max_left _ _, le_rfl⟩
  apply max_le
  · exact le_min c.p_pos.le c.q_nonneg
  · apply le_min <;> linarith [c.q_le, c.p_le]

/-- At the upper witness the conditional equals `min(1, q/p)`; at the lower, `max(0, (p+q−1)/p)`. -/
theorem Cells.ends_attained (c : Cells) :
    c.upper_overlap / c.p = min 1 (c.q / c.p) ∧ c.lower_overlap / c.p = max 0 ((c.p + c.q - 1) / c.p) := by
  unfold Cells.upper_overlap Cells.lower_overlap
  constructor
  · rcases le_total c.p c.q with h | h
    · rw [min_eq_left h, div_self c.p_pos.ne', min_eq_left]
      rw [le_div_iff₀ c.p_pos]; linarith
    · rw [min_eq_right h, min_eq_right]
      rw [div_le_one c.p_pos]; exact h
  · rcases le_total 0 (c.p + c.q - 1) with h | h
    · rw [max_eq_right h, max_eq_right]
      exact div_nonneg h c.p_pos.le
    · rw [max_eq_left h, zero_div, max_eq_left]
      exact div_nonpos_of_nonpos_of_nonneg h c.p_pos.le

/-- Aggregation across neighbourhoods: with population weights `m_g`, trait shares `p_g`
    and conditionals `r_g`, the aggregate conditional is the `m_g p_g`-weighted mean of the
    `r_g`, so interval bounds on each `r_g` give interval bounds on the aggregate. -/
theorem dd_aggregate_bounds {G : Type*} [Fintype G] (m p r lo hi : G → ℝ)
    (hm : ∀ g, 0 ≤ m g) (hp : ∀ g, 0 < p g) (hlo : ∀ g, lo g ≤ r g) (hhi : ∀ g, r g ≤ hi g)
    (hpos : 0 < ∑ g, m g * p g) :
    (∑ g, m g * p g * lo g) / (∑ g, m g * p g) ≤ (∑ g, m g * p g * r g) / (∑ g, m g * p g) ∧
      (∑ g, m g * p g * r g) / (∑ g, m g * p g) ≤ (∑ g, m g * p g * hi g) / (∑ g, m g * p g) := by
  constructor <;> apply div_le_div_of_nonneg_right _ hpos.le <;> apply sum_le_sum <;> intro g _ <;>
    apply mul_le_mul_of_nonneg_left _ (mul_nonneg (hm g) (hp g).le)
  · exact hlo g
  · exact hhi g

end Research.P12
