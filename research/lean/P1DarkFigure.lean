/-
  Researchproofs/P1DarkFigure.lean

  P1. The dark figure of crime: what the true count can be.

  Part A, two sources. Let N be the true number of events, and let two
  lists (police records, survey/hospital/NGO) catch each event with
  probabilities p₁, p₂, with a dependence factor θ so that an event is on
  both lists with probability p₁ p₂ θ (θ = 1 is independence, θ > 1
  positive dependence). Expected list sizes are n₁ = N p₁, n₂ = N p₂ and
  the expected overlap is m = N p₁ p₂ θ. Then

      N = θ · n₁ n₂ / m                       (petersen_identity)

  so independence identifies N as n₁ n₂ / m (Lincoln–Petersen), and if
  only θ ∈ [1/κ, κ] is credible, N lies in [n₁n₂/(κ m), κ n₁n₂/m] with
  both ends attained (petersen_bounds, petersen_bounds_sharp).

  Part B, one recorded rate and one survey. The recorded victimisation
  rate r is at most the true rate v (recording only loses events), and
  the survey rate is a noisy proxy v_obs = v(1−β) + (1−v)α with
  under-reporting β and over-reporting α in known boxes. Combining the
  monotone-recording bound with the P5 label-noise interval gives the
  sharp interval for v, and hence for the dark figure v − r:

      max(r, (v_obs − ᾱ)/(1−ᾱ)) ≤ v ≤ v_obs/(1−β̄)     (dark_figure_bounds)

  Lean checks the arithmetic. Whether two lists are independent, or what
  the misreporting boxes are, is a claim about the world.
-/
import Mathlib
import Researchproofs.P5Fairness

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P1

/-! ### Part A: two sources -/

/-- Expected list sizes and overlap from the true count and capture model. -/
structure TwoSource where
  N : ℝ
  p1 : ℝ
  p2 : ℝ
  θ : ℝ
  N_pos : 0 < N
  p1_pos : 0 < p1
  p2_pos : 0 < p2
  θ_pos : 0 < θ

namespace TwoSource

variable (S : TwoSource)

def n1 : ℝ := S.N * S.p1
def n2 : ℝ := S.N * S.p2
def m : ℝ := S.N * S.p1 * S.p2 * S.θ

theorem m_pos : 0 < S.m := by unfold m; have := S.N_pos; have := S.p1_pos; have := S.p2_pos; have := S.θ_pos; positivity

/-- The true count from expected list sizes and overlap: `N = θ n₁ n₂ / m`. -/
theorem petersen_identity : S.N = S.θ * S.n1 * S.n2 / S.m := by
  unfold n1 n2 m
  have := S.N_pos; have := S.p1_pos; have := S.p2_pos; have := S.θ_pos
  field_simp

/-- Independence (`θ = 1`) identifies the true count as the Lincoln–Petersen ratio. -/
theorem lincoln_petersen (h : S.θ = 1) : S.N = S.n1 * S.n2 / S.m := by
  rw [S.petersen_identity, h, one_mul]

/-- With `θ ∈ [1/κ, κ]` the true count lies in `[n₁n₂/(κ m), κ n₁n₂/m]`. -/
theorem petersen_bounds (κ : ℝ) (hκ : 1 ≤ κ) (hlo : 1 / κ ≤ S.θ) (hhi : S.θ ≤ κ) :
    S.n1 * S.n2 / (κ * S.m) ≤ S.N ∧ S.N ≤ κ * S.n1 * S.n2 / S.m := by
  have hm := S.m_pos
  have hκ0 : 0 < κ := by linarith
  have hnn : 0 < S.n1 * S.n2 := by unfold n1 n2; have := S.N_pos; have := S.p1_pos; have := S.p2_pos; positivity
  rw [S.petersen_identity]
  constructor
  · rw [div_le_div_iff₀ (by positivity) hm]
    have h1 : 1 ≤ S.θ * κ := by
      rw [div_le_iff₀ hκ0] at hlo; linarith
    have key : S.n1 * S.n2 * S.m * 1 ≤ S.n1 * S.n2 * S.m * (S.θ * κ) :=
      mul_le_mul_of_nonneg_left h1 (by positivity)
    have e2 : S.θ * S.n1 * S.n2 * (κ * S.m) = S.n1 * S.n2 * S.m * (S.θ * κ) := by ring
    rw [e2]
    simpa using key
  · rw [div_le_div_iff₀ hm hm]
    have : S.θ * (S.n1 * S.n2) ≤ κ * (S.n1 * S.n2) := mul_le_mul_of_nonneg_right hhi hnn.le
    nlinarith [mul_pos hm hnn]

end TwoSource

/-- Sharpness: the lower end is attained at `θ = 1/κ`. -/
theorem petersen_lower_attained (N p1 p2 κ : ℝ) (hN : 0 < N) (h1 : 0 < p1) (h2 : 0 < p2) (hκ : 0 < κ) :
    let S : TwoSource := ⟨N, p1, p2, 1 / κ, hN, h1, h2, by positivity⟩
    S.n1 * S.n2 / (κ * S.m) = S.N := by
  intro S
  simp only [S, TwoSource.n1, TwoSource.n2, TwoSource.m]
  field_simp

/-- Sharpness: the upper end is attained at `θ = κ`. -/
theorem petersen_upper_attained (N p1 p2 κ : ℝ) (hN : 0 < N) (h1 : 0 < p1) (h2 : 0 < p2) (hκ : 0 < κ) :
    let S : TwoSource := ⟨N, p1, p2, κ, hN, h1, h2, hκ⟩
    κ * S.n1 * S.n2 / S.m = S.N := by
  intro S
  simp only [S, TwoSource.n1, TwoSource.n2, TwoSource.m]
  field_simp

/-! ### Part B: recorded rate plus a noisy survey -/

open Research.P5 in
/-- Sharp interval for the true victimisation rate `v` from a recorded rate
`r ≤ v` and a survey rate `v_obs = v(1-β) + (1-v)α` with `α ∈ [0, αb]`,
`β ∈ [0, βb]`, `αb + βb < 1`. -/
theorem true_rate_bounds (v r α β αb βb : ℝ)
    (hr : r ≤ v) (hα0 : 0 ≤ α) (hαb : α ≤ αb) (hβ0 : 0 ≤ β) (hβb : β ≤ βb)
    (hsum : αb + βb < 1) (hv0 : 0 ≤ v) (hv1 : v ≤ 1) :
    max r ((observedRate v α β - αb) / (1 - αb)) ≤ v ∧ v ≤ observedRate v α β / (1 - βb) := by
  obtain ⟨hlo, hhi⟩ := true_base_rate_bounds v α β αb βb hα0 hαb hβ0 hβb hsum hv0 hv1
  exact ⟨max_le hr hlo, hhi⟩

open Research.P5 in
/-- The dark figure `v − r` is bounded by the same interval shifted by `r`. -/
theorem dark_figure_bounds (v r α β αb βb : ℝ)
    (hr : r ≤ v) (hα0 : 0 ≤ α) (hαb : α ≤ αb) (hβ0 : 0 ≤ β) (hβb : β ≤ βb)
    (hsum : αb + βb < 1) (hv0 : 0 ≤ v) (hv1 : v ≤ 1) :
    max 0 ((observedRate v α β - αb) / (1 - αb) - r) ≤ v - r
      ∧ v - r ≤ observedRate v α β / (1 - βb) - r := by
  obtain ⟨hlo, hhi⟩ := true_rate_bounds v r α β αb βb hr hα0 hαb hβ0 hβb hsum hv0 hv1
  constructor
  · apply max_le
    · linarith
    · have := le_max_right r ((observedRate v α β - αb) / (1 - αb)); linarith
  · linarith

end Research.P1
