/-
  Researchproofs/P1Comparable.lean

  P1, continued: the recording rate is identified only on the survey-comparable
  subset (library scan A14; Patrick, *How Countries Count Crime*, ch. 14).

  A victimisation survey covers an offence set A (its "comparable" offences);
  the police record covers A and the rest, B. Writing V for true offences and
  R for recorded ones, split by set:

      V = VA + VB,   R = RA + RB,   RA ≤ VA,   RB ≤ VB  (a record is an offence).

  The survey identifies VA, so the recording rate on A, rA = RA / VA, is
  identified (`rate_on_A`). VB is never observed: only RB ≤ VB is known.

    * `total_rate_le`: the overall recording rate r = R / V is at most
      R / (VA + RB), attained when every non-comparable offence is recorded.
    * `total_rate_unbounded_below`: for any ε > 0 there is a VB making r < ε
      (non-comparable offences may be as under-recorded as one likes), so
      without an assumption on B the total rate has no positive lower bound.
    * Under the assumption that the rate on B is at least a fraction λ ∈ (0, 1]
      of the rate on A (rB ≥ λ rA): `VB_le_of_rate_bound` gives VB ≤ RB / (λ rA),
      hence `total_interval`: the total count lies in
          [VA + RB, VA + RB / (λ rA)]
      and the dark figure V − R in [VA − RA, VA − RA + RB (1/(λ rA) − 1)].
    * `dark_figure_width`: the width of that interval is RB (1 − λ rA)/(λ rA):
      it vanishes when λ rA = 1 (B recorded as fully as A, and A fully), grows
      without bound as λ rA → 0, and is proportional to RB, the recorded mass
      outside the survey's reach. So a survey that covers a share of recorded
      crime RA / R pins the dark figure to within RB/(λ rA) of itself.

  Everything is algebra over ℝ with positivity side conditions.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P1.Comparable

/-- The counts of a survey/record pair split by the survey's offence set `A` and the rest `B`. -/
structure Split where
  VA : ℝ
  VB : ℝ
  RA : ℝ
  RB : ℝ
  VA_pos : 0 < VA
  VB_nonneg : 0 ≤ VB
  RA_nonneg : 0 ≤ RA
  RB_nonneg : 0 ≤ RB
  RA_le : RA ≤ VA
  RB_le : RB ≤ VB

/-- The recording rate on the comparable set: identified from the survey's `VA`. -/
def rateA (s : Split) : ℝ := s.RA / s.VA

/-- The overall recording rate `R / V`. -/
def rate (s : Split) : ℝ := (s.RA + s.RB) / (s.VA + s.VB)

/-- The dark figure `V − R`. -/
def dark (s : Split) : ℝ := (s.VA + s.VB) - (s.RA + s.RB)

theorem rateA_mem (s : Split) : 0 ≤ rateA s ∧ rateA s ≤ 1 := by
  unfold rateA
  constructor
  · exact div_nonneg s.RA_nonneg s.VA_pos.le
  · rw [div_le_one s.VA_pos]; exact s.RA_le

/-- The rate on `A` depends on `VA` and `RA` only: two splits that agree there agree on it,
whatever `VB` and `RB` are. -/
theorem rate_on_A (s t : Split) (hV : s.VA = t.VA) (hR : s.RA = t.RA) : rateA s = rateA t := by
  unfold rateA; rw [hV, hR]

/-- The total rate is at most `R / (VA + RB)`: the case where every non-comparable offence is
recorded. -/
theorem total_rate_le (s : Split) : rate s ≤ (s.RA + s.RB) / (s.VA + s.RB) := by
  unfold rate
  have h1 : 0 < s.VA + s.RB := by linarith [s.VA_pos, s.RB_nonneg]
  have h2 : 0 < s.VA + s.VB := by linarith [s.VA_pos, s.VB_nonneg]
  apply div_le_div_of_nonneg_left (by linarith [s.RA_nonneg, s.RB_nonneg]) h1
  linarith [s.RB_le]

/-- Without an assumption on `B`, the total rate has no positive lower bound: for every ε > 0
there is a split with the same `VA`, `RA`, `RB` and a total rate below ε. -/
theorem total_rate_unbounded_below (s : Split) (ε : ℝ) (hε : 0 < ε) :
    ∃ t : Split, t.VA = s.VA ∧ t.RA = s.RA ∧ t.RB = s.RB ∧ rate t < ε := by
  -- take VB large: VB = max(RB, (RA + RB)/ε)
  set VB' := max s.RB ((s.RA + s.RB) / ε + 1) with hVB
  have hVB_ge : s.RB ≤ VB' := le_max_left _ _
  have hVB_big : (s.RA + s.RB) / ε + 1 ≤ VB' := le_max_right _ _
  refine ⟨⟨s.VA, VB', s.RA, s.RB, s.VA_pos, le_trans s.RB_nonneg hVB_ge, s.RA_nonneg, s.RB_nonneg,
           s.RA_le, hVB_ge⟩, rfl, rfl, rfl, ?_⟩
  unfold rate
  simp only
  have hden : 0 < s.VA + VB' := by linarith [s.VA_pos, le_trans s.RB_nonneg hVB_ge]
  rw [div_lt_iff₀ hden]
  have : (s.RA + s.RB) / ε ≤ VB' - 1 := by linarith
  have h3 : s.RA + s.RB ≤ ε * (VB' - 1) := by
    rw [div_le_iff₀ hε] at this; linarith
  nlinarith [s.VA_pos, hε]

/-- Under `rB ≥ λ rA` (the non-comparable offences are recorded at least a fraction λ as well
as the comparable ones), `VB ≤ RB / (λ rA)`. -/
theorem VB_le_of_rate_bound (s : Split) (lam : ℝ) (hlam : 0 < lam) (hrA : 0 < rateA s)
    (hB : s.VB = 0 ∨ lam * rateA s ≤ s.RB / s.VB) :
    s.VB ≤ s.RB / (lam * rateA s) := by
  have hpos : 0 < lam * rateA s := mul_pos hlam hrA
  rcases hB with h0 | h
  · rw [h0]; exact div_nonneg s.RB_nonneg hpos.le
  · rcases s.VB_nonneg.lt_or_eq with hVB | hVB
    · rw [le_div_iff₀ hpos]
      rw [le_div_iff₀ hVB] at h
      linarith
    · rw [← hVB]; exact div_nonneg s.RB_nonneg hpos.le

/-- The identified interval for the total count under `rB ≥ λ rA`: `[VA + RB, VA + RB/(λ rA)]`. -/
theorem total_interval (s : Split) (lam : ℝ) (hlam : 0 < lam) (hrA : 0 < rateA s)
    (hB : s.VB = 0 ∨ lam * rateA s ≤ s.RB / s.VB) :
    s.VA + s.RB ≤ s.VA + s.VB ∧ s.VA + s.VB ≤ s.VA + s.RB / (lam * rateA s) := by
  constructor
  · linarith [s.RB_le]
  · linarith [VB_le_of_rate_bound s lam hlam hrA hB]

/-- The dark figure lies in `[VA − RA, VA − RA + RB (1/(λ rA) − 1)]`. -/
theorem dark_interval (s : Split) (lam : ℝ) (hlam : 0 < lam) (hrA : 0 < rateA s)
    (hB : s.VB = 0 ∨ lam * rateA s ≤ s.RB / s.VB) :
    s.VA - s.RA ≤ dark s ∧ dark s ≤ s.VA - s.RA + s.RB * (1 / (lam * rateA s) - 1) := by
  unfold dark
  have h := VB_le_of_rate_bound s lam hlam hrA hB
  constructor
  · linarith [s.RB_le]
  · have : s.RB * (1 / (lam * rateA s) - 1) = s.RB / (lam * rateA s) - s.RB := by ring
    linarith

/-- The width of the dark-figure interval, `RB (1 − λ rA)/(λ rA)`: zero when `λ rA = 1`,
proportional to the recorded mass outside the survey's reach. -/
theorem dark_figure_width (s : Split) (lam : ℝ) (hpos : 0 < lam * rateA s) :
    (s.VA - s.RA + s.RB * (1 / (lam * rateA s) - 1)) - (s.VA - s.RA)
      = s.RB * (1 - lam * rateA s) / (lam * rateA s) := by
  have hne : lam * rateA s ≠ 0 := hpos.ne'
  obtain ⟨hl, hr⟩ := mul_ne_zero_iff.mp hne
  field_simp
  ring

theorem dark_figure_width_zero (s : Split) (lam : ℝ) (h1 : lam * rateA s = 1) :
    s.RB * (1 - lam * rateA s) / (lam * rateA s) = 0 := by
  rw [h1]; simp

end Research.P1.Comparable
