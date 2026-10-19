/-
  IndigenousWellness/Causal.lean

  Belonging and mental health, person by person (Methods, the AIPW equation and the
  replicate variance; Results, the balance figures). R functions certified: `aipwcf()`
  (ATE, its population functional), `baltab()` (standardised difference, before and after
  inverse-probability weighting), `svydes(type = "bootstrap")` with `mse = TRUE` (the
  replicate variance `(1/B) ∑ (θ_b - θ)²`), and the unadjusted weighted difference of
  `imhperson.R`.

  On a finite probability space `Ω` with covariate cell `X`, binary `D` and potential
  outcomes `Y₁, Y₀` (observed `Y = D Y₁ + (1-D) Y₀`), unconfoundedness is stated as mean
  independence of `D` and each potential outcome within every cell, and overlap as
  `0 < e(x) < 1` on cells of positive probability.

    * `aipw_double_robust`: with working models `g₁, g₀, ê` (`0 < ê < 1`), the population AIPW
      functional `E[g₁ - g₀ + D(Y - g₁)/ê - (1-D)(Y - g₀)/(1-ê)]` equals `E[Y₁ - Y₀]` if either
      the outcome models are right on every cell or the propensity is right on every cell.
    * `ipw_ate`: the IPW functional (`g₁ = g₀ = 0`, true propensity) is `E[Y₁ - Y₀]`.
    * `ipw_balance`: weighting by `D/e(X)` and `(1-D)/(1-e(X))` gives every function of the
      covariates its population mean in both groups, so the weighted standardised difference
      has zero population numerator.
    * `aipw_const_eq_unadjusted`: in the sample, AIPW with constant outcome models equal to the
      weighted group means (any constant ê) is exactly the unadjusted weighted difference.
    * `repvar_*`: the replicate variance is ≥ 0, = 0 iff every replicate equals θ̂, and equals
      the centred variance plus the squared bias of the replicate mean (so `mse = TRUE` is at
      least the centred form).
    * `smd_affine`: the standardised difference `(m₁ - m₀)/sd₁` is unchanged by a positive
      affine recoding of a covariate and changes sign under a negative one.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace IndigenousWellness.Causal

/-! ### Population AIPW on a finite probability space -/

variable {Ω 𝒳 : Type*} [Fintype Ω] [Fintype 𝒳] [DecidableEq 𝒳]

/-- A finite probability space. -/
structure Prob (Ω : Type*) [Fintype Ω] where
  p : Ω → ℝ
  nonneg : ∀ ω, 0 ≤ p ω
  total : ∑ ω, p ω = 1

/-- Expectation. -/
def Prob.E (P : Prob Ω) (f : Ω → ℝ) : ℝ := ∑ ω, P.p ω * f ω

/-- A population: covariate cell, binary treatment (strong belonging), potential outcomes. -/
structure Pop (Ω 𝒳 : Type*) where
  X : Ω → 𝒳
  D : Ω → ℝ
  Y1 : Ω → ℝ
  Y0 : Ω → ℝ
  binD : ∀ ω, D ω = 0 ∨ D ω = 1

variable (P : Prob Ω) (M : Pop Ω 𝒳)

/-- Observed outcome. -/
def Pop.Y (ω : Ω) : ℝ := M.D ω * M.Y1 ω + (1 - M.D ω) * M.Y0 ω

/-- Indicator of the cell `X = x`. -/
def Pop.ind (x : 𝒳) (ω : Ω) : ℝ := if M.X ω = x then 1 else 0

/-- Cell moments. -/
def cellE (f : Ω → ℝ) (x : 𝒳) : ℝ := P.E fun ω => M.ind x ω * f ω

/-- `P(X = x)`. -/
def pi (x : 𝒳) : ℝ := cellE P M (fun _ => 1) x

/-- True propensity `e(x) = P(D = 1 | X = x)`. -/
def prop (x : 𝒳) : ℝ := cellE P M M.D x / pi P M x

/-- True outcome regressions `m_d(x) = E[Y_d | X = x]`. -/
def m1 (x : 𝒳) : ℝ := cellE P M M.Y1 x / pi P M x
def m0 (x : 𝒳) : ℝ := cellE P M M.Y0 x / pi P M x

/-- Unconfoundedness: within each cell, `D` is mean-independent of `Y₁` and of `Y₀`. -/
structure Unconf : Prop where
  u1 : ∀ x, cellE P M (fun ω => M.D ω * M.Y1 ω) x * pi P M x = cellE P M M.D x * cellE P M M.Y1 x
  u0 : ∀ x, cellE P M (fun ω => (1 - M.D ω) * M.Y0 ω) x * pi P M x =
    cellE P M (fun ω => 1 - M.D ω) x * cellE P M M.Y0 x

/-- Overlap on the support. -/
def Overlap : Prop := ∀ x, 0 < pi P M x → 0 < prop P M x ∧ prop P M x < 1

/-- The AIPW score with working models. -/
def aipw (g1 g0 eh : 𝒳 → ℝ) (ω : Ω) : ℝ :=
  g1 (M.X ω) - g0 (M.X ω) + M.D ω * (M.Y ω - g1 (M.X ω)) / eh (M.X ω) -
    (1 - M.D ω) * (M.Y ω - g0 (M.X ω)) / (1 - eh (M.X ω))

omit [Fintype 𝒳] in
theorem cellE_apply (f : Ω → ℝ) (x : 𝒳) : cellE P M f x = ∑ ω, P.p ω * (M.ind x ω * f ω) := rfl

omit [Fintype Ω] [Fintype 𝒳] in
theorem ind_nonneg (x : 𝒳) (ω : Ω) : 0 ≤ M.ind x ω := by
  unfold Pop.ind; split_ifs <;> norm_num

omit [Fintype 𝒳] in
theorem pi_nonneg (x : 𝒳) : 0 ≤ pi P M x := by
  unfold pi; rw [cellE_apply]
  exact sum_nonneg fun ω _ => mul_nonneg (P.nonneg ω) (by rw [mul_one]; exact ind_nonneg M x ω)

/-- Splitting an expectation over the covariate cells. -/
theorem E_split (f : Ω → ℝ) : P.E f = ∑ x, cellE P M f x := by
  simp only [cellE_apply, Prob.E, Pop.ind]
  rw [sum_comm]
  refine sum_congr rfl fun ω _ => ?_
  simp_rw [mul_left_comm (P.p ω), ite_mul, one_mul, zero_mul]
  rw [sum_ite_eq]
  simp

omit [Fintype 𝒳] in
theorem cellE_lin (x : 𝒳) (a b c d e : ℝ) (f₁ f₂ f₃ f₄ : Ω → ℝ) :
    cellE P M (fun ω => a + b * f₁ ω - c * f₂ ω - d * f₃ ω + e * f₄ ω) x =
      a * pi P M x + b * cellE P M f₁ x - c * cellE P M f₂ x - d * cellE P M f₃ x +
        e * cellE P M f₄ x := by
  unfold pi
  simp only [cellE_apply, mul_sum, ← sum_add_distrib, ← sum_sub_distrib]
  exact sum_congr rfl fun ω _ => by ring

omit [Fintype 𝒳] in
/-- On a null cell every cell moment vanishes. -/
theorem cellE_null {x : 𝒳} (h : pi P M x = 0) (f : Ω → ℝ) : cellE P M f x = 0 := by
  unfold pi at h
  rw [cellE_apply] at h ⊢
  have hz := (sum_eq_zero_iff_of_nonneg fun ω _ =>
    mul_nonneg (P.nonneg ω) (by rw [mul_one]; exact ind_nonneg M x ω)).mp h
  refine sum_eq_zero fun ω hω => ?_
  have := hz ω hω
  rw [mul_one] at this
  rw [← mul_assoc, this, zero_mul]

omit [Fintype 𝒳] in
theorem cellE_compl (x : 𝒳) : cellE P M (fun ω => 1 - M.D ω) x = pi P M x - cellE P M M.D x := by
  unfold pi; simp only [cellE_apply, ← sum_sub_distrib]; exact sum_congr rfl fun ω _ => by ring

omit [Fintype 𝒳] in
theorem cellE_sub (f g : Ω → ℝ) (x : 𝒳) :
    cellE P M (fun ω => f ω - g ω) x = cellE P M f x - cellE P M g x := by
  simp only [cellE_apply, ← sum_sub_distrib]; exact sum_congr rfl fun ω _ => by ring

omit [Fintype 𝒳] in
/-- Inside a cell a function of the covariates is a constant. -/
theorem cellE_const (k : Ω → ℝ) (c : 𝒳 → ℝ) (x : 𝒳) :
    cellE P M (fun ω => c (M.X ω) * k ω) x = c x * cellE P M k x := by
  simp only [cellE_apply, mul_sum]
  refine sum_congr rfl fun ω _ => ?_
  unfold Pop.ind; split_ifs with hx
  · rw [hx]; ring
  · simp

omit [Fintype 𝒳] in
/-- Per-cell AIPW identity. -/
theorem aipw_cell (U : Unconf P M) (g1 g0 eh : 𝒳 → ℝ) (heh : ∀ x, 0 < eh x ∧ eh x < 1) (x : 𝒳)
    (hO : 0 < pi P M x → 0 < prop P M x ∧ prop P M x < 1)
    (h : 0 < pi P M x → (g1 x = m1 P M x ∧ g0 x = m0 P M x) ∨ eh x = prop P M x) :
    cellE P M (aipw M g1 g0 eh) x = cellE P M M.Y1 x - cellE P M M.Y0 x := by
  have he0 := (heh x).1.ne'
  have he1 : 1 - eh x ≠ 0 := by linarith [(heh x).2]
  -- inside the cell the working models are constants
  have key : cellE P M (aipw M g1 g0 eh) x =
      cellE P M (fun ω => (g1 x - g0 x) + (1 / eh x) * (M.D ω * M.Y1 ω) - (g1 x / eh x) * M.D ω -
        (1 / (1 - eh x)) * ((1 - M.D ω) * M.Y0 ω) + (g0 x / (1 - eh x)) * (1 - M.D ω)) x := by
    simp only [cellE_apply]
    refine sum_congr rfl fun ω _ => ?_
    unfold Pop.ind aipw Pop.Y
    split_ifs with hx
    · rw [hx]; rcases M.binD ω with hd | hd <;> rw [hd] <;> field_simp <;> ring
    · simp
  rw [key, cellE_lin, cellE_compl]
  have hU1 := U.u1 x
  have hU0 := U.u0 x
  rw [cellE_compl] at hU0
  rcases eq_or_lt_of_le (pi_nonneg P M x) with hπ | hπ
  · simp only [cellE_null P M hπ.symm, ← hπ]; ring
  · obtain ⟨hp0, hp1⟩ := hO hπ
    have hcase := h hπ
    unfold m1 m0 at hcase
    unfold prop at hp0 hp1 hcase
    generalize pi P M x = π at *
    generalize cellE P M M.D x = ν at *
    generalize cellE P M (fun ω => M.D ω * M.Y1 ω) x = A1 at *
    generalize cellE P M (fun ω => (1 - M.D ω) * M.Y0 ω) x = A0 at *
    generalize cellE P M M.Y1 x = B1 at *
    generalize cellE P M M.Y0 x = B0 at *
    have hπ' := hπ.ne'
    have hA1 : A1 = ν * B1 / π := by field_simp; linarith
    have hA0 : A0 = (π - ν) * B0 / π := by field_simp; linarith
    rcases hcase with ⟨h1, h2⟩ | h3
    · rw [h1, h2, hA1, hA0]; field_simp; ring
    · have hν : 0 < ν := by
        have := div_pos_iff.mp hp0
        rcases this with ⟨h, _⟩ | ⟨_, h⟩
        · exact h
        · linarith
      have hνπ : ν < π := by rwa [div_lt_one hπ] at hp1
      have : π - ν ≠ 0 := by linarith
      have : ν ≠ 0 := hν.ne'
      have : 1 - ν / π ≠ 0 := by
        rw [sub_ne_zero]; intro h0; rw [eq_div_iff hπ'] at h0; linarith
      rw [h3, hA1, hA0]
      field_simp; ring

/-- **Double robustness.** If either the outcome models or the propensity is right on every
cell of positive probability, the AIPW functional is the average treatment effect. -/
theorem aipw_double_robust (U : Unconf P M) (hO : Overlap P M) (g1 g0 eh : 𝒳 → ℝ)
    (heh : ∀ x, 0 < eh x ∧ eh x < 1)
    (h : (∀ x, 0 < pi P M x → g1 x = m1 P M x ∧ g0 x = m0 P M x) ∨
      (∀ x, 0 < pi P M x → eh x = prop P M x)) :
    P.E (aipw M g1 g0 eh) = P.E fun ω => M.Y1 ω - M.Y0 ω := by
  rw [E_split P M, E_split P M (fun ω => M.Y1 ω - M.Y0 ω)]
  refine sum_congr rfl fun x _ => ?_
  rw [cellE_sub]
  refine aipw_cell P M U g1 g0 eh heh x (hO x) fun hx => ?_
  rcases h with h | h
  · exact Or.inl (h x hx)
  · exact Or.inr (h x hx)

/-- Inverse probability weighting is the special case `g₁ = g₀ = 0`. -/
theorem ipw_ate (U : Unconf P M) (hO : Overlap P M) (heh : ∀ x, 0 < prop P M x ∧ prop P M x < 1) :
    P.E (aipw M (fun _ => 0) (fun _ => 0) (prop P M)) = P.E fun ω => M.Y1 ω - M.Y0 ω :=
  aipw_double_robust P M U hO _ _ _ heh (Or.inr fun _ _ => rfl)

/-- **IPW balance.** With the true propensity, `E[D f(X)/e(X)] = E[(1-D) f(X)/(1-e(X))] = E[f(X)]`
for every function of the covariates, so both weighted groups have the population covariate
means and the weighted standardised difference has zero population numerator. -/
theorem ipw_balance (hO : Overlap P M) (f : 𝒳 → ℝ) :
    P.E (fun ω => M.D ω * f (M.X ω) / prop P M (M.X ω)) = P.E (fun ω => f (M.X ω)) ∧
      P.E (fun ω => (1 - M.D ω) * f (M.X ω) / (1 - prop P M (M.X ω))) = P.E (fun ω => f (M.X ω)) := by
  have hf1 : ∀ x, cellE P M (fun ω => f (M.X ω)) x = f x * pi P M x := by
    intro x; unfold pi; rw [← cellE_const]; simp only [mul_one]
  constructor
  · rw [E_split P M, E_split P M (fun ω => f (M.X ω))]
    refine sum_congr rfl fun x _ => ?_
    have : (fun ω => M.D ω * f (M.X ω) / prop P M (M.X ω)) =
        fun ω => (fun y => f y / prop P M y) (M.X ω) * M.D ω := by
      funext ω; ring
    rw [this, cellE_const P M M.D (fun y => f y / prop P M y) x, hf1]
    rcases eq_or_lt_of_le (pi_nonneg P M x) with hπ | hπ
    · rw [← hπ, cellE_null P M hπ.symm]; ring
    · have h0 := (hO x hπ).1
      unfold prop at h0 ⊢
      have hD : cellE P M M.D x ≠ 0 := by
        intro h; rw [h, zero_div] at h0; exact lt_irrefl 0 h0
      have := hπ.ne'
      field_simp
  · rw [E_split P M, E_split P M (fun ω => f (M.X ω))]
    refine sum_congr rfl fun x _ => ?_
    have : (fun ω => (1 - M.D ω) * f (M.X ω) / (1 - prop P M (M.X ω))) =
        fun ω => (fun y => f y / (1 - prop P M y)) (M.X ω) * (1 - M.D ω) := by
      funext ω; ring
    rw [this, cellE_const P M (fun ω => 1 - M.D ω) (fun y => f y / (1 - prop P M y)) x, hf1, cellE_compl]
    rcases eq_or_lt_of_le (pi_nonneg P M x) with hπ | hπ
    · rw [← hπ, cellE_null P M hπ.symm]; ring
    · have h1 := (hO x hπ).2
      unfold prop at h1 ⊢
      have hπ' := hπ.ne'
      have hlt : cellE P M M.D x < pi P M x := by rwa [div_lt_one hπ] at h1
      have h2 : pi P M x - cellE P M M.D x ≠ 0 := by linarith
      have h3 : 1 - cellE P M M.D x / pi P M x ≠ 0 := by linarith
      field_simp

/-! ### Sample identities -/

variable {ι : Type*}

/-- AIPW with constant outcome models equal to the weighted group means is the unadjusted
weighted difference, for any constant propensity `c` with `c ≠ 0, 1`. -/
theorem aipw_const_eq_unadjusted (s : Finset ι) (w D Y : ι → ℝ) {c : ℝ} (hc0 : c ≠ 0) (hc1 : 1 - c ≠ 0)
    (hW : ∑ i ∈ s, w i ≠ 0) (h1 : ∑ i ∈ s, w i * D i ≠ 0) (h0 : ∑ i ∈ s, w i * (1 - D i) ≠ 0) :
    let g1 := (∑ i ∈ s, w i * D i * Y i) / ∑ i ∈ s, w i * D i
    let g0 := (∑ i ∈ s, w i * (1 - D i) * Y i) / ∑ i ∈ s, w i * (1 - D i)
    (∑ i ∈ s, w i * (g1 - g0 + D i * (Y i - g1) / c - (1 - D i) * (Y i - g0) / (1 - c))) /
        ∑ i ∈ s, w i = g1 - g0 := by
  intro g1 g0
  have e : ∑ i ∈ s, w i * (g1 - g0 + D i * (Y i - g1) / c - (1 - D i) * (Y i - g0) / (1 - c)) =
      (g1 - g0) * ∑ i ∈ s, w i + (1 / c) * (∑ i ∈ s, w i * D i * Y i - g1 * ∑ i ∈ s, w i * D i) -
        (1 / (1 - c)) * (∑ i ∈ s, w i * (1 - D i) * Y i - g0 * ∑ i ∈ s, w i * (1 - D i)) := by
    simp only [mul_sum, ← sum_sub_distrib, ← sum_add_distrib]
    exact sum_congr rfl fun i _ => by field_simp
  have z1 : ∑ i ∈ s, w i * D i * Y i - g1 * ∑ i ∈ s, w i * D i = 0 := by
    simp only [g1]; field_simp; ring
  have z0 : ∑ i ∈ s, w i * (1 - D i) * Y i - g0 * ∑ i ∈ s, w i * (1 - D i) = 0 := by
    simp only [g0]; field_simp; ring
  rw [e, z1, z0]; field_simp; ring

/-- Replicate variance `(1/B) ∑ (θ_b - θ)²` (`svydes(type = "bootstrap")`, `mse = TRUE`). -/
def repvar (s : Finset ι) (θb : ι → ℝ) (θ : ℝ) : ℝ := (∑ b ∈ s, (θb b - θ) ^ 2) / s.card

theorem repvar_nonneg (s : Finset ι) (θb : ι → ℝ) (θ : ℝ) : 0 ≤ repvar s θb θ :=
  div_nonneg (sum_nonneg fun _ _ => sq_nonneg _) (Nat.cast_nonneg _)

theorem repvar_eq_zero_iff {s : Finset ι} (hs : s.Nonempty) (θb : ι → ℝ) (θ : ℝ) :
    repvar s θb θ = 0 ↔ ∀ b ∈ s, θb b = θ := by
  unfold repvar
  have hc : (s.card : ℝ) ≠ 0 := by exact_mod_cast hs.card_pos.ne'
  rw [div_eq_zero_iff, or_iff_left hc, sum_eq_zero_iff_of_nonneg fun b _ => sq_nonneg _]
  simp only [sq_eq_zero_iff, sub_eq_zero]

/-- `mse = TRUE` is the centred variance plus the squared bias of the replicate mean. -/
theorem repvar_decomp {s : Finset ι} (hs : s.Nonempty) (θb : ι → ℝ) (θ : ℝ) :
    repvar s θb θ = repvar s θb ((∑ b ∈ s, θb b) / s.card) + ((∑ b ∈ s, θb b) / s.card - θ) ^ 2 := by
  unfold repvar
  have hc : (s.card : ℝ) ≠ 0 := by exact_mod_cast hs.card_pos.ne'
  set m := (∑ b ∈ s, θb b) / s.card with hm
  have hsum : ∑ b ∈ s, θb b = m * s.card := by rw [hm]; field_simp
  have : ∀ b ∈ s, (θb b - θ) ^ 2 = (θb b - m) ^ 2 + 2 * (m - θ) * θb b - 2 * (m - θ) * m + (m - θ) ^ 2 := by
    intro b _; ring
  rw [sum_congr rfl this, sum_add_distrib, sum_sub_distrib, sum_add_distrib, ← mul_sum, hsum]
  simp only [sum_const, nsmul_eq_mul]
  field_simp; ring

/-! ### The standardised difference of `baltab()` -/

/-- Weighted mean. -/
def wmean (s : Finset ι) (w x : ι → ℝ) : ℝ := (∑ i ∈ s, w i * x i) / ∑ i ∈ s, w i

/-- Unweighted sample standard deviation (`stats::sd`). -/
def sd (s : Finset ι) (x : ι → ℝ) : ℝ :=
  Real.sqrt ((∑ i ∈ s, (x i - (∑ j ∈ s, x j) / s.card) ^ 2) / (s.card - 1))

/-- `smd = (m₁ - m₀) / sd₁`: weighted means of the two groups over the treated group's sd. -/
def smd (s1 s0 : Finset ι) (w x : ι → ℝ) : ℝ := (wmean s1 w x - wmean s0 w x) / sd s1 x

theorem wmean_affine (s : Finset ι) (w x : ι → ℝ) (a b : ℝ) (hW : ∑ i ∈ s, w i ≠ 0) :
    wmean s w (fun i => a * x i + b) = a * wmean s w x + b := by
  unfold wmean
  have : ∑ i ∈ s, w i * (a * x i + b) = a * ∑ i ∈ s, w i * x i + b * ∑ i ∈ s, w i := by
    rw [mul_sum, mul_sum, ← sum_add_distrib]; exact sum_congr rfl fun i _ => by ring
  rw [this]; field_simp

theorem sd_affine (s : Finset ι) (x : ι → ℝ) (a b : ℝ) (hs : s.Nonempty) :
    sd s (fun i => a * x i + b) = |a| * sd s x := by
  unfold sd
  have hc : (s.card : ℝ) ≠ 0 := by exact_mod_cast hs.card_pos.ne'
  have hm : (∑ j ∈ s, (a * x j + b)) / s.card = a * ((∑ j ∈ s, x j) / s.card) + b := by
    rw [sum_add_distrib, ← mul_sum, sum_const, nsmul_eq_mul]; field_simp
  rw [hm]
  have : ∀ i ∈ s, (a * x i + b - (a * ((∑ j ∈ s, x j) / s.card) + b)) ^ 2 =
      a ^ 2 * (x i - (∑ j ∈ s, x j) / s.card) ^ 2 := by intro i _; ring
  rw [sum_congr rfl this, ← mul_sum, mul_div_assoc, Real.sqrt_mul (sq_nonneg a), Real.sqrt_sq_eq_abs]

/-- A positive affine recoding of a covariate leaves the standardised difference unchanged; a
negative one only flips its sign, so `|smd|` (what `baltab()` is read by) is invariant. -/
theorem smd_affine (s1 s0 : Finset ι) (w x : ι → ℝ) {a : ℝ} (b : ℝ) (ha : a ≠ 0) (hs : s1.Nonempty)
    (h1 : ∑ i ∈ s1, w i ≠ 0) (h0 : ∑ i ∈ s0, w i ≠ 0) :
    smd s1 s0 w (fun i => a * x i + b) = (a / |a|) * smd s1 s0 w x ∧
      |smd s1 s0 w (fun i => a * x i + b)| = |smd s1 s0 w x| := by
  have ha' : 0 < |a| := abs_pos.mpr ha
  have e : smd s1 s0 w (fun i => a * x i + b) = (a / |a|) * smd s1 s0 w x := by
    unfold smd
    rw [wmean_affine s1 w x a b h1, wmean_affine s0 w x a b h0, sd_affine s1 x a b hs]
    by_cases hsd : sd s1 x = 0
    · simp [hsd]
    · field_simp; ring
  refine ⟨e, ?_⟩
  rw [e, abs_mul, abs_div, abs_abs, div_self ha'.ne', one_mul]

end IndigenousWellness.Causal
