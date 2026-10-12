/-
  Researchproofs/P4Hotspot.lean

  P4, continued: the hot-spot threshold of the Short–Brantingham
  reaction-diffusion model (Short et al. 2008, Short, Bertozzi and
  Brantingham 2010; D'Orsogna and Perc 2015, Eqs. 4-5). Scan item C8.

  The spatially uniform part of the model is

      B' = −ω B + ε D ρ (A₀ + B),        ρ' = −ρ (A₀ + B) + γ,

  with attractiveness A = A₀ + B, criminal density ρ, decay ω, repeat
  reinforcement ε, diffusion D, generation rate γ.

    * steady_iff: the homogeneous steady state exists, is unique, and is
      B̄ = ε D γ / ω, ρ̄ = γ / Ā with Ā = A₀ + B̄ (the scan's claim).
    * Linearising at wavenumber-squared k (the gradient term −2∇·(ρ∇A/A)
      contributes the k-dependent entry c₁ k = (2 D ρ̄ / Ā) k) gives a 2×2
      matrix M(k) = [[a − d₁ k, b], [c₀ + c₁ k, d − d₂ k]] with trace tr(k)
      and determinant det(k).  unstable_iff: when tr(k) < 0, a growing mode
      (a real eigenvalue σ > 0) exists iff det(k) < 0.
    * det_eq: det(k) = P k² − Q k + R with P = d₁ d₂, Q = a d₂ + d d₁ + b c₁,
      R = a d − b c₀.  stable_all: Q ≤ 0 and R > 0 give det(k) > 0 for every
      k ≥ 0 (no hot spots at any wavelength).  unstable_at_crit: Q > 0 and
      Q² > 4 P R give det < 0 at the critical wavenumber k* = Q / (2 P).
    * For the Short model: tr_neg (the trace is negative at every k ≥ 0,
      because ε D ρ̄ ≤ ω follows from Ā = A₀ + B̄), R_eq (R = ω Ā > 0 always)
      and Q_eq (Q = D (3 ε D ρ̄ − ω − η Ā / z)).  Hence the explicit
      threshold: no_hotspots_below_threshold says 3 ε D ρ̄ ≤ ω + η Ā / z
      makes the uniform state stable at every wavelength, and
      no_hotspots_without_reinforcement (ε = 0) refutes "hot spots for all
      parameters".  hotspots_above_threshold gives the growing mode when
      Q > 0 and Q² > 4 P R.

  Everything is finite algebra over ℝ: no analysis of the PDE is needed
  for the threshold, which is what the empirical use (placing an estimated
  (ω, η) in the stability diagram) requires.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P4.Hotspot

/-- The parameters of the Short–Brantingham model. -/
structure Params where
  A₀ : ℝ
  ω : ℝ
  ε : ℝ
  D : ℝ
  γ : ℝ
  η : ℝ
  z : ℝ
  A₀_pos : 0 < A₀
  ω_pos : 0 < ω
  ε_nonneg : 0 ≤ ε
  D_pos : 0 < D
  γ_pos : 0 < γ
  η_nonneg : 0 ≤ η
  z_pos : 0 < z

/-- `(B, ρ)` is a homogeneous steady state of the uniform system. -/
def IsSteady (p : Params) (B ρ : ℝ) : Prop :=
  -p.ω * B + p.ε * p.D * ρ * (p.A₀ + B) = 0 ∧ -ρ * (p.A₀ + B) + p.γ = 0

/-- The steady dynamic attractiveness `B̄ = ε D γ / ω`. -/
def Bbar (p : Params) : ℝ := p.ε * p.D * p.γ / p.ω

/-- The steady total attractiveness `Ā = A₀ + B̄`. -/
def Abar (p : Params) : ℝ := p.A₀ + Bbar p

/-- The steady criminal density `ρ̄ = γ / Ā`. -/
def ρbar (p : Params) : ℝ := p.γ / Abar p

theorem Bbar_nonneg (p : Params) : 0 ≤ Bbar p := by
  unfold Bbar
  have := p.ε_nonneg; have := p.D_pos; have := p.γ_pos; have := p.ω_pos
  positivity

theorem Abar_pos (p : Params) : 0 < Abar p := by
  unfold Abar; linarith [p.A₀_pos, Bbar_nonneg p]

theorem ρbar_mul_Abar (p : Params) : ρbar p * Abar p = p.γ := by
  unfold ρbar; exact div_mul_cancel₀ _ (Abar_pos p).ne'

theorem ρbar_pos (p : Params) : 0 < ρbar p := by
  unfold ρbar; exact div_pos p.γ_pos (Abar_pos p)

/-- The homogeneous steady state exists and is unique: `B̄ = ε D γ / ω`, `ρ̄ = γ / Ā`. -/
theorem steady_iff (p : Params) (B ρ : ℝ) :
    IsSteady p B ρ ↔ B = Bbar p ∧ ρ = ρbar p := by
  constructor
  · rintro ⟨h1, h2⟩
    have hρA : ρ * (p.A₀ + B) = p.γ := by linarith
    have hωB : p.ω * B = p.ε * p.D * p.γ := by
      have h1' : p.ε * p.D * (ρ * (p.A₀ + B)) = p.ω * B := by linarith [h1]
      rw [hρA] at h1'; linarith
    have hB : B = Bbar p := by
      unfold Bbar; rw [eq_div_iff p.ω_pos.ne']; linarith
    refine ⟨hB, ?_⟩
    have hne : p.A₀ + B ≠ 0 := by
      intro h0; rw [h0, mul_zero] at hρA; linarith [p.γ_pos]
    unfold ρbar Abar; rw [← hB, eq_div_iff hne]; exact hρA
  · rintro ⟨hB, hρ⟩
    subst hB; subst hρ
    have hA := ρbar_mul_Abar p
    unfold Abar at hA
    refine ⟨?_, ?_⟩
    · have hBω : p.ω * Bbar p = p.ε * p.D * p.γ := by
        unfold Bbar; have hω := p.ω_pos.ne'; field_simp
      calc -p.ω * Bbar p + p.ε * p.D * ρbar p * (p.A₀ + Bbar p)
          = -(p.ω * Bbar p) + p.ε * p.D * (ρbar p * (p.A₀ + Bbar p)) := by ring
        _ = 0 := by rw [hBω, hA]; ring
    · calc -ρbar p * (p.A₀ + Bbar p) + p.γ = -(ρbar p * (p.A₀ + Bbar p)) + p.γ := by ring
        _ = 0 := by rw [hA]; ring

/-! ### The linearised mode-k problem for a general two-field system -/

/-- The 2×2 linearisation at wavenumber-squared `k`:
`M(k) = [[a − d₁ k, b], [c₀ + c₁ k, d − d₂ k]]`. -/
structure Lin where
  a : ℝ
  b : ℝ
  c₀ : ℝ
  c₁ : ℝ
  d : ℝ
  d₁ : ℝ
  d₂ : ℝ

/-- Trace of `M(k)`. -/
def tr (L : Lin) (k : ℝ) : ℝ := (L.a - L.d₁ * k) + (L.d - L.d₂ * k)

/-- Determinant of `M(k)`: the dispersion relation's constant term. -/
def det (L : Lin) (k : ℝ) : ℝ := (L.a - L.d₁ * k) * (L.d - L.d₂ * k) - L.b * (L.c₀ + L.c₁ * k)

/-- `σ` is a growth rate of mode `k`: a root of the characteristic polynomial of `M(k)`. -/
def IsGrowth (L : Lin) (k σ : ℝ) : Prop := σ ^ 2 - tr L k * σ + det L k = 0

/-- The coefficients of `det(k) = P k² − Q k + R`. -/
def P (L : Lin) : ℝ := L.d₁ * L.d₂
def Q (L : Lin) : ℝ := L.a * L.d₂ + L.d * L.d₁ + L.b * L.c₁
def R (L : Lin) : ℝ := L.a * L.d - L.b * L.c₀

theorem det_eq (L : Lin) (k : ℝ) : det L k = P L * k ^ 2 - Q L * k + R L := by
  unfold det P Q R; ring

/-- With a negative trace, a growing mode (a positive real growth rate) exists
iff the determinant is negative. -/
theorem unstable_iff (L : Lin) (k : ℝ) (htr : tr L k < 0) :
    (∃ σ, 0 < σ ∧ IsGrowth L k σ) ↔ det L k < 0 := by
  constructor
  · rintro ⟨σ, hσ, hg⟩
    unfold IsGrowth at hg
    nlinarith [mul_pos hσ hσ, mul_neg_of_neg_of_pos htr hσ]
  · intro hdet
    set t := tr L k with ht
    set Δ := det L k with hΔ
    have hdisc : t ^ 2 < t ^ 2 - 4 * Δ := by linarith
    have hdisc0 : 0 ≤ t ^ 2 - 4 * Δ := by nlinarith [sq_nonneg t]
    set s := Real.sqrt (t ^ 2 - 4 * Δ) with hs
    have hs2 : s ^ 2 = t ^ 2 - 4 * Δ := Real.sq_sqrt hdisc0
    have hgt : -t < s := by
      rw [hs]; rw [Real.lt_sqrt (by linarith)]; nlinarith
    refine ⟨(t + s) / 2, by linarith, ?_⟩
    unfold IsGrowth
    rw [← ht, ← hΔ]
    linear_combination (1 / 4 : ℝ) * hs2

/-- `Q ≤ 0` and `R > 0`: the uniform state is stable at every wavelength. -/
theorem stable_all (L : Lin) (hP : 0 ≤ P L) (hQ : Q L ≤ 0) (hR : 0 < R L) :
    ∀ k, 0 ≤ k → 0 < det L k := by
  intro k hk
  rw [det_eq]
  nlinarith [mul_nonneg hP (sq_nonneg k), mul_nonneg (neg_nonneg.mpr hQ) hk]

/-- `Q > 0` and `Q² > 4 P R`: the determinant is negative at the critical
wavenumber `k* = Q / (2 P)`. -/
theorem unstable_at_crit (L : Lin) (hP : 0 < P L) (_hQ : 0 < Q L) (h : 4 * P L * R L < Q L ^ 2) :
    det L (Q L / (2 * P L)) < 0 := by
  rw [det_eq]
  have hPne : P L ≠ 0 := hP.ne'
  have : P L * (Q L / (2 * P L)) ^ 2 - Q L * (Q L / (2 * P L)) + R L
      = (4 * P L * R L - Q L ^ 2) / (4 * P L) := by
    field_simp; ring
  rw [this]
  exact div_neg_of_neg_of_pos (by linarith) (by positivity)

/-- The critical wavenumber is positive. -/
theorem crit_pos (L : Lin) (hP : 0 < P L) (hQ : 0 < Q L) : 0 < Q L / (2 * P L) :=
  div_pos hQ (by linarith)

/-! ### The Short–Brantingham linearisation -/

/-- The mode-k matrix of the Short–Brantingham model at its steady state:
`a = −ω + ε D ρ̄`, `b = ε D Ā`, `c₀ = −ρ̄`, `c₁ = 2 D ρ̄ / Ā`, `d = −Ā`,
`d₁ = η D / z`, `d₂ = D`. -/
def lin (p : Params) : Lin where
  a := -p.ω + p.ε * p.D * ρbar p
  b := p.ε * p.D * Abar p
  c₀ := -ρbar p
  c₁ := 2 * p.D * ρbar p / Abar p
  d := -Abar p
  d₁ := p.η * p.D / p.z
  d₂ := p.D

/-- `ε D ρ̄ ≤ ω`: reinforcement at the steady state never exceeds decay,
because `ω Ā = ω A₀ + ε D γ`. -/
theorem reinforcement_le (p : Params) : p.ε * p.D * ρbar p ≤ p.ω := by
  have hA := Abar_pos p
  have h1 : p.ε * p.D * ρbar p * Abar p = p.ε * p.D * p.γ := by
    rw [mul_assoc, ρbar_mul_Abar]
  have h2 : p.ω * Abar p = p.ω * p.A₀ + p.ε * p.D * p.γ := by
    unfold Abar Bbar; have hω := p.ω_pos.ne'; field_simp
  have h3 : 0 < p.ω * p.A₀ := mul_pos p.ω_pos p.A₀_pos
  by_contra hcon
  rw [not_le] at hcon
  have : p.ω * Abar p < p.ε * p.D * ρbar p * Abar p := by
    exact mul_lt_mul_of_pos_right hcon hA
  linarith

/-- The trace is negative at every wavenumber, so instability is decided by the determinant. -/
theorem tr_neg (p : Params) (k : ℝ) (hk : 0 ≤ k) : tr (lin p) k < 0 := by
  unfold tr lin; simp only
  have h1 := reinforcement_le p
  have h2 := Abar_pos p
  have h3 : 0 ≤ p.η * p.D / p.z * k := by
    have := p.η_nonneg; have := p.D_pos; have := p.z_pos; positivity
  have h4 : 0 ≤ p.D * k := mul_nonneg p.D_pos.le hk
  linarith

/-- `R = ω Ā > 0` for every parameter value. -/
theorem R_eq (p : Params) : R (lin p) = p.ω * Abar p := by
  unfold R lin; simp only; ring

theorem R_pos (p : Params) : 0 < R (lin p) := by
  rw [R_eq]; exact mul_pos p.ω_pos (Abar_pos p)

/-- `P = η D² / z ≥ 0`. -/
theorem P_eq (p : Params) : P (lin p) = p.η * p.D ^ 2 / p.z := by
  unfold P lin; simp only; ring

theorem P_nonneg (p : Params) : 0 ≤ P (lin p) := by
  rw [P_eq]; have := p.η_nonneg; have := p.D_pos; have := p.z_pos; positivity

/-- `Q = D (3 ε D ρ̄ − ω − η Ā / z)`: the sign of `Q` is the threshold. -/
theorem Q_eq (p : Params) : Q (lin p) = p.D * (3 * p.ε * p.D * ρbar p - p.ω - p.η * Abar p / p.z) := by
  unfold Q lin; simp only
  have hA := (Abar_pos p).ne'
  have hz := p.z_pos.ne'
  field_simp
  ring

/-- Below the threshold `3 ε D ρ̄ ≤ ω + η Ā / z` the uniform state is stable at every wavelength. -/
theorem no_hotspots_below_threshold (p : Params)
    (h : 3 * p.ε * p.D * ρbar p ≤ p.ω + p.η * Abar p / p.z) :
    ∀ k, 0 ≤ k → 0 < det (lin p) k := by
  apply stable_all _ (P_nonneg p) _ (R_pos p)
  rw [Q_eq]
  exact mul_nonpos_of_nonneg_of_nonpos p.D_pos.le (by linarith)

/-- Without repeat reinforcement (`ε = 0`) there are no hot spots at any wavelength:
"hot spots for all parameters" is false. -/
theorem no_hotspots_without_reinforcement (p : Params) (hε : p.ε = 0) :
    ∀ k, 0 ≤ k → 0 < det (lin p) k := by
  apply no_hotspots_below_threshold
  rw [hε]
  have := p.ω_pos; have := p.η_nonneg; have := Abar_pos p; have := p.z_pos
  have : 0 ≤ p.η * Abar p / p.z := by positivity
  linarith

/-- Above the threshold, with `Q² > 4 P R`, the critical mode grows: a positive
growth rate exists at `k* = Q / (2 P)`. -/
theorem hotspots_above_threshold (p : Params) (hη : 0 < p.η)
    (hQ : 0 < Q (lin p)) (h : 4 * P (lin p) * R (lin p) < Q (lin p) ^ 2) :
    ∃ σ, 0 < σ ∧ IsGrowth (lin p) (Q (lin p) / (2 * P (lin p))) σ := by
  have hP : 0 < P (lin p) := by
    rw [P_eq]; have := p.D_pos; have := p.z_pos; positivity
  have hk := crit_pos _ hP hQ
  rw [unstable_iff _ _ (tr_neg p _ hk.le)]
  exact unstable_at_crit _ hP hQ h

end Research.P4.Hotspot
