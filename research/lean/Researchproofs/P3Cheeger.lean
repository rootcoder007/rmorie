/-
  Researchproofs/P3Cheeger.lean

  P3, continued: how much displacement a hot-spot boundary can carry — the
  easy direction of the Cheeger inequality (library scan C14; Chung, Four
  proofs of the Cheeger inequality, Theorem 1; Grimmett & Stirzaker
  Theorem 6.14.12).

  A weighted graph on the places: symmetric non-negative weights A u v,
  degrees d v = ∑_u A v u, volume vol S = ∑_{v∈S} d v, cut S = ∑_{u∈S, v∉S} A u v.
  Conductance h(S) = cut S / min(vol S, vol Sᶜ). For a vector f the
  Dirichlet form is E(f) = ½ ∑_{u,v} A u v (f u − f v)² and D(f) = ∑_v d v f v².
  λ₂ is the smallest value of E(f)/D(f) over vectors with ∑_v d v f v = 0
  (its variational characterisation; here that is the definition).

    * testVec_orth, testVec_dnorm, testVec_dirichlet: the two-valued vector
      f = 1_S/vol S − 1_{Sᶜ}/vol Sᶜ is degree-orthogonal to constants and
      has E(f) = cut S · (1/vol S + 1/vol Sᶜ)², D(f) = 1/vol S + 1/vol Sᶜ.
    * rayleigh_testVec: E(f)/D(f) = cut S · (1/vol S + 1/vol Sᶜ).
    * rayleigh_le_two_conductance: that is at most 2 h(S).
    * cheeger_easy: λ₂ ≤ 2 h(S) for every non-trivial S, hence λ₂ ≤ 2 h_G.

  Reading: a hot-spot set with small conductance (little weight crossing its
  boundary relative to its own volume) forces a small spectral gap, so a
  patrol effect confined to it diffuses slowly to the rest of the street
  graph; conversely a large spectral gap rules out any low-conductance set.
  The hard direction λ₂ ≥ h²/2 is not proved here.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P3

variable {V : Type*} [Fintype V] [DecidableEq V]

/-- A weighted undirected graph on the places. -/
structure WGraph (V : Type*) [Fintype V] where
  (A : V → V → ℝ)
  (nonneg : ∀ u v, 0 ≤ A u v)
  (symm : ∀ u v, A u v = A v u)

variable (G : WGraph V)

def deg (v : V) : ℝ := ∑ u, G.A v u
def vol (S : Finset V) : ℝ := ∑ v ∈ S, deg G v
def cut (S : Finset V) : ℝ := ∑ u ∈ S, ∑ v ∈ Sᶜ, G.A u v
def dirichlet (f : V → ℝ) : ℝ := (1 / 2) * ∑ u, ∑ v, G.A u v * (f u - f v) ^ 2
def dnorm (f : V → ℝ) : ℝ := ∑ v, deg G v * f v ^ 2
def rayleigh (f : V → ℝ) : ℝ := dirichlet G f / dnorm G f
def conductance (S : Finset V) : ℝ := cut G S / min (vol G S) (vol G Sᶜ)

/-- λ₂ by its variational characterisation. -/
def lambda2 : ℝ := sInf {r | ∃ f : V → ℝ, (∑ v, deg G v * f v = 0) ∧ dnorm G f ≠ 0 ∧ r = rayleigh G f}

/-- The two-valued test vector of a set `S`. -/
def testVec (S : Finset V) (v : V) : ℝ := if v ∈ S then 1 / vol G S else -(1 / vol G Sᶜ)

theorem deg_nonneg (v : V) : 0 ≤ deg G v := sum_nonneg (fun u _ => G.nonneg v u)

theorem dirichlet_nonneg (f : V → ℝ) : 0 ≤ dirichlet G f := by
  unfold dirichlet
  apply mul_nonneg (by norm_num)
  apply sum_nonneg; intro u _; apply sum_nonneg; intro v _
  exact mul_nonneg (G.nonneg u v) (sq_nonneg _)

theorem dnorm_nonneg (f : V → ℝ) : 0 ≤ dnorm G f :=
  sum_nonneg (fun v _ => mul_nonneg (deg_nonneg G v) (sq_nonneg _))

theorem rayleigh_nonneg (f : V → ℝ) : 0 ≤ rayleigh G f :=
  div_nonneg (dirichlet_nonneg G f) (dnorm_nonneg G f)

/-- The cut is symmetric in `S` and its complement. -/
theorem cut_compl (S : Finset V) : cut G Sᶜ = cut G S := by
  unfold cut
  rw [compl_compl, sum_comm]
  apply sum_congr rfl; intro u _; apply sum_congr rfl; intro v _
  exact G.symm v u

theorem cut_nonneg (S : Finset V) : 0 ≤ cut G S :=
  sum_nonneg (fun u _ => sum_nonneg (fun v _ => G.nonneg u v))

section Test

variable (S : Finset V) (hS : 0 < vol G S) (hSc : 0 < vol G Sᶜ)
include hS hSc

theorem testVec_orth : ∑ v, deg G v * testVec G S v = 0 := by
  rw [← sum_add_sum_compl S]
  have h1 : ∑ v ∈ S, deg G v * testVec G S v = 1 := by
    have e : ∑ v ∈ S, deg G v * testVec G S v = (∑ v ∈ S, deg G v) * (1 / vol G S) := by
      rw [sum_mul]; apply sum_congr rfl; intro v hv; simp [testVec, hv]
    rw [e]; change vol G S * (1 / vol G S) = 1
    field_simp
  have h2 : ∑ v ∈ Sᶜ, deg G v * testVec G S v = -1 := by
    have e : ∑ v ∈ Sᶜ, deg G v * testVec G S v = -((∑ v ∈ Sᶜ, deg G v) * (1 / vol G Sᶜ)) := by
      rw [sum_mul, ← sum_neg_distrib]; apply sum_congr rfl; intro v hv
      have : v ∉ S := mem_compl.1 hv
      simp [testVec, this]
    rw [e]; change -(vol G Sᶜ * (1 / vol G Sᶜ)) = -1
    field_simp
  rw [h1, h2]; ring

theorem testVec_dnorm : dnorm G (testVec G S) = 1 / vol G S + 1 / vol G Sᶜ := by
  unfold dnorm
  rw [← sum_add_sum_compl S]
  have h1 : ∑ v ∈ S, deg G v * testVec G S v ^ 2 = 1 / vol G S := by
    have e : ∑ v ∈ S, deg G v * testVec G S v ^ 2 = (∑ v ∈ S, deg G v) * (1 / vol G S) ^ 2 := by
      rw [sum_mul]; apply sum_congr rfl; intro v hv; simp [testVec, hv]
    rw [e]; change vol G S * (1 / vol G S) ^ 2 = 1 / vol G S
    field_simp <;> ring
  have h2 : ∑ v ∈ Sᶜ, deg G v * testVec G S v ^ 2 = 1 / vol G Sᶜ := by
    have e : ∑ v ∈ Sᶜ, deg G v * testVec G S v ^ 2 = (∑ v ∈ Sᶜ, deg G v) * (1 / vol G Sᶜ) ^ 2 := by
      rw [sum_mul]; apply sum_congr rfl; intro v hv
      have : v ∉ S := mem_compl.1 hv
      simp [testVec, this]
    rw [e]; change vol G Sᶜ * (1 / vol G Sᶜ) ^ 2 = 1 / vol G Sᶜ
    field_simp <;> ring
  rw [h1, h2]

omit hS hSc in
/-- Pairs inside `S` or inside `Sᶜ` contribute nothing; each cross pair contributes `(a+b)²`. -/
theorem testVec_pair (u v : V) :
    G.A u v * (testVec G S u - testVec G S v) ^ 2 =
      if (u ∈ S ↔ v ∈ S) then 0 else G.A u v * (1 / vol G S + 1 / vol G Sᶜ) ^ 2 := by
  by_cases hu : u ∈ S <;> by_cases hv : v ∈ S
  · simp [testVec, hu, hv]
  · rw [if_neg (by simp [hu, hv])]; simp only [testVec, hu, hv, if_true, if_false]; ring
  · rw [if_neg (by simp [hu, hv])]; simp only [testVec, hu, hv, if_true, if_false]; ring
  · simp [testVec, hu, hv]

theorem testVec_dirichlet :
    dirichlet G (testVec G S) = cut G S * (1 / vol G S + 1 / vol G Sᶜ) ^ 2 := by
  unfold dirichlet
  set c := (1 / vol G S + 1 / vol G Sᶜ) ^ 2 with hc
  have key : ∀ u v, G.A u v * (testVec G S u - testVec G S v) ^ 2 =
      if (u ∈ S ↔ v ∈ S) then 0 else G.A u v * c := fun u v => testVec_pair G S u v
  simp_rw [key]
  -- split both sums over S and Sᶜ
  rw [← sum_add_sum_compl S]
  have hin : ∀ u ∈ S, (∑ v, if (u ∈ S ↔ v ∈ S) then (0 : ℝ) else G.A u v * c) = (∑ v ∈ Sᶜ, G.A u v) * c := by
    intro u hu
    rw [← sum_add_sum_compl S, sum_mul]
    have z : ∑ v ∈ S, (if (u ∈ S ↔ v ∈ S) then (0 : ℝ) else G.A u v * c) = 0 := by
      apply sum_eq_zero; intro v hv; simp [hu, hv]
    rw [z, zero_add]; apply sum_congr rfl; intro v hv
    have : v ∉ S := mem_compl.1 hv
    simp [hu, this]
  have hout : ∀ u ∈ Sᶜ, (∑ v, if (u ∈ S ↔ v ∈ S) then (0 : ℝ) else G.A u v * c) = (∑ v ∈ S, G.A u v) * c := by
    intro u hu
    have hu' : u ∉ S := mem_compl.1 hu
    rw [← sum_add_sum_compl S, sum_mul]
    have z : ∑ v ∈ Sᶜ, (if (u ∈ S ↔ v ∈ S) then (0 : ℝ) else G.A u v * c) = 0 := by
      apply sum_eq_zero; intro v hv
      have : v ∉ S := mem_compl.1 hv
      simp [hu', this]
    rw [z, add_zero]; apply sum_congr rfl; intro v hv; simp [hu', hv]
  rw [sum_congr rfl hin, sum_congr rfl hout]
  have h2 : ∑ u ∈ Sᶜ, (∑ v ∈ S, G.A u v) * c = cut G S * c := by
    rw [← sum_mul]; congr 1
    unfold cut; rw [sum_comm]; apply sum_congr rfl; intro u _; apply sum_congr rfl; intro v _
    first | exact G.symm u v | exact G.symm v u
  have h1 : ∑ u ∈ S, (∑ v ∈ Sᶜ, G.A u v) * c = cut G S * c := by
    rw [← sum_mul]; rfl
  rw [h1, h2]; ring

theorem rayleigh_testVec : rayleigh G (testVec G S) = cut G S * (1 / vol G S + 1 / vol G Sᶜ) := by
  unfold rayleigh
  rw [testVec_dirichlet G S hS hSc, testVec_dnorm G S hS hSc]
  have hpos : 0 < 1 / vol G S + 1 / vol G Sᶜ := by positivity
  field_simp <;> ring

/-- `1/vol S + 1/vol Sᶜ ≤ 2 / min(vol S, vol Sᶜ)`, so the Rayleigh quotient is at most `2 h(S)`. -/
theorem rayleigh_le_two_conductance : rayleigh G (testVec G S) ≤ 2 * conductance G S := by
  rw [rayleigh_testVec G S hS hSc]
  unfold conductance
  have hmin : 0 < min (vol G S) (vol G Sᶜ) := lt_min hS hSc
  have ha : 1 / vol G S ≤ 1 / min (vol G S) (vol G Sᶜ) :=
    one_div_le_one_div_of_le hmin (min_le_left _ _)
  have hb : 1 / vol G Sᶜ ≤ 1 / min (vol G S) (vol G Sᶜ) :=
    one_div_le_one_div_of_le hmin (min_le_right _ _)
  have hc := cut_nonneg G S
  calc cut G S * (1 / vol G S + 1 / vol G Sᶜ)
      ≤ cut G S * (2 * (1 / min (vol G S) (vol G Sᶜ))) := by
        apply mul_le_mul_of_nonneg_left _ hc; linarith
    _ = 2 * (cut G S / min (vol G S) (vol G Sᶜ)) := by ring

/-- The test vector is admissible, so λ₂ is at most its Rayleigh quotient. -/
theorem lambda2_le_rayleigh_testVec : lambda2 G ≤ rayleigh G (testVec G S) := by
  unfold lambda2
  apply csInf_le
  · refine ⟨0, ?_⟩
    rintro r ⟨f, _, _, rfl⟩
    exact rayleigh_nonneg G f
  · refine ⟨testVec G S, testVec_orth G S hS hSc, ?_, rfl⟩
    rw [testVec_dnorm G S hS hSc]
    have : 0 < 1 / vol G S + 1 / vol G Sᶜ := by positivity
    exact this.ne'

/-- Cheeger, easy direction: `λ₂ ≤ 2 h(S)` for every set with positive volume on both sides. -/
theorem cheeger_easy : lambda2 G ≤ 2 * conductance G S :=
  (lambda2_le_rayleigh_testVec G S hS hSc).trans (rayleigh_le_two_conductance G S hS hSc)

end Test

end Research.P3
