/-
  Researchproofs/P5Separation.lean

  P5, continued: separation, not misspecification, is why a logistic fit
  with a perfectly predictive covariate "fails to converge" (library scan
  A1; Albert & Anderson 1984; Weisburd & Britt ch. 1 on the Baldus
  proportionality-review logit).

  With labels coded as signs s_i ∈ {−1, +1} (s = 2y − 1), the logistic
  log-likelihood is ℓ(b) = ∑_i −log(1 + exp(−s_i x_i·b)), a sum of
  strictly increasing functions of the margins s_i x_i·b.

    * ll1_strictMono, ll1_neg: each term is strictly increasing and < 0.
    * loglik_lt_shift: if a direction d separates the data completely
      (s_i x_i·d > 0 for every i) then ℓ(b + t d) > ℓ(b) for every b and
      every t > 0.
    * no_mle: hence no maximiser exists — the supremum (0) is approached
      along d and never attained. The iterations of any fitting routine
      march to infinity; nothing about the specification is wrong.
    * no_mle_quasi: quasi-complete separation (s_i x_i·d ≥ 0 for all i,
      > 0 for at least one) is enough.
    * loglik_tendsto_zero: along a completely separating direction the
      log-likelihood tends to its supremum 0.

  Albert–Anderson's converse (overlap ⇒ a finite maximiser exists) is
  not proved here; the R and Python arms detect separation by the
  condition above and report it rather than a divergent coefficient.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset Filter Topology

namespace Research.P5

variable {n p : Type*} [Fintype n] [Fintype p]

/-- Logistic data: covariate rows `x i`, labels as signs `s i ∈ {−1, +1}`. -/
structure Logit (n p : Type*) [Fintype n] [Fintype p] where
  (x : n → p → ℝ)
  (s : n → ℝ)
  (s_sign : ∀ i, s i = 1 ∨ s i = -1)

def dot (x b : p → ℝ) : ℝ := ∑ j, x j * b j

/-- Per-observation log-likelihood as a function of the margin `u = s x·b`: `log σ(u)`. -/
def ll1 (u : ℝ) : ℝ := -Real.log (1 + Real.exp (-u))

theorem ll1_strictMono : StrictMono ll1 := by
  intro a b hab
  unfold ll1
  apply neg_lt_neg
  apply Real.log_lt_log (by positivity)
  have : Real.exp (-b) < Real.exp (-a) := Real.exp_lt_exp.2 (by linarith)
  linarith

theorem ll1_neg (u : ℝ) : ll1 u < 0 := by
  unfold ll1
  have : 0 < Real.log (1 + Real.exp (-u)) := Real.log_pos (by linarith [Real.exp_pos (-u)])
  linarith

def Logit.loglik (D : Logit n p) (b : p → ℝ) : ℝ := ∑ i, ll1 (D.s i * dot (D.x i) b)

/-- Complete separation along `d`: every margin is strictly positive. -/
def CompleteSeparation (D : Logit n p) (d : p → ℝ) : Prop := ∀ i, 0 < D.s i * dot (D.x i) d

/-- Quasi-complete separation: non-negative margins, at least one positive. -/
def QuasiSeparation (D : Logit n p) (d : p → ℝ) : Prop :=
  (∀ i, 0 ≤ D.s i * dot (D.x i) d) ∧ ∃ i, 0 < D.s i * dot (D.x i) d

omit [Fintype n] in
theorem dot_add_smul (x b d : p → ℝ) (t : ℝ) : dot x (b + t • d) = dot x b + t * dot x d := by
  unfold dot
  rw [mul_sum, ← sum_add_distrib]
  apply sum_congr rfl; intro j _
  simp only [Pi.add_apply, Pi.smul_apply, smul_eq_mul]; ring

theorem margin_shift (D : Logit n p) (b d : p → ℝ) (t : ℝ) (i : n) :
    D.s i * dot (D.x i) (b + t • d) = D.s i * dot (D.x i) b + t * (D.s i * dot (D.x i) d) := by
  rw [dot_add_smul]; ring

/-- Along a completely separating direction the log-likelihood strictly increases. -/
theorem loglik_lt_shift [Nonempty n] (D : Logit n p) (d : p → ℝ) (hsep : CompleteSeparation D d)
    (b : p → ℝ) (t : ℝ) (ht : 0 < t) : D.loglik b < D.loglik (b + t • d) := by
  unfold Logit.loglik
  apply sum_lt_sum_of_nonempty univ_nonempty
  intro i _
  apply ll1_strictMono
  rw [margin_shift]
  linarith [mul_pos ht (hsep i)]

/-- Under quasi-complete separation it still increases (weakly termwise, strictly in total). -/
theorem loglik_lt_shift_quasi (D : Logit n p) (d : p → ℝ) (hsep : QuasiSeparation D d)
    (b : p → ℝ) (t : ℝ) (ht : 0 < t) : D.loglik b < D.loglik (b + t • d) := by
  unfold Logit.loglik
  obtain ⟨i₀, hi₀⟩ := hsep.2
  apply sum_lt_sum
  · intro i _
    apply ll1_strictMono.monotone
    rw [margin_shift]
    linarith [mul_nonneg ht.le (hsep.1 i)]
  · exact ⟨i₀, mem_univ _, by
      apply ll1_strictMono
      rw [margin_shift]
      linarith [mul_pos ht hi₀]⟩

/-- No maximum-likelihood estimate exists under complete separation. -/
theorem no_mle [Nonempty n] (D : Logit n p) (d : p → ℝ) (hsep : CompleteSeparation D d) :
    ¬ ∃ b : p → ℝ, ∀ b' : p → ℝ, D.loglik b' ≤ D.loglik b := by
  rintro ⟨b, hb⟩
  have h1 := loglik_lt_shift D d hsep b 1 one_pos
  have h2 := hb (b + (1 : ℝ) • d)
  linarith

/-- Nor under quasi-complete separation. -/
theorem no_mle_quasi (D : Logit n p) (d : p → ℝ) (hsep : QuasiSeparation D d) :
    ¬ ∃ b : p → ℝ, ∀ b' : p → ℝ, D.loglik b' ≤ D.loglik b := by
  rintro ⟨b, hb⟩
  have h1 := loglik_lt_shift_quasi D d hsep b 1 one_pos
  have h2 := hb (b + (1 : ℝ) • d)
  linarith

/-- The log-likelihood is bounded above by zero. -/
theorem loglik_neg [Nonempty n] (D : Logit n p) (b : p → ℝ) : D.loglik b < 0 := by
  unfold Logit.loglik
  exact sum_neg (fun i _ => ll1_neg _) univ_nonempty

theorem ll1_tendsto (c : ℝ) (hc : 0 < c) :
    Tendsto (fun t : ℝ => ll1 (t * c)) atTop (𝓝 0) := by
  have h1 : Tendsto (fun t : ℝ => t * c) atTop atTop := Tendsto.atTop_mul_const hc tendsto_id
  have h2 : Tendsto (fun t : ℝ => Real.exp (-(t * c))) atTop (𝓝 0) :=
    Real.tendsto_exp_neg_atTop_nhds_zero.comp h1
  have h3 : Tendsto (fun t : ℝ => 1 + Real.exp (-(t * c))) atTop (𝓝 (1 + 0)) :=
    tendsto_const_nhds.add h2
  rw [add_zero] at h3
  have h4 : Tendsto (fun t : ℝ => Real.log (1 + Real.exp (-(t * c)))) atTop (𝓝 (Real.log 1)) :=
    (Real.continuousAt_log one_ne_zero).tendsto.comp h3
  rw [Real.log_one] at h4
  have h5 := h4.neg
  rw [neg_zero] at h5
  exact h5

/-- Along a completely separating direction the log-likelihood tends to its supremum 0. -/
theorem loglik_tendsto_zero (D : Logit n p) (d : p → ℝ) (hsep : CompleteSeparation D d) :
    Tendsto (fun t : ℝ => D.loglik (t • d)) atTop (𝓝 0) := by
  have : (fun t : ℝ => D.loglik (t • d)) = fun t => ∑ i, ll1 (t * (D.s i * dot (D.x i) d)) := by
    funext t; unfold Logit.loglik; apply sum_congr rfl; intro i _
    congr 1
    have := margin_shift D 0 d t i
    simp only [zero_add] at this
    rw [this]; unfold dot; simp
  rw [this]
  have h := tendsto_finset_sum (univ : Finset n)
    (fun i _ => ll1_tendsto (D.s i * dot (D.x i) d) (hsep i))
  simpa using h

end Research.P5
