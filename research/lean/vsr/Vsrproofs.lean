/-
  Vsrproofs.lean

  Machine-checked identification theory for

    "Alert Complexity and Placement Volatility in Ontario Restrictive
     Confinement Data" (OTIS A01-RCDD, 2023-2025).

  What is proved here is the MATHEMATICS of the estimators used in the
  paper, over a finite population with known weights: the adjustment
  formula, inverse-probability weighting, the double robustness of the
  augmented estimator, exact-matching covariate balance, the log-link
  incidence-rate-ratio identity, negative-binomial overdispersion, and
  the Lakner stock/flow identity together with a sign-divergence witness
  instantiated at the paper's own published counts.

  What is NOT proved here, and cannot be: that the identification
  assumptions (positivity and conditional mean ignorability) hold of the
  OTIS data. Those are substantive claims about the world. Lean checks
  the implication, never the antecedent.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.unusedSectionVars false

noncomputable section
open Finset

namespace VSR

/-! ### 1. Finite populations -/

/-- A finite population carrying a probability weight on each unit. -/
structure FinPop (Ω : Type*) [Fintype Ω] where
  p : Ω → ℝ
  nonneg : ∀ ω, 0 ≤ p ω
  total : ∑ ω, p ω = 1

namespace FinPop

variable {Ω : Type*} [Fintype Ω] (P : FinPop Ω)

/-- Expectation of `f` over the whole population. -/
def E (f : Ω → ℝ) : ℝ := ∑ ω, P.p ω * f ω

/-- Unnormalised expectation of `f` over a subpopulation `s`. -/
def Ein (s : Finset Ω) (f : Ω → ℝ) : ℝ := ∑ ω ∈ s, P.p ω * f ω

/-- Mass of a subpopulation. -/
def mass (s : Finset Ω) : ℝ := ∑ ω ∈ s, P.p ω

lemma Ein_one (s : Finset Ω) : P.Ein s (fun _ => 1) = P.mass s := by
  simp [Ein, mass]

/-- The law of total expectation over the partition induced by `X`. -/
lemma E_eq_sum_strata {K : Type*} [Fintype K] [DecidableEq K]
    (X : Ω → K) (f : Ω → ℝ) :
    P.E f = ∑ x, P.Ein (univ.filter (fun ω => X ω = x)) f :=
  (Finset.sum_fiberwise univ X (fun ω => P.p ω * f ω)).symm

end FinPop

/-! ### 2. The causal model -/

/-- A finite causal model: weights, a discrete covariate, a binary
treatment and the two potential outcomes. -/
structure Model (Ω K : Type*) [Fintype Ω] [Fintype K] [DecidableEq K] where
  P : FinPop Ω
  X : Ω → K
  A : Ω → Bool
  Y1 : Ω → ℝ
  Y0 : Ω → ℝ

namespace Model

variable {Ω K : Type*} [Fintype Ω] [Fintype K] [DecidableEq K] (M : Model Ω K)

/-- The observed outcome. Consistency (SUTVA's second half) is not an
axiom here: the observed outcome is *defined* as the potential outcome
selected by the realised treatment. -/
def Y : Ω → ℝ := fun ω => if M.A ω then M.Y1 ω else M.Y0 ω

/-- The stratum `{ω : X ω = x}`. -/
def stratum (x : K) : Finset Ω := univ.filter (fun ω => M.X ω = x)

/-- The treated units of stratum `x`. -/
def treatedIn (x : K) : Finset Ω := (M.stratum x).filter (fun ω => M.A ω = true)

/-- Stratum mass, `P(X = x)`. -/
def px (x : K) : ℝ := M.P.mass (M.stratum x)

/-- Treated mass in a stratum, `P(X = x, A = 1)`. -/
def q1 (x : K) : ℝ := M.P.mass (M.treatedIn x)

/-- The propensity score `e(x) = P(A = 1 | X = x)`. -/
def e (x : K) : ℝ := M.q1 x / M.px x

/-- The observed treated-arm mean in a stratum. Estimable from data. -/
def mu1 (x : K) : ℝ := M.P.Ein (M.treatedIn x) M.Y / M.q1 x

/-- The counterfactual stratum mean of `Y1`. NOT estimable from data. -/
def nu1 (x : K) : ℝ := M.P.Ein (M.stratum x) M.Y1 / M.px x

/-- The identification assumptions, stated once.

`posX`, `pos1` : positivity — every stratum is occupied and contains
treated units.

`ign` : conditional mean ignorability for `Y1`. The observed treated
mean in a stratum equals the stratum's counterfactual mean. This is the
assumption that observational data cannot verify; everything below is a
theorem *given* it. -/
structure Ident : Prop where
  posX : ∀ x, 0 < M.px x
  pos1 : ∀ x, 0 < M.q1 x
  ign : ∀ x, M.mu1 x = M.nu1 x

/-- The tower property, restated on `stratum`. -/
lemma E_eq_sum_strata' (f : Ω → ℝ) : M.P.E f = ∑ x, M.P.Ein (M.stratum x) f := by
  rw [M.P.E_eq_sum_strata M.X f]
  simp only [stratum]

/-- Summing a constant-plus-scaled-term over a subpopulation. -/
lemma sum_split (s : Finset Ω) (F G : Ω → ℝ) (c : ℝ) :
    (∑ ω ∈ s, F ω) + (∑ ω ∈ s, G ω) / c = ∑ ω ∈ s, (F ω + G ω / c) := by
  rw [Finset.sum_div, ← Finset.sum_add_distrib]

/-! ### 3. Identification -/

/-- Within a stratum, restricting to the treated units is the same as
zeroing out the untreated ones. -/
lemma Ein_treated (x : K) (f : Ω → ℝ) :
    M.P.Ein (M.treatedIn x) f
      = ∑ ω ∈ M.stratum x, if M.A ω then M.P.p ω * f ω else 0 := by
  rw [treatedIn, FinPop.Ein, Finset.sum_filter]

/-- `Ein (treatedIn x) Y = q1 x * mu1 x` whenever the stratum has treated mass. -/
lemma Ein_treated_Y (x : K) (hq : M.q1 x ≠ 0) :
    M.P.Ein (M.treatedIn x) M.Y = M.q1 x * M.mu1 x := by
  rw [mu1]; field_simp

/-- **Theorem 1 (adjustment / backdoor formula).** Under positivity and
conditional mean ignorability, the mean potential outcome under
treatment is the covariate-weighted average of the observed treated
means. Every quantity on the right is estimable. -/
theorem adjustment (h : M.Ident) :
    M.P.E M.Y1 = ∑ x, M.px x * M.mu1 x := by
  rw [M.E_eq_sum_strata' M.Y1]
  refine Finset.sum_congr rfl (fun x _ => ?_)
  have hpx : M.px x ≠ 0 := (h.posX x).ne'
  rw [h.ign x, nu1]
  field_simp

/-- The inverse-probability-weighted transform of the observed data. -/
def ipwTerm : Ω → ℝ :=
  fun ω => (if M.A ω then 1 else 0) / M.e (M.X ω) * M.Y ω

/-- **Theorem 2 (inverse probability weighting).** Weighting the
observed outcomes of the treated by the reciprocal of their propensity
score recovers the same estimand as Theorem 1. -/
theorem ipw (h : M.Ident) : M.P.E M.ipwTerm = M.P.E M.Y1 := by
  rw [M.E_eq_sum_strata' M.ipwTerm, M.adjustment h]
  refine Finset.sum_congr rfl (fun x _ => ?_)
  have hpx : M.px x ≠ 0 := (h.posX x).ne'
  have hq : M.q1 x ≠ 0 := (h.pos1 x).ne'
  have he : M.e x ≠ 0 := by
    rw [e]; exact div_ne_zero hq hpx
  have hstep : M.P.Ein (M.stratum x) M.ipwTerm
      = (∑ ω ∈ M.stratum x, if M.A ω then M.P.p ω * M.Y ω else 0) / M.e x := by
    rw [FinPop.Ein, Finset.sum_div]
    refine Finset.sum_congr rfl (fun ω hω => ?_)
    have hx : M.X ω = x := by
      simpa [stratum] using (Finset.mem_filter.mp hω).2
    cases hA : M.A ω <;> simp [ipwTerm, hx, hA] <;> ring
  rw [hstep, ← M.Ein_treated x M.Y, M.Ein_treated_Y x hq, e]
  field_simp

/-- The augmented (doubly robust) transform, for an arbitrary outcome
model `m` and an arbitrary propensity model `et`. -/
def aipwTerm (m : K → ℝ) (et : K → ℝ) : Ω → ℝ :=
  fun ω => m (M.X ω) + (if M.A ω then 1 else 0) * (M.Y ω - m (M.X ω)) / et (M.X ω)

/-- The stratum-level value of the augmented transform. This single
identity yields both robustness directions. -/
lemma Ein_aipw (x : K) (m et : K → ℝ) (hq : M.q1 x ≠ 0) (het : et x ≠ 0) :
    M.P.Ein (M.stratum x) (M.aipwTerm m et)
      = M.px x * m x + (M.q1 x / et x) * (M.mu1 x - m x) := by
  have hsplit : M.P.Ein (M.stratum x) (M.aipwTerm m et)
      = (∑ ω ∈ M.stratum x, M.P.p ω * m x)
        + (∑ ω ∈ M.stratum x, if M.A ω then M.P.p ω * (M.Y ω - m x) else 0) / et x := by
    rw [FinPop.Ein, sum_split]
    refine Finset.sum_congr rfl (fun ω hω => ?_)
    have hx : M.X ω = x := by
      simpa [stratum] using (Finset.mem_filter.mp hω).2
    cases hA : M.A ω <;> simp [aipwTerm, hx, hA] <;> ring
  have hmass : (∑ ω ∈ M.stratum x, M.P.p ω * m x) = M.px x * m x := by
    rw [px, FinPop.mass, Finset.sum_mul]
  have hnum : (∑ ω ∈ M.stratum x, if M.A ω then M.P.p ω * (M.Y ω - m x) else 0)
      = M.q1 x * M.mu1 x - M.q1 x * m x := by
    have : (∑ ω ∈ M.stratum x, if M.A ω then M.P.p ω * (M.Y ω - m x) else 0)
        = (∑ ω ∈ M.stratum x, if M.A ω then M.P.p ω * M.Y ω else 0)
          - (∑ ω ∈ M.stratum x, if M.A ω then M.P.p ω * m x else 0) := by
      rw [← Finset.sum_sub_distrib]
      exact Finset.sum_congr rfl (fun ω _ => by cases hA : M.A ω <;> simp [hA] <;> ring)
    rw [this, ← M.Ein_treated x M.Y, M.Ein_treated_Y x hq,
        ← M.Ein_treated x (fun _ => m x), FinPop.Ein]
    simp [q1, FinPop.mass, Finset.sum_mul]
  rw [hsplit, hmass, hnum]
  field_simp

/-- **Theorem 3a (double robustness, correct propensity score).** If the
propensity model is correct, the augmented estimand equals the target
for *any* outcome model `m`, including a badly wrong one. -/
theorem aipw_propensity_correct (h : M.Ident) (m : K → ℝ) :
    M.P.E (M.aipwTerm m M.e) = M.P.E M.Y1 := by
  rw [M.E_eq_sum_strata' (M.aipwTerm m M.e), M.adjustment h]
  refine Finset.sum_congr rfl (fun x _ => ?_)
  have hpx : M.px x ≠ 0 := (h.posX x).ne'
  have hq : M.q1 x ≠ 0 := (h.pos1 x).ne'
  have he : M.e x ≠ 0 := by rw [e]; exact div_ne_zero hq hpx
  rw [M.Ein_aipw x m M.e hq he, e]
  field_simp
  ring

/-- **Theorem 3b (double robustness, correct outcome model).** If the
outcome model is correct, the augmented estimand equals the target for
*any* propensity model `et` that never vanishes, including a badly wrong
one. -/
theorem aipw_outcome_correct (h : M.Ident) (et : K → ℝ) (het : ∀ x, et x ≠ 0) :
    M.P.E (M.aipwTerm M.mu1 et) = M.P.E M.Y1 := by
  rw [M.E_eq_sum_strata' (M.aipwTerm M.mu1 et), M.adjustment h]
  refine Finset.sum_congr rfl (fun x _ => ?_)
  have hq : M.q1 x ≠ 0 := (h.pos1 x).ne'
  rw [M.Ein_aipw x M.mu1 et hq (het x)]
  ring

end Model

/-! ### 4. Exact matching -/

/-- **Theorem 4 (exact matching gives exact balance).** If every treated
unit is paired with a distinct control unit carrying the *same*
covariate value, then the treated and matched-control groups have
identical covariate totals — for every covariate function `g`
simultaneously. The standardized mean differences reported as `0.000`
in the paper's balance table are therefore a consequence of the
matching construction, not a finding about the data. -/
theorem matching_balance {Ω K : Type*} [DecidableEq Ω] [DecidableEq K]
    (X : Ω → K) (T : Finset Ω) (σ : Ω → Ω)
    (hinj : ∀ a ∈ T, ∀ b ∈ T, σ a = σ b → a = b)
    (hX : ∀ t ∈ T, X (σ t) = X t) (g : K → ℝ) :
    ∑ c ∈ T.image σ, g (X c) = ∑ t ∈ T, g (X t) := by
  rw [Finset.sum_image hinj]
  exact Finset.sum_congr rfl (fun t ht => by rw [hX t ht])

/-- The matched control group has the same cardinality as the treated
group: 7,260 pairs in the paper. -/
theorem matching_card {Ω : Type*} [DecidableEq Ω] (T : Finset Ω) (σ : Ω → Ω)
    (hinj : ∀ a ∈ T, ∀ b ∈ T, σ a = σ b → a = b) :
    (T.image σ).card = T.card :=
  Finset.card_image_of_injOn hinj

/-! ### 5. The model-specific identities -/

/-- **Theorem 5 (incidence rate ratio).** Under a log link, the ratio of
expected counts between treatment arms is `exp β₁`, free of every other
term in the linear predictor. This is what licenses reading `IRR = 1.34`
as a multiplicative contrast holding covariates and the cluster
intercept fixed. -/
theorem irr_log_link (β₀ β₁ xγ u : ℝ) :
    Real.exp (β₀ + β₁ * 1 + xγ + u) / Real.exp (β₀ + β₁ * 0 + xγ + u) = Real.exp β₁ := by
  rw [← Real.exp_sub]
  ring_nf

/-- **Theorem 6 (negative binomial overdispersion).** For every positive
mean and every finite positive dispersion parameter, the negative
binomial variance strictly exceeds the Poisson variance. The choice of
the negative binomial over the Poisson is therefore forced by any
detected overdispersion, not a matter of taste. -/
theorem nb_var_gt_poisson (μ θ : ℝ) (hμ : 0 < μ) (hθ : 0 < θ) :
    μ < μ + μ ^ 2 / θ := by
  have : 0 < μ ^ 2 / θ := div_pos (pow_pos hμ 2) hθ
  linarith

/-! ### 6. The Lakner stock-and-flow measures -/

/-- Average daily population: total person-days divided by days in the
period. A *stock*: days per day. -/
def adp (sumDays t : ℝ) : ℝ := sumDays / t

/-- Average length of stay: total person-days divided by people. A
*flow*: days per person. -/
def alos (sumDays N : ℝ) : ℝ := sumDays / N

/-- **Theorem 7 (Lakner identity).** `adp = N · alos / t`. The stock is
the flow scaled by throughput over the period. -/
theorem lakner_identity (sumDays N t : ℝ) (hN : N ≠ 0) (ht : t ≠ 0) :
    adp sumDays t = N * alos sumDays N / t := by
  rw [adp, alos]; field_simp

/-- **Theorem 8 (sign divergence is real, at the paper's own numbers).**
Instantiated at the published OTIS b02 counts for 2023 and 2025 —
115,674 and 126,121 person-days, 12,647 and 9,608 people, 15,495,050 and
16,256,538 exposure-days — the number of people fell, while the average
length of stay AND the average daily population both rose. A flow rate
and a stock rate built from the same records can therefore move in
opposite directions, and reporting one as if it were the other reverses
the finding. -/
theorem lakner_sign_divergence :
    (9608 : ℝ) < 12647 ∧
    alos 115674 12647 < alos 126121 9608 ∧
    adp 115674 15495050 < adp 126121 16256538 := by
  simp only [alos, adp]
  refine ⟨by norm_num, by norm_num, by norm_num⟩

/-- **Theorem 9 (confounding factor).** The crude contrast is the
adjusted contrast times a bias factor; at the paper's numbers the crude
Poisson rate ratio of 4.273 carries a factor of about 3.19 over the
adjusted 1.34, so most of the crude association is composition, not
effect. -/
theorem confounding_factor (crude adjusted : ℝ) (hadj : adjusted ≠ 0) :
    crude = adjusted * (crude / adjusted) := by
  field_simp

theorem confounding_factor_otis :
    (3 : ℝ) < 4273 / 1340 ∧ (4273 : ℝ) / 1340 < 32 / 10 := by
  constructor <;> norm_num

/-! ### 7. Negative control: the assumptions are not decorative

A theorem whose hypotheses can never fail proves nothing about the data.
The witness below is a four-unit population in a single stratum where
positivity holds but conditional mean ignorability does not, and there
the adjustment formula returns the wrong answer. The identification
assumptions of Section 3 are therefore load-bearing: no amount of
matching, weighting or augmentation rescues an analysis in which the
treated differ systematically on the potential outcome itself. -/

namespace Counterexample

/-- Four units, one stratum, equal weights. -/
def P4 : FinPop (Fin 4) where
  p := fun _ => 1 / 4
  nonneg := by intro _; norm_num
  total := by norm_num [Fin.sum_univ_four]

/-- Units 0 and 1 are treated and have `Y1 = 0`; units 2 and 3 are
untreated and have `Y1 = 10`. Treatment is assigned *because* of the
potential outcome — precisely the confounding that ignorability rules
out. -/
def Mbad : Model (Fin 4) (Fin 1) where
  P := P4
  X := fun _ => 0
  A := fun ω => decide (ω.val < 2)
  Y1 := fun ω => if ω.val < 2 then 0 else 10
  Y0 := fun _ => 0

lemma px_eq : Mbad.px 0 = 1 := by
  simp only [Model.px, Model.stratum, FinPop.mass, Finset.sum_filter, Mbad, P4,
             Fin.sum_univ_four]
  norm_num

lemma q1_eq : Mbad.q1 0 = 1 / 2 := by
  simp only [Model.q1, Model.treatedIn, Model.stratum, FinPop.mass,
             Finset.sum_filter, Finset.filter_filter, Mbad, P4, Fin.sum_univ_four]
  norm_num

lemma mu1_eq : Mbad.mu1 0 = 0 := by
  simp only [Model.mu1, Model.treatedIn, Model.stratum, Model.Y, FinPop.Ein,
             Finset.sum_filter, Finset.filter_filter, Mbad, P4, Fin.sum_univ_four]
  norm_num

lemma nu1_eq : Mbad.nu1 0 = 5 := by
  rw [Model.nu1, px_eq]
  simp only [Model.stratum, FinPop.Ein, Finset.sum_filter, Mbad, P4,
             Fin.sum_univ_four]
  norm_num

lemma EY1_eq : Mbad.P.E Mbad.Y1 = 5 := by
  simp only [FinPop.E, Mbad, P4, Fin.sum_univ_four]
  norm_num

/-- Positivity holds in the witness. -/
theorem counterexample_positivity : 0 < Mbad.px 0 ∧ 0 < Mbad.q1 0 := by
  rw [px_eq, q1_eq]; norm_num

/-- Ignorability fails: the treated mean of `Y1` is 0, the stratum mean is 5. -/
theorem counterexample_ignorability_fails : Mbad.mu1 0 ≠ Mbad.nu1 0 := by
  rw [mu1_eq, nu1_eq]; norm_num

/-- **Theorem 10 (the assumption is load-bearing).** In the witness the
adjustment formula is off by the entire confounding gap: the mean
potential outcome is 5 and the formula returns 0. -/
theorem counterexample_adjustment_fails :
    Mbad.P.E Mbad.Y1 ≠ ∑ x, Mbad.px x * Mbad.mu1 x := by
  rw [EY1_eq]
  simp only [Fin.sum_univ_one, px_eq, mu1_eq]
  norm_num

end Counterexample

/-! ### 8. The counterfactual arm

The arm that "is not in the release" is `Y1` on the control units and `Y0`
on the treated units. This section says exactly three things about it.

(1) It is not identified: two models can agree on every observable --- the
covariate, the treatment and the observed outcome, unit by unit --- and still
disagree about the estimand. No estimator can separate them, because no
function of the observed data can.

(2) It is nonetheless *bounded*, with no ignorability assumption at all,
whenever the outcome range is known (Manski). The bounds are sound but their
width is the outcome range times the mass of the unobserved arm.

(3) Monotonicity assumptions tighten the bounds, and the tightened interval is
informative. Each is an assumption, and each is stated as a hypothesis so that
what it buys is visible. -/

namespace Model

variable {Ω K : Type*} [Fintype Ω] [Fintype K] [DecidableEq K] (M : Model Ω K)

/-- The treated arm. -/
def treated : Finset Ω := univ.filter (fun ω => M.A ω = true)

/-- The control arm. -/
def control : Finset Ω := univ.filter (fun ω => ¬ (M.A ω = true))

/-- The two arms partition the population. -/
lemma E_split_arms (f : Ω → ℝ) :
    M.P.E f = M.P.Ein M.treated f + M.P.Ein M.control f := by
  rw [FinPop.E, treated, control, FinPop.Ein, FinPop.Ein]
  exact (Finset.sum_filter_add_sum_filter_not univ _ _).symm

/-- On the treated arm the observed outcome *is* `Y1`. -/
lemma Ein_treated_Y1 : M.P.Ein M.treated M.Y1 = M.P.Ein M.treated M.Y := by
  rw [FinPop.Ein, FinPop.Ein]
  refine Finset.sum_congr rfl (fun ω hω => ?_)
  have : M.A ω = true := (Finset.mem_filter.mp hω).2
  simp [Model.Y, this]

/-- **Theorem 11 (Manski upper bound).** With no ignorability assumption
whatever, if the outcome never exceeds `yhi` then the mean potential outcome
under treatment is at most the observed treated contribution plus `yhi` times
the mass of the arm we cannot see. -/
theorem manski_upper (yhi : ℝ) (hY : ∀ ω, M.Y1 ω ≤ yhi) :
    M.P.E M.Y1 ≤ M.P.Ein M.treated M.Y + yhi * M.P.mass M.control := by
  rw [M.E_split_arms M.Y1, M.Ein_treated_Y1]
  have hc : M.P.Ein M.control M.Y1 ≤ yhi * M.P.mass M.control := by
    rw [FinPop.Ein, FinPop.mass, Finset.mul_sum]
    refine Finset.sum_le_sum (fun ω _ => ?_)
    have := M.P.nonneg ω
    nlinarith [hY ω]
  linarith

/-- **Theorem 12 (Manski lower bound).** Symmetrically. -/
theorem manski_lower (ylo : ℝ) (hY : ∀ ω, ylo ≤ M.Y1 ω) :
    M.P.Ein M.treated M.Y + ylo * M.P.mass M.control ≤ M.P.E M.Y1 := by
  rw [M.E_split_arms M.Y1, M.Ein_treated_Y1]
  have hc : ylo * M.P.mass M.control ≤ M.P.Ein M.control M.Y1 := by
    rw [FinPop.Ein, FinPop.mass, Finset.mul_sum]
    refine Finset.sum_le_sum (fun ω _ => ?_)
    have := M.P.nonneg ω
    nlinarith [hY ω]
  linarith

/-- **Theorem 13 (monotone treatment response).** If treatment never lowers
the outcome for anyone, the average effect is non-negative. This is an
assumption about the world, and this theorem is exactly what it buys. -/
theorem mtr_nonneg (h : ∀ ω, M.Y0 ω ≤ M.Y1 ω) :
    M.P.E M.Y0 ≤ M.P.E M.Y1 := by
  rw [FinPop.E, FinPop.E]
  refine Finset.sum_le_sum (fun ω _ => ?_)
  have := M.P.nonneg ω
  nlinarith [h ω]

/-- **Theorem 14 (arm decomposition of the average effect).** The average
effect splits exactly into the treated and control contributions, which is
the identity the reported ATE/ATT/ATC triple must satisfy. -/
theorem ate_arm_decomposition :
    M.P.E M.Y1 - M.P.E M.Y0
      = (M.P.Ein M.treated M.Y1 - M.P.Ein M.treated M.Y0)
        + (M.P.Ein M.control M.Y1 - M.P.Ein M.control M.Y0) := by
  rw [M.E_split_arms M.Y1, M.E_split_arms M.Y0]; ring

end Model

/-- The reported AIPW triple is internally consistent on the equal-arm matched
frame: `ATE = P(T)·ATT + P(C)·ATC` at 0.045 = 0.5(0.066) + 0.5(0.024). -/
theorem aipw_triple_consistent :
    (0.5 : ℝ) * 0.066 + 0.5 * 0.024 = 0.045 := by norm_num

/-- The AIPW arm means reproduce the reported ATE: 0.173 - 0.128 = 0.045. -/
theorem aipw_arm_means_consistent :
    (0.173 : ℝ) - 0.128 = 0.045 := by norm_num

/-! ### 9. Non-identification of the counterfactual arm -/

namespace NoId

/-- Two units, one treated and one control, one stratum. -/
def P2 : FinPop (Fin 2) where
  p := fun _ => 1 / 2
  nonneg := by intro _; norm_num
  total := by norm_num [Fin.sum_univ_two]

/-- Model A: the control unit would have had `Y1 = 0`. -/
def MA : Model (Fin 2) (Fin 1) where
  P := P2
  X := fun _ => 0
  A := fun ω => decide (ω.val = 0)
  Y1 := fun ω => if ω.val = 0 then 1 else 0
  Y0 := fun _ => 0

/-- Model B: identical in everything observable, but the control unit would
have had `Y1 = 10`. -/
def MB : Model (Fin 2) (Fin 1) where
  P := P2
  X := fun _ => 0
  A := fun ω => decide (ω.val = 0)
  Y1 := fun ω => if ω.val = 0 then 1 else 10
  Y0 := fun _ => 0

/-- The two models are observationally identical: same weights, same
covariate, same treatment, and the same observed outcome for every unit. -/
theorem observationally_identical :
    MA.P.p = MB.P.p ∧ MA.X = MB.X ∧ MA.A = MB.A ∧ MA.Y = MB.Y := by
  refine ⟨rfl, rfl, rfl, ?_⟩
  funext ω
  fin_cases ω <;> simp [Model.Y, MA, MB]

/-- **Theorem 15 (the counterfactual arm is not identified).** Yet the
estimands differ: `E[Y1]` is 1/2 under model A and 11/2 under model B. No
statistic computed from the observed data can distinguish them, so no
estimator --- matched, weighted, augmented or cross-fitted --- can recover
`E[Y1]` without an assumption that reaches beyond the data. -/
theorem estimands_differ : MA.P.E MA.Y1 ≠ MB.P.E MB.Y1 := by
  have ha : MA.P.E MA.Y1 = 1 / 2 := by
    norm_num [FinPop.E, MA, P2, Fin.sum_univ_two]
  have hb : MB.P.E MB.Y1 = 11 / 2 := by
    norm_num [FinPop.E, MB, P2, Fin.sum_univ_two]
  rw [ha, hb]; norm_num

end NoId

end VSR
