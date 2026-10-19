# IndigenousWellness

Lean 4 (Mathlib, `v4.34.0`) proofs of the mathematics behind the First Nations, Metis and
Inuit health and mental wellness estimators of `rmoriebricklayer` 1.0.0, checked against the
methods paper (`indigenous_wellness_methods_paper/main.tex`, section Methods). The library
`IndigenousWellness` is a `[[lean_lib]]` in `lakefile.toml` (and a default target); it
imports only Mathlib. On a small machine compile it one file at a time from `research/lean`:
`nice lake env lean IndigenousWellness/<File>.lean` (add `-o .lake/build/lib/lean/IndigenousWellness/<File>.olean`
to let `IndigenousWellness.lean` and `Audit.lean` import it). `lake env lean IndigenousWellness/Audit.lean`
prints the axioms of all 120 theorems (`audit.txt`): only `propext`, `Classical.choice`,
`Quot.sound`; no `sorryAx`.

What is proved is the mathematics under the stated hypotheses. Sampling claims (coverage,
asymptotic normality, the chi-square law of Q, the bootstrap's validity) are not formalised.

## Intervals.lean — Methods, "The published-interval calculus", "Two continua", "Healing as process", "The Call to Action 19 scale"

| Theorem | Paper | R function |
|---|---|---|
| `seLin_nonneg`, `seLog_nonneg`, `seLin_symm` | Definition (s_g, σ_g) | `indgap`, `limfrom` |
| `seLin_scale`, `seLog_scale`, `ratio_scale`, `diff_scale` | rescaling invariance (percent vs proportion) | `indgap`, `dualcont` |
| `diffSe_ge`, `diffInt_contains` | d ± z√(s_g²+s_r²); Δ ± z√(s₁²+s₂²) | `indgap`, `healchg` |
| `logLo_eq`, `logHi_eq`, `logInt_pos`, `logInt_contains`, `logInt_mul`, `logInt_contains_one_iff` | exp{log ρ ± z√(σ_g²+σ_r²)}; strength ratio interval | `indgap`, `dualcont` |
| `ratio_lt_one_iff`, `ratio_gt_one_iff` | sign of gap; "R > 1 means more people report the wellness" | `indgap`, `dualcont` |
| `disratio_higher`, `disratio_gt_one_iff`, `inv_logHi`, `inv_logLo`, `inv_interval`, `disratio_interval` | equation for T; "T > 1 means the group fares worse"; "T = 1.19 … 19% longer" | `trchlthgap` |
| `expit_pos`, `expit_lt_one`, `expit_strictMono`, `expit_logit`, `logit_strictMonoOn`, `logitInt`, `logitInt_mono`, `hasDerivAt_logit` | "The interval is built on the logit scale" | `ordcmp` |

## Ordinal.lean — Methods, "Whole rating distributions" (equations for A and Var(Â))

| Theorem | Paper | R function |
|---|---|---|
| `mw_add_swap`, `mw_swap`, `mw_self` | A; A(q,p) = 1 − A(p,q); A(p,p) = 1/2 | `ordcmp` (`probbet`) |
| `mw_nonneg`, `mw_le_one`, `cliff_bounds` | A ∈ [0,1]; δ = 2A − 1 ∈ [−1,1], antisymmetric | `ordcmp` (`delta`) |
| `placement_ref` | G₁(k) = P_{>k} + p_k/2 has q-mean A | `ordcmp` |
| `var_nonneg`, `ordvar_nonneg` | Var(Â) formula is a sum of two variances, ≥ 0 | `ordcmp` |

## Balance.lean — Methods, "Holistic balance" (Definition, both equations), "The four directions"

| Theorem | Paper | R function |
|---|---|---|
| `gm_eq_prod`, `gm_pos`, `gm_le_level`, `level_pos` | exp(∑ w log p) is the weighted geometric mean; AM-GM | `holbal` |
| `bal_pos`, `bal_le_one`, `atkinson_bounds` | 0 < B ≤ 1; B = 1 − 𝒜₁ | `holbal` |
| `bal_eq_one_iff`, `bal_eq_one_iff_equal`, `bal_const` | "equality exactly when every domain is at the same level" (of positive weight) | `holbal` |
| `bal_scale` | "B is scale-invariant" | `holbal` |
| `level_between` | L between the weakest and strongest domain | `holbal` |
| `bal_example` | the 80/80/20/20 example: L = 1/2, G = 2/5, **B = 4/5** | `holbal` |
| `bal2`, `bal_not_concave` | "Since B is concave in the p_k" — **false** | `holbal` |
| `balci_bounds` | equation for B_lo, B_hi: 0 ≤ B_lo ≤ B̂ ≤ B_hi ≤ 1 | `holbal` |
| `direction_se` | direction se √(∑ s_j²)/m | `mhcont` |

## Standardisation.lean — Methods, "Age standardisation", "Census profile shares"

| Theorem | Paper | R function |
|---|---|---|
| `std_between`, `std_const`, `stdweights_sum`, `std_own_is_crude` | ∑ w_i p_i is a convex combination; equals the crude rate for the own standard | `stdprev`, `agestd`, `cnsstd` |
| `std_se_le`, `stdprev_cut` | se √(∑ w² s²) ≤ max s; the cut interval contains the estimate | `stdprev` |
| `fayfeuer_moments`, `fayfeuer_upper_moments`, `dsr_var_nonneg`, `fayfeuer_single` | Fay–Feuer gamma interval: moment matching; reduces to Garwood for one stratum | `agestd` |
| `round_unbiased`, `sum_range_id_real`, `sum_range_sq_real`, `roundvar_avg`, `roundvar_five` | c = (b² − 1)/6, c = 4 for b = 5 | `profshr` |
| `share_delta` | rounding term c(1 + p²)/N² | `profshr` |
| `whalf_eq`, `wilson_bounds` | Wilson interval at the effective size lies in [0,1] and contains p | `profshr` |

## Indices.lean — Methods, "Community Well-Being", "Community-weighted protective and risk factors", "Wellness beside distress", "A test before pooling"

| Theorem | Paper | R function |
|---|---|---|
| `cwbinc_at_a`, `cwbinc_at_b`, `cwbinc_strictMonoOn`, `cwbinc_bounds`, `clamp_bounds` | income component 100(log y − log a)/(log b − log a) | `cwbscr`, `cnscwb` |
| `cwb_between`, `cwb_bounds`, `components_bounds` | CWB averages four 0–100 components | `cwbscr`, `cnscwb` |
| `commidx_abs_le`, `commidx_between` | C = ∑ w p / ∑ \|w\| | `commidx` |
| `framing_bounds`, `framing_overall_between` | F = \|𝒟\|/\|ℐ\|, overall and per group | `frmaud` |
| `cochran_nonneg`, `cochran_eq`, `cochran_eq_zero_iff`, `pooled_between`, `i2_bounds`, `i2_paper` | equation for Q and I²; Results "Q = 11.97 on 2 df, I² = 83%" | `dstntst` |

## Association.lean — Methods, "Connection across places"

| Theorem | Paper | R function |
|---|---|---|
| `corr_abs_le_one`, `corr_self` | Spearman's ρ (Pearson on ranks) ∈ [−1,1] | `connassoc` |
| `perm_mean`, `perm_ssq`, `perm_corr` | permutation distribution: only the cross-product moves | `connassoc` |
| `exact_p_bounds` | exact p over all n! orderings ∈ [1/n!, 1] | `connassoc` |
| `mc_p_bounds` | Monte Carlo (b + 1)/(R + 1) ∈ (0,1] | `connassoc` |

## Causal.lean — Methods, "Belonging and mental health, person by person" (AIPW equation, replicate variance); Results balance figures

| Theorem | Paper | R function |
|---|---|---|
| `E_split`, `cellE_*`, `aipw_cell` | per-cell algebra of the AIPW score | `aipwcf` |
| `aipw_double_robust` | "consistent if either the outcome or the propensity model is right" (population/oracle form) | `aipwcf` (ATE) |
| `ipw_ate` | IPW as the case g₁ = g₀ = 0 | `aipwcf` |
| `ipw_balance` | after inverse-probability weighting the covariate means agree (population) | `baltab(w = wipw)` in `imhperson.R` |
| `aipw_const_eq_unadjusted` | AIPW with constant models = unadjusted weighted difference | `imhperson.R` (unadjusted) vs `aipwcf` |
| `repvar_nonneg`, `repvar_eq_zero_iff`, `repvar_decomp` | Var̂(θ̂) = (1/1000) ∑ (θ̂⁽ᵇ⁾ − θ̂)² | `svydes(type = "bootstrap", mse = TRUE)`, `aipwcf(des = )` |
| `wmean_affine`, `sd_affine`, `smd_affine` | standardised difference (0.20, 0.11, …) | `baltab` |
