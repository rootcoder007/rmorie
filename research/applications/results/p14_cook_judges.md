## P14 judge leniency: can Cook County's felony judges be ordered?

Data: Cook County State's Attorney sentencing file (305,884 rows); primary charge, current sentence, original sentencing, prison/jail/boot camp against probation/conditional discharge/supervision, 2011-2023, the 26th Street courthouse, offence categories with at least 2,000 sentences and judges with at least 300: 86,568 sentences, 53 judges, 14 categories.

Monotonicity says a stricter judge incarcerates everyone a more lenient judge does. The file has no outcome after sentencing, so no Wald ratio is formed here; what can be tested is the implication that needs none: inside every offence category the stricter judge overall is still at least as strict (Research.P14Slope.subgroup_propensity_mono). Each pair of judges with 50 or more cases in a category is compared, one-sided, and the p-values Holm-adjusted over all comparisons; a significant reversal means that pair is nested in neither direction (crossing_refutes_both).

### Across offence categories

| comparisons | reversed | share_reversed | significant | pairs_refuted | judges_involved |
|---|---|---|---|---|---|
| 6,564 | 1,745 | 0.266 | 36 | 36 | 28 |

| group | judges | cases | slope |
|---|---|---|---|
| Aggravated Battery Police Officer | 31 | 2,530 | 0.449 |
| Aggravated DUI | 36 | 6,142 | 1.22 |
| Armed Robbery | 21 | 1,704 | -0.118 |
| Burglary | 34 | 3,811 | 0.863 |
| Driving With Suspended Or Revoked License | 33 | 3,491 | 1.08 |
| Escape - Failure to Return | 21 | 1,529 | 0.366 |
| Failure to Register as a Sex Offender | 18 | 1,206 | 2.27 |
| Narcotics | 53 | 36,628 | 0.947 |
| Possession of Stolen Motor Vehicle | 20 | 1,462 | 1.59 |
| Residential Burglary | 15 | 1,237 | 1.42 |
| Retail Theft | 35 | 3,351 | 1.85 |
| Robbery | 18 | 1,356 | 0.672 |
| Theft | 21 | 1,526 | 1.13 |
| UUW - Unlawful Use of Weapon | 49 | 13,225 | 1.48 |

The slope is the regression of a judge's rate in the category on the judge's overall rate (weighted by cases): positive slopes everywhere are what average monotonicity looks like; a slope at or below zero would mean the overall ranking says nothing about that category.

### The strongest reversals

| group | stricter | lenient | rate_stricter | rate_lenient | group_rate_stricter | group_rate_lenient | n_stricter | n_lenient | p_adjusted |
|---|---|---|---|---|---|---|---|---|---|
| UUW - Unlawful Use of Weapon | judge 47 | judge 49 | 0.51 | 0.498 | 0.421 | 0.822 | 273 | 191 | 2.09e-14 |
| UUW - Unlawful Use of Weapon | judge 40 | judge 49 | 0.535 | 0.498 | 0.433 | 0.822 | 178 | 191 | 2.78e-11 |
| UUW - Unlawful Use of Weapon | judge 43 | judge 49 | 0.528 | 0.498 | 0.512 | 0.822 | 369 | 191 | 2.80e-09 |
| UUW - Unlawful Use of Weapon | judge 18 | judge 19 | 0.583 | 0.582 | 0.582 | 0.823 | 306 | 417 | 3.27e-09 |
| UUW - Unlawful Use of Weapon | judge 23 | judge 49 | 0.578 | 0.498 | 0.532 | 0.822 | 295 | 191 | 2.23e-07 |
| UUW - Unlawful Use of Weapon | judge 30 | judge 49 | 0.56 | 0.498 | 0.549 | 0.822 | 417 | 191 | 2.90e-07 |
| UUW - Unlawful Use of Weapon | judge 37 | judge 49 | 0.543 | 0.498 | 0.525 | 0.822 | 236 | 191 | 4.36e-07 |
| UUW - Unlawful Use of Weapon | judge 34 | judge 49 | 0.556 | 0.498 | 0.53 | 0.822 | 232 | 191 | 8.92e-07 |
| UUW - Unlawful Use of Weapon | judge 22 | judge 49 | 0.578 | 0.498 | 0.55 | 0.822 | 309 | 191 | 1.75e-06 |
| UUW - Unlawful Use of Weapon | judge 48 | judge 49 | 0.504 | 0.498 | 0.524 | 0.822 | 185 | 191 | 2.33e-06 |
| UUW - Unlawful Use of Weapon | judge 11 | judge 14 | 0.621 | 0.599 | 0.651 | 0.834 | 416 | 435 | 2.99e-06 |
| UUW - Unlawful Use of Weapon | judge 28 | judge 49 | 0.562 | 0.498 | 0.568 | 0.822 | 384 | 191 | 5.38e-06 |

### Across years, narcotics only

| comparisons | reversed | share_reversed | significant | pairs_refuted | judges_involved |
|---|---|---|---|---|---|
| 4,056 | 1,233 | 0.304 | 4 | 4 | 7 |

Reading: 1,745 of 6,564 comparisons reverse the overall order, and 36 reversals are significant after adjustment, involving 36 pairs and 28 of the 53 judges. Nesting is a statement about the same defendant before two judges, so the subgroup implication compares like with like only when cases reach judges as good as at random within the category. Felony cases at 26th Street are assigned to courtrooms by calendar, not by lottery, so each significant reversal is one of two things: the pair is not nested (monotonicity fails), or the two judges saw different kinds of case inside the category (the instrument is not as good as random). Either one breaks a judge design that ranks these judges on one leniency scale; a design here needs average monotonicity, case-mix controls finer than the category, or the defier sensitivity of morie_judge_iv().

Theorems: Research.P14Slope.propensity_mono, Research.P14Slope.subgroup_propensity_mono, Research.P14Slope.subgroup_reversal_refutes, Research.P14Slope.crossing_refutes_both.
