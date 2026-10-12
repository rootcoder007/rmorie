# P15: the gender gap in Ontario segregation placement lengths, split

Source: OTIS segregation detailed dataset, 82,001 placements (fiscal years 2023-2025), consecutive days per placement.
Group 1 = women, group 0 = men; covariates = age category x region of the most recent placement
(15 cells).

## The mean gap, DFL reweighting (Research.P15.Reweight.decomposition)

| quantity | value |
|---|---|
| mean days, women | 4.014 |
| mean days, men | 2.593 |
| counterfactual (men reweighted to women's cells) | 2.45 |
| structure (women minus counterfactual) | 1.564 |
| composition (counterfactual minus men) | -0.1428 |
| largest covariate-share mismatch after reweighting | 1.11e-16 |

## By fiscal year

| fiscal_year | placements | women | mean_women | mean_men | counterfactual | structure | composition |
|---|---|---|---|---|---|---|---|
| 2023 | 29,341 | 5,517 | 2.96 | 2.39 | 2.26 | 0.698 | -0.13 |
| 2024 | 26,794 | 3,713 | 4.05 | 2.58 | 2.43 | 1.62 | -0.153 |
| 2025 | 25,866 | 4,301 | 5.34 | 2.83 | 2.74 | 2.6 | -0.0879 |

## The quantile gaps, RIF (Research.P15.RIF.rif_gap)

| tau | q_female | q_male | gap | quantile_gap | recentering_gap |
|---|---|---|---|---|---|
| 0.5 | 2 | 2 | 0.04348 | 0 | 0.04348 |
| 0.75 | 4 | 3 | 0.5266 | 1 | -0.4734 |
| 0.9 | 7 | 4 | 2.371 | 3 | -0.6291 |

## Reading

Women's placements are longer on average by 1.42 days. Reweighting men to women's age-by-region mix moves men's mean by -0.143 days (composition), so 1.56 days of the gap is structure: the same cell, a longer placement. The quantile gaps say where: the medians coincide (both 2 days), and the gap opens in the upper tail (3 days at the 90th percentile), which is the pattern a few long placements make, not a shift of the whole distribution.

What the split does not say: 'structure' is the part the two covariates do not explain, not discrimination; the placement reason, the alerts (mental health, suicide risk) and the institution are in the file and are the next covariates to add. The theorem's identity holds exactly (the mismatch above is zero), so the arithmetic is not in question, only the covariate set.

