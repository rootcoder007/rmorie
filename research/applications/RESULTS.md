# Ledger data applications: results

Generated 2026-10-12 by research/applications/run_all.R (rmorie 1.4.3).

| script | status |
|---|---|
| p03_toronto_neighbourhood_network.R | ok |
| p07_concentration_toronto.R | ok |
| p10_near_repeat_bne.R | ok |
| p15_otis_placement_lengths.R | ok |

# P3: the Toronto neighbourhood network as the interference design

Source: City of Toronto Neighbourhoods (158 polygons, WGS84) and Neighbourhood Improvement Areas; queen contiguity (475 edges, degrees 3 to 11, mean 6).
The ledger names street segments; the public files carry neighbourhoods, so this is the neighbourhood-level
version of the design computation and is labelled as such.

## The NIA cut (33 of 158 neighbourhoods)

| quantity | value |
|---|---|
| edges cut | 98 |
| volume of S | 190 |
| volume of the complement | 760 |
| conductance h(S) | 0.515789473684211 |
| Rayleigh quotient of the test vector | 0.644736842105263 |
| lambda_2 of the normalised Laplacian | 0.0157451440809582 |
| lambda_2 <= Rayleigh <= 2 h(S) holds | TRUE |

The conductance is the share of the NIAs' edge volume that leaves the set: 0.516. That is a scatter, not a cluster: about half of every NIA's neighbours are outside the set, so a treatment confined to the NIAs puts most of its contiguity spillover on neighbourhoods that are not treated, and a comparison of NIAs with their neighbours is a comparison of treated places with the places that received the spillover. lambda_2 = 0.01575 is the spectral gap of the whole city graph; the proved chain lambda_2 <= E(f)/D(f) <= 2 h(S) is checked above, and the gap between 0.01575 and 1.03 is the room the Cheeger hard direction (lambda_2 >= h^2/2, still open in Lean) would close.

## Exposure probabilities under a completely randomised allocation of 33 treated neighbourhoods (5,000 draws)

| class | smallest_probability | largest_probability |
|---|---|---|
| untreated, no treated neighbour | 0.0558 | 0.3956 |
| untreated, a treated neighbour | 0.389 | 0.7394 |
| treated | 0.1948 | 0.2242 |

The ten neighbourhoods with the rarest exposure class (the ones that set the Horvitz-Thompson variance):

| neighbourhood | degree | nia | p_untreated_no_treated_neighbour | p_untreated_treated_neighbour | p_treated |
|---|---|---|---|---|---|
| 098 | 11 | FALSE | 0.0558 | 0.7394 | 0.2048 |
| 052 | 9 | FALSE | 0.0858 | 0.705 | 0.2092 |
| 128 | 9 | FALSE | 0.0868 | 0.7078 | 0.2054 |
| 039 | 9 | FALSE | 0.088 | 0.694 | 0.218 |
| 120 | 9 | FALSE | 0.0882 | 0.7048 | 0.207 |
| 038 | 9 | FALSE | 0.0898 | 0.7044 | 0.2058 |
| 040 | 9 | FALSE | 0.0898 | 0.6972 | 0.213 |
| 071 | 9 | FALSE | 0.0912 | 0.7 | 0.2088 |
| 095 | 9 | FALSE | 0.0918 | 0.7 | 0.2082 |
| 056 | 9 | FALSE | 0.094 | 0.6934 | 0.2126 |

Every neighbourhood must have a positive probability of each exposure class for the Horvitz-Thompson estimator of the spillover effect to exist (Research.P3's design condition); the table shows the classes and the smallest probabilities, which set the variance. The outcomes (break and enter by neighbourhood) are not used here: the NIAs were not randomised, so an HT estimate would not be one.


---

# P7: concentration of crime across Toronto's 158 neighbourhoods

Source: City of Toronto / Toronto Police Service, Neighbourhood Crime Rates (158 neighbourhoods, 2014-2025).
Statistic: the Gini of the neighbourhood counts. Null: every neighbourhood draws a Poisson count at the
common mean (2,000 simulations, seed 1). Bound: Gini <= CV / sqrt(2), so a Poisson null caps the Gini at
about 1 / sqrt(2 mean) (`Research.P7.Bound.gini_le_cv`, `morie_concentration_null_bound()`).

## Every offence, 2025

| offence | total | mean | gini | null_bound | null_mean | null_q95 | excess | p_value |
|---|---|---|---|---|---|---|---|---|
| ASSAULT | 24,686 | 156 | 0.39 | 0.0566 | 0.0448 | 0.049 | 0.345 | 0 |
| AUTOTHEFT | 7,240 | 45.8 | 0.417 | 0.104 | 0.0825 | 0.0908 | 0.334 | 0 |
| BIKETHEFT | 2,121 | 14.4 | 0.629 | 0.186 | 0.147 | 0.162 | 0.482 | 0 |
| BREAKENTER | 6,092 | 38.6 | 0.34 | 0.114 | 0.09 | 0.0984 | 0.25 | 0 |
| HOMICIDE | 45 | 1.22 | 0.139 | 0.641 | 0.475 | 0.573 | -0.336 | 1 |
| ROBBERY | 2,522 | 16 | 0.4 | 0.177 | 0.14 | 0.153 | 0.26 | 0 |
| SHOOTING | 262 | 2.52 | 0.384 | 0.446 | 0.344 | 0.386 | 0.0397 | 0.056 |
| THEFTFROMMV | 6,801 | 43 | 0.355 | 0.108 | 0.0852 | 0.0933 | 0.27 | 0 |
| THEFTOVER | 1,951 | 12.3 | 0.447 | 0.201 | 0.158 | 0.174 | 0.289 | 0 |

## Break and enter, every year

| year | total | mean | gini | null_bound | null_mean | null_q95 | excess | p_value |
|---|---|---|---|---|---|---|---|---|
| 2014 | 7,184 | 45.5 | 0.305 | 0.105 | 0.0829 | 0.0908 | 0.222 | 0 |
| 2015 | 6,911 | 43.7 | 0.309 | 0.107 | 0.0845 | 0.0924 | 0.224 | 0 |
| 2016 | 6,402 | 40.5 | 0.317 | 0.111 | 0.0878 | 0.0964 | 0.229 | 0 |
| 2017 | 6,883 | 43.6 | 0.341 | 0.107 | 0.0847 | 0.0928 | 0.256 | 0 |
| 2018 | 7,565 | 47.9 | 0.351 | 0.102 | 0.0807 | 0.0885 | 0.27 | 0 |
| 2019 | 8,438 | 53.4 | 0.369 | 0.0968 | 0.0765 | 0.0835 | 0.293 | 0 |
| 2020 | 6,914 | 43.8 | 0.388 | 0.107 | 0.0845 | 0.0928 | 0.304 | 0 |
| 2021 | 5,670 | 35.9 | 0.4 | 0.118 | 0.0933 | 0.102 | 0.307 | 0 |
| 2022 | 6,043 | 38.2 | 0.355 | 0.114 | 0.0904 | 0.0986 | 0.265 | 0 |
| 2023 | 7,634 | 48.3 | 0.358 | 0.102 | 0.0804 | 0.0879 | 0.277 | 0 |
| 2024 | 6,824 | 43.2 | 0.343 | 0.108 | 0.0851 | 0.093 | 0.258 | 0 |
| 2025 | 6,092 | 38.6 | 0.34 | 0.114 | 0.09 | 0.0984 | 0.25 | 0 |

## Reading

For every offence with more than a handful of events the observed Gini sits far above the Poisson null's mean and its 95th percentile (p = 0 in 2,000 draws): the concentration is not the arithmetic of rare counts. Homicide and shootings (means of 1.2 and 2.5 per neighbourhood) are the cases the null bound warns about: their null Ginis are already 0.48 and 0.34, so most of their observed concentration is what a Poisson count at that mean produces by itself.

The null is the equal-rate Poisson; neighbourhood populations range from 8,089 to 44,636, so part of the excess is population, not concentration of risk. A rate-adjusted null (Poisson at a mean proportional to population) is the next step and needs a proved bound of its own.


---

# P10: near-repeat contagion in Toronto break and enter, 2014-2024

Source: Toronto Police Service Public Safety Data Portal, Break and Enter occurrences (neighbourhood and
occurrence date; rows with no neighbourhood or an occurrence year outside 2014-2024 dropped).

## Second moments of the weekly neighbourhood counts

| quantity | value |
|---|---|
| neighbourhoods | 158 |
| weeks | 574 |
| occurrences | 76,466 |
| mean weekly count (median across neighbourhoods) | 0.7064 |
| dispersion var/mean (median) | 1.325 |
| share of neighbourhoods with dispersion > 1 | 1 |
| share with dispersion > 1.5 | 0.1646 |
| gamma-mixture size with the same moments (median) | 2.278 |

Every neighbourhood's weekly series is over-dispersed. By `Research.P10.Moments.second_moments_do_not_identify`
that over-dispersion is reproduced exactly by a Poisson process whose rate varies across weeks (a gamma mixture
with the size above) and by a self-exciting process with no heterogeneity at all; the witness laws of
`morie_contagion_moment_witness()` (dispersion 1 and 1) are the finite version of the same fact.
The second moments therefore commit the analyst to over-dispersion and to nothing about its source.

## What the temporal order adds: Hawkes fits of the five busiest neighbourhoods, 2014-2024

| neighbourhood | occurrences | dispersion | mu_per_day | kappa | omega_per_day | half_life_days |
|---|---|---|---|---|---|---|
| 095 | 1,676 | 1.405 | 0.1873 | 0.5481 | 0.05608 | 12.36 |
| 073 | 1,634 | 1.538 | 0.2976 | 0.2832 | 0.4076 | 1.701 |
| 078 | 1,581 | 2.106 | 0.08305 | 0.7912 | 0.03861 | 17.95 |
| 001 | 1,542 | 1.671 | 0.2852 | 0.2446 | 0.5369 | 1.291 |
| 164 | 1,436 | 1.399 | 0.1354 | 0.6274 | 0.03965 | 17.48 |

The branching ratio is identified by the conditional intensity, the dependence of today's rate on the timing of past events, which is a third-order (and higher) property. Reading kappa as 'the share of break-ins caused by earlier break-ins' assumes the background is constant; a background that moves with the seasons produces clustering the Hawkes fit attributes to self-excitation. The constant-versus-KDE background comparison (library scan A17) is the honest next step and is not done here.


---

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
| largest covariate-share mismatch after reweighting | 0.000000000000000111 |

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


---

