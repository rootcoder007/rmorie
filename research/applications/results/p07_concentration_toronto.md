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

## Rank persistence: the top-16 break-and-enter neighbourhoods, year to year

| years | overlap | null_mean | p_value |
|---|---|---|---|
| 2014-2015 | 12 | 1.62 | 9.07e-12 |
| 2015-2016 | 11 | 1.62 | 6.05e-10 |
| 2016-2017 | 12 | 1.62 | 9.07e-12 |
| 2017-2018 | 13 | 1.62 | 8.00e-14 |
| 2018-2019 | 11 | 1.62 | 6.05e-10 |
| 2019-2020 | 14 | 1.62 | 3.66e-16 |
| 2020-2021 | 12 | 1.62 | 9.07e-12 |
| 2021-2022 | 13 | 1.62 | 8.00e-14 |
| 2022-2023 | 11 | 1.62 | 6.05e-10 |
| 2023-2024 | 13 | 1.62 | 8.00e-14 |
| 2024-2025 | 12 | 1.62 | 9.07e-12 |

Under the null that this year's top-16 set is a random 16-subset with no memory of last year's, the overlap is hypergeometric with mean 1.6 (`Research.P7.Persistence.overlap_prob`, `morie_rank_persistence_null()`). The observed overlaps are 11 to 14 of 16, with null upper-tail probabilities below 1e-6 in every pair of years: the hot neighbourhoods persist, and the persistence is not what a memoryless draw produces, which is the part of the law of concentration that chance cannot explain.

## Reading

For every offence with more than a handful of events the observed Gini sits far above the Poisson null's mean and its 95th percentile (p = 0 in 2,000 draws): the concentration is not the arithmetic of rare counts. Homicide and shootings (means of 1.2 and 2.5 per neighbourhood) are the cases the null bound warns about: their null Ginis are already 0.48 and 0.34, so most of their observed concentration is what a Poisson count at that mean produces by itself.

The null is the equal-rate Poisson; neighbourhood populations range from 8,089 to 44,636, so part of the excess is population, not concentration of risk. A rate-adjusted null (Poisson at a mean proportional to population) is the next step and needs a proved bound of its own.

