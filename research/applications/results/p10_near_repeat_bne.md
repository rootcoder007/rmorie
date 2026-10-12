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

