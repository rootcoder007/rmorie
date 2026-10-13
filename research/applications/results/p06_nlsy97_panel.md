## P6 the age-crime curve: a cohort panel (NLSY97)

Data: Bureau of Justice Statistics, Recidivism in the National Longitudinal Survey of Youth 1997, Standalone Data (Rounds 1 to 13), ICPSR 34562-v1 (2014), DS0001: 8,984 respondents. Offending at an age is any of six self-reported acts (destroying property, theft under or over $50, other property crime, selling drugs, assault) in the calendar year the respondent turned that age. The file is used under ICPSR's terms of use and is not redistributed here.

### The aggregate curve

| age | respondents | offending_share |
|---|---|---|
| 14 | 3,493 | 0.3 |
| 15 | 5,194 | 0.3 |
| 16 | 6,927 | 0.292 |
| 17 | 8,405 | 0.259 |
| 18 | 8,127 | 0.221 |
| 19 | 7,902 | 0.181 |
| 20 | 6,224 | 0.153 |
| 21 | 4,577 | 0.13 |
| 22 | 2,978 | 0.111 |
| 23 | 1,383 | 0.103 |

This curve alone cannot separate one offender type from a mixture of types: every mixture's aggregate is also the curve of a single type (Research.P6.aggregate_not_identifying, single_aggregate).

### The same people at two ages

| age_a | age_b | n | rate_a | rate_b | cross_cov | correlation | p_value |
|---|---|---|---|---|---|---|---|
| 14 | 15 | 3,322 | 0.299 | 0.281 | 0.069 | 0.335 | 0.0005 |
| 14 | 18 | 3,226 | 0.299 | 0.192 | 0.0326 | 0.181 | 0.0005 |
| 15 | 20 | 3,143 | 0.317 | 0.14 | 0.0364 | 0.225 | 0.0005 |
| 16 | 17 | 6,574 | 0.292 | 0.238 | 0.0739 | 0.382 | 0.0005 |
| 16 | 20 | 4,681 | 0.306 | 0.148 | 0.0356 | 0.218 | 0.0005 |
| 18 | 19 | 7,595 | 0.221 | 0.177 | 0.0578 | 0.365 | 0.0005 |
| 18 | 22 | 2,799 | 0.258 | 0.109 | 0.028 | 0.206 | 0.0005 |
| 20 | 23 | 1,265 | 0.172 | 0.1 | 0.0333 | 0.293 | 0.0005 |

Under a single type (everyone's offending at each age drawn from one shared rate, independently across ages) the covariance across ages is exactly zero (Research.P6.Panel.one_type_cov_zero); the permutation null keeps each age's marginal and breaks the link. Every pair above has a positive covariance, every one beyond all 2,000 permutations, and correlations from 0.18 to 0.38: the people who offend at one age are the ones who offend at another, which one shared curve cannot produce. A two-type population with 20% at rates 0.70 and 0.50 against 80% at 0.10 and 0.05 would give covariance 0.0432 (two_type_cov), the order of what is observed. The test refutes invariance-of-type; it does not by itself say how many types there are (the Teicher-type identification of K types from T periods is the open part of P6).

Theorems: Research.P6.single_aggregate, Research.P6.aggregate_not_identifying, Research.P6.Panel.cross_moment_eq, Research.P6.Panel.one_type_cov_zero, Research.P6.Panel.two_type_cov, Research.P6.Panel.aggregate_blind.
