## P11 sentencing effects as intervals: custody against community supervision

Data: Statistics Canada, Juristat 85-002-X 2024001 article 00002, table 2: adults released from custody or starting community supervision in five provincial programs, 2015/16 to 2018/19 (68,225 people in the one-year follow-up), and whether they were convicted again within one, two or three years, by type of new conviction.

The effect is the difference in reconviction had everyone been sentenced to custody rather than community supervision. The data identify each sentence's outcome only for those who got it, so:

* **No assumption** (`morie_sentence_effect_bounds()`): the interval has width exactly one and contains zero (Research.P11.outcome_bounds, ate_width_one, ate_contains_zero).
* **Monotone treatment selection** (`morie_sentence_effect_mts()`): people sent to custody would be no less likely to be reconvicted than those supervised in the community under either sentence. The naive difference becomes an upper bound (Research.P11.Pop.mts_ate_le_naive).
* **Monotone treatment response as well**: custody does not lower anyone's chance of reconviction. The effect then lies in [0, naive difference] (Research.P11.Pop.mtr_mts_bounds).

| follow_up | conviction | custody_rate | community_rate | naive | worst_lower | worst_upper | mts_lower | mts_upper | mtr_mts_lower | mtr_mts_upper |
|---|---|---|---|---|---|---|---|---|---|---|
| one-year | any | 0.526 | 0.283 | 0.243 | -0.353 | 0.647 | -0.353 | 0.243 | 0 | 0.243 |
| one-year | violent | 0.164 | 0.0756 | 0.0884 | -0.355 | 0.645 | -0.355 | 0.0884 | 0 | 0.0884 |
| one-year | property | 0.277 | 0.111 | 0.165 | -0.336 | 0.664 | -0.336 | 0.165 | 0 | 0.165 |
| one-year | other | 0.181 | 0.0719 | 0.109 | -0.346 | 0.654 | -0.346 | 0.109 | 0 | 0.109 |
| one-year | administration_of_justice | 0.413 | 0.219 | 0.194 | -0.354 | 0.646 | -0.354 | 0.194 | 0 | 0.194 |
| two-year | any | 0.616 | 0.36 | 0.255 | -0.369 | 0.631 | -0.369 | 0.255 | 0 | 0.255 |
| two-year | violent | 0.247 | 0.123 | 0.124 | -0.354 | 0.646 | -0.354 | 0.124 | 0 | 0.124 |
| two-year | property | 0.362 | 0.16 | 0.202 | -0.335 | 0.665 | -0.335 | 0.202 | 0 | 0.202 |
| two-year | other | 0.267 | 0.116 | 0.151 | -0.342 | 0.658 | -0.342 | 0.151 | 0 | 0.151 |
| two-year | administration_of_justice | 0.5 | 0.274 | 0.226 | -0.357 | 0.643 | -0.357 | 0.226 | 0 | 0.226 |
| three-year | any | 0.662 | 0.404 | 0.258 | -0.38 | 0.62 | -0.38 | 0.258 | 0 | 0.258 |
| three-year | violent | 0.302 | 0.156 | 0.146 | -0.354 | 0.646 | -0.354 | 0.146 | 0 | 0.146 |
| three-year | property | 0.412 | 0.192 | 0.22 | -0.336 | 0.664 | -0.336 | 0.22 | 0 | 0.22 |
| three-year | other | 0.324 | 0.147 | 0.176 | -0.341 | 0.659 | -0.341 | 0.176 | 0 | 0.176 |
| three-year | administration_of_justice | 0.546 | 0.303 | 0.243 | -0.358 | 0.642 | -0.358 | 0.243 | 0 | 0.243 |

Reading, any new conviction within two years: 61.6% after custody and 36.0% after community supervision, a naive difference of 25.5 points. Without assumptions the effect of custody lies anywhere in [-36.9, 63.1] points; under selection monotonicity in [-36.9, 25.5]; adding response monotonicity, in [0, 25.5]. The naive gap is the largest effect custody could have under the assumptions the field usually makes about who is sent to custody, not an estimate of the effect, and nothing in these data rules out an effect of zero.

The cohorts differ before sentencing (table 1: 59% of the custody cohort had ten or more prior convictions against 20% of the community cohort), which is the selection the MTS assumption describes; the published tables give no covariates jointly with outcomes, so the intervals are not tightened by conditioning.

Theorems: Research.P11.outcome_bounds, Research.P11.ate_width_one, Research.P11.ate_contains_zero, Research.P11.Pop.mts_ate_le_naive, Research.P11.Pop.mtr_mts_bounds.
