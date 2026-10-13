## P17 incapacitation: what a custody year prevents, read from published reconviction

Data: Statistics Canada, Juristat 85-002-X 2024001 article 00002 (reconvictions of adults released from custody or community supervision in five provincial programs, 2015/16 to 2018/19), table 4; Government of Ontario, Rates of recidivism in Ontario, releases by risk level, 2014/15.

Only convictions are published, so the unit is convictions: the conviction rate while free is lambda q, and the incapacitation identity's prevented share lambda q S/(1 + lambda q S) depends on lambda and q only through it (Research.P17.steady_state_rate, prevented_share_eq).

### The conviction hazard falls with time in the community

| quarter | custody | community | custody_rate | community_rate |
|---|---|---|---|---|
| 0 to 3 | 0.3 | 0.14 | 0.357 | 0.151 |
| 3 to 6 | 0.17 | 0.08 | 0.186 | 0.0834 |
| 6 to 9 | 0.11 | 0.06 | 0.117 | 0.0619 |
| 9 to 12 | 0.09 | 0.05 | 0.0943 | 0.0513 |
| 12 to 15 | 0.06 | 0.04 | 0.0619 | 0.0408 |
| 15 to 18 | 0.05 | 0.03 | 0.0513 | 0.0305 |
| 18 to 21 | 0.05 | 0.02 | 0.0513 | 0.0202 |
| 21 to 24 | 0.04 | 0.02 | 0.0408 | 0.0202 |
| 24 to 27 | 0.04 | 0.02 | 0.0408 | 0.0202 |
| 27 to 30 | 0.03 | 0.02 | 0.0305 | 0.0202 |
| 30 to 33 | 0.03 | 0.02 | 0.0305 | 0.0202 |
| 33 to 36 | 0.03 | 0.02 | 0.0305 | 0.0202 |
| 36 to 39 | 0.03 | 0.02 | 0.0305 | 0.0202 |
| 39 to 42 | 0.02 | 0.01 | 0.0202 | 0.0101 |
| 42 to 45 | 0.01 | 0.01 | 0.0101 | 0.0101 |
| 45 to 48 | 0.01 | 0.01 | 0.0101 | 0.0101 |

Read as the rate path of a person still conviction-free, the hazard is non-increasing, which is the desistance assumption of Research.P17Replacement. A year of custody then prevents between S lambda(t0 + S) and S lambda(t0) first reconvictions (prevented_ge_const, prevented_le_const), and fewer when it is served later (later_sentence_prevents_less):

| released_from | starts_after_quarters | prevented | upper | lower | later |
|---|---|---|---|---|---|
| custody | 0 | 0.754 | 1.43 | 0.248 | 0.459 |
| custody | 4 | 0.205 | 0.248 | 0.163 | 0.184 |
| custody | 8 | 0.132 | 0.163 | 0.122 | 0.122 |
| community | 0 | 0.347 | 0.603 | 0.163 | 0.237 |
| community | 4 | 0.112 | 0.163 | 0.0808 | 0.0911 |
| community | 8 | 0.0808 | 0.0808 | 0.0808 | 0.0808 |

A year imposed at release prevents 0.754 first reconvictions per person released from custody, against 1.427 from the entry-rate constant estimate (its upper bound); the same year served two years later prevents 0.132. For community supervision the figures are 0.347, 0.603 and 0.081.

### A uniform term by risk level (Ontario, institutional releases)

| risk_level | releases | reconvicted_2y_pct | conviction_rate_per_year | prevented_share_6m | prevented_share_12m |
|---|---|---|---|---|---|
| Very low | 76 | 2.6 | 0.0132 | 0.00654 | 0.013 |
| Low | 265 | 3.8 | 0.0194 | 0.00959 | 0.019 |
| Medium | 668 | 16.9 | 0.0926 | 0.0442 | 0.0847 |
| High | 934 | 38.4 | 0.242 | 0.108 | 0.195 |
| Very high | 667 | 64.6 | 0.519 | 0.206 | 0.342 |

The prevented share rises with the conviction rate (Research.P17.high_rate_more_prevented): a uniform six-month term removes 0.7% of a very-low-risk releasee's convictions and 20.6% of a very-high-risk one's, 32 times as much.

Caveats: the bounds above are theorems about one person's non-increasing rate path; the published path is the cohort's hazard among those still conviction-free, which falls both because people desist and because the most likely to be reconvicted leave the risk set first. The table cannot separate the two. The hazard counts first reconvictions only, and neither source says how much offending lies behind each conviction (q), so the results are in convictions, not crimes.

Theorems: Research.P17.steady_state_rate, Research.P17.cycle_rate, Research.P17.prevented_share_eq, Research.P17.prevented_share_lt_one, Research.P17.rate_antitone_in_S, Research.P17.rate_antitone_in_q, Research.P17.marginal_prevention_eq, Research.P17.high_rate_more_prevented, Research.P17Replacement.prevented_le_const, Research.P17Replacement.prevented_ge_const, Research.P17Replacement.later_sentence_prevents_less, Research.P17Replacement.replaced_antitone, Research.P17Replacement.prevented_net_le.
