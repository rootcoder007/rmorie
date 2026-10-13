## P18 selective labels: New York pretrial release, 2019-2024

Data: New York State DCJS Supplemental Pretrial Release Data File (1,379,219 arrest cycles, cases arraigned 2019-2024). 1,145,222 defendants were either at liberty pending trial (released on recognizance or non-monetary conditions, or bail posted within five days: 943,330) or held (remanded or bail not posted: 201,892); cases disposed at arraignment are excluded. The outcome is a fingerprintable rearrest within 180 days, observed only for those at liberty: 21.6% of them were rearrested.

A rule's rearrest rate is identified only on the part of its set that judges released; for the part they held it lies anywhere from no one to everyone rearrested (Research.P18.unobserved_bounds), so the interval's width is exactly the held share (unobserved_width) and a rule that releases only people judges already released is scored exactly (nested_rate_identified).

### Four release rules, statewide

| rule | outcome | cases | held_share | naive_rate | lower | upper | width | identified |
|---|---|---|---|---|---|---|---|---|
| misdemeanour charge, no pending case | any rearrest | 429,549 | 0.0426 | 0.124 | 0.118 | 0.161 | 0.0426 | FALSE |
| non-violent charge, no violent prior | any rearrest | 809,916 | 0.117 | 0.203 | 0.179 | 0.296 | 0.117 | FALSE |
| non-violent charge, no violent prior, no pending case | any rearrest | 540,444 | 0.0531 | 0.123 | 0.117 | 0.17 | 0.0531 | FALSE |
| release everyone | any rearrest | 1,145,222 | 0.176 | 0.216 | 0.178 | 0.354 | 0.176 | FALSE |
| misdemeanour charge, no pending case | violent felony rearrest | 429,549 | 0.0426 | 0.0169 | 0.0161 | 0.0587 | 0.0426 | FALSE |
| non-violent charge, no violent prior | violent felony rearrest | 809,916 | 0.117 | 0.0293 | 0.0259 | 0.143 | 0.117 | FALSE |
| non-violent charge, no violent prior, no pending case | violent felony rearrest | 540,444 | 0.0531 | 0.0166 | 0.0157 | 0.0688 | 0.0531 | FALSE |
| release everyone | violent felony rearrest | 1,145,222 | 0.176 | 0.0358 | 0.0295 | 0.206 | 0.176 | FALSE |

Reading: the naive rate (rearrest among the rule's set that judges released) is one point inside each interval, chosen by assuming the held would have behaved like the released. Releasing everyone has a rearrest rate between 17.8% and 35.4%; even the rule that releases only non-violent charges with no violent prior leaves 11.7% of its set unobserved.

### The same rule (non-violent charge, no violent prior) court by court (33 courts with 3,000+ cases)

| court | type | cases | release_share | rule_cases | held_share | lower | upper | naive_rate |
|---|---|---|---|---|---|---|---|---|
| Kings Criminal Court | NYC Criminal Court | 190,186 | 0.882 | 122,480 | 0.0666 | 0.203 | 0.27 | 0.218 |
| Bronx Criminal Court | NYC Criminal Court | 111,497 | 0.876 | 66,648 | 0.0611 | 0.207 | 0.268 | 0.22 |
| Nassau District Court | Non-NYC City or District Court | 66,005 | 0.876 | 52,681 | 0.0726 | 0.133 | 0.205 | 0.143 |
| Queens Criminal Court | NYC Criminal Court | 150,213 | 0.875 | 103,977 | 0.0782 | 0.169 | 0.247 | 0.183 |
| Richmond Criminal Court | NYC Criminal Court | 31,356 | 0.866 | 22,248 | 0.0805 | 0.202 | 0.282 | 0.22 |
| Suffolk 1st District Court | Non-NYC City or District Court | 77,218 | 0.839 | 63,615 | 0.112 | 0.168 | 0.281 | 0.19 |
| New York Criminal Court | NYC Criminal Court | 145,770 | 0.819 | 94,961 | 0.114 | 0.21 | 0.325 | 0.237 |
| Oswego City Court | Non-NYC City or District Court | 4,241 | 0.815 | 3,741 | 0.15 | 0.231 | 0.382 | 0.272 |
| Lockport City Court | Non-NYC City or District Court | 3,347 | 0.808 | 2,866 | 0.147 | 0.18 | 0.327 | 0.211 |
| Kingston City Court | Non-NYC City or District Court | 4,071 | 0.807 | 3,312 | 0.135 | 0.181 | 0.316 | 0.209 |

| court | type | cases | release_share | rule_cases | held_share | lower | upper | naive_rate |
|---|---|---|---|---|---|---|---|---|
| Albany City Criminal Court | Non-NYC City or District Court | 10,164 | 0.676 | 6,601 | 0.203 | 0.136 | 0.339 | 0.17 |
| Auburn City Court | Non-NYC City or District Court | 4,114 | 0.673 | 3,438 | 0.277 | 0.157 | 0.434 | 0.217 |
| Buffalo City Court | Non-NYC City or District Court | 31,513 | 0.622 | 19,739 | 0.276 | 0.138 | 0.414 | 0.191 |
| Syracuse City Court | Non-NYC City or District Court | 25,723 | 0.622 | 16,902 | 0.255 | 0.167 | 0.423 | 0.225 |
| Poughkeepsie City Court | Non-NYC City or District Court | 3,822 | 0.591 | 2,811 | 0.311 | 0.116 | 0.427 | 0.168 |

Across courts the rank correlation between a court's release share and the width of the rule's interval there is -0.95: the most lenient courts leave the least of the rule unobserved, which is the contraction argument (Research.P18.chain_contraction), here across different caseloads rather than one defendant pool, so the courts' intervals are each valid for their own cases and not for one another's.

Theorems: Research.P18.observed_rate_is_conditional, Research.P18.nested_rate_identified, Research.P18.unobserved_bounds, Research.P18.unobserved_width, Research.P18.unobserved_ends_attained.
