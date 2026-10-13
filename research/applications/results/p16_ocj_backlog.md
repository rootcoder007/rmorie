## P16 court backlog: Ontario Court of Justice criminal statistics

Data: OCJ criminal court statistics, clearance-rate and disposition-rate workbooks (2018-10 to 2026-03, 61 courts, each month from its latest edition), and the offence-based workbook for 2026.

Little's law on a docket (Research.P16.little) makes the time-average number pending equal to the filing rate times the mean time a case spends in court, so the pending count and the filings fix that mean (little_target): implied days = pending / filings per day. The published mean counts only cases disposed in the window and excludes bench-warrant time, so it is the truncated mean of Research.P16Censoring; the ratio of implied to published is how much longer cases stay than the published figure says. The disposed-case bands give a second, model-free lower bound on the published mean (band_floor_le_mean), and the open 'over 18 months' band is why no upper bound exists (no_upper_bound).

### Province, by calendar year

| year | received | pending | mean_days | implied_mean_days | ratio | band_floor_bound | over_18 |
|---|---|---|---|---|---|---|---|
| 2019 | 240,736 | 117,419 | 150 | 178 | 1.19 | 56.9 | 0.0284 |
| 2020 | 205,635 | 152,546 | 155 | 272 | 1.75 | 58.5 | 0.0345 |
| 2021 | 213,403 | 162,282 | 141 | 278 | 1.97 | 45.2 | 0.0281 |
| 2022 | 215,367 | 153,331 | 171 | 260 | 1.52 | 72.8 | 0.0453 |
| 2023 | 194,583 | 123,146 | 179 | 231 | 1.29 | 78.3 | 0.0599 |
| 2024 | 247,852 | 149,423 | 176 | 221 | 1.25 | 75.6 | 0.0524 |
| 2025 | 249,612 | 151,860 | 183 | 222 | 1.21 | 80.2 | 0.0476 |

In 7 of 7 full years the backlog implies a longer stay than the published mean (ratio above 1), most in 2020-2022, when the pandemic closures grew the docket; the published mean is above its band floor bound in 7 of 7.

### Regions, the 12 months 2025-04 to 2026-03

| court | received | pending | mean_days | implied_mean_days | ratio |
|---|---|---|---|---|---|
| West | 49,537 | 27,750 | 162 | 204 | 1.26 |
| Toronto | 42,588 | 32,823 | 231 | 281 | 1.22 |
| Central East | 46,877 | 25,197 | 165 | 196 | 1.19 |
| Central West | 44,449 | 29,405 | 212 | 241 | 1.14 |
| East | 30,283 | 15,454 | 178 | 186 | 1.05 |

### Courts, the same 12 months (52 courts, highest ratio first)

| court | received | pending | mean_days | implied_mean_days | ratio |
|---|---|---|---|---|---|
| Welland | 494 | 683 | 153 | 505 | 3.3 |
| Dryden | 2,002 | 1,674 | 164 | 305 | 1.86 |
| Gore Bay | 1,252 | 763 | 139 | 222 | 1.6 |
| Sudbury | 4,967 | 3,294 | 158 | 242 | 1.53 |
| Fort Frances | 1,123 | 829 | 179 | 269 | 1.5 |
| Espanola | 713 | 537 | 183 | 275 | 1.5 |
| Guelph | 3,234 | 2,291 | 175 | 259 | 1.47 |
| Owen Sound | 2,200 | 1,162 | 136 | 193 | 1.42 |
| Elliot Lake | 703 | 533 | 202 | 277 | 1.37 |
| Cochrane | 1,992 | 1,096 | 148 | 201 | 1.35 |
| Walkerton | 1,587 | 1,363 | 233 | 314 | 1.35 |
| Sault Ste. Marie | 3,645 | 1,822 | 137 | 182 | 1.33 |
| Orangeville | 2,025 | 1,435 | 195 | 259 | 1.33 |
| London | 11,736 | 6,301 | 152 | 196 | 1.29 |
| Haileybury | 1,301 | 707 | 154 | 198 | 1.29 |
| Durham | 12,098 | 6,367 | 150 | 192 | 1.28 |
| Kitchener | 9,536 | 4,794 | 145 | 183 | 1.27 |
| Thunder Bay | 5,636 | 3,380 | 173 | 219 | 1.26 |
| L'Orignal | 1,339 | 666 | 147 | 181 | 1.24 |
| North Bay | 2,723 | 1,584 | 172 | 212 | 1.23 |
| Cayuga | 728 | 351 | 144 | 176 | 1.23 |
| Parry Sound | 1,238 | 673 | 162 | 199 | 1.23 |
| Stratford | 1,608 | 641 | 119 | 146 | 1.22 |
| Brantford | 3,889 | 2,028 | 157 | 190 | 1.21 |
| Newmarket | 13,639 | 8,047 | 178 | 215 | 1.21 |
| Woodstock | 2,430 | 962 | 120 | 145 | 1.2 |
| Brockville | 2,226 | 1,179 | 161 | 193 | 1.2 |
| Sarnia | 3,533 | 2,012 | 173 | 208 | 1.2 |
| Windsor | 7,539 | 5,036 | 205 | 244 | 1.19 |
| Pembroke | 2,228 | 1,121 | 155 | 184 | 1.19 |
| Goderich | 1,204 | 545 | 139 | 165 | 1.18 |
| Timmins | 3,868 | 1,630 | 130 | 154 | 1.18 |
| Bracebridge | 1,485 | 831 | 173 | 204 | 1.18 |
| Orillia | 1,881 | 1,064 | 177 | 206 | 1.17 |
| Barrie | 9,713 | 4,996 | 162 | 188 | 1.16 |
| Lindsay | 2,193 | 1,265 | 184 | 211 | 1.14 |
| St. Catharines | 7,137 | 3,533 | 158 | 181 | 1.14 |
| Chatham | 2,894 | 1,711 | 189 | 216 | 1.14 |
| Kenora | 4,251 | 2,415 | 182 | 207 | 1.14 |
| Brampton | 17,138 | 14,088 | 271 | 300 | 1.11 |
| Kingston | 2,816 | 1,680 | 197 | 218 | 1.11 |
| Simcoe | 1,170 | 501 | 142 | 156 | 1.1 |
| Napanee | 1,237 | 556 | 149 | 164 | 1.1 |
| Perth | 1,614 | 586 | 121 | 133 | 1.1 |
| Peterborough | 3,863 | 1,707 | 148 | 161 | 1.09 |
| Milton | 4,527 | 2,940 | 219 | 237 | 1.08 |
| Belleville | 3,537 | 1,946 | 193 | 201 | 1.04 |
| Picton | 349 | 176 | 177 | 184 | 1.04 |
| Hamilton | 7,341 | 3,847 | 189 | 191 | 1.01 |
| Ottawa | 10,990 | 5,366 | 181 | 178 | 0.984 |
| Cobourg | 2,005 | 920 | 171 | 167 | 0.979 |
| Cornwall | 3,947 | 2,178 | 206 | 201 | 0.978 |

### Offence groups, 2026 workbook (annual, provincial)

| offence_group | received | pending | published_mean_days | implied_mean_days | ratio |
|---|---|---|---|---|---|
| Federal Statute | 11,032 | 7,190 | 184 | 238 | 1.29 |
| Crimes Against Property | 53,676 | 28,480 | 155 | 194 | 1.25 |
| Administration of Justice | 69,631 | 27,222 | 116 | 143 | 1.23 |
| Crimes Against the Person | 79,678 | 59,899 | 242 | 274 | 1.13 |
| Other Criminal Code | 13,524 | 9,418 | 225 | 254 | 1.13 |
| Criminal Code Traffic | 21,607 | 16,264 | 244 | 275 | 1.13 |

Reading: the published mean time to disposition is a statement about the cases that finished. Where the implied mean is well above it, the cases still pending are older than the ones disposed, which is the condition under which Research.P16Censoring proves the published mean understates the truth. Little's law here is an identity over a window; month-end counts and a docket that is not in steady state make the implied figure an approximation, which is why it is read as a ratio over full years and over twelve-month windows, not month by month.

Theorems: Research.P16.little, Research.P16.little_backlog, Research.P16.little_target, Research.P16Censoring.disposed_understates, Research.P16Censoring.no_upper_bound, Research.P16Censoring.band_floor_le_mean.
