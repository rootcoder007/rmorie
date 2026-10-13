## P2 selection: the veil of darkness in Nashville's traffic stops

Data: Stanford Open Policing Project, Nashville TN standardized stops (release 2020-04-01, 3,092,351 rows; Pierson et al. 2020). Vehicular stops of Black and white drivers in the inter-twilight window 16:32 to 20:39 (the earliest sunset to the latest end of civil twilight, local time, computed for each date at 36.16 N, 86.78 W); stops between sunset and dusk are dropped: 268,323 stops in daylight and 227,760 in darkness.

Within the window the drivers are the same people in light and dark, and only daylight lets an officer see race before the stop. The ratio of the Black-to-white odds among daylight stops to the same odds in darkness is the relative visible-race factor (Research.P2.Veil. odds_ratio_identifies_bias); above one means Black drivers are stopped more when they can be seen.

### Pooled over the window

| group | day | night | share_day | share_night | bias_ratio | sign |
|---|---|---|---|---|---|---|
| white | 149,513 | 124,737 | 0.557 | 0.548 | 1 | higher |
| black | 118,810 | 103,023 | 0.443 | 0.452 | 0.962 | lower |

### By half-hour of clock time

| clock | day_stops | dark_stops | black_share_day | black_share_dark | bias_ratio |
|---|---|---|---|---|---|
| 17:00-17:30 | 57,969 | 10,288 | 0.438 | 0.446 | 0.97 |
| 17:30-18:00 | 43,911 | 17,754 | 0.449 | 0.442 | 1.03 |
| 18:00-18:30 | 33,363 | 21,502 | 0.451 | 0.446 | 1.02 |
| 18:30-19:00 | 28,777 | 25,338 | 0.443 | 0.443 | 0.999 |
| 19:00-19:30 | 23,380 | 31,019 | 0.441 | 0.444 | 0.985 |
| 19:30-20:00 | 15,145 | 43,990 | 0.449 | 0.455 | 0.976 |
| 20:00-20:30 | 1,989 | 57,192 | 0.458 | 0.462 | 0.984 |

Holding the clock time fixed (Mantel-Haenszel over the half-hour bins) the daylight-to-darkness odds ratio is 0.998. Black drivers are not a larger share of daylight stops than of darkness stops at the same time of day, so the veil test finds no visibility-driven disparity here. The design's assumption is the theorem's antecedent: who drives at a given clock time does not change with light by race (a differential shift is observationally the same as bias, exposure_shift_confounds), and the result is a ratio, so a common factor on both groups is invisible (bias_not_absolute).

Theorems: Research.P2.Veil.odds_ratio_identifies_bias, Research.P2.Veil.share_sign, Research.P2.Veil.exposure_shift_confounds, Research.P2.Veil.bias_not_absolute.
