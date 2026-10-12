# NCVS 1992-2022: the victimisation applications of the ledger

Data: ICPSR 38604 (BJS, National Crime Victimization Survey, Concatenated File 1992-2022),
https://doi.org/10.3886/ICPSR38604.v1, served as `ncvs_38604/{incident,person,household}` at
data.rmorie.com with the ICPSR codebooks, user guide and terms of use. Weights: WGTVICCY per
victimisation with the BJS series rule (a series counts its reported incidents, at most 10);
WGTPERCY persons and WGTHHCY households as denominators. Reported = V4399 "Were the police
informed" = Yes. Classes: violent = TOC 1-20, property = 31-59, personal theft = 21-23.

Script: `research/applications/ncvs_victimisation.R` (run 2026-10-12).

## P1: the survey/record pair (Research.P1.survey_record_interval, dark-figure version)

Misreport bounds stated: alpha_max = 0.05 (over-report), beta_max = 0.25 (under-report).
Rates per person (violent, personal theft) or per household (property); v_obs is the survey
rate, r the rate the police learned of; [v_lower, v_upper] the identified set for the true rate.

| year | class | v_obs | r | reported share | v_lower | v_upper | dark figure (lower, upper) | ratio upper |
|---|---|---|---|---|---|---|---|---|
| 1993 | personal theft | 0.0025 | 0.0007 | 0.272 | 0.0007 | 0.0033 | (0.0000, 0.0026) | 4.91 |
| 1998 | personal theft | 0.0014 | 0.0005 | 0.326 | 0.0005 | 0.0019 | (0.0000, 0.0014) | 4.09 |
| 2003 | personal theft | 0.0010 | 0.0004 | 0.380 | 0.0004 | 0.0013 | (0.0000, 0.0009) | 3.51 |
| 2008 | personal theft | 0.0007 | 0.0004 | 0.650 | 0.0004 | 0.0009 | (0.0000, 0.0005) | 2.05 |
| 2013 | personal theft | 0.0006 | 0.0002 | 0.417 | 0.0002 | 0.0007 | (0.0000, 0.0005) | 3.20 |
| 2018 | personal theft | 0.0006 | 0.0002 | 0.414 | 0.0002 | 0.0007 | (0.0000, 0.0005) | 3.22 |
| 2022 | personal theft | 0.0005 | 0.0001 | 0.230 | 0.0001 | 0.0007 | (0.0000, 0.0006) | 5.80 |
| 1993 | property | 0.3541 | 0.1128 | 0.319 | 0.3201 | 0.4722 | (0.2073, 0.3594) | 4.19 |
| 1998 | property | 0.2390 | 0.0800 | 0.335 | 0.1990 | 0.3187 | (0.1190, 0.2387) | 3.98 |
| 2003 | property | 0.1756 | 0.0658 | 0.375 | 0.1322 | 0.2341 | (0.0664, 0.1683) | 3.56 |
| 2008 | property | 0.1443 | 0.0579 | 0.402 | 0.0992 | 0.1924 | (0.0413, 0.1344) | 3.32 |
| 2013 | property | 0.1324 | 0.0478 | 0.361 | 0.0867 | 0.1765 | (0.0389, 0.1287) | 3.70 |
| 2018 | property | 0.1090 | 0.0372 | 0.341 | 0.0621 | 0.1453 | (0.0249, 0.1081) | 3.90 |
| 2022 | property | 0.1023 | 0.0325 | 0.318 | 0.0550 | 0.1364 | (0.0225, 0.1038) | 4.19 |
| 1993 | violent | 0.0806 | 0.0342 | 0.425 | 0.0342 | 0.1074 | (0.0000, 0.0732) | 3.14 |
| 1998 | violent | 0.0550 | 0.0249 | 0.452 | 0.0249 | 0.0733 | (0.0000, 0.0484) | 2.95 |
| 2003 | violent | 0.0327 | 0.0154 | 0.471 | 0.0154 | 0.0436 | (0.0000, 0.0282) | 2.83 |
| 2008 | violent | 0.0255 | 0.0118 | 0.465 | 0.0118 | 0.0340 | (0.0000, 0.0221) | 2.87 |
| 2013 | violent | 0.0233 | 0.0107 | 0.458 | 0.0107 | 0.0311 | (0.0000, 0.0204) | 2.91 |
| 2018 | violent | 0.0233 | 0.0099 | 0.427 | 0.0099 | 0.0310 | (0.0000, 0.0211) | 3.12 |
| 2022 | violent | 0.0235 | 0.0097 | 0.415 | 0.0097 | 0.0313 | (0.0000, 0.0216) | 3.22 |

### Breakdown of the 1993 to 2022 decline (Research.P1Breakdown)

beta* is the under-reporting in 2022 that would lift the 2022 upper bound to the 1993 lower bound;
the decline survives the stated beta_max when the 2022 bound stays below it.

| class | v_obs 2022 | v_lower 1993 | beta* | survives beta_max |
|---|---|---|---|---|
| personal theft | 0.0005 | 0.0007 | 0.191 | FALSE |
| property | 0.1023 | 0.3201 | 0.680 | TRUE |
| violent | 0.0235 | 0.0342 | 0.313 | TRUE |

## P7 and P10: concentration of victimisations on units (Research.P7, Research.P10.Moments)

Counts per unit-year (zeros from the person and household files). gini_all is the Gini over
every unit, gini_positive over the victimised; null_zero_share and null_gini_same_positive are
what a Poisson with the same mean would give (Research.P7.gini_zero_decomposition,
poisson_zero_prob). The dispersion index var/mean and the zero share are the two moments that
contagion and heterogeneity fit alike (Research.P10.Moments): the data cannot tell them apart.

| year | unit | n | mean | dispersion | zero share | Poisson zero | excess zero | Gini all | Gini victimised | Poisson Gini | top 1% share |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 1994 | person (violent + personal theft) | 129,504 | 0.0556 | 4.66 | 0.9693 | 0.9459 | 0.0234 | 0.981 | 0.397 | 0.967 | 0.627 |
| 1994 | household (property) | 73,840 | 0.2182 | 2.48 | 0.8549 | 0.8040 | 0.0509 | 0.895 | 0.280 | 0.859 | 0.235 |
| 2008 | person (violent + personal theft) | 99,587 | 0.0177 | 3.74 | 0.9887 | 0.9825 | 0.0063 | 0.992 | 0.329 | 0.988 | 0.928 |
| 2008 | household (property) | 61,321 | 0.0974 | 2.26 | 0.9276 | 0.9072 | 0.0204 | 0.944 | 0.227 | 0.928 | 0.323 |
| 2022 | person (violent + personal theft) | 185,616 | 0.0157 | 4.42 | 0.9906 | 0.9844 | 0.0061 | 0.994 | 0.362 | 0.990 | 1.000 |
| 2022 | household (property) | 152,794 | 0.0508 | 2.18 | 0.9623 | 0.9504 | 0.0119 | 0.971 | 0.226 | 0.962 | 0.455 |

## A14: the comparable subset (Research.P1.Comparable)

The survey's recording rate r_A is identified on the offences it covers (the reported share,
2022). For every recorded offence outside its reach (homicide, arson, commercial crime; a
police count the survey never sees) the dark figure's interval is wider by the factor below
under the assumption that such offences are recorded at least half as well as the comparable
ones (lambda = 1/2): width = R_B (1 - lambda r_A) / (lambda r_A). The FBI count R_B is not held
here, so the factor is reported, not the interval.

| class | r_A (2022) | interval width per recorded offence outside the survey |
|---|---|---|
| personal theft | 0.230 | 7.70 |
| property | 0.318 | 5.29 |
| violent | 0.415 | 3.82 |
