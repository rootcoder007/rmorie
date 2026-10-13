# Ledger data applications

The research ledger (`research/LEDGER.md`) closes each problem in three steps: a Lean proof,
an R function with a test, and an application to the data the problem is about. This folder
holds the applications. Each script reads public files from `MORIE_DATA_ROOT` (default
`~/data/datasets`), runs the ledger's research functions on them and writes one Markdown
result under `results/`; `run_all.R` runs them all and assembles `RESULTS.md`.

```sh
MORIE_DATA_ROOT=~/data/datasets Rscript research/applications/run_all.R
```

The files are not shipped with the package. They are the public releases listed below, saved
under the folder names the scripts expect.

| folder | file | source |
|---|---|---|
| `OT/` | `neighbourhood-crime-rates - 4326.csv` | City of Toronto open data, Neighbourhood Crime Rates (Toronto Police Service), 158 neighbourhoods, 2014-2025 |
| `OT/` | `centreline-4326.csv` | City of Toronto open data, Toronto Centreline (road segments with their end intersections) |
| `TPS/` | `tps_break_enter_d14_2024.csv` | TPS break and enter, Division 14, 2024 (shipped with rmoriebricklayer) |
| `OT/` | `Neighbourhoods - 4326.geojson` | City of Toronto open data, Neighbourhoods (with the Neighbourhood Improvement Area classification) |
| `TPS/BreakandEnter/CSV/` | `Break_and_Enter_Open_Data_*.csv` | Toronto Police Service Public Safety Data Portal, Break and Enter occurrences |
| `OTIS/` | `b01_segregation_detailed_dataset.csv` | Ontario Ministry of the Solicitor General, OTIS segregation detailed dataset (open data) |
| `OCJ/` | `ocj_clearance_rate_criminal.csv`, `ocj_disposition_rate_criminal.csv`, `ocj_offence_based_criminal.csv` | Ontario Court of Justice criminal court statistics, every workbook 2012-2026 as tidy CSV (also at data.rmorie.com) |
| `NYPRETRIAL/` | `PretrialRelease_2019_2024_Part_1.csv`, `..._Part_2.csv` | New York State DCJS, Supplemental Pretrial Release Data File (also at data.rmorie.com as `ny_pretrial_supplemental/part_1`, `part_2`) |
| `OPP/` | `tn_nashville_2020_04_01.csv` | Stanford Open Policing Project, Nashville TN stops (ODC-By; also at data.rmorie.com as `opp_nashville/stops`) |
| `COOK/` | `cook_sentencing.csv` | Cook County State's Attorney, Sentencing (Cook County open data) |
| `JURISTAT/` | `reconviction_2024_tbl02_grid.csv`, `reconviction_2024_tbl04_grid.csv` | Statistics Canada, Juristat 85-002-X 2024001 article 00002, table 4 |
| `ONTARIO/` | `ontario_adult_releases_by_risk_2014_15.csv` | Government of Ontario, Rates of recidivism in Ontario |

## What is applied

| script | problem | functions | theorems |
|---|---|---|---|
| `p02_veil_nashville.R` | P2 selection | `morie_veil_of_darkness()` | `Research.P2.Veil.odds_ratio_identifies_bias` |
| `p03_toronto_neighbourhood_network.R` | P3 interference | `PolygonContiguity()`, `morie_cheeger_bound()`, `morie_spillover_exposure_probs()` | `Research.P3.cheeger_easy`, the HT design condition |
| `p07_concentration_toronto.R` | P7 concentration | `morie_concentration_null_bound()` | `Research.P7.Bound.gini_le_cv`, `Research.P7.Mixture` |
| `p10_near_repeat_bne.R` | P10 near repeats | `morie_contagion_moment_witness()`, `morie_tps_hawkes_temporal_fit()` | `Research.P10.Moments` |
| `p15_otis_placement_lengths.R` | P15 disparity | `morie_dfl_reweight()`, `morie_rif_gap()` | `Research.P15.Reweight`, `Research.P15.RIF` |
| `p03_street_segments.R` | P3 interference, street segments | `morie_cheeger_bound()`, `morie_spillover_exposure_probs()` | `Research.P3.cheeger_easy`, the HT design condition |
| `p11_custody_reconviction.R` | P11 sentencing bounds | `morie_sentence_effect_bounds()`, `morie_sentence_effect_mts()` | `Research.P11.ate_width_one`, `Research.P11.Pop.mtr_mts_bounds` |
| `p14_cook_judges.R` | P14 judge leniency | `morie_judge_monotonicity()` | `Research.P14Slope.subgroup_propensity_mono`, `crossing_refutes_both` |
| `p16_ocj_backlog.R` | P16 court backlog | `morie_backlog_published()` | `Research.P16.little_target`, `Research.P16Censoring.band_floor_le_mean` |
| `p18_ny_pretrial.R` | P18 selective labels | `morie_selective_labels()` | `Research.P18.unobserved_bounds`, `unobserved_width` |
| `p17_reconviction_incapacitation.R` | P17 incapacitation | `morie_incapacitation()`, `morie_incapacitation_career()` | `Research.P17.high_rate_more_prevented`, `Research.P17Replacement.later_sentence_prevents_less` |

## What stays open, and why

The ledger names applications whose data is not public or not held here. They are listed so
nobody reads the folder as complete.

| problem | application named | why it is not here |
|---|---|---|
| P1 dark figure | a survey/record pair | done with the NCVS 1992-2022 (ICPSR 38604, `ncvs_38604/*` at data.rmorie.com): `ncvs_victimisation.R`, results/ncvs_victimisation.md (P1 interval and breakdown by year and class; A14 comparable-subset rate; P7 concentration on persons and households; P10 moments) |
| P6 age-crime | a cohort | no cohort file; OTIS gives age categories of people in segregation, not offending by age |
| P12 ecological | PSDP/CPADS individuals against TPS neighbourhood aggregates | the only neighbourhood trait held is the NIA flag, which is 0/1 per neighbourhood, so the Duncan-Davis interval is a point; census shares are needed |
| P14 judge leniency | the Wald ratio (complier effect) | monotonicity is tested on Cook County sentencing (`p14_cook_judges.R`), but no public file pairs judge assignment with a post-sentence outcome |
| P17 incapacitation | offending (not conviction) rates by custody length | applied in convictions to published reconviction hazards (`p17_reconviction_incapacitation.R`); no public file gives custody lengths by person with offending |
