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
| `OT/` | `Neighbourhoods - 4326.geojson` | City of Toronto open data, Neighbourhoods (with the Neighbourhood Improvement Area classification) |
| `TPS/BreakandEnter/CSV/` | `Break_and_Enter_Open_Data_*.csv` | Toronto Police Service Public Safety Data Portal, Break and Enter occurrences |
| `OTIS/` | `b01_segregation_detailed_dataset.csv` | Ontario Ministry of the Solicitor General, OTIS segregation detailed dataset (open data) |

## What is applied

| script | problem | functions | theorems |
|---|---|---|---|
| `p03_toronto_neighbourhood_network.R` | P3 interference | `PolygonContiguity()`, `morie_cheeger_bound()`, `morie_spillover_exposure_probs()` | `Research.P3.cheeger_easy`, the HT design condition |
| `p07_concentration_toronto.R` | P7 concentration | `morie_concentration_null_bound()` | `Research.P7.Bound.gini_le_cv`, `Research.P7.Mixture` |
| `p10_near_repeat_bne.R` | P10 near repeats | `morie_contagion_moment_witness()`, `morie_tps_hawkes_temporal_fit()` | `Research.P10.Moments` |
| `p15_otis_placement_lengths.R` | P15 disparity | `morie_dfl_reweight()`, `morie_rif_gap()` | `Research.P15.Reweight`, `Research.P15.RIF` |

## What stays open, and why

The ledger names applications whose data is not public or not held here. They are listed so
nobody reads the folder as complete.

| problem | application named | why it is not here |
|---|---|---|
| P1 dark figure | a survey/record pair (GSS victimisation + UCR) | no victimisation survey microdata is held; the substance-use surveys (CSUS, CPADS, CCS) carry no victimisation items |
| P2 selection | Toronto stops with a mobility proxy | the public Regulated Interactions table is counts by year and category only, with no time of day or group |
| P3 interference | TPS street segments | the public files carry neighbourhoods, not the street-segment network; the neighbourhood version is here and labelled |
| P6 age-crime | a cohort | no cohort file; OTIS gives age categories of people in segregation, not offending by age |
| P11 sentencing bounds | OTIS/CPADS custody vs community and reconviction | OTIS is segregation placements, not sentences; CPADS is a student substance-use survey |
| P12 ecological | PSDP/CPADS individuals against TPS neighbourhood aggregates | the only neighbourhood trait held is the NIA flag, which is 0/1 per neighbourhood, so the Duncan-Davis interval is a point; census shares are needed |
| P14 judge leniency | OTIS custody decisions by judge | OTIS carries no judge or court identifiers |
| P16 backlog | Ontario Court of Justice disposition statistics | the OCJ tables are not held locally |
| P17 incapacitation | OTIS custody lengths with reconviction rates | OTIS carries no custody lengths and Ontario publishes no reconviction rate by length |
| P18 selective labels | Ontario bail outcomes | no released-cohort file is public |
