# researchproofs

Lean 4 proofs for the criminology open-problems ledger (`../LEDGER.md`).
Each `Researchproofs/P<n>*.lean` file formalises the identification results behind one
problem, and the R and Python functions that implement the estimators
cite the theorem they rest on (see `../README.md` for the table).

| Files | Problem |
|---|---|
| `P1DarkFigure`, `P1Breakdown`, `P1Hierarchy`, `P1ThreeList`, `P1LeCam` | the dark figure of crime as a partial-identification problem |
| `P2Selection`, `P2Collider`, `P2Benchmark`, `P2RelativeRisk`, `P2Interracial` | selection and collider bias in policing data |
| `P3HorvitzThompson`, `P3Variance`, `P3Interference`, `P3SYG`, `P3Cheeger` | survey estimation under interference |
| `P4Urn`, `P4Rate`, `P4Feedback`, `P4Limit`, `P4Mitigation` | feedback loops in predictive policing |
| `P5Fairness`, `P5Compare`, `P5Hazard`, `P5Ranking`, `P5Rescale`, `P5Separation` | fairness measures and their impossibilities |
| `P6AgeCrime` | the age-crime curve |
| `P7Concentration`, `P7Distinct`, `P7Mixture` | concentration of crime in places and people |
| `P8Deterrence`, `P8Necessity`, `P8Comparative` | deterrence |
| `P9Recording` | crime recording as a linear map |
| `P10Contagion`, `P10Extinction` | branching-ratio arithmetic of self-exciting crime processes |
| `P11Bounds`, `P11Monotone`, `P11Contaminated`, `P11Coverage`, `P11Selection` | sentencing bounds under contamination |
| `P12Ecological`, `P12Bounds` | the between/within decomposition of an ecological correlation |
| `P13Meta`, `P13HKSJ` | pooling site evaluations: random effects and the HKSJ interval |
| `P14Instrument`, `P14Slope` | judge-leniency designs: what the instrument identifies |
| `P15Decomposition`, `P15Reweight` | sentencing disparity: the unexplained part of a decomposition |
| `P16Backlog`, `P16Censoring` | court backlog: Little's law on a docket and censored durations |
| `P17Incapacitation`, `P17Replacement` | incapacitation: what a sentence year buys, with replacement |
| `P18Selective` | selective labels: evaluating a release rule from the released alone |
| `P19Regression`, `P19Shrinkage` | regression to the mean at selected hot spots |

357 theorems and lemmas in all. What is proved is the mathematics: given
the stated assumptions, the estimand is identified, bounded or diverges as
claimed. Whether the assumptions hold of any data set is a substantive
claim that Lean cannot check.

## Building

Lean `v4.34.0` with Mathlib (pinned in `lakefile.toml`):

```sh
lake build
lake env lean Audit.lean    # prints the axioms every theorem depends on
```

`Audit.lean` runs `#print axioms` over every theorem; `audit.txt` is its
last output. Nothing depends on `sorryAx`.

The MRP proofs for the VSR paper live in the sibling project `vsr/`.
