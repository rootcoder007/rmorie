# rmorie dependency-reduction log (alpha branch)

Goal: bring the r-universe Windows/oldrel build under the hard 100-min cap by
reducing the Suggests tree, WITHOUT losing functionality. Full investigation:
`GRAD_STUDIES/rmorie_dependency_investigation_2026-06-30.txt` (VSR).

Method for every change: verify the package is actually used in ACTIVE code
(getParseData AST, not regex; comments stripped), never drop on usage-count
alone (plugin-strings / DBI drivers / S3 classes / lazy loads are invisible —
e.g. rgenoud is load-bearing via Matching's lazy load). Recover hand-rolled code
from git only when it was truly lost (many "replaced" functions are still base-R
in the current tree — check first).

Baseline: 146 Suggests; 70 compiled + non-recommended; ~246 compiled packages
source-built on Windows/oldrel. Base-recommended (boot, MASS, mgcv, rpart,
survival) are FREE (ship with R) — never hand-roll to drop those.

## Batch 1 (2026-07-01) — drop 9 provably-unused packages  [146 -> 137]
Dropped from Suggests (0 active `pkg::` calls in R/, verified via AST):
  torch, effects, effectsize, elasticnet, margins   (dead everywhere)
  resample, irr, tseries, mutoss                     (0 active calls; only
                                                      \code{}/\pkg{} doc text)
- torch: CNN/RNN (cnnge.R/rnnge.R) are hand-rolled base-R, no torch calls.
- effects: never referenced (the "effects::" hits were marginaleffects:: substrings).
- irr/tseries/mutoss/resample: only mentioned in man/*.Rd prose (\code/\pkg, not
  \link) -> zero R CMD check impact. Functionality they "replaced" (effect sizes,
  jarque-bera, multiple-testing) is still present as base-R / stats::.
Verification: read.dcf parses; Suggests=137; no dropped pkg used in active code;
man references are text-only (no \link). Full build/load validation on zeus +
local R pending.

## Planned next batches (not yet done)
- Batch 2 (single trivial call -> inline base-R, then drop):
  data.table (as.data.table @irm.R:80), lmtest (coeftest @effects.R:86),
  bootstrap (jackknife @690), simpleboot (two.boot @1321), harmonicmeanp (p.hmp).
- Batch 3 (few closed-form calls -> reimplement, then drop):
  poolr (fisher/stouffer/tippett), EValue (evalue/RR/evalues.OLS), qvalue (Storey).
- Batch 4 (consolidate / optionalize the real build-cost drivers):
  ML learners -> keep ranger+glmnet, drop caret/randomForest/gbm/xgboost/e1071/
  kernlab/ipred; optionalize compile-giants arrow + duckdb; niche pdftools,
  Rtsne, smacof, rugarch/rmgarch, np, gmm.
- KEEP (load-bearing, verified): rgenoud, ranger, glmnet, basicspace, duckdb, survey.
