# Missing / to-nativize backlog

Functions that exist only in archived branches, or that exist as thin
wrappers over CRAN packages and must be reimplemented natively.

Rule: morie is **not** a wrapper package. A CRAN package may be an
optional *extender*, never the only path. See
`feedback_wrapper_as_extender` and `feedback_morie_is_not_a_wrapper_package`.

## Open

_(none — see Closed)_

## Closed

### `morie_causal_mediation` — ABSENT at HEAD, wrapper-only in archive

Found 2026-07-30 while reconciling the duplicate rmorie checkouts. Lived
only on `staging-archive/worktree-agent-aebe168405933b189-causal`
(commit `4d78188`, 2026-05-25) and was a **pure wrapper**: it called
`causalweight::medweight()` and `stop()`ed outright when the package was
absent, so a plain install had no mediation estimator at all.

The other 14 functions on those four archived branches are all present
at HEAD, and their core paths have since been nativized (`e_value` at
HEAD is the native VanderWeele-Ding computation via `morie_evalue()`,
not an `EValue::` call). Merging the branches would have dragged
May-25 copies of 441 files back over the native-specialization work, so
they stay archived; only this one gap needed closing.

Reimplemented natively over `stats::glm` (base R) using the Huber (2014)
inverse-probability-weighting mediation estimator. No `causalweight`
dependency.
