
## Missing R twins (parity gap, NOT "Python-only OK")

Per Vee 2026-07-30: a Python fn module with no R counterpart is a
MISSING PORT, not an acceptable Python-only. Each needs an R version in
both trees for true three-way parity. Surfaced by the verification
sweep as "no R twin"; confirmed against parity_check.py (287 of 36,459
Python modules currently have BOTH R twins, so the vast majority are
un-ported -- this list is only the ones the sweep touched and fixed,
where correctness is now settled and the port can be written against a
known-good reference).

Confirmed-correct Python, R twin status:
- bshrk  (horseshoe Gibbs, Makalic & Schmidt 2016) -- PORTED 2026-07-30
- empby  (parametric EB, Morris 1983) -- PORTED 2026-07-30
- eslsmt (Reinsch smoothing spline) -- PORTED 2026-07-30
- vlfctn/regime_value (AIPW policy value) -- PORTED 2026-07-30
- otmapnk (monotone 1-D transport map) -- PORTED 2026-07-30
- dppca -- NOT twinless after all: `morie_dp_pca` already exists in
  dp_native2.R (delegates to morie_dp_covariance); no port needed.
- and the broader set of ~36,000 fn modules with no R counterpart

The five ports live in `R/verification_ports_native.R`, byte-identical
in r-package/morie/R and r-morie-oss/R, using morie_ export names and the
native `.morie_logit_fit` (no stats::glm delegation). Deterministic
members (eslsmt, empby, regime_value) match Python bit-for-bit; the
RNG/fit members (bshrk, otmapnk) are property-checked.

PENDING l14 (blocked on tailscale re-auth 2026-07-30): run roxygen to
emit NAMESPACE exports + man/*.Rd for the five, then R CMD check, then
`scripts/audit/verify_ports_parity.R`. Do NOT push until that is green
(no-push-without-R-CMD-check rule). The ~36,000 remaining un-ported fn
modules are a separate standing backlog, not a same-day task.
