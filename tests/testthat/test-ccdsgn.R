# Same inputs and anchors as the three-way harness ledger/wave3/test_ccdsgn.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("ccdsgn runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  KEYS <- c("a", "b", "c", "chisq", "ci_high", "ci_low", "d", "estimate", "log_or", "method", "n", "se_log", "significant")
  r <- Ccdsgn(c(30, 20), c(15, 35), NULL, NULL, 0.95); record_keys("counts", r, KEYS)
  i <- 0:49; cases <- as.integer((i * 3) %% 5 < 2); j <- 0:59; controls <- as.integer((j * 7) %% 11 < 3)
  r2 <- Ccdsgn(cases, controls, NULL, NULL, 0.95); record_keys("ind", r2, KEYS)
  anchor("or", abs(as.numeric(r$estimate) - 3.5) < 1e-12, "odds ratio 30*35/(20*15)")
  anchor("n", as.integer(r2$n) == 110L, "n of the indicator form")
})
