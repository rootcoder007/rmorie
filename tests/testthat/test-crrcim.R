# Same inputs and anchors as the three-way harness ledger/wave3/test_crrcim.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("crrcim runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  N <- 50L; i <- 0:(N - 1L)
  time <- 1 + ((i * 7) %% 11) * 0.75 + ((i * 3) %% 5) * 0.25
  ev <- (i * 5) %% 3
  KEYS <- c("cif", "estimate", "method", "n", "n_event", "n_risk", "surv", "time")
  r <- Crrcim(time, ev, 1); record_keys("c1", r, KEYS)
  r2 <- Crrcim(time, ev, 2); record_keys("c2", r2, KEYS)
  anchor("monotone", all(diff(as.numeric(r$cif)) >= -1e-12), "cif non-decreasing")
  anchor("n", as.integer(r$n) == N, "n")
})
