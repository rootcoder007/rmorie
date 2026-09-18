# Same inputs and anchors as the three-way harness ledger/wave3/test_bndcvr.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("bndcvr runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  N <- 60L; i <- 0:(N - 1L)
  theta <- 0.5
  lower <- ifelse(i %% 9 == 0, theta + 1 / 16, theta - 0.25 - ((i * 3) %% 5) / 32)
  upper <- theta + 0.25 + ((i * 5) %% 7) / 32
  KEYS <- c("coverage", "mean_width", "method", "n_covered", "nominal", "p_value", "reject")
  r <- Bndcvr(lower, upper, theta, 0.05); record_keys("a", r, KEYS)
  anchor("n_covered", as.integer(r$n_covered) == sum(lower <= theta & theta <= upper), "count of covered intervals")
  anchor("coverage", abs(as.numeric(r$coverage) - 53 / 60) < 1e-12, "53 of 60 covered")
})
