# Same inputs and anchors as the three-way harness ledger/wave3/test_bndsdo.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("bndsdo runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  N <- 60L; i <- 0:(N - 1L)
  D <- as.integer((i * 3) %% 7 >= 3)
  y <- 1 + 0.75 * D + ((i * 7) %% 11) / 16 + ((i * 13) %% 7) / 32
  KEYS <- c("estimate", "lower", "method", "n", "naive", "p_treated", "upper", "wc_lower", "wc_upper", "width")
  r1 <- Bndsdo(y, D, 1); record_keys("pos", r1, KEYS)
  r2 <- Bndsdo(y, D, -1); record_keys("neg", r2, KEYS)
  anchor("naive", abs(as.numeric(r1$naive) - (mean(y[D == 1]) - mean(y[D == 0]))) < 1e-10, "naive is the mean difference")
  anchor("order", r1$lower <= r1$upper + 1e-12, "lower <= upper")
})
