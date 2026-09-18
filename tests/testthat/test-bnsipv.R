# Same inputs and anchors as the three-way harness ledger/wave3/test_bnsipv.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("bnsipv runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  N <- 60L; i <- 0:(N - 1L)
  Z <- (i * 5) %% 3
  D <- as.integer((Z >= 1 & (i * 3) %% 5 != 0) | (Z == 0 & (i * 7) %% 11 == 0))
  y <- as.integer(0.5 * D + ((i * 7) %% 11) / 16 > 0.7)
  KEYS <- c("estimate", "iv_lower", "iv_upper", "lower", "method", "mtr_lower", "mtr_upper", "n", "refuted", "upper", "width")
  r <- Bnsipv(y, D, Z); record_keys("a", r, KEYS)
  anchor("order", r$lower <= r$upper + 1e-12, "lower <= upper")
  anchor("n", as.integer(r$n) == N, "n")
})
