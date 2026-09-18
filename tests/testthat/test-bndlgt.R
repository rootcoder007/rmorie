# Same inputs and anchors as the three-way harness ledger/wave3/test_bndlgt.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("bndlgt runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  N <- 60L; i <- 0:(N - 1L)
  D <- as.integer((i * 3) %% 7 >= 3); X <- (i * 5) %% 3
  y <- as.integer(((i * 7) %% 11) / 16 + 0.5 * D + 0.125 * X > 0.6)
  KEYS <- c("estimate", "lower", "method", "n", "n_strata", "upper", "width")
  r <- Bndlgt(y, D, X); record_keys("a", r, KEYS)
  anchor("order", r$lower <= r$estimate + 1e-12 && r$estimate <= r$upper + 1e-12, "lower <= estimate <= upper")
  anchor("n", as.integer(r$n) == N && as.integer(r$n_strata) == 3L, "n and strata")
})
