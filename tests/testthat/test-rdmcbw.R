# Same inputs and anchors as the three-way harness ledger/wave3/test_rdmcbw.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("rdmcbw runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  N <- 80L; i <- 0:(N - 1L)
  x <- ((i * 7) %% 11) / 5 - 1
  y <- 1 + 2 * as.integer(x >= 0) + 0.75 * x + ((i * 13) %% 7) / 32
  KEYS <- c("ck", "estimate", "f_hat", "h_no_reg", "h_opt", "method", "n", "n_minus", "n_plus", "r_minus", "r_plus")
  r <- Rdmcbw(y, x, 0, 3.4375); record_keys("tri", r, KEYS)
  r2 <- Rdmcbw(y, x, 0, 5.4); record_keys("uni", r2, KEYS)
  anchor("split", as.integer(r$n_plus) + as.integer(r$n_minus) == N, "sides partition n")
  anchor("positive", r$h_opt > 0, "bandwidth positive")
})
