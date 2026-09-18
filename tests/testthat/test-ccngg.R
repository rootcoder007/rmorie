# Same inputs and anchors as the three-way harness ledger/wave3/test_ccngg.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("ccngg runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  N <- 60L; i <- 0:(N - 1L)
  X <- cbind(((i * 7) %% 11) / 16, ((i * 5) %% 7) / 8)
  cl <- (i * 3) %% 4
  y <- 1 + 0.5 * X[, 1] + 0.75 * X[, 2] + 0.5 * cl + ((i * 13) %% 7) / 32
  KEYS <- c("estimate", "icc", "method", "n", "n_groups", "var_fixed", "var_random", "var_resid")
  r <- Ccngg(y, X, NULL, cl); record_keys("a", r, KEYS)
  anchor("icc", r$icc >= 0 && r$icc <= 1, "icc in [0,1]")
  anchor("groups", as.integer(r$n_groups) == 4L && as.integer(r$n) == N, "groups and n")
})
