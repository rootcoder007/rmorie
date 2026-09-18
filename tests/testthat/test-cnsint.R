# Same inputs and anchors as the three-way harness ledger/wave3/test_cnsint.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("cnsint runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  n <- 40L; k <- 6L
  X <- matrix(0L, n, k); for (i in 0:(n - 1L)) for (j in 0:(k - 1L)) X[i + 1L, j + 1L] <- as.integer(((i * 7) %% 11) / 16 + ((j * 3) %% 5) / 16 + ((i * j + 3) %% 7) / 28 > 0.75)
  group <- (0:(n - 1L)) %% 2
  KEYS <- c("b", "b_focal", "b_reference", "drift", "estimate", "k", "method", "n", "n_anchor", "theta_mean_focal", "theta_mean_reference")
  r <- Cnsint(X, NULL, group, NULL, 50); record_keys("all", r, KEYS)
  anchor("k", as.integer(r$k) == k && as.integer(r$n) == n, "k and n")
})
