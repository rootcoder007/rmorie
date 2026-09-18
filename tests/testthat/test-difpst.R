# Same inputs and anchors as the three-way harness ledger/wave3/test_difpst.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("difpst runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  n <- 40L; k <- 6L
  X <- matrix(0L, n, k); for (i in 0:(n - 1L)) for (j in 0:(k - 1L)) X[i + 1L, j + 1L] <- as.integer(((i * 7) %% 11) / 16 + ((j * 3) %% 5) / 16 + ((i * j + 3) %% 7) / 28 > 0.75)
  group <- (0:(n - 1L)) %% 2
  KEYS <- c("estimate", "ets", "flagged", "k", "method", "mh_alpha", "mh_delta", "n_focal", "n_reference", "p_diff", "p_focal", "p_reference")
  r <- Difpst(X, group, 1); record_keys("a", r, KEYS)
  anchor("k", as.integer(r$k) == k, "k")
  anchor("n", as.integer(r$n_focal) + as.integer(r$n_reference) == n, "groups partition n")
})
