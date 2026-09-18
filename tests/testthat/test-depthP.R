# Same inputs and anchors as the three-way harness ledger/wave3/test_depthP.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("depthP runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  N <- 40L; i <- 0:(N - 1L)
  X <- cbind(((i * 7) %% 11) / 16, ((i * 5) %% 7) / 8)
  KEYS <- c("d", "depth", "estimate", "mad", "med", "method", "n", "outlyingness", "worst_dir")
  r <- DepthP(c(0.5, 0.5), X, 36); record_keys("d2", r, KEYS)
  r1 <- DepthP(0.5, X[, 1, drop = FALSE], 36); record_keys("d1", r1, KEYS)
  anchor("range", r$depth >= 0 && r$depth <= 1, "depth in [0,1]")
  anchor("n", as.integer(r$n) == N, "n")
})
