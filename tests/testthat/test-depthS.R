# Same inputs and anchors as the three-way harness ledger/wave3/test_depthS.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("depthS runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  N <- 24L; i <- 0:(N - 1L)
  X <- cbind(((i * 7) %% 11) / 16, ((i * 5) %% 7) / 8)
  KEYS <- c("d", "depth", "ecdf", "estimate", "method", "n", "n_containing", "n_simplices")
  r <- DepthS(X, c(0.5, 0.5)); record_keys("a", r, KEYS)
  anchor("simplices", as.integer(r$n_simplices) == 2024L, "choose(24, 3)")
  anchor("range", r$depth >= 0 && r$depth <= 1, "depth in [0,1]")
})
