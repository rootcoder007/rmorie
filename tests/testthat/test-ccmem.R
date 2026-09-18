# Same inputs and anchors as the three-way harness ledger/wave3/test_ccmem.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("ccmem runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  N <- 60L; i <- 0:(N - 1L)
  y <- 2 + 0.5 * ((i * 3) %% 4) + 0.25 * ((i * 5) %% 3) + ((i * 13) %% 7) / 32
  c1 <- (i * 3) %% 4; c2 <- (i * 5) %% 3
  KEYS <- c("estimate", "method", "n_levels", "n_units", "row_sums")
  r <- Ccmem(y, c1, c2, NULL); record_keys("two", r, KEYS)
  r1 <- Ccmem(y, c1, NULL, NULL); record_keys("one", r1, KEYS)
  anchor("n_units", as.integer(r$n_units) == N, "n_units")
})
