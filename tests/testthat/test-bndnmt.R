# Same inputs and anchors as the three-way harness ledger/wave3/test_bndnmt.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("bndnmt runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  N <- 60L; i <- 0:(N - 1L)
  Z <- i %% 2
  D <- as.integer((Z == 1 & (i * 3) %% 5 != 0) | (Z == 0 & (i * 7) %% 11 == 0))
  y <- 0.5 * D + ((i * 7) %% 11) / 16 + ((i * 13) %% 7) / 32
  KEYS <- c("estimate", "itt_y", "lower", "method", "n", "pi_c_max", "pi_d_max", "pi_net", "upper", "wald", "width")
  r <- Bndnmt(y, D, Z); record_keys("a", r, KEYS)
  anchor("order", r$lower <= r$upper + 1e-12, "lower <= upper")
  anchor("n", as.integer(r$n) == N, "n")
})
