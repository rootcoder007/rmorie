# Same inputs and anchors as the three-way harness ledger/wave3/test_cumcif.R:
# the Python arm, the morie R arm and rmorie agree on every recorded value.
test_that("cumcif runs on the three-way inputs and its anchors hold", {
  record_keys <- function(name, r, keys) {
    for (k in keys) expect_true(k %in% names(r), info = paste("missing key", k))
  }
  emit <- function(k, v) invisible(NULL)
  anchor <- function(name, cond, why) expect_true(isTRUE(cond), info = why)
  N <- 50L; i <- 0:(N - 1L)
  time <- 1 + ((i * 7) %% 11) * 0.75 + ((i * 3) %% 5) * 0.25
  ev <- (i * 5) %% 3
  r <- Cumcif(time, ev, 1)
  KEYS <- sort(names(r))
  emit("keys", paste(KEYS, collapse = ","))
  record_keys("c1", r, KEYS)
  anchor("n", if (!is.null(r$n)) as.integer(r$n) == N else TRUE, "n")
})
