# A function that seeds internally must not replace the caller's stream.
# "Consuming" randomness (advancing the stream) is fine; "replacing" it
# means two sessions started from different seeds continue identically.

.stream_after <- function(seed, f) {
  set.seed(seed)
  f()
  stats::runif(1)
}

test_that("an internally seeded call leaves the caller's stream where a plain seed would", {
  x <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
  f <- function() morie_bootstrap_ci(x, B = 50L)
  a <- .stream_after(11, f)
  b <- .stream_after(22, f)
  expect_false(isTRUE(all.equal(a, b)))
  set.seed(11)
  expect_identical(a, stats::runif(1))
  set.seed(22)
  expect_identical(b, stats::runif(1))
})

test_that("the seeded call itself is still reproducible", {
  x <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
  r1 <- morie_bootstrap_ci(x, B = 50L, seed = 7L)
  set.seed(999)
  r2 <- morie_bootstrap_ci(x, B = 50L, seed = 7L)
  expect_identical(r1, r2)
})

test_that("a session without .Random.seed has none afterwards either", {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv()))
  if (had) rm(".Random.seed", envir = globalenv())
  morie_bootstrap_ci(c(1, 2, 3, 4, 5), B = 20L)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("seeding twice inside one function restores the first saved state", {
  twice <- function() {
    rmorie:::.rmorie_local_seed(1L)
    stats::runif(1)
    rmorie:::.rmorie_local_seed(2L)
    stats::runif(1)
  }
  set.seed(5)
  ref <- stats::runif(1)
  set.seed(5)
  twice()
  expect_identical(stats::runif(1), ref)
  expect_null(rmorie:::.rmorie_local_seed(NULL))
})

test_that("morie_det_rng() still seeds the session by contract", {
  morie_det_rng("test", 1L)
  a <- stats::runif(1)
  morie_det_rng("test", 1L)
  expect_identical(a, stats::runif(1))
})

test_that("the deterministic_seed path restores the caller's stream too", {
  f <- function() {
    rmorie:::.rmorie_local_det_rng("t", 1L)
    stats::runif(1)
  }
  set.seed(5)
  ref <- stats::runif(1)
  set.seed(5)
  a <- f()
  expect_identical(stats::runif(1), ref)
  set.seed(9)
  expect_identical(f(), a)
})
