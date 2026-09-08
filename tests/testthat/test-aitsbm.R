.aitsbm_fixture <- function(seed = 42, n = 200, D = 5) {
  set.seed(seed)
  exp(matrix(rnorm(n * D), n, D))
}

test_that("the variation array is subcompositionally coherent", {
  # Re-closing multiplies every part of a row by one common factor,
  # which drops out of log(x_i / x_j). tau must be identical.
  r <- morie_subcompositional_incoherence(.aitsbm_fixture(), idx = c(1, 2, 3))
  expect_equal(r$tau_full, r$tau_sub, tolerance = 1e-12)
  expect_equal(r$tau_delta, 0, tolerance = 1e-12)
})

test_that("the raw correlation is not coherent", {
  r <- morie_subcompositional_incoherence(.aitsbm_fixture(), idx = c(1, 2, 3))
  expect_equal(r$delta, r$rho_sub - r$rho_full, tolerance = 1e-12)
  expect_gt(abs(r$delta), 0.05)
})

test_that("a two-part subcomposition correlates -1 exactly", {
  # Closing two parts gives (p, 1 - p).
  r <- morie_subcompositional_incoherence(.aitsbm_fixture(), idx = c(1, 2))
  expect_equal(r$rho_sub, -1, tolerance = 1e-12)
})

test_that("closure manufactures correlation from independent parts", {
  r <- morie_subcompositional_incoherence(.aitsbm_fixture(seed = 7, n = 500, D = 4),
                                          idx = c(1, 2, 3))
  expect_gt(abs(r$rho_full), 1e-3)
})

test_that("morie_subcompositional_incoherence validates its inputs", {
  x <- .aitsbm_fixture()
  bad <- x
  bad[1, 1] <- 0
  expect_error(morie_subcompositional_incoherence(bad, idx = c(1, 2, 3)),
               "strictly positive")
  expect_error(morie_subcompositional_incoherence(.aitsbm_fixture(D = 4),
                                                  idx = c(1, 2, 3, 4)),
               "fewer than")
  expect_error(morie_subcompositional_incoherence(.aitsbm_fixture(D = 2),
                                                  idx = c(1, 2)),
               "at least 3 parts")
  expect_error(morie_subcompositional_incoherence(x, idx = c(1, 1, 2)),
               "repeat")
})
