# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 31 -- the morie_dcc_multivariate_garch() rmgarch branch.
#
# rmgarch::dccfit() throws "$ operator not defined for this S4 class"
# under the installed rmgarch/rugarch -- an S4-layout incompatibility
# inside those packages, which morie_dcc_multivariate_garch() catches and
# degrades past. We cannot fix rmgarch's internals, but we can test
# morie's own rmgarch-branch glue (coefficient-name extraction, result
# assembly) by mocking the rmgarch / rugarch / stats entry points to
# return rmgarch-shaped values, so the branch executes end to end.

test_that("morie_dcc_multivariate_garch assembles the rmgarch-path result", {
  set.seed(31)
  X <- matrix(rnorm(40 * 3), 40, 3)

  testthat::local_mocked_bindings(
    dccfit = function(...) "mock-dcc-fit",
    rcor = function(...) array(0.3, c(40, 3, 3)),
    .package = "rmgarch"
  )
  testthat::local_mocked_bindings(
    likelihood = function(...) -123.45,
    .package = "rugarch"
  )
  testthat::local_mocked_bindings(
    coef = function(object, ...) {
      c("[Joint]dcca1" = 0.02, "[Joint]dccb1" = 0.95)
    },
    sigma = function(object, ...) matrix(0.1, 40, 3),
    .package = "stats"
  )

  res <- morie_dcc_multivariate_garch(X)
  expect_equal(res$method, "DCC(1,1) via rmgarch")
  expect_equal(res$a, 0.02)
  expect_equal(res$b, 0.95)
  expect_equal(res$loglik, -123.45)
  expect_equal(dim(res$conditional_variance), c(40L, 3L))
})
