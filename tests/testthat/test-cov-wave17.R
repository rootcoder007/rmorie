# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 17 -- xavir.R (Xavier/Glorot init) and the
# .morie_silverman_h bandwidth helper's degenerate-input branches.

test_that("morie_xavir_xavier_init builds uniform and normal initialisers", {
  u <- morie_xavir_xavier_init(4, 6, seed = 1L, uniform = TRUE)
  expect_equal(u$method, "uniform")
  expect_equal(dim(u$weights), c(4L, 6L))
  expect_equal(u$shape, c(4, 6))
  expect_true(is.finite(u$std))

  nrm <- morie_xavir_xavier_init(4, 6, seed = 1L, uniform = FALSE)
  expect_equal(nrm$method, "normal")
  expect_equal(dim(nrm$weights), c(4L, 6L))

  expect_error(morie_xavir_xavier_init(0, 5), "must be > 0")
  expect_error(morie_xavir_xavier_init(5, -1), "must be > 0")
})

test_that("morie_xavir_xavier_init leaves the global RNG state unchanged", {
  set.seed(123L)
  before <- stats::runif(1)
  set.seed(123L)
  invisible(morie_xavir_xavier_init(3, 3, seed = 99L))
  after <- stats::runif(1)
  expect_equal(before, after)
})

test_that(".morie_silverman_h handles degenerate (zero-spread) input", {
  # constant input -> sd = 0 and IQR = 0 -> the sigma<=0 fallback
  h0 <- morie:::.morie_silverman_h(rep(3, 20))
  expect_true(is.finite(h0) && h0 > 0)
  # IQR = 0 but sd > 0 -> the iq>0 == FALSE branch (sigma <- s)
  h1 <- morie:::.morie_silverman_h(c(rep(0, 18), 5, -5))
  expect_true(is.finite(h1) && h1 > 0)
  # ordinary spread -> the iq>0 branch (min(s, iq))
  set.seed(1)
  h2 <- morie:::.morie_silverman_h(stats::rnorm(60))
  expect_true(is.finite(h2) && h2 > 0)
  # exercised end-to-end through a Fauzi callable on constant data
  expect_true(is.list(fzcvm(rep(7, 12))))
})
