# Coverage tests for R/ddpest_native.R (Quintana et al. 2022, dependent
# Dirichlet processes): single-weights and single-atoms constructions,
# marginal checks, the shared-mass correlation and predictive densities.

test_that("single-weights DDP: shared stick-breaking weights, moving atoms", {
  atom <- function(x, h) x + h
  r <- morie_ddpest_single_weights(c(0, 1.5), alpha = 2, K = 5, atom_fn = atom, seed = 4)
  u <- .ghc_unif(.ghc_rng(4), 5)
  v <- 1 - u^(1 / 2)
  w <- v * cumprod(c(1, 1 - v[-5]))
  expect_equal(r$weights, w / sum(w), tolerance = 1e-12)
  expect_equal(r$G[["1.5"]]$atoms, as.list(1.5 + 0:4))
  expect_identical(r$G[["0"]]$weights, r$G[["1.5"]]$weights)
  expect_identical(morie_ddpest_dependent_dp, morie_ddpest_single_weights)
})

test_that("single-atoms DDP: fixed support, covariate-dependent masses", {
  wf <- function(x, h) exp(-(h - x)^2)
  r <- morie_ddpest_single_atoms(c(0, 2), alpha = 1, K = 4, weight_fn = wf)
  expect_equal(r$G[["2"]]$weights, exp(-((0:3) - 2)^2) / sum(exp(-((0:3) - 2)^2)), tolerance = 1e-12)
  expect_equal(r$atoms, as.list(0:3))
  sm <- morie_ddpest_single_atoms(1, 1, 3, wf, atom_sampler = function(e, h) h * 10 + .ghc_unif(e, 1), seed = 2)
  e <- .ghc_rng(2)
  expect_equal(unlist(sm$atoms), vapply(0:2, function(h) h * 10 + .ghc_unif(e, 1), 0), tolerance = 1e-12)
  expect_error(morie_ddpest_single_atoms(0, 1, 2, function(x, h) h - 1), "negative")
  expect_error(morie_ddpest_single_atoms(0, 1, 2, function(x, h) 0), "vanish")
})

test_that("marginal checks, shared-mass correlation and predictive density", {
  G <- list(`0` = list(weights = c(0.5, 0.3, 0.2), atoms = list(0, 1, 2)),
    `1` = list(weights = c(0.2, 0.3, 0.5), atoms = list(0, 1, 5)))
  expect_true(morie_ddpest_check_marginals(G)$ok)
  bad <- G
  bad$`1`$weights <- c(0.2, 0.3, 0.4)
  cm <- morie_ddpest_check_marginals(bad)
  expect_false(cm$ok)
  expect_equal(cm$offenders[[1]]$sum, 0.9)
  reg <- function(a) a <= 1
  c1 <- morie_ddpest_correlation(G, 0, 1, reg)
  expect_equal(c(c1$G_x1, c1$G_x2), c(0.8, 0.5))
  expect_equal(c1$shared_mass, 0.2 + 0.3)
  expect_false(c1$identical)
  expect_true(morie_ddpest_correlation(G, 0, 0, reg)$identical)
  expect_error(morie_ddpest_correlation(G, 0, 7, reg), "not in the collection")
  k <- function(y, a) dnorm(y, a, 0.5)
  d <- morie_ddpest_predict_density(G, 1, c(-1, 0.5, 4), k)
  expect_equal(d$density, vapply(c(-1, 0.5, 4), function(y) sum(G$`1`$weights * dnorm(y, c(0, 1, 5), 0.5)), 0), tolerance = 1e-12)
  expect_error(morie_ddpest_predict_density(G, 3, 0, k), "no measure")
})

test_that("dispatcher, dependence kinds and cheatsheet", {
  expect_equal(morie_ddpest("dependence_kind", "single_atoms")$varies_with_x, "weights")
  expect_equal(morie_ddpest_dependence_kind("both")$effect, "the general case")
  expect_error(morie_ddpest_dependence_kind("none"), "kind must be one of")
  expect_error(morie_ddpest("sample"), "unknown method")
  expect_match(morie_ddpest("cheatsheet"), "SINGLE-WEIGHTS")
  expect_match(morie_ddpest_cheatsheet(), "DDP")
})
