# Polya tree prior (Muller & Quintana 2004 Sec. 2.3; Lavine 1992):
# level parameters, dyadic partition paths, Beta(a, a) branch draws by
# Marsaglia-Tsang gamma pairs, and the piecewise-constant density.

test_that("level parameters follow the c m^2, constant and linear rules", {
  expect_equal(poltrx_level_parameters(3, 2)$alpha, 18)
  expect_equal(poltrx_level_parameters(3, 2, "constant")$alpha, 2)
  expect_equal(poltrx_level_parameters(3, 2, "linear")$alpha, 6)
  expect_error(poltrx_level_parameters(0), "numbered from 1")
  expect_error(poltrx_level_parameters(1, 0), "positive")
  expect_error(poltrx_level_parameters(1, 1, "cubic"), "rule must be")
  expect_identical(poltrx_continuity_regime("m_squared")$draws, "absolutely continuous")
  expect_identical(poltrx_continuity_regime("constant")$draws, "discrete, DP-like")
  expect_identical(poltrx_continuity_regime("linear")$draws, "borderline")
  expect_error(poltrx_continuity_regime("x"), "rule must be")
})

test_that("partition index gives the binary expansion of x", {
  pi3 <- poltrx_partition_index(0.7, 3)
  # 0.7 = 0.101100..._2
  expect_identical(pi3$epsilon, c(1L, 0L, 1L))
  expect_equal(pi3$interval, c(0.625, 0.75))
  pz <- poltrx_partition_index(3.2, 2, lo = 2, hi = 6)
  expect_identical(pz$epsilon, c(0L, 1L))
  expect_equal(pz$interval, c(3, 4))
  expect_error(poltrx_partition_index(1.5, 2), "outside")
})

test_that("eps_from_key inverts the key format", {
  expect_identical(poltrx_eps_from_key("()"), integer(0))
  expect_identical(poltrx_eps_from_key("(0,1,1)"), c(0L, 1L, 1L))
})

test_that("the gamma sampler replays Marsaglia-Tsang on the shared stream", {
  shape <- 2.5
  e <- .ghc_rng(5)
  g <- poltrx_gamma(e, shape)
  u <- .ghc_unif(.ghc_rng(5), 3L)
  d <- shape - 1 / 3
  cc <- 1 / sqrt(9 * d)
  z <- sqrt(-2 * log(u[1])) * cos(2 * pi * u[2])
  v <- (1 + cc * z)^3
  if (v > 0 && log(u[3]) < 0.5 * z^2 + d - d * v + d * log(v)) {
    expect_equal(g, d * v, tolerance = 1e-12)
  }
  # moments over many draws: mean = var = shape
  e2 <- .ghc_rng(11)
  x <- vapply(1:4000, function(i) poltrx_gamma(e2, shape), numeric(1))
  expect_lt(abs(mean(x) - shape), 4 * sqrt(shape / 4000))
  expect_lt(abs(stats::var(x) / shape - 1), 0.1)
  xs <- vapply(1:4000, function(i) poltrx_gamma(e2, 0.5), numeric(1))
  expect_lt(abs(mean(xs) - 0.5), 4 * sqrt(0.5 / 4000))
})

test_that("finite trees draw Beta(a_m, a_m) branch probabilities in bin() order", {
  tr <- poltrx_finite_tree(3, c = 1.5, seed = 7)
  keys <- c("()", "(0)", "(1)", "(0,0)", "(0,1)", "(1,0)", "(1,1)")
  expect_identical(names(tr$Y), keys)
  expect_identical(tr$n_nodes, 7L)
  e <- .ghc_rng(7)
  lev <- c(1, 2, 2, 3, 3, 3, 3)
  ref <- vapply(lev, function(m) {
    a <- 1.5 * m^2
    g0 <- poltrx_gamma(e, a)
    g1 <- poltrx_gamma(e, a)
    g0 / (g0 + g1)
  }, numeric(1))
  expect_equal(unname(unlist(tr$Y)), ref, tolerance = 1e-12)
  expect_error(poltrx_finite_tree(0), "at least one level")
})

test_that("set probabilities multiply branch probabilities down the path", {
  tr <- poltrx_finite_tree(3, seed = 2)
  Y <- tr$Y
  sp <- poltrx_set_probability(c(1, 0, 1), tr)
  expect_equal(sp$probability, (1 - Y[["()"]]) * Y[["(1)"]] * (1 - Y[["(1,0)"]]),
               tolerance = 1e-12)
  expect_identical(poltrx_set_probability(integer(0), tr)$probability, 1)
  expect_error(poltrx_set_probability(c(0, 0, 0, 0), tr), "truncated at level 3")
})

test_that("the tree density puts each path's mass on its own dyadic bin", {
  tr <- poltrx_finite_tree(3, c = 0.7, rule = "linear", seed = 4)
  dn <- poltrx_tree_density(tr, lo = -1, hi = 3)
  expect_equal(dn$total, 1, tolerance = 1e-12)
  expect_equal(dn$density, dn$probabilities / 0.5, tolerance = 1e-12)
  for (k in 1:8) {
    mid <- -1 + (k - 0.5) * 0.5
    path <- poltrx_partition_index(mid, 3, lo = -1, hi = 3)$epsilon
    expect_equal(dn$probabilities[k], poltrx_set_probability(path, tr)$probability,
                 tolerance = 1e-12)
    expect_equal(dn$edges[[k]], c(-1 + (k - 1) * 0.5, -1 + k * 0.5))
  }
  d2 <- poltrx_tree_density(tr, level = 2)
  expect_equal(d2$probabilities[3], poltrx_set_probability(c(1, 0), tr)$probability)
})

test_that("morie_poltrx bundles parameters, regime, tree and density", {
  r <- morie_poltrx(2, seed = 3)
  expect_equal(r$level_parameters$alpha, 4)
  expect_identical(r$tree$Y, poltrx_finite_tree(2, seed = 3)$Y)
  expect_equal(r$density$total, 1, tolerance = 1e-12)
})
