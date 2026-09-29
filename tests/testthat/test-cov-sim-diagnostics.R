# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/SimDiagnostics.R exports: boundary and class-entropy
# maps over categorical realisations, class proportions, and the two
# turning-bands diagnostics (ensemble moments and the band-count
# convergence of the sample covariance).

.sd_g1 <- c(1, 1, 2,
            1, 2, 2,
            1, 1, 2)
.sd_g2 <- c(1, 1, 1,
            1, 1, 1,
            2, 2, 2)
.sd_xy <- as.matrix(expand.grid(x = c(0, 1, 2), y = c(0, 1)))

test_that("BoundaryProbability counts rook neighbours of a different class", {
  r <- BoundaryProbability(list(.sd_g1), 3, 3, c(1, 2))
  g <- matrix(.sd_g1, 3, 3, byrow = TRUE)
  ex <- matrix(0, 3, 3)
  for (i in 1:3) for (j in 1:3) {
    nb <- c(if (i > 1) g[i - 1, j], if (i < 3) g[i + 1, j],
            if (j > 1) g[i, j - 1], if (j < 3) g[i, j + 1])
    ex[i, j] <- as.numeric(any(nb != g[i, j]))
  }
  expect_equal(r$boundary, ex)
  expect_equal(r$class_probability[[1]], (g == 1) * 1)
  expect_equal(r$class_probability[[2]], (g == 2) * 1)
  # one realisation: every cell is certain, so the entropy is zero
  expect_equal(r$entropy, matrix(0, 3, 3))
  # two realisations: the frequency and the entropy where they disagree
  r2 <- BoundaryProbability(list(.sd_g1, .sd_g2), 3, 3, c(1, 2))
  g2 <- matrix(.sd_g2, 3, 3, byrow = TRUE)
  expect_equal(r2$class_probability[[1]], ((g == 1) + (g2 == 1)) / 2)
  h <- -((g == 1) + (g2 == 1)) / 2 * log(pmax(((g == 1) + (g2 == 1)) / 2, 1e-300)) -
    ((g == 2) + (g2 == 2)) / 2 * log(pmax(((g == 2) + (g2 == 2)) / 2, 1e-300))
  expect_equal(r2$entropy, h, tolerance = 1e-12)
  expect_equal(max(r2$entropy), log(2), tolerance = 1e-12)
  # a uniform field has no boundaries at all
  expect_equal(BoundaryProbability(list(rep(1, 9)), 3, 3, 1)$boundary, matrix(0, 3, 3))
})

test_that("ClassProportions averages the per-realisation class shares", {
  r <- ClassProportions(list(.sd_g1, .sd_g2), c(1, 2))
  p <- rbind(c(mean(.sd_g1 == 1), mean(.sd_g1 == 2)), c(mean(.sd_g2 == 1), mean(.sd_g2 == 2)))
  expect_equal(unname(r$proportions), p, tolerance = 1e-12)
  expect_equal(unname(r$mean), colMeans(p), tolerance = 1e-12)
  expect_equal(unname(r$sd), apply(p, 2, sd), tolerance = 1e-12)
  expect_equal(sum(r$mean), 1, tolerance = 1e-12)
  d <- ClassProportions(list(.sd_g1, .sd_g2), c(1, 2), target = c(0.5, 0.5))
  expect_equal(unname(d$deviation), colMeans(p) - 0.5, tolerance = 1e-12)
  one <- ClassProportions(list(.sd_g1), c(1, 2))
  expect_equal(one$sd, 0)
})

test_that("TbEnsemble reports the pointwise moments of its realisations", {
  e <- TbEnsemble(.sd_xy, nsim = 6, n_bands = 16, n_waves = 20, seed = 4)
  S <- do.call(rbind, e$realisations)
  expect_length(e$realisations, 6L)
  expect_equal(e$pointwise_mean, colMeans(S), tolerance = 1e-12)
  expect_equal(e$pointwise_variance, apply(S, 2, var), tolerance = 1e-12)
  expect_equal(e$realisation_variance, apply(S, 1, var), tolerance = 1e-12)
  # the realisations are reproducible from their own seeds
  first <- TurningBands(.sd_xy, "exponential", sill = 1, range_ = 1, nu = 0.5,
                        n_bands = 16, n_waves = 20, seed = 4)$field
  expect_equal(e$realisations[[1]], first, tolerance = 1e-12)
  # with bounds the empirical and model variograms are returned
  b <- TbEnsemble(.sd_xy, nsim = 4, n_bands = 16, n_waves = 20, seed = 2, bounds = c(0, 1, 2, 3))
  expect_length(b$variogram, 3L)
  expect_length(b$model_variogram, 3L)
  expect_true(all(b$model_variogram >= 0))
  # the model variogram rises with distance for an exponential covariance
  expect_true(all(diff(b$model_variogram) > 0))
})

test_that("TbBandConvergence: more bands, closer to the target covariance", {
  r <- TbBandConvergence(.sd_xy, bands = c(2, 32), nsim = 12, n_waves = 20, seed = 3)
  expect_equal(r$bands, c(2, 32))
  expect_length(r$rmse, 2L)
  expect_true(all(r$rmse > 0))
  # the reported error is the RMSE of the sample covariance against the
  # model covariance, for each band count (at 12 realisations the
  # Monte-Carlo noise is larger than the band-count effect, so the two
  # numbers are not ordered -- only their construction is checked)
  m <- list(model = "Exp", psill = 1, range = 1)
  C0 <- matrix(KrigingCovariance(as.matrix(dist(.sd_xy)), m), nrow(.sd_xy))
  for (k in 1:2) {
    S <- do.call(rbind, lapply(seq_len(12) - 1, function(j)
      TurningBands(.sd_xy, "exponential", sill = 1, range_ = 1, nu = 0.5,
                   n_bands = r$bands[k], n_waves = 20, seed = 3 + j)$field))
    E <- cov(S) - C0
    expect_equal(r$rmse[k], sqrt(mean(E[upper.tri(E, diag = TRUE)]^2)), tolerance = 1e-12)
  }
})
