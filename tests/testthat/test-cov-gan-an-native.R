# Coverage for AnoGAN scoring (Schlegl et al. 2017): the L1 residual and
# feature-matching discrimination losses, the (1 - lambda) / lambda
# anomaly score, the residual map, the Mann-Whitney AUC of score
# separation (against wilcox.test's U statistic) and latent inversion with
# a fixed generator, regenerated step by step.

test_that("residual, discrimination and combined anomaly scores", {
  x <- c(1, 2, 3, 4)
  g <- c(1.5, 2, 2, 5)
  expect_equal(residual_loss(x, g), 2.5)
  expect_equal(discrimination_loss(c(0.1, 0.9), c(0.3, 0.4)), 0.7, tolerance = 1e-12)
  s <- anomaly_score(x, g, c(0.1, 0.9), c(0.3, 0.4), lam = 0.2)
  expect_equal(s$score, 0.8 * 2.5 + 0.2 * 0.7, tolerance = 1e-12)
  expect_error(residual_loss(x, g[-1]), "differ in size")
  expect_error(discrimination_loss(1, 2), "scalar discriminator output")
  expect_error(discrimination_loss(1:2, 1:3), "feature vectors differ")
  expect_error(anomaly_score(x, g, 1:2, 1:2, lam = 2), "lambda must lie in \\[0,1\\]")
})

test_that("the residual map is |x - G(z)| in row-major image shape", {
  x <- c(0, 1, 2, 3, 4, 5)
  g <- c(0, 1, 5, 3, 4, 4)
  m <- residual_map(x, g, shape = c(2, 3))
  expect_equal(m$map, matrix(abs(x - g), 2, 3, byrow = TRUE))
  expect_identical(m$max, 3)
  expect_equal(residual_map(x, g)$map, abs(x - g))
  expect_error(residual_map(x, g, shape = c(4, 2)), "does not match 6 values")
  expect_error(residual_map(x, g[-1]), "differ in size")
})

test_that("score separation is the Mann-Whitney AUC with ties counted half", {
  a <- c(0.1, 0.4, 0.4, 0.2)
  b <- c(0.4, 0.9, 0.3)
  s <- score_separation(a, b)
  u <- stats::wilcox.test(b, a, exact = FALSE)$statistic
  expect_equal(s$auc, unname(u) / 12, tolerance = 1e-12)
  expect_equal(c(s$mean_normal, s$mean_anomalous), c(mean(a), mean(b)))
  expect_identical(score_separation(c(0, 0), c(0, 0))$auc, 0.5)
  expect_true(score_separation(1:3, 4:6)$separated)
  expect_error(score_separation(numeric(0), 1), "both populations")
})

test_that("latent inversion descends the forward-difference gradient with a fixed generator", {
  G <- function(z) c(z[1] + z[2], z[1] - z[2], 2 * z[1])
  Fe <- function(v) c(sum(v), v[1] * v[3])
  x <- c(1, 0.2, 1.1)
  r <- morie_gan_an(x, G, Fe, z_dim = 2, steps = 15, lr = 0.05, lam = 0.3, seed = 4)
  L <- function(z) 0.7 * sum(abs(x - G(z))) + 0.3 * sum(abs(Fe(x) - Fe(G(z))))
  z <- (.ghc_unif(.ghc_rng(4), 2) - 0.5) * 2
  best <- L(z)
  bz <- z
  hist <- numeric(0)
  for (t in 0:14) {
    b <- L(z)
    hist <- c(hist, b)
    if (b < best) {
      best <- b
      bz <- z
    }
    gr <- vapply(1:2, function(i) (L(replace(z, i, z[i] + 1e-4)) - b) / 1e-4, 1)
    z <- z - 0.05 / (1 + 0.05 * t) * gr
  }
  if (L(z) < best) bz <- z
  expect_equal(r$loss_history, hist, tolerance = 1e-12)
  expect_equal(r$z, bz, tolerance = 1e-12)
  expect_equal(r$score, L(bz), tolerance = 1e-12)
  expect_equal(r$reconstruction, G(bz), tolerance = 1e-12)
  expect_equal(r$final_step, 0.05 / (1 + 14 * 0.05), tolerance = 1e-12)
  expect_lt(r$score, hist[1])
  expect_identical(anogan, morie_gan_an)
  expect_identical(gan_anomaly, morie_gan_an)
  expect_identical(gananomaly, morie_gan_an)
})
