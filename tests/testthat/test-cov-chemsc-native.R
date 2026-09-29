# Coverage tests for R/chemsc_native.R (Eldridge et al. 1997 ChemScore as
# in CCDC GOLD). The Gaussian-smoothed block is checked against numerical
# convolution of the hard block with a normal kernel.

test_that("block function and its Gaussian smoothing", {
  expect_equal(morie_chemsc_block(0.1, 0.25, 0.65), 1)
  expect_equal(morie_chemsc_block(-0.45, 0.25, 0.65), 0.5, tolerance = 1e-12)
  expect_equal(morie_chemsc_block(0.9, 0.25, 0.65), 0)
  expect_error(morie_chemsc_block(0.1, 0.5, 0.5), "must exceed")
  # the smoothed block is E[g(|d| + e)], e ~ N(0, sigma^2), with g the
  # one-sided ramp: 1 up to d_ideal, linear to 0 at d_max
  g <- function(u) pmin(1, pmax(0, (0.65 - u) / (0.65 - 0.25)))
  for (d in c(0, 0.2, 0.3, 0.45, 0.6, 0.7, -0.3)) {
    ref <- integrate(function(u) g(u) * dnorm(u, abs(d), 0.1), -Inf, Inf, rel.tol = 1e-12)$value
    expect_equal(morie_chemsc_smooth_block(d, 0.25, 0.65, 0.1), ref, tolerance = 1e-8, info = d)
  }
  expect_equal(morie_chemsc_smooth_block(0.4, 0.25, 0.65, 0), morie_chemsc_block(0.4, 0.25, 0.65))
})

test_that("hydrogen-bond, metal and lipophilic terms", {
  hb <- morie_chemsc_hbond(2.2, 140, c(120, 175), smoothing = "none")
  ref <- morie_chemsc_block(0.35, 0.25, 0.65) * morie_chemsc_block(-40, 30, 80) *
    morie_chemsc_block(-60, 70, 80) * morie_chemsc_block(-5, 70, 80)
  expect_equal(hb, ref, tolerance = 1e-12)
  expect_equal(hb, (0.3 / 0.4) * (40 / 50), tolerance = 1e-12)
  expect_equal(morie_chemsc_hbond(1.85, 180, numeric(0), par = list(HBOND_R_SIGMA = 0)),
    morie_chemsc_smooth_block(0, 30, 80, 10), tolerance = 1e-12)
  expect_equal(morie_chemsc_metal(2.8, smoothing = "none"), 0.5, tolerance = 1e-12)
  expect_equal(morie_chemsc_metal(2.0, smoothing = "none"), 1)
  expect_equal(morie_chemsc_lipophilic(5.6, smoothing = "none"), 0.5, tolerance = 1e-12)
  expect_equal(morie_chemsc_lipophilic(8, smoothing = "none"), 0)
  expect_error(morie_chemsc_metal(2.8, smoothing = "box"), "smoothing must be")
})

test_that("rotor, clash and torsion terms", {
  fr <- list(c(0, 1), c(1, 1), c(0.5, 0))
  expect_equal(morie_chemsc_rot(fr), 1 + (2 / 3) * (0.5 + 1 + 0.25), tolerance = 1e-12)
  expect_equal(morie_chemsc_rot(list()), 0)
  expect_equal(morie_chemsc_clash(2.5), 0.6, tolerance = 1e-12)
  expect_equal(morie_chemsc_clash(1.0, "metal", slope = 2), 0.6, tolerance = 1e-12)
  expect_equal(morie_chemsc_clash(4, "sulphur"), 0)
  expect_error(morie_chemsc_clash(1, "ionic"), "kind must be")
  expect_equal(morie_chemsc_torsion(60, 1.5, 3, pi), 1.5 * (1 + cos(pi - pi)), tolerance = 1e-12)
})

test_that("score assembles dG and fitness from the terms", {
  hb <- list(list(2.0, 165, 150), list(2.4, 120, numeric(0)))
  s <- morie_chemsc_score(hbonds = hb, metals = 2.9, lipophilic = c(4.5, 6.0),
    rotatable = list(c(1, 0)), clashes = list(list(2.8, "general")),
    torsions = list(c(30, 1, 2, 0)), dg0 = -5.48, smoothing = "gaussian")
  s_hb <- morie_chemsc_hbond(2.0, 165, 150) + morie_chemsc_hbond(2.4, 120, numeric(0))
  s_mt <- morie_chemsc_metal(2.9)
  s_lp <- morie_chemsc_lipophilic(4.5) + morie_chemsc_lipophilic(6.0)
  h_rot <- morie_chemsc_rot(list(c(1, 0)))
  dg <- -5.48 - 3.34 * s_hb - 6.03 * s_mt - 0.117 * s_lp + 2.56 * h_rot
  expect_equal(s$dg, dg, tolerance = 1e-12)
  expect_equal(s$fitness, -dg - 0.3 - morie_chemsc_torsion(30, 1, 2, 0), tolerance = 1e-12)
  c2 <- morie_chemsc_score(hbonds = hb, coefficients = list(HBOND_COEFFICIENT = -1))
  expect_equal(c2$dg, -s_hb, tolerance = 1e-12)
  expect_error(morie_chemsc_score(smoothing = "box"), "smoothing must be")
})

test_that("coordinates to pairwise terms", {
  rec <- list(
    list(0, 0, 0, "donor", -1, 0, 0),
    list(5, 0, 0, "lipophilic"),
    list(0, 6, 0, "metal")
  )
  lig <- list(
    list(2.9, 0, 0, "acceptor", 3.9, 0, 0),
    list(5, 5, 0, "lipophilic")
  )
  r <- morie_chemsc(rec, lig, smoothing = "none")
  # donor H at the origin side: H-acceptor distance 2.9, angles 180 and 0
  expect_equal(r$n_hbond, 1L)
  expect_equal(r$hbond, morie_chemsc_hbond(2.9, 180, 0, smoothing = "none"), tolerance = 1e-12)
  d_ma <- sqrt(2.9^2 + 36)
  expect_equal(r$metal, morie_chemsc_metal(d_ma, smoothing = "none"), tolerance = 1e-12)
  expect_equal(r$lipophilic, morie_chemsc_lipophilic(5, smoothing = "none"), tolerance = 1e-12)
  expect_equal(r$n_clash, 6L)
  expect_match(morie_chemsc_cheatsheet(), "ChemScore")
})
