# SPDX-License-Identifier: AGPL-3.0-or-later
#
# spols  -- OLS semivariogram fitting  (Schabenberger & Gotway 2005, Sec 4.5.1)
# spwls  -- Cressie WLS                (Sec 4.5.1, eq 4.34)
# spreml -- REML                       (Sec 4.5.2, eq 4.39)
#
# The objective FUNCTIONS are pinned against the Python arm bitwise; the
# fitted parameters are checked only to a stated tolerance, because scipy's
# and R's Nelder-Mead implementations settle on slightly different points of
# the flat ridge these objectives have along (nugget + partial sill).

fit_grid <- function() {
  g <- (0:11) / 1.5
  as.matrix(expand.grid(x = g, y = g))
}

fit_field <- function(coords) {
  sin(coords[, 1] * 0.7) + cos(coords[, 2] * 0.5) + 0.3 * coords[, 1]
}

test_that("the empirical semivariogram matches the Python arm", {
  coords <- fit_grid()
  ev <- .sp_empirical_variogram(coords, fit_field(coords), n_bins = 12)
  expect_equal(length(ev$lag), 12L)
  expect_equal(sum(ev$gamma, na.rm = TRUE), 5.44990053530051, tolerance = 1e-12)
  expect_equal(sum(ev$n_pairs), 6906)
})

test_that("the OLS objective is the plain residual sum of squares", {
  coords <- fit_grid()
  ev <- .sp_empirical_variogram(coords, fit_field(coords), n_bins = 12)
  f <- .schab_objective("ols", ev$lag, ev$gamma, ev$n_pairs, "exponential")
  ok <- is.finite(ev$gamma) & is.finite(ev$lag) & ev$n_pairs > 0
  fitted <- .sp_semivariogram(ev$lag[ok], 0.3, 2.0, 6.0, "exponential")
  resid <- ev$gamma[ok] - fitted
  expect_equal(f(c(0.3, 2.0, 6.0)), sum(resid * resid), tolerance = 1e-14)
  # pinned against the Python arm
  expect_equal(f(c(0.3, 2.0, 6.0)), 15.7884892484632, tolerance = 1e-12)
})

test_that("the WLS objective is equation 4.34", {
  # eq (4.34) is sum |N| / (2 gamma^2) * resid^2. Dividing through gives the
  # familiar (1/2) sum |N| [gamma_hat/gamma - 1]^2; assert the identity rather
  # than assuming the two forms were transcribed consistently.
  coords <- fit_grid()
  ev <- .sp_empirical_variogram(coords, fit_field(coords), n_bins = 12)
  f <- .schab_objective("wls", ev$lag, ev$gamma, ev$n_pairs, "exponential")
  ok <- is.finite(ev$gamma) & is.finite(ev$lag) & ev$n_pairs > 0
  fitted <- .sp_semivariogram(ev$lag[ok], 0.3, 2.0, 6.0, "exponential")
  alt <- 0.5 * sum(ev$n_pairs[ok] * (ev$gamma[ok] / fitted - 1)^2)
  expect_equal(f(c(0.3, 2.0, 6.0)), alt, tolerance = 1e-12)
  # pinned against the Python arm
  expect_equal(f(c(0.3, 2.0, 6.0)), 1809.00668526471, tolerance = 1e-12)
})

test_that("the fit improves on its starting values", {
  # Regression guard for a real defect: with a scalar objective and a
  # quasi-Newton solver this family stopped after one iteration and returned
  # the starting heuristic as the fit, reporting success. The start sat near
  # the truth, so the numbers looked plausible.
  coords <- fit_grid()
  ev <- .sp_empirical_variogram(coords, fit_field(coords), n_bins = 12)
  ok <- is.finite(ev$gamma) & is.finite(ev$lag) & ev$n_pairs > 0
  sb <- .schab_start_and_bounds(ev$lag[ok], ev$gamma[ok])
  for (kind in c("ols", "wls")) {
    f <- .schab_objective(kind, ev$lag, ev$gamma, ev$n_pairs, "exponential")
    res <- if (kind == "ols") spols(ev, "exponential") else spwls(ev, "exponential")
    got <- c(res$nugget, res$partial_sill, res$range)
    expect_false(isTRUE(all.equal(got, sb$start)))
    expect_lt(res$objective, f(sb$start))
  }
})

test_that("fitted parameters agree with the Python arm to solver tolerance", {
  coords <- fit_grid()
  ev <- .sp_empirical_variogram(coords, fit_field(coords), n_bins = 12)
  o <- spols(ev, "exponential")
  w <- spwls(ev, "exponential")
  expect_equal(o$range, 49.0838324789, tolerance = 1e-6)
  expect_equal(o$objective, 0.0772183885839, tolerance = 1e-6)
  expect_equal(w$range, 49.0838324787, tolerance = 1e-4)
  expect_equal(w$objective, 210.529009138, tolerance = 1e-5)
})

test_that("parameters stay inside the valid space", {
  coords <- fit_grid()
  ev <- .sp_empirical_variogram(coords, fit_field(coords), n_bins = 12)
  for (res in list(spols(ev, "exponential"), spwls(ev, "exponential"))) {
    expect_gte(res$nugget, 0)
    expect_gte(res$partial_sill, 0)
    expect_gt(res$range, 0)
    expect_equal(res$sill, res$nugget + res$partial_sill)
  }
})

test_that("counts reach WLS and do not leak into OLS", {
  # |N(h_m)| appears in (4.34) and nowhere in the OLS criterion.
  coords <- fit_grid()
  ev <- .sp_empirical_variogram(coords, fit_field(coords), n_bins = 12)
  skew <- ev
  half <- seq_len(length(ev$n_pairs) %/% 2)
  skew$n_pairs[half] <- skew$n_pairs[half] * 40
  expect_false(isTRUE(all.equal(spwls(ev, "exponential")$range,
                                spwls(skew, "exponential")$range)))
  expect_equal(spols(ev, "exponential")$range,
               spols(skew, "exponential")$range, tolerance = 1e-9)
})

# ---------------------------------------------------------------- spreml ---

test_that("the contrast matrix annihilates the mean structure", {
  X <- matrix(1, nrow = 25, ncol = 1)
  K <- .schab_error_contrasts(X)
  expect_equal(dim(K), c(24L, 25L))
  expect_true(all(abs(K %*% X) < 1e-10))
  expect_equal(qr(K)$rank, 24L)
})

test_that("the contrast matrix handles a regression mean", {
  coords <- fit_grid()[1:30, ]
  X <- cbind(1, coords)
  K <- .schab_error_contrasts(X)
  expect_equal(nrow(K), 30L - 3L)
  expect_true(all(abs(K %*% X) < 1e-10))
})

test_that("REML matches the Python arm and is not the least squares answer", {
  coords <- fit_grid()[1:80, ]
  z <- fit_field(fit_grid())[1:80]
  r <- spreml(coords, z, NULL, "exponential")
  expect_equal(r$range, 40.3550020679, tolerance = 1e-6)
  expect_equal(r$neg2_restricted_loglik, -66.0511309219, tolerance = 1e-6)
  ev <- .sp_empirical_variogram(fit_grid(), fit_field(fit_grid()), n_bins = 12)
  expect_false(isTRUE(all.equal(r$range, spwls(ev, "exponential")$range)))
})

test_that("REML reports the number of contrasts and stays in the valid space", {
  coords <- fit_grid()[1:80, ]
  z <- fit_field(fit_grid())[1:80]
  r <- spreml(coords, z, NULL, "exponential")
  expect_equal(r$n_contrasts, 79L)
  expect_gte(r$nugget, 0)
  expect_gte(r$partial_sill, 0)
  expect_gt(r$range, 0)
})

test_that("the fitting family rejects bad input", {
  expect_error(spols(matrix(1, nrow = 2, ncol = 1), "exponential"))
  expect_error(spwls(list(lag = 1, gamma = 1, n_pairs = 1), "exponential"))
  expect_error(spreml(fit_grid()[1:20, ], rep(1, 19), NULL, "exponential"))
})
