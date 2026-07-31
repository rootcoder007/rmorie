# SPDX-License-Identifier: AGPL-3.0-or-later
#
# spdjkr -- disjunctive kriging, Schabenberger Sec 5.6.4, eqs (5.64)-(5.72)
# spmidw -- inverse distance weighting, Bivand et al. (2013) Sec 8.3.1
#
# spmidw is NOT a Schabenberger method: inverse distance weighting appears in
# that book only in the subject index, so it is grounded in its own source.
#
# Disjunctive kriging is checked against the book's Example 5.12 identities
# and against Parseval, not against its own output.

dj_x <- c(0, 0.5, 1, -1.3, 2.7)

test_that("the Hermite recurrence reproduces the closed forms", {
  h <- .schab_hermite_e(dj_x, 4)
  expect_equal(h[1, ], rep(1, length(dj_x)))
  expect_equal(h[2, ], dj_x)
  expect_equal(h[3, ], dj_x^2 - 1)
  expect_equal(h[4, ], dj_x^3 - 3 * dj_x)
  expect_equal(h[5, ], dj_x^4 - 6 * dj_x^2 + 3)
})

test_that("the standardised system is orthonormal", {
  # E[eta_p eta_m] = delta_pm -- the property the method rests on, and the
  # reason H_p is divided by sqrt(p!) rather than used raw.
  q <- .schab_gauss_hermite(40)
  eta <- .schab_hermite_orthonormal(q$nodes, 5)
  expect_equal(sum(q$weights), 1, tolerance = 1e-12)
  expect_lt(max(abs(eta %*% (q$weights * t(eta)) - diag(6))), 1e-12)
})

test_that("Example 5.12 gives the identity expansion", {
  # For g(Z) = Z the expansion is just H_1: b_0 = 0, b_1 = 1, rest zero.
  b <- .schab_hermite_coefficients(function(v) v, 6)
  expect_equal(b[1], 0, tolerance = 1e-12)
  expect_equal(b[2], 1, tolerance = 1e-12)
  expect_lt(max(abs(b[3:7])), 1e-12)
})

test_that("indicator coefficients use the closed form, not quadrature", {
  # eq (5.72) gives b_0 = F(z_k) exactly. Quadrature is exact for
  # polynomials and the indicator is a step function, so it converges
  # slowly and silently -- in the canonical case for the method.
  zk <- 0.7
  exact <- .schab_indicator_coefficients(zk, 6)
  expect_equal(exact[1], pnorm(zk), tolerance = 1e-14)
  quad <- .schab_hermite_coefficients(function(v) as.numeric(v <= zk), 6)
  expect_gt(abs(quad[1] - exact[1]), 1e-3)      # the failure being guarded
})

test_that("indicator coefficients satisfy Parseval", {
  # I^2 = I so sum b_p^2 = F(z_k); partial sums climb toward it, slowly,
  # which is why the text advises only a few terms.
  zk <- 0.7
  target <- pnorm(zk)
  sums <- vapply(c(6, 14, 40, 120),
                 function(p) sum(.schab_indicator_coefficients(zk, p)^2),
                 numeric(1))
  expect_true(all(sums < target))
  expect_equal(sums, sort(sums))
})

test_that("an extreme threshold is degenerate", {
  b <- .schab_indicator_coefficients(8, 6)
  expect_equal(b[1], 1, tolerance = 1e-12)
  expect_lt(max(abs(b[-1])), 1e-10)
})

test_that("disjunctive component variances are bounded", {
  # eq (5.69): sigma^2_eta = 1 - lambda'rho, and eta_p has unit variance.
  g <- 0:4
  C <- as.matrix(expand.grid(x = g, y = g))
  y <- sin(C[, 1] * 0.6) + cos(C[, 2] * 0.4)
  b <- .schab_hermite_coefficients(function(v) v, 6)
  r <- .schab_disjunctive_kriging(C, y, c(2.3, 1.7),
                                  function(h) exp(-h / 3), b, 6)
  expect_true(all(r$component_variances[-1] >= -1e-12))
  expect_true(all(r$component_variances[-1] <= 1 + 1e-12))
  expect_true(is.finite(r$prediction))
  expect_gte(r$variance, 0)
})

test_that("the Hermite machinery matches the Python arm", {
  expect_equal(.schab_indicator_coefficients(0.7, 6)[1],
               0.758036347776927, tolerance = 1e-14)
})

# ------------------------------------------------------------------- IDW ---

idw_fixture <- function() {
  g <- 0:4
  C <- as.matrix(expand.grid(x = g, y = g))
  list(C = C, z = exp(sin(C[, 1] * 0.6) + cos(C[, 2] * 0.4)))
}

test_that("IDW is exact at an observation", {
  # "If s0 coincides with an observation location, the observed value is
  # returned to avoid infinite weights."
  f <- idw_fixture()
  r <- spmidw(f$C, f$z, f$C[4, ])
  expect_equal(r$prediction, f$z[4], tolerance = 1e-12)
  expect_true(r$exact_hits)
})

test_that("IDW weights are normalised and positive", {
  f <- idw_fixture()
  r <- spmidw(f$C, f$z, c(2.3, 1.7))
  expect_equal(sum(r$weights), 1, tolerance = 1e-12)
  expect_true(all(r$weights > 0))
})

test_that("IDW converges to nearest neighbour for large power", {
  f <- idw_fixture()
  d <- sqrt(rowSums((f$C - matrix(c(2.3, 1.7), nrow(f$C), 2, byrow = TRUE))^2))
  expect_equal(spmidw(f$C, f$z, c(2.3, 1.7), power = 60)$prediction,
               f$z[which.min(d)], tolerance = 1e-8)
})

test_that("IDW with zero power is the unweighted mean", {
  f <- idw_fixture()
  expect_equal(spmidw(f$C, f$z, c(2.3, 1.7), power = 0)$prediction,
               mean(f$z), tolerance = 1e-12)
})

test_that("IDW reports no prediction variance", {
  # "inverse distance does not provide prediction error variances" -- NULL
  # is honest; a fabricated number would not be.
  f <- idw_fixture()
  expect_null(spmidw(f$C, f$z, c(2.3, 1.7))$variance)
})

test_that("IDW rejects bad input", {
  f <- idw_fixture()
  expect_error(spmidw(f$C, f$z, c(1, 1), power = -1))
  expect_error(spmidw(f$C, f$z[-1], c(1, 1)))
  expect_error(spmidw(f$C, f$z, c(1, 1, 1)))
})
