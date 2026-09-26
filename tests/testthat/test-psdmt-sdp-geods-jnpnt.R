## psdmt / dpss --------------------------------------------------------------
test_that("morie_psdmt matches morie.fn.psdmt and is a one-sided density", {
  x <- sin(0.37 * (1:257)) + 0.5 * cos(1.9 * (1:257))
  r <- morie_psdmt(x, fs = 100, nw = 3)
  expect_equal(r$psd[1:3], c(1.1950770027087269e-05, 2.417794752222197e-05, 2.5028505924370897e-05),
               tolerance = 1e-9)
  expect_equal(r$value, 0.6244615006546191, tolerance = 1e-9)
  expect_equal(r$concentrations, c(0.9999998656864341, 0.999990779561947, 0.9997155091648665,
                                   0.9949200133348849, 0.946165514354919), tolerance = 1e-9)
  # Parseval: the one-sided density integrates to about the variance
  set.seed(2)
  z <- rnorm(1024)
  expect_equal(morie_psdmt(z, fs = 50)$value, mean((z - mean(z))^2), tolerance = 0.05)
})

test_that("morie_dpss tapers are orthonormal and concentrated", {
  w <- morie_dpss(64, 3, Kmax = 5, return_ratios = TRUE)
  expect_equal(w$tapers %*% t(w$tapers), diag(5), tolerance = 1e-10)
  expect_true(all(diff(w$ratios) < 0))
  expect_true(w$ratios[1] > 0.9999)
  expect_true(sum(w$tapers[1, ]) > 0)
})

## sdpwts ---------------------------------------------------------------------
test_that("morie_solve_sdp reaches the optimum within the m / t gap", {
  r <- sdpwts_solve_sdp(1, matrix(c(0, 1, 1, 0), 2), list(diag(2)), 2)
  expect_true(r$objective - 1 >= 0)
  expect_true(r$objective - 1 <= r$gap)
  e <- sdpwts_min_eigenvalue_sdp(matrix(c(3, 1, 1, 2), 2))
  expect_equal(e$lambda_min, (5 - sqrt(5)) / 2, tolerance = 1e-12)
  expect_true(e$error <= e$gap)
  expect_error(sdpwts_solve_sdp(1, matrix(c(0, 1, 1, 0), 2), list(diag(2)), 0.5), "STRICTLY")
})

## geods ----------------------------------------------------------------------
test_that("morie_geods follows straight lines, also in spherical coordinates", {
  flat <- function(x) diag(c(-1, 1, 1, 1))
  r <- morie_geods(flat, c(0, 0, 0, 0), c(1, 0.5, 0, 0), c(0, 2), 11)
  expect_equal(r$position[11, ], c(2, 1, 0, 0), tolerance = 1e-9)
  sph <- function(q) diag(c(-1, 1, q[2]^2, (q[2] * sin(q[3]))^2))
  s <- morie_geods(sph, c(0, 1, pi / 2, 0), c(1, 0, 0, 1), c(0, 2), 5)
  expect_equal(s$position[, 2], sqrt(1 + s$tau^2), tolerance = 1e-8)
  expect_equal(s$position[, 4], atan(s$tau), tolerance = 1e-8)
  expect_error(morie_geods(flat, c(0, 0, 0), c(1, 0, 0, 0)), "length-4")
})

## jnpnt ----------------------------------------------------------------------
test_that("morie_jnpnt fits the continuous joinpoint hazard like morie.fn.jnpnt", {
  tt <- c(0.3, 1.2, 0.7, 2.5, 3.1, 0.2, 4.4, 1.9, 0.9, 5.6, 2.2, 0.4, 1.5, 3.8, 6.1, 0.6, 2.9, 1.1, 7.3, 0.8)
  ee <- c(1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1)
  f <- rmorie:::.morie_jnp_fit(tt, ee, 2)
  expect_equal(f$theta, c(-1.1022556324387012, -0.14446523122989086, 0.3806913320289969), tolerance = 1e-8)
  expect_equal(f$ll, -32.770953231621235, tolerance = 1e-10)
  r <- morie_jnpnt(tt, ee, max_joinpoints = 1)
  expect_equal(r$n_events, 16L)
  expect_true(is.finite(r$bic))
  expect_error(morie_jnpnt(tt, ee[-1]), "same length")
})
