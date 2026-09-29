# NeRF pieces (Mildenhall et al. 2020): positional encoding, stratified
# ray samples replayed on the shared stream, the quadrature volume
# rendering weights, inverse-CDF hierarchical sampling, and the
# view-independence check on density.

test_that("positional encoding stacks sin/cos at frequencies 2^j pi", {
  p <- c(0.3, -0.7)
  enc <- positional_encoding(p, L = 3)
  ref <- c(p, unlist(lapply(0:2, function(j) {
    f <- 2^j * pi
    as.vector(rbind(sin(f * p), cos(f * p)))
  })))
  expect_equal(enc, ref, tolerance = 1e-15)
  expect_length(positional_encoding(p, 3, include_input = FALSE), 12L)
  expect_error(positional_encoding(p, 0), "at least 1")
})

test_that("stratified ray samples fall one per bin along the unit direction", {
  r <- ray_points(c(0, 0, 1), c(0, 3, 4), 2, 6, 4, seed = 5)
  e <- .ghc_rng(5)
  ts <- 2 + (0:3) + vapply(1:4, function(i) .ghc_unif(e, 1L), 1)
  expect_equal(r$t, ts, tolerance = 1e-15)
  expect_equal(r$points, t(vapply(ts, function(tv) c(0, 0, 1) + tv * c(0, 0.6, 0.8), numeric(3))),
               tolerance = 1e-15)
  mid <- ray_points(c(0, 0, 0), c(1, 0, 0), 0, 1, 2, stratified = FALSE)
  expect_equal(mid$t, c(0.25, 0.75))
  expect_error(ray_points(c(0, 0, 0), c(0, 0, 0), 0, 1, 2), "zero")
  expect_error(ray_points(c(0, 0, 0), c(1, 0, 0), 1, 1, 2), "t_far > t_near")
})

test_that("volume rendering weights are T_i (1 - exp(-sigma_i delta_i))", {
  s <- c(0.5, 2, 0, 1)
  tt <- c(1, 1.5, 2.5, 3)
  C <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(1, 1, 1))
  r <- volume_render(s, C, tt)
  delta <- c(diff(tt), 1e10)
  a <- 1 - exp(-s * delta)
  Tr <- cumprod(c(1, 1 - a[-4]))
  w <- Tr * a
  expect_equal(r$weights, w, tolerance = 1e-15)
  expect_equal(r$colour, as.numeric(t(C) %*% w), tolerance = 1e-15)
  expect_equal(r$transmittance_final, prod(1 - a), tolerance = 1e-15)
  expect_equal(r$accumulated_alpha + r$transmittance_final, 1, tolerance = 1e-15)
  expect_error(volume_render(-s, C, tt), "negative")
  expect_error(volume_render(s[-1], C, tt), "differ in length")
  expect_identical(morie_nrfrad, volume_render)
})

test_that("hierarchical sampling inverts the weight CDF", {
  bins <- c(0, 1, 2, 3)
  w <- c(0.1, 0.7, 0.2)
  s <- sample_pdf(bins, w, 6, seed = 2, eps = 0)
  e <- .ghc_rng(2)
  cdf <- cumsum(w / sum(w))
  ref <- vapply(1:6, function(k) {
    u <- .ghc_unif(e, 1L)
    i <- min(which(u <= cdf), 3)
    bins[i] + .ghc_unif(e, 1L)
  }, 1)
  expect_equal(s, sort(ref), tolerance = 1e-15)
  expect_error(sample_pdf(bins, c(1, 2), 3), "2 weights do not match 4 bins")
})

test_that("density must not depend on the viewing direction", {
  good <- function(p, d) list(sigma = sum(p^2), rgb = d)
  bad <- function(p, d) list(sigma = sum(p^2) + d[1], rgb = d)
  dirs <- list(c(1, 0, 0), c(0, 1, 0), c(0.6, 0.8, 0))
  g <- density_is_view_independent(good, c(1, 2, 3), dirs)
  expect_true(g$view_independent)
  expect_equal(g$sigmas, rep(14, 3))
  b <- density_is_view_independent(bad, c(1, 2, 3), dirs)
  expect_false(b$view_independent)
  expect_equal(b$max_deviation, 1)
})
