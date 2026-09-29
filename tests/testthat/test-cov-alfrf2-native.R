# Coverage tests for R/alfrf2_native.R (Watson et al. 2023, RFdiffusion
# motif scaffolding): DDPM schedule and forward noising, Kabsch
# superposition, ideal-spacing projection and the reverse diffusion.

rf_P <- rbind(c(0, 0, 0), c(1.5, 0.2, -0.3), c(0.4, 2.1, 0.8), c(-1, 0.7, 1.9), c(0.9, -1.2, 0.5))

rf_rot <- function(a, b, c) {
  Rz <- rbind(c(cos(a), -sin(a), 0), c(sin(a), cos(a), 0), c(0, 0, 1))
  Ry <- rbind(c(cos(b), 0, sin(b)), c(0, 1, 0), c(-sin(b), 0, cos(b)))
  Rx <- rbind(c(1, 0, 0), c(0, cos(c), -sin(c)), c(0, sin(c), cos(c)))
  Rz %*% Ry %*% Rx
}

test_that("linear DDPM schedule and forward noising", {
  s <- morie_alfrf2_schedule(5, 1e-3, 0.05)
  b <- seq(1e-3, 0.05, length.out = 5)
  expect_equal(s$betas, c(0, b), tolerance = 1e-12)
  expect_equal(s$abar, c(1, cumprod(1 - b)), tolerance = 1e-12)
  expect_error(morie_alfrf2_schedule(0), "at least one step")
  expect_error(morie_alfrf2_schedule(3, 0.1, 0.05), "rise through")
  x0 <- matrix(1:6, 2)
  eps <- matrix(c(0.5, -1, 0.2, 0.3, 1, -0.4), 2)
  expect_equal(morie_alfrf2_noise(x0, 0.64, eps), 0.8 * x0 + 0.6 * eps, tolerance = 1e-12)
})

test_that("Kabsch recovers a rigid motion and matches the SVD solution", {
  R0 <- rf_rot(0.7, -0.4, 1.1)
  Q <- t(R0 %*% t(rf_P)) + matrix(c(1, -2, 0.5), 5, 3, byrow = TRUE)
  k <- morie_alfrf2_kabsch(rf_P, Q)
  expect_equal(k$R, R0, tolerance = 1e-10)
  expect_equal(k$t, c(1, -2, 0.5), tolerance = 1e-10)
  expect_equal(k$rmsd, 0, tolerance = 1e-9)
  Q2 <- Q + rbind(c(0.1, 0, 0), c(0, -0.2, 0.1), c(0.05, 0.05, 0), c(0, 0, -0.1), c(-0.1, 0.1, 0.05))
  p <- sweep(rf_P, 2, colMeans(rf_P))
  q <- sweep(Q2, 2, colMeans(Q2))
  sv <- svd(crossprod(p, q))
  dd <- sign(det(sv$v %*% t(sv$u)))
  Rs <- sv$v %*% diag(c(1, 1, dd)) %*% t(sv$u)
  moved <- t(Rs %*% t(p)) + matrix(colMeans(Q2), 5, 3, byrow = TRUE)
  expect_equal(morie_alfrf2_rmsd(rf_P, Q2), sqrt(mean(rowSums((moved - Q2)^2))), tolerance = 1e-9)
  expect_error(morie_alfrf2_kabsch(rf_P[1:2, ], Q[1:2, ]), "three points")
  expect_error(morie_alfrf2_kabsch(rf_P, Q[1:4, ]), "same number")
})

test_that("ideal-spacing projection keeps fixed residues and restores bond lengths", {
  x <- rbind(c(0, 0, 0), c(5, 0, 0))
  y <- morie_alfrf2_ideal(x, fixed = integer(0), passes = 1)
  expect_equal(sqrt(sum((y[2, ] - y[1, ])^2)), 3.8, tolerance = 1e-12)
  expect_equal(colMeans(y), colMeans(x), tolerance = 1e-12)
  z <- morie_alfrf2_ideal(x, fixed = 0L, passes = 1)
  expect_equal(z[1, ], c(0, 0, 0))
  expect_equal(z[2, ], c(3.8, 0, 0), tolerance = 1e-12)
  chain <- rbind(c(0, 0, 0), c(2, 0, 0), c(4, 1, 0), c(7, 1, 1))
  cz <- morie_alfrf2_ideal(chain, fixed = integer(0), passes = 50)
  expect_equal(sqrt(rowSums(diff(cz)^2)), rep(3.8, 3), tolerance = 1e-6)
})

test_that("reverse diffusion places the motif exactly", {
  motif <- list(list(0L, c(0, 0, 0)), list(2L, c(3.8, 0, 0)), list(4L, c(7.6, 0, 0)))
  r <- morie_alfrf2(motif, 6, T = 10, seed = 2)
  expect_equal(r$motif_max_deviation, 0)
  expect_equal(r$backbone[c(1, 3, 5), ], r$motif_target)
  expect_equal(r$spacing, sqrt(rowSums(diff(r$backbone)^2)), tolerance = 1e-12)
  expect_equal(r$radius_of_gyration, sqrt(mean(rowSums(sweep(r$backbone, 2, colMeans(r$backbone))^2))), tolerance = 1e-12)
  expect_length(r$trace, 10L)
  expect_identical(morie_alfrf2(motif, 6, T = 10, seed = 2), r)
  pr <- morie_alfrf2(motif, 6, T = 5, denoise = "prior", seed = 1)
  expect_equal(pr$motif_max_deviation, 0)
  cb <- morie_alfrf2(motif, 6, T = 5, denoiser = function(x, t) x * 0.5, seed = 1)
  expect_identical(cb$denoise, "callable")
  expect_error(morie_alfrf2(motif, 2), "fewer than three")
  expect_error(morie_alfrf2(list(list(9L, c(0, 0, 0))), 6), "outside the design")
  expect_error(morie_alfrf2(motif, 6, denoise = "learned"), "prior or ideal")
  expect_match(morie_alfrf2_cheatsheet(), "RFdiffusion")
})
