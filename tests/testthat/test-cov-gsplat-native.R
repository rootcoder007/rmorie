# Coverage tests for R/gsplat_native.R (Kerbl et al. 2023, 3D Gaussian
# splatting): covariance from scale and rotation, EWA projection,
# front-to-back compositing and adaptive density control, both spellings.

test_that("covariance R S S' R' from a quaternion and scales", {
  q <- c(0.9, 0.2, -0.3, 0.1)
  s <- c(0.5, 1.2, 0.3)
  u <- q / sqrt(sum(q^2))
  w <- u[1]
  v <- u[2:4]
  vx <- rbind(c(0, -v[3], v[2]), c(v[3], 0, -v[1]), c(-v[2], v[1], 0))
  R <- diag(3) + 2 * w * vx + 2 * vx %*% vx
  for (f in list(covariance_from_scale_rotation, morie_gsplat_covariance)) {
    r <- f(s, q)
    expect_equal(r$rotation, R, tolerance = 1e-12)
    expect_equal(r$covariance, R %*% diag(s^2) %*% t(R), tolerance = 1e-12)
    expect_equal(eigen(r$covariance)$values, sort(s^2, decreasing = TRUE), tolerance = 1e-12)
    expect_error(f(c(1, -1, 1), q), "positive")
    expect_error(f(s, c(0, 0, 0, 0)), "quaternion is zero")
  }
})

test_that("PSD check and the EWA projection J W S W' J'", {
  S <- rbind(c(2, 0.5, 0), c(0.5, 1, 0.2), c(0, 0.2, 0.7))
  for (f in list(is_positive_semidefinite, morie_gsplat_psd)) {
    p <- f(S)
    expect_equal(p$min_eigenvalue, min(eigen(S)$values), tolerance = 1e-12)
    expect_true(p$psd)
    expect_false(f(rbind(c(1, 2), c(2, 1)))$psd)
  }
  W <- rbind(c(0.8, -0.6, 0), c(0.6, 0.8, 0), c(0, 0, 1))
  J <- rbind(c(1 / 3, 0, -0.1), c(0, 1 / 3, 0.05))
  for (f in list(project_covariance, morie_gsplat_project)) {
    pr <- f(S, W, J)
    expect_equal(pr$projected, J %*% W %*% S %*% t(W) %*% t(J), tolerance = 1e-12)
    expect_equal(pr$dim, 2L)
  }
})

test_that("front-to-back alpha compositing", {
  C <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  a <- c(0.5, 0.4, 0.8)
  d <- c(3, 1, 2)
  for (f in list(alpha_composite, morie_gsplat_composite, gaussian_splatting, gaussiansplatting, morie_gsplat)) {
    r <- f(C, a, depths = d)
    # depth order 2, 3, 1
    ref <- 0.4 * C[2, ] + 0.6 * 0.8 * C[3, ] + 0.6 * 0.2 * 0.5 * C[1, ]
    expect_equal(r$colour, ref, tolerance = 1e-12)
    expect_equal(r$transmittance, 0.6 * 0.2 * 0.5, tolerance = 1e-12)
    expect_equal(r$coverage, 1 - 0.06, tolerance = 1e-12)
    expect_equal(f(C, a)$colour, 0.5 * C[1, ] + 0.5 * 0.4 * C[2, ] + 0.3 * 0.8 * C[3, ], tolerance = 1e-12)
    expect_error(f(C, a[-1]), "2 alphas")
    expect_error(f(C, c(0.5, 1.2, 0)), "\\[0,1\\]")
  }
})

test_that("adaptive density control: prune, split, clone", {
  g <- c(1e-3, 5e-5, 3e-4, 1e-3, 1e-2)
  s <- c(0.02, 0.5, 0.005, 0.001, 0.3)
  o <- c(0.5, 0.9, 0.3, 0.001, 0.7)
  a <- adaptive_density_control(g, s, o)
  expect_equal(a$split, c(0L, 4L))
  expect_equal(a$clone, 2L)
  expect_equal(a$prune, 3L)
  expect_equal(a$n_after, 5 + 1 + 2 - 1)
  b <- morie_gsplat_density(g, s, o)
  expect_equal(b$split, c(1L, 5L))
  expect_equal(b$clone, 3L)
  expect_equal(b$prune, 4L)
  expect_equal(b$n_after, a$n_after)
  expect_error(adaptive_density_control(g, s[-1], o), "differ in length")
  expect_error(morie_gsplat_density(g, s, o[-1]), "differ in length")
})
