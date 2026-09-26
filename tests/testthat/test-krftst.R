kr_i <- 0:29
kr_g <- kr_i %/% 5
kr_x <- sin(1.3 * kr_i) + 0.1 * kr_i
kr_y <- 2 + 0.5 * kr_x + c(-0.6, 0.3, 0.9, -0.2, 0.4, -0.8)[kr_g + 1] + 0.5 * sin(2.9 * kr_i + 0.4)

test_that("krftst equals pbkrtest::KRmodcomp for variance components", {
  zz <- outer(kr_g, kr_g, "==") * 1
  s <- 0.41778383883297898 * zz + 0.13995228058289766 * diag(30)
  r <- krftst(cbind(1, kr_x), kr_y, s, list(zz, diag(30)), matrix(c(0, 1), 1))
  # KRmodcomp(lmer(y ~ x + (1 | g)), update(., . ~ . - x))
  expect_equal(r$F, 18.458409881288013, tolerance = 1e-11)
  expect_equal(r$ddf, 27.367979299962421, tolerance = 1e-11)
  expect_equal(r$scaling, 1)
  expect_equal(r$p_value, 0.00019657909126163887, tolerance = 1e-10)
})

kr_j <- 0:24
kr_co <- cbind(10 * ((kr_j * 0.618034) %% 1), 10 * ((kr_j * 0.414214 + 0.3) %% 1))
kr_d <- as.matrix(dist(kr_co))
kr_xs <- cbind(1, kr_co[, 1])
kr_ys <- 1 + 0.2 * kr_co[, 1] + sin(kr_co[, 2]) + 0.3 * cos(5.3 * kr_j)
kr_r <- exp(-3 * kr_d / 4)
kr_s <- 1.5 * kr_r
diag(kr_s) <- 1.8
kr_dr <- kr_r * 3 * kr_d / 16

test_that("krftst equals pbkrtest's internals for a spatial covariance", {
  r <- krftst(kr_xs, kr_ys, kr_s, list(diag(25), kr_r, 1.5 * kr_dr), matrix(c(0, 1), 1))
  expect_equal(c(r$F, r$ddf, r$Phi_adjusted[2, 2]),
               c(1.2806152728363422, 2.0926745247882712, 0.017622414876145718), tolerance = 1e-11)
})

test_that("the R_ij terms of (6.53) match finite differences of Sigma", {
  d2r <- kr_r * (9 * kr_d^2 / 256 - 6 * kr_d / 64)
  r <- krftst(kr_xs, kr_ys, kr_s, list(diag(25), kr_r, 1.5 * kr_dr), matrix(c(0, 1), 1),
              d2Sigma = list("2,3" = kr_dr, "3,3" = 1.5 * d2r))
  expect_equal(r$Phi_adjusted[2, 2], 0.039393056880032834, tolerance = 1e-7)
  expect_equal(r$Phi_adjusted[1, 2], -0.20150629908819473, tolerance = 1e-7)
})
