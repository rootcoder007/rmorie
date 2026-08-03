# MVSML chapter 6b: multi-trait / BMTME.

test_that("eq (6.10) ridge form reproduces G (p.194)", {
  set.seed(5)
  A <- matrix(rnorm(6 * 8), nrow = 6)
  G <- morie_mvsml_grm(A) + diag(0.4, 6)
  r <- morie_mvsml_multitrait_ridge(diag(6), G)
  expect_equal(r$L_G %*% t(r$L_G), G, tolerance = 1e-9)
  expect_equal(r$X1, r$L_G, tolerance = 1e-12)
  expect_equal(r$X1 %*% t(r$X1), G, tolerance = 1e-9)
})

test_that("the inverse-Wishart draw has mean S/(nu-p-1)", {
  set.seed(3)
  nu <- 30; S <- diag(c(4, 9))
  d <- replicate(600, morie_mvsml_inv_wishart(nu, S),
                 simplify = FALSE)
  m00 <- mean(vapply(d, function(m) m[1, 1], 0))
  m11 <- mean(vapply(d, function(m) m[2, 2], 0))
  expect_equal(m00, 4 / (nu - 3), tolerance = 0.15)
  expect_equal(m11, 9 / (nu - 3), tolerance = 0.15)
  expect_equal(d[[1]][1, 2], d[[1]][2, 1], tolerance = 1e-9)
})

test_that("BMTME conditionals follow the p.196 Gibbs steps", {
  set.seed(5)
  A <- matrix(rnorm(6 * 8), nrow = 6)
  G <- morie_mvsml_grm(A) + diag(0.4, 6)
  J <- 6L; I <- 2L; nT <- 2L
  Y <- matrix(rnorm(J * nT, 4, 1), nrow = J)
  Z2 <- matrix(0, J, I * J); Z2[cbind(1:J, 1:J)] <- 1
  b1 <- cbind(0.1 * (1:J), -0.05 * (1:J))
  b2 <- cbind(0.02 * (1:(I * J)), 0.01 * (1:(I * J)))
  r <- morie_mvsml_bmtme_conditionals(Y, diag(J), Z2, G,
                                      matrix(c(1, .2, .2, 1), 2),
                                      diag(2), diag(2),
                                      b1 = b1, b2 = b2)
  expect_equal(r$nu_T_post, 4 + J + I * J)        # step 5
  expect_equal(r$nu_E_post, 4 + J * I)            # step 6
  expect_equal(r$scale_T[1, 2], r$scale_T[2, 1], tolerance = 1e-9)
  expect_gt(r$scale_T[1, 1], 0)
  r0 <- morie_mvsml_bmtme_conditionals(Y, diag(J), Z2, G,
                                       matrix(c(1, .2, .2, 1), 2),
                                       diag(2), diag(2), b1 = b1)
  expect_lt(r0$scale_T[1, 1], r$scale_T[1, 1])
})
