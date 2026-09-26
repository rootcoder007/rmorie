h8 <- 1:8

test_that("zegcnt reproduces the book's rho1 / 2 example (p. 294)", {
  expect_equal(zegcnt(1, 1, 1, 0.6)$corr, 0.3, tolerance = 1e-15)
  expect_equal(zegcnt(2, 0.5, 0.8, 0.9)$corr, 0.9 / sqrt((1 + 1 / 1.6) * (1 + 1 / 0.4)),
               tolerance = 1e-15)
})

test_that("nnlsq equals nnls::nnls", {
  A <- outer(0:5, 0:3, function(i, j) sin(1.3 * i + 0.7 * j) + 0.1 * (i == j))
  r <- nnlsq(A, cos(0.9 * (0:5)))
  expect_equal(r$x, c(0.96830801856704418, 0, 0, 0.41394598214996808), tolerance = 1e-13)
  expect_equal(r$residual_norm, 1.0687783699685953, tolerance = 1e-13)
})

test_that("spnpsv fits the weights nnls finds on the J0 basis (eq 4.47)", {
  g <- 1.1 - 1.1 * exp(-h8 / 3) + 0.04 * sin(2 * h8)
  r <- spnpsv(h8, c(0.1, 0.375, 0.65, 0.925, 1.2), gamma_hat = g, d = 2)
  expect_equal(r$weights, c(0, 0.23583024764001043, 0.34227825940611956, 0,
                            0.30395031089466884), tolerance = 1e-12)
  expect_equal(r$rss, 0.051067101651968011, tolerance = 1e-12)
})

test_that("spkrnv covariance equals integrate() of besselJ (eqs 4.49-4.50)", {
  h <- c(0, 1, 5, 12, 30)
  expect_equal(spkrnv(h, 1.5, 0.1, 0.3, 2, 1)$covariance,
               c(1.5, 1.483806617205643, 1.1276535116158839, 0.07569893717565343,
                 -0.03382520149877942), tolerance = 1e-13)
  expect_equal(spkrnv(h, 1.5, -0.1, 0.1, 2, 1)$covariance,
               c(1.5, 1.4993752343285029, 1.4845207599690675, 1.4147235444680419,
                 1.0968918130024661), tolerance = 1e-13)
  expect_equal(spkrnv(h, 1.5, 0.1, 0.4, 2, 0.25)$covariance,
               c(1.5, 1.4822482806342059, 1.0912352688918874, -0.060034974151034377,
                 0.15168877044325332), tolerance = 1e-13)
})

test_that("spkrnv covariance holds at long, oscillating lags", {
  expect_equal(spkrnv(c(200, 450), 1.5, 0.1, 0.3, 2, 1)$covariance,
               c(-0.00038512817440674435, 0.00045158119913447583), tolerance = 1e-10)
})

test_that("spkrnv fit reaches the optim minimum of criterion (4.51)", {
  gh <- c(0.007731718001499521, 0.032119773873861314, 0.06548960554339905,
          0.1235270685278768, 0.17729197449014697, 0.2628109442161538,
          0.3308649625471822, 0.43506276044506675)
  f <- spkrnv(h8, d = 2, gamma_hat = gh, start = c(0, 0.2))
  expect_lt(abs(f$q - 0.00019471369070002), 1e-15)
  expect_equal(f$theta_u, 0.225600036776, tolerance = 1e-5)
  expect_equal(f$sill_effective, 1.795434, tolerance = 1e-5)
})

test_that("cndchk equals the eigenvalues on a Helmert basis (Problem 2.2)", {
  d <- as.matrix(dist(rbind(c(0, 0), c(1, 0), c(0, 2), c(3, 1), c(2, 2.5), c(4, 4))))
  sph <- ifelse(d == 0, 0, ifelse(d <= 3.5, 1.5 * d / 3.5 - 0.5 * (d / 3.5)^3, 1))
  a <- cndchk(sph)
  b <- cndchk(d^2.5)
  expect_equal(a$max_eigenvalue, -0.39425481876645674, tolerance = 1e-12)
  expect_true(a$valid)
  expect_equal(b$max_eigenvalue, 10.041207757544395, tolerance = 1e-12)
  expect_false(b$valid)
})
