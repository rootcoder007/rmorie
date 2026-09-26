gls_n <- 36
gls_i <- 0:(gls_n - 1)
gls_co <- cbind(10 * ((gls_i * 0.618034) %% 1), 10 * ((gls_i * 0.414214 + 0.3) %% 1))
gls_z <- sin(1.1 * gls_co[, 1]) + cos(0.9 * gls_co[, 2]) + sin(0.7 * gls_co[, 1] + 1.3 * gls_co[, 2]) +
  0.3 * sin(7.1 * gls_i)
gls_br <- c(0, 0.8, 1.6, 2.4, 3.2, 4.0, 5.0, 6.0)

test_that("Cressie's R(theta) equals 2 tr(A_i S A_j S) (pp. 164-165)", {
  lp <- .schab_glsvg_pairs(gls_co, gls_br)
  r <- .schab_glsvg_cov(lp$d, lp$classes, 0.2, 2.0, 5.0, "exponential")
  expect_equal(diag(r), c(0.29962184631112182, 0.11615419706154145, 0.26416136940204921,
                          0.36320271149342753, 0.40353978660983908, 0.49314340113395816,
                          0.54969891769341506), tolerance = 1e-12)
  expect_equal(r[1, ], c(0.29962184631112182, 0.028774211115752343, 0.067566773046412426,
                         0.060439906183633568, 0.057427907166929748, 0.062522130860420813,
                         0.053651774335276646), tolerance = 1e-12)
})

test_that("spglsv reaches the GLS estimate an optim loop finds (eqs 4.30-4.31)", {
  f <- spglsv(gls_co, gls_z, gls_br, "exponential")
  expect_equal(f$gamma_hat, c(0.27569486041069963, 0.86082657910209071, 2.4608017740039405,
                              2.458627367132832, 1.7760948942595789, 1.9238149595719074,
                              2.0367351887342777), tolerance = 1e-14)
  expect_equal(f$sill, 2.2182678263156599, tolerance = 2e-6)
  expect_equal(f$range, 6.3002832224103127, tolerance = 2e-6)
  expect_lt(f$nugget, 1e-10)
  expect_equal(f$criterion, 26.695525008551186, tolerance = 1e-6)
})

test_that("vgdrift equals Var(Z_i - Z_j) / 2 + (mu_i - mu_j)^2 / 2 (eq 5.35)", {
  d <- vgdrift(gls_co, cbind(1, gls_co), c(2, 0.3, -0.2), gls_br, 0.1, 1.0, 4.0, "exponential")
  expect_equal(d$expected, c(0.57541916978473129, 0.67803046303768688, 0.96757852460361293,
                             1.3400351558026318, 1.3936400637999211, 1.5497724270514073,
                             2.196069585434957), tolerance = 1e-13)
})
