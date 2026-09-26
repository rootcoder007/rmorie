cmp_i <- 0:39
cmp_x <- 10 * ((cmp_i * 0.618034) %% 1)
cmp_y <- 10 * ((cmp_i * 0.414214 + 0.3) %% 1)
cmp_w <- 0.5 + 0.1 * cmp_x
cmp_z <- 1 + 0.4 * cmp_w + sin(0.9 * cmp_x) + cos(0.7 * cmp_y) + 0.5 * sin(0.6 * cmp_x + 1.1 * cmp_y) +
  0.8 * ((97 * cmp_i) %% 13 - 6) / 6
# -2 logLik of nlme::gls(z ~ w, correlation = corExp(form = ~ x + y, nugget = TRUE / FALSE)), gls(z ~ w)
cmp_nlme <- list(ml = c(102.72806151208869, 102.72806149996285, 119.18462546682153),
                 reml = c(100.36362758924545, 100.36362757931411, 120.09536318618083))

test_that("spcmp fits equal nlme::gls and AIC follows (6.59)-(6.60)", {
  for (m in c("ml", "reml")) {
    r <- spcmp(cbind(cmp_x, cmp_y), cmp_z, cbind(1, cmp_w), "exponential", m)
    ref <- cmp_nlme[[m]]
    expect_lt(abs(r$fits$independent$neg2loglik - ref[3]), 1e-10)
    expect_lt(abs(r$fits$no_nugget$neg2loglik - ref[2]), 1e-8)
    expect_lt(abs(r$fits$nugget$neg2loglik - ref[1]), 1e-7)
    expect_lt(abs(r$lrt_spatial - (ref[3] - ref[2])), 1e-7)
    extra <- if (m == "ml") 2 else 0
    expect_equal(r$fits$independent$aic, r$fits$independent$neg2loglik + 2 * (1 + extra))
  }
})

test_that("the boundary nugget test halves the chi-square p-value", {
  r <- spcmp(cbind(cmp_x, cmp_y), cmp_z, cbind(1, cmp_w), "exponential", "reml")
  expect_lt(r$lrt_nugget, 1e-6)
  expect_equal(r$p_nugget, 0.5 * r$p_nugget_naive)
})

test_that("spml REML range is nlme's corExp range times three", {
  f <- spml(cbind(cmp_x, cmp_y), cmp_z, "exponential", "reml", cbind(1, cmp_w))
  expect_lt(abs(f$neg2loglik - 100.36362757931411), 1e-7)
  expect_equal(f$range, 3 * 2.8749791287983926, tolerance = 1e-4)
})
