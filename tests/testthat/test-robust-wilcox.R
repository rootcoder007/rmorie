# Parity of the R robust core against morie Python and against WRS.
#
# Every expected value is one WRS (Wilcox's own Rallfun-v45.R) produces,
# and the Python arm was checked against the same numbers, so a failure
# here means the two arms of the package have diverged.

X  <- c(12, 45, 23, 79, 19, 92, 30, 58, 132)
XS <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
YS <- c(2, 4, 5, 9, 10, 13, 14, 17, 18, 21)
GX <- c(14.1, 11.2, 15.5, 9.8, 13.3, 12.2, 16.2, 10.7)
GY <- c(18.4, 14.9, 12.5, 17.7, 15.8, 19.1, 13.9, 16.6, 11.1)
GY8 <- c(18.4, 14.9, 12.5, 17.7, 15.8, 19.1, 13.9, 16.6)
G3 <- c(21.2, 19.8, 24.1, 17.3, 22.5, 20.4, 18.9, 23.7)
IDEALF <- c(-29.6, -20.9, -19.7, -15.4, -12.3, -8.0,
            -4.3, 0.8, 2.0, 6.2, 11.2, 25.0)
MASK <- c(2, 2, 3, 3, 3, 4, 4, 4, 100000, 100000)

test_that("ideal fourths reproduce the book's worked example", {
  f <- morie_ideal_fourths(IDEALF)
  expect_equal(f$j, 3)
  expect_equal(f$h, 0.41667, tolerance = 1e-4)
  expect_equal(f$q1, -17.9, tolerance = 1e-3)   # book prints 1 decimal
  expect_equal(f$q2, 4.45, tolerance = 1e-6)
  expect_equal(morie_idealf_iqr(IDEALF), 22.35, tolerance = 1e-3)
})

test_that("Winsorized variance matches the values printed on p.28", {
  expect_equal(morie_winsorized_variance(X, 0.2), 937.9, tolerance = 1e-4)
  expect_equal(morie_winsorized_variance(X, 0), 1596.8, tolerance = 1e-4)
  expect_equal(morie_winsorized_variance(X, 0), stats::var(X))
  expect_lt(morie_winsorized_variance(X, 0.2), stats::var(X))
})

test_that("MADN and the MAD-median rule match the masking example p.33", {
  expect_equal(stats::median(MASK), 3.5)
  expect_equal(morie_madn(MASK), 0.7413, tolerance = 1e-4)
  r <- morie_mad_median_rule(MASK)
  expect_equal(r$n_outliers, 2)
  expect_equal(unique(r$outliers), 100000)
})

test_that("robust location estimators match WRS", {
  expect_equal(morie_harrell_davis(X), 46.6180132770, tolerance = 1e-8)
  expect_equal(morie_harrell_davis(X, 0.25), 23.5087932814, tolerance = 1e-8)
  expect_equal(morie_mom_estimator(X), 44.75, tolerance = 1e-9)
  expect_equal(morie_one_step_m(X), 50.9176160000, tolerance = 1e-7)
  expect_equal(morie_pbos(X), 47.7142857143, tolerance = 1e-9)
})

test_that("robust correlations match WRS", {
  expect_equal(morie_percentage_bend_correlation(XS, YS)$cor,
               0.9947292172, tolerance = 1e-9)
  w <- morie_winsorized_correlation(XS, YS)
  expect_equal(w$cor, 0.9935833618, tolerance = 1e-9)
  expect_equal(w$p_value, 1.5572661e-05, tolerance = 1e-6)
  expect_equal(w$df, length(XS) - 2 * floor(0.2 * length(XS)) - 2)
})

test_that("Yuen reduces to Welch with no trimming", {
  yu <- morie_yuen_test(GX, GY, tr = 0)
  we <- morie_welch_test(GX, GY)
  expect_equal(yu$statistic, we$statistic, tolerance = 1e-10)
  expect_equal(yu$df, we$df, tolerance = 1e-10)
  expect_equal(yu$p_value, we$p_value, tolerance = 1e-12)
})

test_that("trimmed-mean inference matches WRS trimse and trimci", {
  expect_equal(morie_trimmed_mean_se(X), 17.0143770401, tolerance = 1e-9)
  r <- morie_trimmed_mean_ci(X)
  expect_equal(r$estimate, 49.4285714286, tolerance = 1e-9)
  expect_equal(r$ci[1], 7.7958906093, tolerance = 1e-7)
  expect_equal(r$ci[2], 91.0612522479, tolerance = 1e-7)
  expect_equal(r$p_value, 0.0271530641, tolerance = 1e-8)
})

test_that("dependent-groups Yuen matches WRS yuend", {
  r <- morie_yuen_paired(GX, GY8)
  expect_equal(r$estimate, -3.3833333333, tolerance = 1e-9)
  expect_equal(r$se, 1.6194649322, tolerance = 1e-9)
  expect_equal(r$statistic, -2.0891673948, tolerance = 1e-8)
  expect_equal(r$df, 5)
  expect_equal(r$p_value, 0.0909960208, tolerance = 1e-9)
  expect_false(r$degenerate)
})

test_that("lockstep pairs are reported, not divided by zero", {
  a <- c(10, 12, 14, 16, 18, 20, 22, 24)
  r <- morie_yuen_paired(a, a + 3)
  expect_true(r$degenerate)
  expect_equal(r$se, 0)
  expect_true(is.nan(r$statistic))
})

test_that("rank-based two-group methods match WRS", {
  cd <- morie_cliff_delta(GX, GY)
  expect_equal(cd$delta, -0.5555555556, tolerance = 1e-9)
  expect_equal(cd$ci[1], -0.8511568140, tolerance = 1e-8)
  expect_equal(cd$ci[2], 0.0075732462, tolerance = 1e-8)
  bm <- morie_brunner_munzel(GX, GY)
  expect_equal(bm$statistic, 2.3959644869, tolerance = 1e-8)
  expect_equal(bm$df, 14.6853762121, tolerance = 1e-7)
  expect_equal(bm$p_value, 0.0303742085, tolerance = 1e-8)
})

test_that("Brunner-Munzel reports complete separation", {
  r <- morie_brunner_munzel(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10))
  expect_true(r$separated)
  expect_equal(r$p_hat, 1)
  expect_true(is.nan(r$p_value))
})

test_that("one-way designs match WRS t1way and bdm", {
  a <- morie_trimmed_mean_anova(list(GX, GY, G3))
  expect_equal(a$statistic, 17.8615076274, tolerance = 1e-8)
  expect_equal(a$df1, 2)
  expect_equal(a$df2, 10.6285218540, tolerance = 1e-8)
  expect_equal(a$p_value, 3.99066342e-04, tolerance = 1e-6)

  b <- morie_brunner_dette_munk(list(GX, GY, G3))
  expect_equal(b$statistic, 23.6976204295, tolerance = 1e-8)
  expect_equal(b$df1, 1.8972472385, tolerance = 1e-8)
  expect_equal(b$df2, 20.4586365284, tolerance = 1e-7)
  expect_equal(b$p_value, 5.98022771e-06, tolerance = 1e-6)
  expect_equal(b$q_hat, c(0.24, 0.4466666667, 0.82), tolerance = 1e-9)
})

test_that("the two one-way tests agree on a clear difference", {
  # different assumptions, same conclusion here
  expect_lt(morie_trimmed_mean_anova(list(GX, GY, G3))$p_value, 0.01)
  expect_lt(morie_brunner_dette_munk(list(GX, GY, G3))$p_value, 0.01)
})

test_that("t1way on two groups is Yuen's t squared", {
  f <- morie_trimmed_mean_anova(list(GX, GY))
  t <- morie_yuen_test(GX, GY)
  expect_equal(f$statistic, t$statistic^2, tolerance = 1e-8)
  expect_equal(f$p_value, t$p_value, tolerance = 1e-9)
})

test_that("boxplot rules match WRS outbox", {
  b <- morie_boxplot_outliers(X)
  expect_equal(b$lower, -70.8333333333, tolerance = 1e-9)
  expect_equal(b$upper, 175.8333333333, tolerance = 1e-9)
  cc <- morie_boxplot_outliers(X, carling = TRUE)
  expect_equal(cc$lower, -81.2600454890, tolerance = 1e-9)
  expect_equal(cc$upper, 171.2600454890, tolerance = 1e-9)
  # Carling is centred on the median
  expect_equal((cc$lower + cc$upper) / 2, stats::median(X),
               tolerance = 1e-9)
})

test_that("effect size, median error and Winsorized regression match WRS", {
  # R's integrate() value, which our adaptive quadrature matches; WRS
  # prints -0.836340302559 because MASS::area is looser by 9.9e-10
  expect_equal(morie_akp_effect_size(GX, GY)$effect_size,
               -0.836340301553, tolerance = 1e-9)
  expect_equal(morie_akp_effect_size(GX, GY, tr = 0)$cterm, 1)
  expect_equal(morie_median_se(X)$se, 23.2934689878, tolerance = 1e-9)
  expect_equal(morie_median_se(GX)$se, 1.2423183460, tolerance = 1e-9)
  w <- morie_winsorized_regression(XS, YS)
  expect_equal(w$intercept, -0.2720258730, tolerance = 1e-9)
  expect_equal(unname(w$slope[1]), 2.1066687702, tolerance = 1e-9)
  expect_true(w$converged)
})

test_that("median standard error flags ties", {
  expect_true(morie_median_se(c(1, 2, 2, 2, 3, 3, 4, 5, 5))$ties)
  expect_false(morie_median_se(X)$ties)
})

test_that("robust estimators resist an outlier that moves the mean", {
  base <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  wild <- c(base[-10], 10000)
  expect_equal(morie_trimmed_mean(base, 0.2), morie_trimmed_mean(wild, 0.2),
               tolerance = 1e-9)
  expect_gt(abs(mean(base) - mean(wild)), 900)
})
