test_that("gktau equals DescTools::GoodmanKruskalTau", {
  r <- gktau(matrix(c(10, 30, 5, 0, 20, 30, 5, 0, 0), 3, byrow = TRUE))
  expect_equal(c(r$tau_col_given_row, r$tau_row_given_col), c(0.23599632690541772, 0.28003494975972054),
               tolerance = 1e-13)
})

test_that("normtl matches qt with ncp and Howe's formula (Table 6.21)", {
  expect_equal(normtl(10, 0.95, 0.95, "one")$k, 2.9109634130810349, tolerance = 1e-12)
  expect_equal(normtl(10, 0.95, 0.95, "two")$k, 3.3819134905090209, tolerance = 1e-13)
  expect_equal(normtl(30, 0.90, 0.99, "two")$k, 2.3847381969510941, tolerance = 1e-13)
})

test_that("corrho reproduces cor.test and the book's Fisher and Samiuddin examples", {
  r <- corrho(0.84011830527061726, 10)
  expect_equal(c(r$statistic, r$p_value), c(4.3808985556112692, 0.0023459622144385321), tolerance = 1e-12)
  expect_equal(corrho(0.966, 14, 0.8)$statistic, 3.0847449560475293, tolerance = 1e-13)
  expect_equal(corrho(0.966, 14, 0.8, method = "samiuddin")$statistic, 3.7069458788857004, tolerance = 1e-13)
})

test_that("corcmp pools and compares correlations (eqs 7.395-7.409)", {
  r <- corcmp(c(0.6, 0.7, 0.8), c(28, 33, 23))
  expect_equal(c(r$chi2, r$r_pooled, r$ci), c(1.8273479552539205, 0.70184762492712993,
                                               0.56803393224890819, 0.799508931302501), tolerance = 1e-13)
  r <- corcmp(c(0.6, 0.8), c(28, 23))
  expect_equal(c(r$z_two, r$p_two), c(1.3515503603605483, 0.17651919922959225), tolerance = 1e-12)
  r <- corcmp(c(0.422, 0.388, 0.569), c(30, 30, 30))
  expect_equal(c(r$r_gem, r$t_gem), c(0.45966666666666667, 4.7999256665164065), tolerance = 1e-13)
})

test_that("corwil equals psych::r.test", {
  r <- corwil(0.85, 0.71, 0.80, 30)
  expect_equal(c(r$statistic, r$p_value), c(2.168701837454599, 0.039082529371308745), tolerance = 1e-12)
})

test_that("corss follows eq (7.405)", {
  expect_equal(corss(0.6, power = 0.9)$n_exact, 24.869824430385489, tolerance = 1e-13)
  expect_equal(corss(0.2, power = 0.9)$n, 259)
  expect_equal(corss(0.6, n = 25)$power, 0.90168014141326802, tolerance = 1e-13)
})

test_that("rpmcor equals the book's between-case r and rmcorr::rmcorr", {
  x <- c(47, 46, 50, 52, 46, 36, 47, 46, 36, 44, 49, 50, 42, 48, 60, 47, 51, 57, 49, 49,
         51, 46, 46, 45, 52, 54, 48, 47, 47, 54, 63, 70, 63, 58, 59, 61, 67, 64, 59, 61)
  y <- c(51, 53, 57, 54, 55, 53, 54, 57, 61, 57, 52, 56, 46, 52, 53, 49, 52, 50, 50, 49,
         46, 48, 47, 55, 49, 61, 53, 48, 50, 44, 64, 62, 66, 64, 62, 62, 58, 62, 67, 59)
  r <- rpmcor(x, y, rep(1:4, each = 10))
  expect_equal(r$r_between, 0.77200223038047033, tolerance = 1e-13)
  expect_equal(c(r$r_within, r$p_within), c(-0.021182045737978591, 0.90096938110056957), tolerance = 1e-10)
  expect_equal(r$df_within, 35)
})
