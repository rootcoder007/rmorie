test_that("corrci matches cor.test and the Hotelling correction", {
  r <- corrci(0.687, 50)
  expect_equal(c(r$lower, r$upper), c(0.50527306379043613, 0.81038243291053280), tolerance = 1e-13)
  x <- 1:10
  y <- c(2, 1, 4, 3, 7, 5, 6, 9, 10, 8)
  r <- corrci(cor(x, y), 10)
  expect_equal(c(r$lower, r$upper), cor.test(x, y)$conf.int[1:2], tolerance = 1e-13)
  h <- corrci(0.687, 50, method = "hotelling")
  expect_equal(c(h$z, h$lower, h$upper),
               c(0.82618316681286408, 0.49765798223771052, 0.80270722653182658), tolerance = 1e-13)
})

test_that("corcin, corrng and chin1 reproduce the book", {
  expect_equal(corcin(0.5, 0.8)$n_exact, 53.924556983524468, tolerance = 1e-13)
  expect_equal(corcin(0.5, 0.8)$n, 54)
  expect_equal(unlist(corrng(0.6, 0.9)), c(lower = 0.19128808451674617, upper = 0.88871191548325390),
               tolerance = 1e-13)
  r <- chin1(matrix(c(1, 5, 5, 1), 2, byrow = TRUE))
  expect_equal(c(r$statistic, r$p_value), c(44 / 9, 0.027030076547772494), tolerance = 1e-13)
  expect_equal(chin1(matrix(c(1, 5, 4, 2), 2, byrow = TRUE))$statistic, 2.828571428571428736,
               tolerance = 1e-13)
})

test_that("ctresid equals chisq.test residuals and stdres", {
  tb <- matrix(c(14, 22, 32, 18, 16, 8, 8, 2, 0), 3, byrow = TRUE)
  cs <- suppressWarnings(chisq.test(tb))
  r <- ctresid(tb)
  expect_equal(r$pearson, unclass(cs$residuals), tolerance = 1e-13, ignore_attr = TRUE)
  expect_equal(r$adjusted, unclass(cs$stdres), tolerance = 1e-13, ignore_attr = TRUE)
  expect_equal(r$statistic, 21.576470588235292, tolerance = 1e-13)
})

test_that("rrexct equals poisson.test", {
  r <- rrexct(40, 20, 22, 30)
  pt <- poisson.test(c(40, 22), c(20, 30))
  expect_equal(c(r$lower, r$upper, r$ratio), c(pt$conf.int, pt$estimate), tolerance = 1e-13,
               ignore_attr = TRUE)
  expect_equal(r$p_value, pt$p.value, tolerance = 1e-12)
  r <- rrexct(3, 7.5, 11, 12, conf_level = 0.9)
  pt <- poisson.test(c(3, 11), c(7.5, 12), conf.level = 0.9)
  expect_equal(c(r$lower, r$upper, r$p_value), c(pt$conf.int, pt$p.value), tolerance = 1e-12,
               ignore_attr = TRUE)
})

test_that("gmconj, invbpr, bnappx and bvncnd", {
  r <- gmconj(c(0.8, 1.9, 0.4, 2.7, 1.1), 2, 1)
  expect_equal(c(r$shape, r$rate, r$lower, r$upper),
               c(7, 7.9, 0.35624848753416022, 1.65309797753401067), tolerance = 1e-13)
  r <- gmconj(c(3, 0, 2, 5), 2, 1, "poisson")
  expect_equal(c(r$shape, r$rate, r$lower, r$upper),
               c(12, 5, 1.2401150217444432, 3.9364077026603912), tolerance = 1e-13)
  r <- invbpr(20, 100)
  expect_equal(unlist(r), c(estimate = 19 / 119, lower = 0.10487850197666279, upper = 0.23805309050431389),
               tolerance = 1e-13)
  r <- bnappx(3, 10, 0.4)
  expect_equal(c(r$z, r$approx, r$exact), c(-0.5 / sqrt(2.4), 0.37344281669518198, pbinom(3, 10, 0.4)),
               tolerance = 1e-13)
  expect_equal(bnappx(3, 10, 0.4, FALSE)$z, -1 / sqrt(2.4), tolerance = 1e-13)
  expect_equal(unlist(bvncnd(80, 170, 70, 10, 12, 0.6)), c(mean = 175, sd = 8), tolerance = 1e-13)
  expect_equal(unlist(bvncnd(160, 170, 70, 10, 12, 0.6, "x")), c(mean = 62.8, sd = 9.6), tolerance = 1e-13)
})
