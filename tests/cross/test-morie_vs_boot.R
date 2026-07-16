# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: native bootstrap (module 28) vs boot + simpleboot.
# Under a common seed the native resample stream matches boot's exactly,
# so replicate matrices and every CI type agree to machine precision.

test_that("native ordinary bootstrap matches boot::boot replicate-wise", {
  skip_if_not_installed("boot")
  stat_i <- function(d, i) mean(d[i])
  x <- as.numeric(datasets::mtcars$mpg)
  set.seed(101); mb <- morie_boot(x, stat_i, R = 500)
  set.seed(101); bb <- boot::boot(x, stat_i, R = 500)
  expect_equal(mb$t0, bb$t0, tolerance = 1e-12)
  expect_equal(unname(mb$t[, 1]), unname(bb$t[, 1]), tolerance = 1e-12)
})

test_that("native boot.ci norm/basic/perc/bca match boot::boot.ci", {
  skip_if_not_installed("boot")
  stat_i <- function(d, i) mean(d[i])
  x <- as.numeric(datasets::mtcars$mpg)
  set.seed(202); mb <- morie_boot(x, stat_i, R = 2000)
  set.seed(202); bb <- boot::boot(x, stat_i, R = 2000)
  bc <- boot::boot.ci(bb, type = c("norm", "basic", "perc", "bca"))
  mc <- morie_boot_ci(mb, type = c("norm", "basic", "perc", "bca"))
  # boot stores the two endpoints in the last two columns of each row.
  expect_equal(mc$norm,  as.numeric(bc$normal[1, 2:3]),     tolerance = 1e-8)
  expect_equal(mc$basic, as.numeric(bc$basic[1, 4:5]),      tolerance = 1e-8)
  expect_equal(mc$perc,  as.numeric(bc$percent[1, 4:5]),    tolerance = 1e-8)
  expect_equal(mc$bca,   as.numeric(bc$bca[1, 4:5]),        tolerance = 1e-8)
})

test_that("native BCa influence (empinf.reg) matches boot on a nonlinear stat", {
  skip_if_not_installed("boot")
  stat_i <- function(d, i) {
    v <- d[i]; median(v) / (stats::IQR(v) + 1)
  }
  set.seed(7); x <- rgamma(60, 2, 1)
  set.seed(303); mb <- morie_boot(x, stat_i, R = 1500)
  set.seed(303); bb <- boot::boot(x, stat_i, R = 1500)
  mc <- morie_boot_ci(mb, type = "bca")$bca
  bc <- as.numeric(boot::boot.ci(bb, type = "bca")$bca[1, 4:5])
  expect_equal(mc, bc, tolerance = 1e-8)
})

test_that("native tsboot (moving + stationary) matches boot::tsboot", {
  skip_if_not_installed("boot")
  set.seed(9); y <- as.numeric(arima.sim(list(ar = 0.5), 120))
  sfun <- function(s) mean(s)
  for (sm in c("fixed", "geom")) {
    set.seed(404); mb <- morie_tsboot(y, sfun, R = 400, l = 8, sim = sm)
    set.seed(404); bb <- boot::tsboot(y, sfun, R = 400, l = 8, sim = sm)
    expect_equal(mb$t0, bb$t0, tolerance = 1e-12, info = sm)
    expect_equal(unname(mb$t[, 1]), unname(bb$t[, 1]),
                 tolerance = 1e-10, info = sm)
  }
})

test_that("native two.boot matches simpleboot::two.boot", {
  skip_if_not_installed("simpleboot")
  set.seed(11); a <- rnorm(40, 5); b <- rnorm(35, 4)
  set.seed(505); mb <- morie_two_boot(a, b, statistic = mean, R = 800)
  set.seed(505); sb <- simpleboot::two.boot(a, b, FUN = mean, R = 800)
  expect_equal(mb$t0, as.numeric(sb$t0), tolerance = 1e-12)
  expect_equal(unname(mb$t[, 1]), unname(sb$t[, 1]), tolerance = 1e-10)
})
