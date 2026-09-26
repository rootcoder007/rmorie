## Reference values: survRM2 rmst1 / rmst2 and survival::survdiff on the
## deterministic data below (the same data as morie tests/test_survival_reference.py).
.surv_data <- function() {
  i <- 0:149
  x1 <- sin(1.3 * i)
  x2 <- as.numeric(cos(0.9 * i) > 0)
  g <- i %% 2
  rate <- exp(0.4 * x1 - 0.5 * x2 + 0.3 * g)
  tt <- round(-log(0.02 + 0.96 * abs(sin(2.3 * i + 0.4))) / rate * 5, 1)
  cc <- round(2 + 8 * abs(cos(1.9 * i)), 1)
  data.frame(t = pmax(pmin(tt, cc), 0.1), e = as.numeric(tt <= cc), g = g, x1 = x1)
}

test_that("morie_rmst matches survRM2 rmst1", {
  d <- .surv_data()
  r <- morie_rmst(d$t, d$e, tau = 8)
  expect_equal(r$rmst, 2.9384051107858156, tolerance = 1e-13)
  expect_equal(r$se, 0.2285745202098439, tolerance = 1e-12)
})

test_that("morie_rmst_difference matches survRM2 rmst2", {
  d <- .surv_data()
  r <- morie_rmst_difference(d$t, d$e, d$g, tau = 8)
  expect_equal(r$rmst_diff, -0.539046955656763, tolerance = 1e-12)
  expect_equal(r$p_value, 0.235697249072946, tolerance = 1e-12)
})

test_that("morie_weighted_logrank matches survdiff (rho = 0, 1) and the weighted formula", {
  d <- .surv_data()
  expect_equal(morie_weighted_logrank(d$t, d$e, d$g, "logrank")$statistic, 1.326822153, tolerance = 1e-9)
  expect_equal(morie_weighted_logrank(d$t, d$e, d$g, "peto")$statistic, 1.064620461630706, tolerance = 1e-12)
  expect_equal(morie_weighted_logrank(d$t, d$e, d$g, "gehan")$statistic, 1.036767145, tolerance = 1e-9)
  expect_equal(morie_weighted_logrank(d$t, d$e, d$g, "tarone")$statistic, 1.208864877, tolerance = 1e-9)
})

test_that("morie_concordance_index matches survival::concordance (tie rules)", {
  d <- .surv_data()
  expect_equal(morie_concordance_index(d$t, d$e, d$x1)$c_index, 0.56794871794871793, tolerance = 1e-13)
})
