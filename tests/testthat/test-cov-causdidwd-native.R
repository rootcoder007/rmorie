# Coverage tests for R/causdidwd_native.R (Wooldridge 2025): TWFE by
# dummies and by the two-way Mundlak regression, ETWFE and imputation
# against lm, and the simple, cohort and event-time aggregations.

cw_panel <- function() {
  u <- rep(1:6, each = 4)
  t <- rep(c(8, 9, 10, 11), 6)
  g <- rep(c(10, 10, 9, 9, NA, NA), each = 4)
  x <- sin(1:24) + 0.1 * u
  tau <- ifelse(!is.na(g) & t >= g, 1 + 0.5 * (t - g) + 0.3 * (g == 9), 0)
  y <- u * 0.4 + (t - 8) * 0.7 + tau + 0.8 * x + 0.05 * cos(3 * (1:24))
  list(y = y, u = u, t = t, g = g, x = x)
}

test_that("TWFE by dummies equals lm, and two-way Mundlak reproduces it", {
  d <- cw_panel()
  tw <- morie_two_way_fixed_effects(d$y, d$u, d$t, d$x)
  ref <- coef(lm(d$y ~ d$x + factor(d$u) + factor(d$t)))[2]
  expect_equal(tw$coef, unname(ref), tolerance = 1e-9)
  expect_equal(c(tw$n_units, tw$n_periods, tw$n_columns), c(6L, 4L, 1L + 1L + 5L + 3L))
  mu <- morie_two_way_mundlak(d$y, d$u, d$t, d$x)
  expect_equal(mu$coef, tw$coef, tolerance = 1e-9)
  expect_equal(mu$n_columns, 4L)
  expect_error(morie_two_way_fixed_effects(d$y, d$u, d$t, d$x[-1]), "X has 23 rows")
  expect_error(morie_two_way_fixed_effects(d$y, rep(1, 24), d$t, d$x), "at least 2 units")
  expect_error(morie_two_way_mundlak(d$y[-1], d$u, d$t, d$x), "must agree in length")
  expect_error(morie_two_way_mundlak(1:3, 1:3, 1:3, 1:3), "at least 4 observations")
})

test_that("ETWFE cells are the saturated regression; imputation agrees", {
  d <- cw_panel()
  e <- morie_etwfe(d$y, d$u, d$t, as.list(d$g))
  # numeric periods order as numbers, so 10 and 11 come after 8 and 9
  expect_equal(e$periods, c("8", "9", "10", "11"))
  cells <- c("10\r10", "10\r11", "9\r10", "9\r11", "9\r9")
  expect_equal(names(e$att), cells)
  D <- vapply(cells, function(k) {
    p <- strsplit(k, "\r")[[1]]
    as.numeric(!is.na(d$g) & d$g == as.numeric(p[1]) & d$t == as.numeric(p[2]))
  }, numeric(24))
  ref <- coef(lm(d$y ~ D + factor(d$u) + factor(d$t)))[2:6]
  expect_equal(unname(e$att), unname(ref), tolerance = 1e-9)
  expect_equal(e$estimate, mean(ref), tolerance = 1e-9)
  expect_equal(e$cohorts, c("10", "9"))
  im <- morie_imputation(d$y, d$u, d$t, as.list(d$g))
  un <- is.na(d$g) | d$t < d$g
  fit <- lm(y ~ factor(u) + factor(t), data = data.frame(y = d$y, u = d$u, t = d$t)[un, ])
  res <- d$y[!un] - predict(fit, data.frame(u = d$u, t = d$t)[!un, ])
  key <- paste(d$g[!un], d$t[!un], sep = "\r")
  expect_equal(im$att[cells], tapply(res, key, mean)[cells], tolerance = 1e-9, ignore_attr = TRUE)
  expect_equal(unname(im$att[cells]), unname(e$att), tolerance = 1e-9)
  expect_equal(im$n_untreated_used, sum(un))
  ex <- morie_etwfe(d$y, d$u, d$t, as.list(d$g), X = d$x)
  Dx <- coef(lm(d$y ~ D + d$x + factor(d$u) + factor(d$t)))[2:6]
  expect_equal(unname(ex$att), unname(Dx), tolerance = 1e-9)
  ix <- morie_imputation(d$y, d$u, d$t, as.list(d$g), X = d$x)
  expect_equal(ix$n_cells, 5L)
  expect_identical(morie_etwfedid, morie_etwfe)
  expect_identical(morie_causdidwd, morie_etwfe)
  expect_identical(morie_causal_did_wooldridge_eta, morie_etwfe)
  expect_error(morie_etwfe(d$y, d$u, d$t, as.list(d$g)[-1]), "23 adoption periods")
  expect_error(morie_etwfe(d$y, d$u, d$t, as.list(rep(12, 24))), "not a period in the data")
  expect_error(morie_etwfe(d$y, d$u, d$t, as.list(rep(NA, 24))), "no unit is ever treated")
  expect_error(morie_etwfe(d$y, d$u, d$t, as.list(d$g), X = d$x[1:3]), "X has 3 rows")
  expect_error(morie_imputation(d$y, d$u, d$t, as.list(rep(8, 24))), "too few untreated")
})

test_that("aggregation: simple, cohort and event-time profiles", {
  d <- cw_panel()
  e <- morie_etwfe(d$y, d$u, d$t, as.list(d$g))
  a <- e$att
  expect_equal(morie_aggregate(e)$estimate, mean(a))
  w <- setNames(c(1, 2, 1, 1, 3), names(a))
  expect_equal(morie_aggregate(e, weights = w)$estimate, sum(a * w) / 8)
  co <- morie_aggregate(e, "cohort")
  expect_equal(co$profile, c(`10` = mean(a[1:2]), `9` = mean(a[3:5])))
  ev <- morie_aggregate(e, "event")
  expect_equal(ev$profile, c(`0` = mean(a[c("10\r10", "9\r9")]), `1` = mean(a[c("10\r11", "9\r10")]), `2` = unname(a["9\r11"])))
  expect_equal(ev$estimate, mean(ev$profile))
  im <- morie_imputation(d$y, d$u, d$t, as.list(d$g))
  expect_equal(morie_aggregate(im, "event")$profile, ev$profile, tolerance = 1e-9)
  expect_equal(morie_aggregate(list(att = im$att), "event")$profile, ev$profile, tolerance = 1e-9)
  expect_match(.causdidwd_morie_cheatsheet(), "MUNDLAK")
  expect_error(morie_aggregate(e, "calendar"), "simple, event or cohort")
  expect_error(morie_aggregate(list(att = numeric(0))), "nothing to aggregate")
  expect_error(morie_aggregate(e, weights = w * 0), "sum to zero")
})
