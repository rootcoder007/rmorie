# Lakner (1976) stock and flow measures (ported with bricklayer's tests).
#
# The anchors here are Lakner's own worked examples from A Manual of
# Statistical Sampling Methods for Corrections Planners (1976). They are
# published numbers computed by someone else, so they can disagree with
# this code -- which is the property a test needs and which a test
# comparing these functions to each other would not have.

test_that("the average daily population is Lakner's worked example", {
  # p.15: 3,000 inmates served 13,500 detention days in a year.
  expect_equal(mrm_adp(13500), 13500 / 365)
  expect_equal(round(mrm_adp(13500), 8), 36.98630137)
  expect_equal(round(mrm_adp(13500)), 37)
  # summing per-person days gives the same answer as the total
  expect_equal(mrm_adp(c(5000, 8500)), mrm_adp(13500))
})

test_that("the average length of stay is Lakner's worked example", {
  # p.17: 2,700 inmates admitted and released served 12,150 days.
  expect_equal(mrm_alos(12150, 2700), 4.5)
  # the manual's own text rounds 12,150/2,700 to 4.5 days
  expect_equal(round(mrm_alos(12150, 2700), 1), 4.5)
})

test_that("admissions invert the identity, as on p.20", {
  # p.20: t = 365, average daily population 25, 1,750 admissions imply a
  # stay of 5.2 days.
  implied <- 25 * 365 / 1750
  expect_equal(round(implied, 7), 5.2142857)
  expect_equal(round(implied, 1), 5.2)
  # and inverting gets the admissions back
  expect_equal(mrm_admissions(25, implied), 1750)
})

test_that("person-days come out of periodic headcounts, as on p.21", {
  # p.21: counts on 255 days summing to 34,935 over a 365-day year.
  counts <- rep(34935 / 255, 255)
  expect_equal(mrm_adp_from_counts(counts), 50005)
  expect_equal(round(mrm_adp_from_counts(counts)), 50005)
  # the counts need not be equal, only their mean matters
  set.seed(1)
  jittered <- 34935 / 255 + c(-10, 10, rep(0, 253))
  expect_equal(mrm_adp_from_counts(jittered), 50005)
})

test_that("the identity adp = n * alos / t holds", {
  # This is algebra rather than data, so it is asserted once and then
  # relied on: days = people x stay, so dividing by t either way agrees.
  days <- 126121; people <- 9608
  expect_equal(mrm_adp(days), mrm_admissions(mrm_adp(days), mrm_alos(days, people)) *
                          mrm_alos(days, people) / 365)
  expect_equal(mrm_admissions(mrm_adp(days), mrm_alos(days, people)), people)
})

test_that("stock and flow can carry opposite signs", {
  # The reason these functions exist. Fewer people, held longer: the
  # flow rate falls while the stock rate rises, and a report quoting
  # either alone states the wrong direction for the other.
  sf <- mrm_stock_flow(days = c(115674, 126121), people = c(12647, 9608),
                   period = c("2023", "2025"),
                   exposure = c(15495050, 16256538))
  expect_s3_class(sf, "mrm_stock_flow")
  expect_identical(nrow(sf), 2L)
  # people down, stay up, days up
  expect_lt(sf$people_change[2], 0)
  expect_gt(sf$alos_change[2], 0)
  expect_gt(sf$days_change[2], 0)
  # and the two rates disagree on the sign
  expect_lt(sf$flow_rate_change[2], 0)
  expect_gt(sf$stock_rate_change[2], 0)
  # the decomposition is exact: days = people x stay
  p <- sf$people_change[2] / 100
  l <- sf$alos_change[2] / 100
  expect_equal((1 + p) * (1 + l) - 1, sf$days_change[2] / 100)
  # adp is days per day; the stock rate is adp against the exposure
  expect_equal(sf$adp[2], 126121 / 365)
  expect_equal(sf$stock_rate[2], 1e5 * (126121 / 365) / 16256538)
  expect_equal(sf$flow_rate[2], 1e5 * 9608 / 16256538)
})

test_that("stock_flow works without an exposure", {
  sf <- mrm_stock_flow(days = c(100, 200), people = c(10, 10))
  expect_false("stock_rate" %in% names(sf))
  expect_equal(sf$alos, c(10, 20))
  expect_equal(sf$adp, c(100, 200) / 365)
  expect_equal(sf$days_change[2], 100)
})

test_that("the arguments are validated", {
  expect_error(mrm_adp(13500, t = 0), "positive")
  expect_error(mrm_adp(13500, t = -1), "positive")
  expect_error(mrm_adp(numeric(0)), "must not be empty")
  expect_error(mrm_adp(c(1, NA)), "must not contain NA")
  expect_error(mrm_adp(-1), "non-negative")
  expect_error(mrm_alos(100, 0), "positive")
  expect_error(mrm_admissions(25, 0), "positive")
  expect_error(mrm_stock_flow(days = c(1, 2), people = 1), "same length")
  expect_error(mrm_stock_flow(days = c(1, 2), people = c(1, 2),
                          exposure = c(1, 2, 3)),
               "length 1 or the same length")
})

test_that("a single exposure is recycled across periods", {
  sf <- mrm_stock_flow(days = c(100, 200), people = c(10, 20),
                   exposure = 1000, per = 1000)
  expect_equal(sf$exposure, c(1000, 1000))
  expect_equal(sf$flow_rate, c(10, 20))
})

test_that("period_days counts both ends, and leap years look after themselves", {
  # A period from the 1st to the 31st is thirty-one days of exposure.
  expect_equal(mrm_period_days("2024-01-01", "2024-01-31"), 31)
  expect_equal(mrm_period_days("2024-01-01", "2024-01-01"), 1)
  # nobody has to know which years are leap years
  expect_equal(mrm_period_days("2024-01-01", "2024-12-31"), 366)
  expect_equal(mrm_period_days("2023-01-01", "2023-12-31"), 365)
  expect_equal(mrm_period_days("2025-04-01", "2026-03-31"), 365)
  # and it feeds straight into mrm_adp()
  expect_equal(mrm_adp(13500, mrm_period_days("2023-01-01", "2023-12-31")),
               mrm_adp(13500, 365))
  expect_error(mrm_period_days("2024-12-31", "2024-01-01"), "must not precede")
  expect_error(mrm_period_days(c("2024-01-01", "2024-01-02"), "2024-12-31"),
               "a single date")
})

test_that("stay_summary describes the distribution, not just its mean", {
  # Most stays short, a few long: the mean sits well above the median,
  # which is the reason both are reported.
  stays <- c(rep(1, 40), rep(3, 30), rep(10, 20), 60, 90, 120)
  ss <- mrm_stay_summary(stays)
  expect_identical(ss$n, length(stays))
  expect_equal(ss$total_days, sum(stays))
  expect_equal(ss$mean, mean(stays))
  expect_equal(ss$median, stats::median(stays))
  expect_equal(ss$max, 120)
  expect_gt(ss$mean, ss$median)
  # the mean agrees with mrm_alos() on the same data
  expect_equal(ss$mean, mrm_alos(sum(stays), length(stays)))
  # the interval is the ordinary t interval on a mean
  se <- stats::sd(stays) / sqrt(length(stays))
  tq <- stats::qt(0.975, df = length(stays) - 1L)
  expect_equal(ss$se, se)
  expect_equal(ss$lower, mean(stays) - tq * se)
  expect_equal(ss$upper, mean(stays) + tq * se)
  # a wider level gives a wider interval
  w <- mrm_stay_summary(stays, conf_level = 0.99)
  expect_lt(w$lower, ss$lower)
  expect_gt(w$upper, ss$upper)
})

test_that("stay_summary copes with a single person and refuses nonsense", {
  one <- mrm_stay_summary(7)
  expect_identical(one$n, 1L)
  expect_equal(one$mean, 7)
  # no spread and no interval from one observation, said with NA rather
  # than with a fabricated zero
  expect_true(is.na(one$sd))
  expect_true(is.na(one$lower))
  expect_true(is.na(one$upper))
  expect_error(mrm_stay_summary(c(1, -1)), "non-negative")
  expect_error(mrm_stay_summary(numeric(0)), "must not be empty")
  expect_error(mrm_stay_summary(c(1, 2), conf_level = 1), "strictly inside")
})

test_that("stock_flow measures against the previous period when asked", {
  sf1 <- mrm_stock_flow(days = c(100, 200, 400), people = c(10, 10, 10),
                    period = c("a", "b", "c"))
  sf2 <- mrm_stock_flow(days = c(100, 200, 400), people = c(10, 10, 10),
                    period = c("a", "b", "c"), baseline = "previous")
  # against the first: b is +100%, c is +300%
  expect_equal(sf1$days_change, c(0, 100, 300))
  # against the previous: each doubling is +100%, and the first has none
  expect_true(is.na(sf2$days_change[1]))
  expect_equal(sf2$days_change[-1], c(100, 100))
  expect_identical(attr(sf2, "stock_flow")$baseline, "previous")
  expect_error(mrm_stock_flow(days = 1:2, people = c(1, 1), baseline = "middle"),
               "should be one of")
})

test_that("the measures hold together on a period that is not a year", {
  # A thirty-day month, which is the case a hardcoded 365 would break.
  t30 <- mrm_period_days("2025-04-01", "2025-04-30")
  expect_equal(t30, 30)
  sf <- mrm_stock_flow(days = c(2400, 2750), people = c(300, 250),
                   period = c("Apr", "May"), t = t30)
  expect_equal(sf$adp, c(2400, 2750) / 30)
  expect_equal(sf$alos, c(8, 11))
  # people down, stay up, days up: the decomposition still multiplies out
  p <- sf$people_change[2] / 100; l <- sf$alos_change[2] / 100
  expect_equal((1 + p) * (1 + l) - 1, sf$days_change[2] / 100)
})
