test_that("published court statistics read through Little's law", {
  b <- morie_backlog_published(21265, 30, 148904, 175, band_shares = rbind(c(0.808, 0.105, 0.034, 0.053)),
                               band_floors = c(0, 304, 456, 548))
  lam <- 21265 / 30
  expect_equal(b$filing_rate, lam)
  expect_equal(b$implied_mean_days, 148904 / lam)
  expect_equal(b$ratio, (148904 / lam) / 175)
  expect_equal(b$little_backlog, lam * 175)
  expect_equal(b$band_floor_bound, 0.105 * 304 + 0.034 * 456 + 0.053 * 548)
  expect_true("Research.P16Censoring.band_floor_le_mean" %in% attr(b, "theorems"))
  # the bound holds on any docket with those bands: cases at their floors attain it
  floors <- c(0, 304, 456, 548)
  set.seed(4)
  band <- sample(1:4, 400, replace = TRUE, prob = c(0.808, 0.105, 0.034, 0.053))
  dur <- floors[band] + c(304, 152, 92, 600)[band] * runif(400)
  sh <- tabulate(band, 4) / 400
  bb <- morie_backlog_published(1, 1, 0, mean(dur), band_shares = rbind(sh), band_floors = floors)
  expect_lte(bb$band_floor_bound, mean(dur))
  expect_equal(bb$band_floor_bound, mean(floors[band]))
  # Little's law on a stationary docket: a constant inflow with a fixed duration
  w <- morie_backlog_published(received = rep(300, 3), days = 30, pending = 10 * 45, mean_days = 45)
  expect_equal(w$ratio, rep(1, 3))
  expect_null(w$band_floor_bound)
})

test_that("published-statistics inputs that cannot be read are refused", {
  expect_error(morie_backlog_published(NA, 30, 1, 1), "finite")
  expect_error(morie_backlog_published(0, 30, 1, 1), "positive")
  expect_error(morie_backlog_published(1, 30, -1, 1), "non-negative")
  expect_error(morie_backlog_published(1:3, 1:2, 1, 1), "length 1 or 3")
  expect_error(morie_backlog_published(1, 1, 1, 1, band_shares = rbind(c(0.5, 0.5), c(0.5, 0.5))), "one row per window")
  expect_error(morie_backlog_published(1, 1, 1, 1, band_shares = rbind(c(0.5, 0.5))), "one lower edge")
  expect_error(morie_backlog_published(1, 1, 1, 1, band_shares = rbind(c(-0.5, 1.5)), band_floors = 0:1), "non-negative")
  expect_error(morie_backlog_published(1, 1, 1, 1, band_shares = rbind(c(0.2, 0.2)), band_floors = 0:1), "sum to 1")
  expect_error(morie_backlog_published(1, 1, 1, 1, band_shares = rbind(c(0.5, 0.5)), band_floors = c(5, 1)), "increasing")
})
