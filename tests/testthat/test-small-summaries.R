test_that("posterior summaries", {
  ch <- cbind(c(1, 2, 4, 0.5), c(5, 3, 4, 6.5))
  expect_equal(Bamse(ch)$ses, c(sd(ch[, 1]), sd(ch[, 2])))
  expect_equal(Bypvl(c(0.2, 1.5, 0.9, 2.2, 1.1, 1), 1)$value, 4 / 6)
  expect_equal(Nocse(c(0.1, 0.25), 0.1)$ci_half_widths, qnorm(0.95) * c(0.1, 0.25))
})

test_that("health economics", {
  expect_equal(Cdyll(c(2, 1, 3, 5), c(30.5, 12, 4, 1.5))$estimate, 2 * 30.5 + 12 + 12 + 7.5)
  q <- Hecea(c(1, -2, 3, 0.5, -1, 2), c(0.1, 0.2, -0.3, 0.4, -0.5, 0))$value
  expect_equal(unlist(q), c(NE = 200 / 6, NW = 200 / 6, SE = 100 / 6, SW = 100 / 6))
})

test_that("signal basics", {
  x <- c(1, 2, 6, -3, 0.5)
  expect_equal(Dcsub(x)$filtered, x - mean(x))
  expect_equal(Sactv(x)$value, var(x) * 4 / 5, tolerance = 1e-14)
  expect_equal(Prdur(c(100, 900, 1700), c(260, 1070, 1850), 500)$pr_intervals, c(0.32, 0.34, 0.30), tolerance = 1e-15)
})

test_that("marker data and sentencing", {
  G <- rbind(c(0, 1, 2, 1), c(1, 1, 2, 0), c(2, 0, 1, 1), c(1, 1, 0, 1), c(0, 2, 1, 1))
  h <- Hetlc(G)
  p <- colMeans(G) / 2
  expect_equal(h$H_exp, 2 * p * (1 - p))
  expect_equal(h$H_obs, colMeans(G == 1))
  expect_equal(h$het_freq_individual, rowMeans(G == 1))
  expect_equal(Mmaxn(c(2, 4, 3, 10))$x_norm, (c(2, 4, 3, 10) - 2) / 8)
  s <- Sntmn(data.frame(offense = c("a", "a", "b", "b", "b"), sentence_days = c(60, 90, 30, 40, 100),
                        mandatory_min_days = c(60, 60, 30, 60, 60)))
  expect_equal(c(s$value, s$pct_below_minimum, s$mean_above_minimum), c(0.4, 0.2, 35))
})
