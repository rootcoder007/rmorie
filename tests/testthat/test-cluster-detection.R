llr <- function(yin, ein, ty) yin * log(yin / ein) + (ty - yin) * log((ty - yin) / (ty - ein))

test_that("EllipticScan and FlexScan hand values", {
  P <- rbind(cbind(0:5, 0), cbind(0:5, 1))
  y <- c(9, 8, 9, 8, 0, 0, 0, 1, 0, 0, 1, 0)
  c1 <- EllipticScan(P, y, rep(10, 12), nsim = 0)$clusters[[1]]
  expect_equal(sort(c1$zone), 1:4)
  expect_equal(c(c1$shape, c1$angle), c(2, 180))
  expect_equal(c1$llr, llr(34, 4 * sum(y) / 12, sum(y)) * sqrt(8 / 9), tolerance = 1e-12)
  W <- 1 * (abs(outer(1:5, 1:5, `-`)) == 1)
  f <- FlexScan(cbind(0:4, 0), c(8, 7, 1, 1, 1), rep(10, 5), W, k = 3, nsim = 0)
  expect_equal(f$clusters[[1]]$zone, 1:2)
  expect_equal(f$clusters[[1]]$llr, llr(15, 7.2, 18), tolerance = 1e-12)
})

test_that("NormalScan, CuzickEdwards, LawsonWaller and FixedCircleScan", {
  x <- c(5, 6, 1, 2, 1.5, 0.5)
  r <- NormalScan(cbind(0:5, 0), x, nsim = 0)
  expect_equal(r$llr, 3 * log((mean(x^2) - mean(x)^2) / (1.75 / 6)), tolerance = 1e-12)
  ce <- CuzickEdwards(cbind(c(0, 1, 10, 11, 20, 21), 0), c(1, 1, 0, 0, 1, 0), k = 1, nsim = 0)
  expect_equal(c(ce$statistic, ce$expected), c(2, 1.2))
  lw <- LawsonWaller(c(4, 2, 1, 1), rep(2, 4), c(1, 0.5, 0.25, 0.25))
  expect_equal(c(lw$U, lw$variance), c(1.5, 0.75))
  g <- FixedCircleScan(rbind(c(0, 0), c(0.5, 0), c(5, 5), c(9, 1)), c(6, 5, 1, 0), rep(10, 4), 1, overlap = 0.5,
                       alpha = 1)
  for (cc in g$circles) expect_equal(cc$pvalue, stats::ppois(cc$observed - 1, cc$expected, lower.tail = FALSE))
  expect_error(FixedCircleScan(cbind(0, 0), 1, 1, 1, method = "bad"))
})
