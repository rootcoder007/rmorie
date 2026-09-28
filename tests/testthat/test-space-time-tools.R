.st_u <- .morie_random_uniform(400, seed = 101, stream = 0)
.st_P <- cbind(5 * .st_u[1:30], 5 * .st_u[31:60])
.st_A <- as.matrix(stats::dist(.st_P))
.st_B <- abs(outer(.st_u[61:90], .st_u[61:90], `-`)) + 0.3 * .st_A
.st_x <- sin((0:39) / 3) + .st_u[101:140]
.st_y <- sin(((0:39) - 2) / 3) + .st_u[151:190]

test_that("MantelTest, CrossCorrelation and Kde2d equal vegan, stats and MASS", {
  expect_equal(MantelTest(.st_A, .st_B, nsim = 0)$statistic, 0.8497560621438426, tolerance = 1e-13)
  expect_equal(MantelTest(.st_A, .st_B, "spearman", nsim = 0)$statistic, 0.8490692293898711,
      tolerance = 1e-13)
  expect_equal(CrossCorrelation(.st_x, .st_y, 5)$acf, c(0.30996448894970935, 0.5406055815151168,
      0.6876623568274371, 0.814468514007617, 0.8652994204476522, 0.7587733120073065, 0.589869687841489,
      0.3283135459682955, 0.022779862564475485, -0.2189276269344393, -0.40471547462860946),
      tolerance = 1e-13)
  kx <- .st_u[1:50] * 4
  ky <- .st_u[51:100] * 3 + .st_u[1:50]
  expect_equal(Kde2d(kx, ky, n = 6)$z[3, ], c(0.04034458422372524, 0.06447721495396387, 0.0785027406442221,
      0.055707295052120125, 0.043940821620001944, 0.014439222430394085), tolerance = 1e-13)
})

test_that("Knox, near-repeat, Rossmo and aoristic by hand", {
  k <- KnoxTest(rbind(c(0, 0), c(0.5, 0), c(5, 5), c(5.2, 5)), c(1, 2, 10, 11), 1, 2, nsim = 9)
  expect_equal(c(k$observed, k$expected), c(2, 2 / 3))
  t <- NearRepeatTable(rbind(c(0, 0), c(0.5, 0), c(5, 5), c(5.2, 5)), c(1, 2, 10, 11), c(1, 10), c(2, 20),
      nsim = 9)
  expect_equal(t$observed, rbind(c(2, 0), c(0, 4)))
  expect_equal(sum(t$expected), 6)
  a <- 2 / 2^1.2
  b <- 1 / 10^1.2 + 1 / 8^1.2
  expect_equal(GeographicProfile(rbind(c(0, 0), c(2, 0)), rbind(c(1, 0), c(5, 5)), B = 1.5), c(a,
      b) / (a + b))
  expect_equal(AoristicWeights(c(0.5, 2), c(2.5, 2), 0:3)$totals, c(0.25, 0.5, 1.25))
  expect_error(MantelTest(.st_A, .st_B, "kendall", nsim = 0), "method")
})
