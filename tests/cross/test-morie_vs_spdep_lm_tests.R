skip_if_not_installed("spdep")

set.seed(5)
n <- 40
xy <- cbind(stats::runif(n), stats::runif(n))
lw <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(xy, k = 5)), style = "W")
W <- spdep::listw2mat(lw)
X <- cbind(stats::rnorm(n), stats::runif(n))
y <- as.vector(solve(diag(n) - 0.4 * W, X %*% c(1, -2) + stats::rnorm(n)))
f <- stats::lm(y ~ X)

test_that("lm* match spdep::lm.RStests", {
  a <- spdep::lm.RStests(f, lw, test = "all")
  d <- lmdiag(y, X, W)
  for (nm in c("RSerr", "RSlag", "adjRSerr", "adjRSlag", "SARMA")) {
    expect_equal(d[[nm]], unname(a[[nm]]$statistic), tolerance = 1e-10)
    expect_equal(d[[paste0("p_", nm)]], unname(a[[nm]]$p.value), tolerance = 1e-9)
  }
  expect_equal(lmerr(y, X, W)$statistic, unname(a$RSerr$statistic), tolerance = 1e-10)
  expect_equal(lmrlag(y, X, W)$statistic, unname(a$adjRSlag$statistic), tolerance = 1e-10)
})

test_that("lmslx and lmsdm match spdep::SD.RStests", {
  b <- spdep::SD.RStests(f, lw, test = "all")
  expect_equal(lmslx(y, X, W)$statistic, unname(b$SDM_RSWX$statistic), tolerance = 1e-10)
  expect_equal(lmsdm(y, X, W)$statistic, unname(b$SDM_Joint$statistic), tolerance = 1e-10)
  expect_equal(lmsdm(y, X, W)$adjRSWX, unname(b$SDM_adjRSWX$statistic), tolerance = 1e-10)
  expect_equal(lmsdm(y, X, W)$p_value, unname(b$SDM_Joint$p.value), tolerance = 1e-9)
})
