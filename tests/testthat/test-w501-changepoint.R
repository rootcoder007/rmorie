# Anchored tests for the w5_01 changepoint family:
# Pelt, Chgseg, Binseg, EDivisive, KernelCusum.

x1 <- c(0.1, -0.2, 0.05, 0.3, -0.1, 5.2, 4.9, 5.1, 5.3, 4.8,
        1.9, 2.1, 2.0, 1.8, 2.2)

test_that("Pelt matches the changepoint-package anchor and eq-3 arithmetic", {
  r <- Pelt(x1, "mean", 3.0)
  # Anchor: changepoint::cpt.mean(method='PELT', penalty='Manual',
  # pen.value=3, test.stat='Normal') -> cpts {5, 10} (2026-08-09,
  # R 4.6.1); cpt.meanvar pen 6 -> {5, 10}.
  expect_identical(r$changepoints, c(5L, 10L))
  rmv <- Pelt(x1, "meanvar", 6.0)
  expect_identical(rmv$changepoints, c(5L, 10L))
  ss <- function(v) sum((v - mean(v))^2)
  tot <- ss(x1[1:5]) + ss(x1[6:10]) + ss(x1[11:15])
  expect_lt(abs(r$objective - (tot + 2 * 3.0)), 1e-12)
  skip_if_not_installed("changepoint")
  m <- changepoint::cpt.mean(x1, method = "PELT", penalty = "Manual",
                             pen.value = 3.0, test.stat = "Normal")
  expect_identical(r$changepoints, as.integer(changepoint::cpts(m)))
})

test_that("Binseg matches the BinSeg anchor and single-split arithmetic", {
  r <- Binseg(x1, 2)
  expect_identical(r$changepoints, c(5L, 10L))
  ss <- function(v) sum((v - mean(v))^2)
  gains <- vapply(1:14, function(t) {
    ss(x1) - ss(x1[1:t]) - ss(x1[(t + 1):15])
  }, numeric(1))
  expect_identical(r$order[1], which.max(gains))
  expect_lt(abs(r$improvements[1] - max(gains)), 1e-12)
  skip_if_not_installed("changepoint")
  b <- changepoint::cpt.mean(x1, method = "BinSeg", Q = 2,
                             penalty = "Manual", pen.value = 0)
  expect_identical(r$changepoints, as.integer(sort(changepoint::cpts(b))))
})

test_that("Chgseg is the mean-cost PELT", {
  a <- Chgseg(x1, 3.0)
  b <- Pelt(x1, "mean", 3.0)
  expect_identical(a$changepoints, b$changepoints)
  expect_identical(a$objective, b$objective)
})

test_that("EDivisive Qhat matches an independent double-loop route", {
  qdirect <- function(x, a, tau, kappa, alpha = 1) {
    X <- x[(a + 1):tau]
    Y <- x[(tau + 1):kappa]
    n <- length(X)
    m <- length(Y)
    between <- sum(outer(X, Y, function(u, v) abs(u - v)^alpha))
    dX <- abs(outer(X, X, "-"))^alpha
    dY <- abs(outer(Y, Y, "-"))^alpha
    e <- 2 * between / (n * m) -
      sum(dX[upper.tri(dX)]) / (n * (n - 1) / 2) -
      sum(dY[upper.tri(dY)]) / (m * (m - 1) / 2)
    (n * m / (n + m)) * e
  }
  D <- .w501_pairwise_alpha(matrix(x1, ncol = 1), 1)
  P <- .w501_prefix2d(D)
  for (abk in list(c(0, 5, 10), c(0, 5, 15), c(5, 10, 15), c(2, 6, 14))) {
    expect_lt(abs(.w501_qhat(P, abk[1], abk[2], abk[3]) -
                    qdirect(x1, abk[1], abk[2], abk[3])), 1e-12)
  }
})

test_that("EDivisive locates the two obvious changes (ecp anchor)", {
  # Anchor: ecp::e.divisive(matrix(x1), sig.lvl=0.05, R=199,
  # min.size=2) -> estimates {1, 6, 11, 16}, i.e. tau = {5, 10}
  # (2026-08-09, R 4.6.1).
  r <- EDivisive(x1, sig = 0.05, R = 99L, min_size = 2L, seed = 1)
  expect_identical(sort(r$changepoints), c(5L, 10L))
  expect_true(all(r$p_values[1:2] <= 0.05))
})

test_that("KernelCusum linear kernel equals the closed form", {
  r <- KernelCusum(x1, kernel = "linear", gamma = 0.1,
                   kmin = 2L, kmax = 13L)
  n <- 15
  k <- 5
  g <- 0.1
  mu1 <- mean(x1[1:k])
  mu2 <- mean(x1[(k + 1):n])
  v1 <- mean((x1[1:k] - mu1)^2)
  v2 <- mean((x1[(k + 1):n] - mu2)^2)
  sw <- (k * v1 + (n - k) * v2) / n
  kfdr <- (k * (n - k) / n) * (mu2 - mu1)^2 / (sw + g)
  d1 <- sw / (sw + g)
  expect_identical(r$estimate, 5L)
  expect_lt(abs(r$kfdr - kfdr), 1e-10)
  expect_lt(abs(r$d1 - d1), 1e-10)
  expect_lt(abs(r$d2 - d1 * d1), 1e-10)
  expect_lt(abs(r$statistic - (kfdr - d1) / sqrt(2 * d1 * d1)), 1e-9)
})

test_that("KernelCusum gaussian kernel finds the big change", {
  r <- KernelCusum(x1, kernel = "gaussian", gamma = 0.1)
  expect_identical(r$estimate, 5L)
  expect_gt(r$statistic, 5)
})
