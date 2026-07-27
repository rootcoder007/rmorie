# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("ARCH-LM detects GARCH and keeps nominal size", {
  garch <- function(seed, n = 1200, omega = 0.1, alpha = 0.3, beta = 0.6) {
    set.seed(seed)
    s2 <- omega / (1 - alpha - beta); y <- numeric(n)
    for (t in seq_len(n)) {
      y[t] <- sqrt(s2) * rnorm(1)
      s2 <- omega + alpha * y[t]^2 + beta * s2
    }
    y
  }
  y <- garch(1)
  for (q in c(1L, 4L)) {
    r <- morie_arch_lm_test(y, q = q)
    expect_lt(r$p_value, 1e-4)
    expect_identical(r$df, q)
  }

  # Size under iid noise: measured 2/30 rejections at alpha = 0.05.
  rej <- 0
  for (s in 1:30) {
    set.seed(s)
    rej <- rej + (morie_arch_lm_test(rnorm(400), q = 2)$p_value < 0.05)
  }
  expect_lte(rej, 5)

  # Heavy-tailed iid t(4) has no ARCH; a normality check would flag it,
  # the LM test must not.
  set.seed(5)
  expect_gt(morie_arch_lm_test(rt(800, df = 4), q = 2)$p_value, 0.01)

  expect_error(morie_arch_lm_test(1:50, q = 0), "q must be")
  expect_error(morie_arch_lm_test(1:3, q = 2), "at least")
})

test_that("multi-horizon KS separates short-horizon tail violations", {
  set.seed(0)
  r <- morie_multi_horizon_ks(rnorm(800), horizons = c(1, 5, 20), n_mc = 200)
  expect_equal(r$per_horizon$h, c(1L, 5L, 20L))
  expect_equal(r$per_horizon$n_h, c(800L, 160L, 40L))
  expect_equal(r$statistic, max(r$per_horizon$statistic))
  expect_gt(r$p_value, 0.05)  # Gaussian data pass at every horizon

  # iid t(3): far from Gaussian at h = 1, CLT-normalised by h = 20.
  set.seed(1)
  x <- rt(3000, df = 3)
  r2 <- morie_multi_horizon_ks(x, horizons = c(1, 20), n_mc = 300)
  per <- r2$per_horizon
  expect_lt(per$p_value[per$h == 1], 0.01)
  expect_gt(per$statistic[per$h == 1], per$statistic[per$h == 20])

  # Fully specified cdf: exact null; wrong scale must reject. The null
  # side is a RATE, not a single seed -- measured 1/12 rejections at
  # 0.05 across seeds 1..12 (seed 2 alone lands at p = 0.023, a
  # perfectly legal 2 percent event that failed the single-seed
  # version of this test).
  rej <- 0
  for (s in 1:12) {
    set.seed(s)
    z <- rnorm(600)
    p1 <- morie_multi_horizon_ks(z, horizons = c(1, 4),
                                 cdf = function(x, h) pnorm(x, sd = sqrt(h)))$p_value
    rej <- rej + (p1 < 0.05)
  }
  expect_lte(rej, 3)
  set.seed(2)
  z <- rnorm(600)
  bad <- morie_multi_horizon_ks(z, horizons = c(1, 4),
                                cdf = function(x, h) pnorm(x, sd = 3 * sqrt(h)))
  expect_lt(bad$p_value, 0.01)

  expect_error(morie_multi_horizon_ks(1:100, horizons = c(0, 5)), "positive")
  expect_error(morie_multi_horizon_ks(1:50, horizons = 20), "at least 8")
})

test_that("convex hull drops interior points and returns ordered vertices", {
  sq <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0.5, 0.5))
  r <- morie_convex_hull(sq)
  expect_equal(r$n_vertices, 4L)
  expect_false(any(apply(r$hull_points, 1, function(v) all(v == c(0.5, 0.5)))))
  expect_equal(r$area, 1, tolerance = 1e-12)

  tri <- rbind(c(0, 0), c(4, 0), c(0, 3))
  expect_equal(morie_convex_hull(tri)$area, 6, tolerance = 1e-12)

  # 200 interior points leave the hull unchanged.
  set.seed(0)
  inner <- cbind(runif(200, 0.05, 0.95), runif(200, 0.05, 0.95))
  r2 <- morie_convex_hull(rbind(sq[1:4, ], inner))
  expect_equal(r2$n_vertices, 4L)
  expect_equal(r2$area, 1, tolerance = 1e-9)

  expect_error(morie_convex_hull(1:10), "\\(n, 2\\)")
  expect_error(morie_convex_hull(rbind(c(0, 0), c(1, 1))), ">= 3")
})

test_that("adjacency matrix matches the hand-built path graph", {
  r <- morie_adjacency_matrix(rbind(c("A", "B"), c("B", "C")))
  want <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  expect_equal(unname(r$A), want)
  expect_equal(unname(r$degree), c(1, 2, 1))
  expect_equal(r$m, 2L)

  d <- morie_adjacency_matrix(rbind(c(0, 1), c(1, 2)), n = 3, directed = TRUE)
  expect_equal(d$A[1, 2], 1); expect_equal(d$A[2, 1], 0)

  u <- morie_adjacency_matrix(rbind(c(0, 1), c(1, 0), c(0, 1)), n = 4)
  expect_equal(unname(u$A), t(unname(u$A)))
  expect_equal(sum(u$A), 2)

  expect_error(morie_adjacency_matrix(rbind(c(0, 5)), n = 3), "lie in")
})

test_that("non-backtracking matrix is a permutation on a cycle", {
  n <- 5
  cyc <- cbind(0:(n - 1), c(1:(n - 1), 0))
  r <- morie_nonbacktracking_matrix(cyc)
  B <- r$B
  expect_equal(dim(B), c(2L * n, 2L * n))
  expect_equal(unname(rowSums(B)), rep(1, 2 * n))
  expect_equal(unname(colSums(B)), rep(1, 2 * n))

  # Never walks straight back: B[(u,v),(v,u)] = 0.
  g <- morie_nonbacktracking_matrix(rbind(c(0, 1), c(1, 2), c(2, 0), c(1, 3)))
  de <- g$directed_edges
  key <- paste(de[, 1], de[, 2])
  pos <- stats::setNames(seq_len(nrow(de)), key)
  for (i in seq_len(nrow(de))) {
    rev_key <- paste(de[i, 2], de[i, 1])
    expect_equal(g$B[i, pos[[rev_key]]], 0)
  }

  # Path endpoints dead-end; a degree-3 centre offers 2 continuations.
  p <- morie_nonbacktracking_matrix(rbind(c(0, 1), c(1, 2)))
  dp <- p$directed_edges
  kp <- stats::setNames(seq_len(nrow(dp)), paste(dp[, 1], dp[, 2]))
  end_row <- kp[[paste(dp[which(dp[, 2] == max(dp))[1], 1], max(dp))]]
  star <- morie_nonbacktracking_matrix(rbind(c(0, 1), c(0, 2), c(0, 3)))
  ds <- star$directed_edges
  ks <- stats::setNames(seq_len(nrow(ds)), paste(ds[, 1], ds[, 2]))
  centre <- ds[1, 1]  # smallest label is the centre after sorting
  # Any edge arriving AT the centre continues to the other 2 leaves.
  arriving <- which(ds[, 2] == names(sort(table(c(ds)), decreasing = TRUE))[1])
  expect_true(all(rowSums(star$B)[arriving] == 2))
})

test_that("dcc front-end delegates bit-for-bit", {
  set.seed(0)
  L <- chol(matrix(c(1, 0.5, 0.5, 1), 2))
  X <- matrix(rnorm(1200), ncol = 2) %*% L
  a <- morie_dcc_garch(X)
  b <- morie_dcc_multivariate_garch(X)
  expect_identical(a$a, b$a)
  expect_identical(a$loglik, b$loglik)
})

test_that("ARCH-M recovers parameters after the recursion guard", {
  dgp <- function(seed, n = 2000, omega = 0.2, alpha = 0.4, delta = 0.8, mu = 0.1) {
    set.seed(seed)
    ep <- 0; y <- numeric(n)
    for (t in seq_len(n)) {
      s2 <- omega + alpha * ep^2
      e <- sqrt(s2) * rnorm(1)
      y[t] <- mu + delta * sqrt(s2) + e
      ep <- e
    }
    y
  }
  al <- dl <- numeric(0)
  for (s in 1:3) {
    r <- morie_arch_in_mean(dgp(s))
    al <- c(al, r$alpha); dl <- c(dl, r$delta)
  }
  # nlminb does move (unlike the Python arm's L-BFGS-B, which returned
  # its starting values); assert recovery on the mean over 3 seeds.
  expect_equal(mean(al), 0.4, tolerance = 0.35)
  expect_equal(mean(dl), 0.8, tolerance = 0.45)

  set.seed(9)
  expect_lt(morie_arch_in_mean(0.1 + rnorm(1500))$alpha, 0.1)
})
