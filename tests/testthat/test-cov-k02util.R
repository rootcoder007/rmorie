# Coverage tests for R/k02util.R: meta-analytic pooling, distribution
# helpers, golden-section search, Gauss-Hermite rules, modularity and
# BFS distances.

test_that("fixed- and random-effects pooling and the moment estimator", {
  y <- c(0.3, 0.8, 0.1, 0.55, -0.2)
  v <- c(0.04, 0.09, 0.05, 0.02, 0.1)
  w <- 1 / v
  mu <- sum(w * y) / sum(w)
  fe <- k02fe(y, v)
  expect_equal(fe$mu, mu, tolerance = 1e-12)
  expect_equal(fe$Q, sum(w * (y - mu)^2), tolerance = 1e-12)
  expect_equal(fe$var, 1 / sum(w), tolerance = 1e-12)
  dl <- k02dl(y, v)
  t2 <- max(0, (fe$Q - 4) / (sum(w) - sum(w^2) / sum(w)))
  expect_equal(dl$tau2, t2, tolerance = 1e-12)
  expect_equal(dl$mu, sum(y / (v + t2)) / sum(1 / (v + t2)), tolerance = 1e-12)
  expect_equal(k02dl(c(0.1, 0.1), c(0.1, 0.1))$tau2, 0)
  # with tau0 = 0 the generalised moment step is DerSimonian-Laird
  expect_equal(k02mm(y, v, 0), t2, tolerance = 1e-12)
  a <- 1 / (v + 0.05)
  yb <- sum(a * y) / sum(a)
  num <- sum(a * (y - yb)^2) - sum(a * v) + sum(a^2 * v) / sum(a)
  expect_equal(k02mm(y, v, 0.05), max(0, num / (sum(a) - sum(a^2) / sum(a))), tolerance = 1e-12)
})

test_that("distribution helpers", {
  expect_equal(k02z(0.975), qnorm(0.975))
  expect_equal(k02tq(0.9, 7), qt(0.9, 7))
  expect_equal(k02p2z(-1.3), 2 * pnorm(-1.3), tolerance = 1e-12)
  expect_equal(k02p2t(2.1, 12), 2 * pt(-2.1, 12), tolerance = 1e-12)
  expect_equal(k02pchi(5.2, 3), 1 - pchisq(5.2, 3), tolerance = 1e-12)
})

test_that("golden-section search brackets the minimum", {
  # near a quadratic minimum f differs by ~eps only within sqrt(eps) of
  # the minimiser, so a comparison-based search locates it to ~1e-8
  f <- function(x) (x - 1.3)^2 + 0.5
  expect_equal(k02gold(f, -4, 6), 1.3, tolerance = 1e-7)
  g <- function(x) cosh(x - 0.4)
  expect_equal(k02gold(g, -1, 3), 0.4, tolerance = 1e-7)
  # the bracket shrinks by 0.618 per step: 10 steps leave width 10 * 0.618^10
  expect_lt(abs(k02gold(f, -4, 6, iters = 10) - 1.3), 10 * 0.618034^10)
})

test_that("Gauss-Hermite rule equals the Golub-Welsch eigen-solution", {
  for (n in c(3, 6, 10)) {
    gh <- k02gh(n)
    J <- matrix(0, n, n)
    J[cbind(1:(n - 1), 2:n)] <- J[cbind(2:n, 1:(n - 1))] <- sqrt((1:(n - 1)) / 2)
    e <- eigen(J, symmetric = TRUE)
    expect_equal(sort(gh$x), sort(e$values), tolerance = 1e-12)
    w <- sqrt(pi) * e$vectors[1, ]^2
    expect_equal(gh$w[order(gh$x)], w[order(e$values)], tolerance = 1e-10)
    # exact for x^(2n - 2): the integral of x^2k exp(-x^2) is Gamma(k + 1/2)
    k <- n - 1
    expect_equal(sum(gh$w * gh$x^(2 * k)), gamma(k + 0.5), tolerance = 1e-10)
  }
})

test_that("modularity and all-pairs BFS distances", {
  A <- matrix(0, 6, 6)
  ed <- rbind(c(1, 2), c(2, 3), c(1, 3), c(4, 5), c(5, 6), c(4, 6), c(3, 4))
  A[ed] <- 1
  A[ed[, 2:1]] <- 1
  comm <- c(1, 1, 1, 2, 2, 2)
  k <- rowSums(A)
  m2 <- sum(A)
  B <- A - outer(k, k) / m2
  expect_equal(k02mod(A, comm), sum(B * outer(comm, comm, "==")) / m2, tolerance = 1e-12)
  expect_equal(k02mod(matrix(0, 3, 3), 1:3), 0)
  D <- k02bfs(A)
  Fw <- ifelse(A > 0, 1, Inf)
  diag(Fw) <- 0
  for (m in 1:6) Fw <- pmin(Fw, outer(Fw[, m], Fw[m, ], "+"))
  expect_equal(D, matrix(as.integer(Fw), 6))
  A2 <- A
  A2[3, 4] <- A2[4, 3] <- 0
  expect_equal(k02bfs(A2)[1, 6], -1L)
})
