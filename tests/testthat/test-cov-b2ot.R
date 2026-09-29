# Coverage for the optimal-transport shelf (Cuturi 2013; Peyre & Cuturi
# 2019). The Sinkhorn plan is checked through the two conditions that
# characterise the entropic optimum uniquely -- the marginals and the
# Gibbs form T = diag(u) exp(-C/eps) diag(v) -- and against the exact
# transport LP (linprog) as eps shrinks; the costs, entropies, maps and
# potentials are recomputed from their definitions.

.a <- c(0.2, 0.5, 0.3)
.b <- c(0.1, 0.4, 0.25, 0.25)
.C <- outer(c(0, 1, 2.5), c(0.3, 1.1, 2, 2.8), function(x, y) (x - y)^2)

test_that("entropies of a plan follow eqs (4.1)-(4.2)", {
  T <- matrix(c(0.1, 0, 0.3, 0.2, 0.15, 0.25), 2)
  pos <- T[T > 0]
  e <- Otnegent(T)
  expect_equal(e$estimate, -sum(pos * (log(pos) - 1)), tolerance = 1e-12)
  expect_equal(e$shannon, -sum(pos * log(pos)), tolerance = 1e-12)
  expect_equal(Otentreg(T, 0.3)$estimate, 0.3 * e$estimate, tolerance = 1e-12)
  expect_error(Otnegent(-T), "non-negative")
  expect_error(Otentreg(T, 0), "epsilon must be positive")
})

test_that("the Sinkhorn plan has the target marginals and the Gibbs form", {
  eps <- 0.4
  s <- Otsinkh(.a, .b, .C, eps, max_iter = 2000)
  T <- s$T
  expect_equal(rowSums(T), .a, tolerance = 1e-12)
  expect_equal(colSums(T), .b, tolerance = 1e-12)
  expect_equal(T, outer(s$u, s$v) * exp(-.C / eps), tolerance = 1e-12)
  expect_equal(s$estimate, sum(T * .C), tolerance = 1e-12)
  expect_lt(s$marginal_error, 1e-12)
  sd <- Sinkdist(.a, .b, .C, eps, max_iter = 2000)
  pos <- T[T > 0]
  expect_equal(sd$estimate, sum(T * .C), tolerance = 1e-12)
  expect_equal(sd$objective, sum(T * .C) + eps * sum(pos * (log(pos) - 1)), tolerance = 1e-12)
  expect_equal(sd$lambda_, 1 / eps)
  # unnormalised marginals are closed to probability vectors first
  expect_equal(Otsinkh(2 * .a, 5 * .b, .C, eps, 2000)$T, T, tolerance = 1e-12)
  expect_error(Otsinkh(.a, .b[-1], .C, eps), "do not match the shape")
  expect_error(Sinkdist(.a, .b, .C, 0), "eps must be positive")
})

test_that("small-eps Sinkhorn cost approaches the exact 1-D transport cost", {
  # sorted 1-D supports with a convex cost: the optimal plan is the
  # monotone (north-west corner) coupling
  nw <- function(a, b) {
    T <- matrix(0, length(a), length(b))
    i <- 1
    j <- 1
    while (i <= length(a) && j <= length(b)) {
      q <- min(a[i], b[j])
      T[i, j] <- q
      a[i] <- a[i] - q
      b[j] <- b[j] - q
      if (a[i] <= 1e-15) i <- i + 1 else j <- j + 1
    }
    T
  }
  ot <- sum(nw(.a, .b) * .C)
  eps <- 0.02
  s <- Otsinkh(.a, .b, .C, eps, max_iter = 20000)
  expect_lt(s$marginal_error, 1e-9)
  # <P_eps, C> - OT <= eps (H(P_eps) - H(P*)) <= eps (log(nm) + 1)
  expect_gte(s$estimate, ot - 1e-9)
  expect_lte(s$estimate - ot, eps * (log(12) + 1))
})

test_that("iterations-to-tolerance, marginal violation and soft assignment", {
  it <- Otsinkit(.a, .b, .C, 0.4, tol = 1e-6, max_iter = 100)
  errs <- vapply(1:100, function(k) Otsinkh(.a, .b, .C, 0.4, k)$marginal_error, 1)
  expect_equal(it$trace, errs, tolerance = 1e-12)
  expect_identical(it$estimate, as.numeric(which(errs < 1e-6)[1]))
  expect_true(it$reached)
  nr <- Otsinkit(.a, .b, .C, 0.4, tol = 1e-30, max_iter = 5)
  expect_false(nr$reached)
  expect_identical(nr$estimate, 5)
  expect_error(Otsinkit(.a, .b, .C, 0.4, tol = 0), "tol must be positive")
  T <- matrix(c(0.1, 0.05, 0.2, 0.15, 0.3, 0.2), 2)
  v <- Otsinktol(T, c(0.5, 0.5), c(0.2, 0.3, 0.5))
  expect_equal(c(v$row_error, v$col_error), c(max(abs(rowSums(T) - 0.5)), max(abs(colSums(T) - c(0.2, 0.3, 0.5)))), tolerance = 1e-12)
  expect_error(Otsinktol(T, c(0.5, 0.5), c(0.5, 0.5)), "do not match")
  sa <- Otsoftas(.a, .b, .C, 0.4, 500)
  P <- sa$T / rowSums(sa$T)
  expect_equal(sa$estimate, P, tolerance = 1e-12)
  expect_equal(rowSums(sa$estimate), rep(1, 3), tolerance = 1e-12)
  expect_identical(sa$hard, apply(P, 1, which.max) - 1L)
  expect_equal(sa$entropy_mean, mean(apply(P, 1, function(p) -sum(p[p > 0] * log(p[p > 0])))), tolerance = 1e-12)
})

test_that("ground costs, barycentric map, potentials, free energy and push-forward", {
  X <- rbind(c(0, 0), c(1, 2), c(-1, 0.5))
  Y <- rbind(c(1, 1), c(0, -1))
  D <- as.matrix(stats::dist(rbind(X, Y)))[1:3, 4:5]
  expect_equal(Otcostsq(X, Y)$estimate, D^2, ignore_attr = TRUE, tolerance = 1e-12)
  expect_equal(Otcostlp(X, Y, 2)$estimate, D, ignore_attr = TRUE, tolerance = 1e-12)
  expect_equal(Otcostlp(X, Y, 1)$estimate, as.matrix(stats::dist(rbind(X, Y), "manhattan"))[1:3, 4:5], ignore_attr = TRUE, tolerance = 1e-12)
  expect_equal(Otcostlp(X, Y, Inf)$estimate, as.matrix(stats::dist(rbind(X, Y), "maximum"))[1:3, 4:5], ignore_attr = TRUE, tolerance = 1e-12)
  expect_equal(Otcostlp(X, Y, 3)$estimate, as.matrix(stats::dist(rbind(X, Y), "minkowski", p = 3))[1:3, 4:5], ignore_attr = TRUE, tolerance = 1e-12)
  expect_error(Otcostlp(X, Y, 0.5), "p must be at least 1")
  expect_error(Otcostsq(X, Y[, 1, drop = FALSE]), "same dimension")
  T <- matrix(c(0.2, 0, 0.1, 0.1, 0.3, 0.3), 3)
  bm <- Otbarmap(T, Y)
  W <- T / rowSums(T)
  expect_equal(bm$estimate, W %*% Y, tolerance = 1e-12)
  sp <- sum(vapply(1:3, function(i) sum(W[i, ] * rowSums(sweep(Y, 2, (W %*% Y)[i, ])^2)), 1)) / 3
  expect_equal(bm$displacement, sp, tolerance = 1e-12)
  expect_error(Otbarmap(T, X), "one row per column")
  s <- Otsinkh(.a, .b, .C, 0.4, 2000)
  lp <- Otlogpot(s$u, s$v, 0.4)
  expect_equal(lp$estimate, 0.4 * log(s$u), tolerance = 1e-12)
  expect_equal(lp$g, 0.4 * log(s$v), tolerance = 1e-12)
  expect_error(Otlogpot(c(1, 0), 1, 1), "strictly positive")
  fe <- Otfreeen(s$T, .C, .a, .b, lp$estimate, lp$g, 0.4)
  pos <- s$T[s$T > 0]
  primal <- sum(s$T * .C) + 0.4 * sum(pos * (log(pos) - 1))
  expect_equal(fe$estimate, primal - sum(.a * lp$estimate) - sum(.b * lp$g), tolerance = 1e-12)
  # at the Sinkhorn fixed point <T, C> = <T, f + g> - eps sum T log T, so
  # the gap reduces to minus eps times the plan's mass
  expect_equal(fe$estimate, -0.4 * sum(s$T), tolerance = 1e-9)
  expect_error(Otfreeen(s$T, .C[, 1:3], .a, .b, lp$estimate, lp$g, 0.4), "same shape")
  pf <- Otpushfw(c(0.2, 0.5, 0.3), c(2, -0.5, 1), c(0, 1, 2))
  expect_equal(pf$estimate, c(0.1, 1, 0.3))
  expect_error(Otpushfw(1:2, c(1, 0), 1:2), "singular")
})
