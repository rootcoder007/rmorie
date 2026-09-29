# Coverage for svdd_native .. systmp exports. Every expectation is
# recomputed in the test body.

test_that("morie_svdd solves the SVDD dual to its KKT conditions", {
  X <- cbind(c(0.1, 0.9, 0.4, 1.2, 0.6, 3.0, 0.3), c(0.2, 0.5, 1.1, 0.8, 0.1, 2.8, 0.7))
  for (kern in c("rbf", "linear", "poly")) {
    r <- morie_svdd(X, C = 0.4, kernel = kern, gamma = 0.7)
    K <- switch(kern, rbf = exp(-0.7 * unname(as.matrix(stats::dist(X)))^2), linear = X %*% t(X),
                poly = (0.7 * X %*% t(X) + 1)^3)
    a <- r$alpha
    expect_equal(sum(a), 1, tolerance = 1e-10)
    expect_true(all(a >= -1e-12 & a <= 0.4 + 1e-12))
    g <- 2 * as.numeric(K %*% a) - diag(K)
    free <- a > 1e-6 & a < 0.4 - 1e-6
    lam <- mean(g[free])
    # the dual solver stops at tol = 1e-10 on its own KKT gap
    expect_equal(g[free], rep(lam, sum(free)), tolerance = 1e-6)
    expect_true(all(g[a <= 1e-6] >= lam - 1e-6))
    expect_true(all(g[a >= 0.4 - 1e-6] <= lam + 1e-6))
    d2 <- diag(K) - 2 * as.numeric(K %*% a) + sum(a * (K %*% a))
    expect_equal(r$distance2, d2, tolerance = 1e-10)
    bnd <- r$boundary_ + 1
    R2 <- if (length(bnd)) mean(d2[bnd]) else max(d2[r$support_ + 1])
    expect_equal(r$R2, max(0, R2), tolerance = 1e-12)
    expect_equal(r$predict(X), d2 - r$R2 <= 0)
  }
  lin <- morie_svdd(X, nu = 0.5, kernel = "linear")
  expect_equal(lin$C, 1 / 3.5)
  expect_equal(lin$center, colSums(lin$alpha * X), tolerance = 1e-12)
  z <- rbind(c(0.5, 0.5), c(5, 5))
  expect_equal(lin$decision(z), rowSums(sweep(z, 2, lin$center)^2) - lin$R2, tolerance = 1e-10)
  expect_error(lin$decision(matrix(1, 1, 3)), "test data has 3 columns")
  expect_error(morie_svdd(X, C = 1, nu = 0.5), "not both")
  expect_error(morie_svdd(X, nu = 1.5), "nu must lie")
  expect_error(morie_svdd(X, C = 0.1), "infeasible")
  expect_error(morie_svdd(X, kernel = "sigmoid"), "kernel must be")
})

test_that("Svmwolfe evaluates the SVM Wolfe dual", {
  X <- cbind(c(1, 2, -1, -2), c(0.5, 1, -0.5, -1.5))
  y <- c(1, 1, -1, -1)
  a <- c(0.2, 0.1, 0.25, 0.05)
  r <- Svmwolfe(a, X, y)
  G <- X %*% t(X)
  L <- sum(a) - 0.5 * sum(outer(a * y, a * y) * G)
  expect_equal(r$dual, L, tolerance = 1e-12)
  expect_equal(r$quadratic_term, sum(a) - L, tolerance = 1e-12)
  expect_equal(r$constraint_sum, sum(a * y), tolerance = 1e-12)
  expect_equal(Svmwolfe(a, X, y, K = diag(4))$dual, sum(a) - 0.5 * sum(a^2), tolerance = 1e-12)
  expect_error(Svmwolfe(a[-1], X, y), "same length")
})

test_that("morie_svycox matches weighted Breslow Cox with a design sandwich", {
  skip_if_not_installed("survival")
  tm <- c(2, 5, 3, 8, 1, 7, 4, 6, 5, 3, 9, 2)
  ev <- c(1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1)
  x <- cbind(c(0.5, -0.2, 1.1, 0.3, -0.8, 0.9, 0.0, -0.5, 1.4, 0.2, -1.0, 0.7),
             c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0))
  w <- c(1.5, 2, 1, 3, 1.2, 2.5, 1, 1.8, 2.2, 1, 1.4, 2)
  st <- rep(c("a", "b"), each = 6)
  cl <- rep(1:6, each = 2)
  r <- morie_svycox(tm, ev, x, weights = w, strata = st, cluster = cl)
  f <- survival::coxph(survival::Surv(tm, ev) ~ x, weights = w, ties = "breslow",
                       control = survival::coxph.control(eps = 1e-11, iter.max = 100))
  # tied times (2, 3, 5): the risk set is every subject with T >= t
  expect_equal(unname(r$coefficients), unname(stats::coef(f)), tolerance = 1e-8)
  expect_equal(r$model_std_errors, unname(sqrt(diag(f$naive.var %||% stats::vcov(f)))), tolerance = 1e-8)
  sr <- stats::residuals(f, type = "score")
  tot <- rowsum(w * sr, paste(st, cl))
  sh <- sub(" .*", "", rownames(tot))
  Vd <- matrix(0, 2, 2)
  for (h in unique(sh)) {
    th <- tot[sh == h, , drop = FALSE]
    dv <- sweep(th, 2, colMeans(th))
    Vd <- Vd + nrow(th) / (nrow(th) - 1) * crossprod(dv)
  }
  Ii <- solve(r$information)
  V <- Ii %*% unname(Vd) %*% Ii
  expect_equal(r$vcov, V, tolerance = 1e-8)
  expect_equal(r$design_effect, diag(V) / r$model_std_errors^2, tolerance = 1e-8)
  expect_equal(r$z, unname(r$coefficients) / sqrt(diag(V)), tolerance = 1e-8)
  expect_error(morie_svycox(tm, ev, x, strata = rep(c("a", "b"), c(1, 11))), "single cluster")
  expect_error(morie_svycox(tm, 0 * ev, x), "no events")
  expect_error(morie_svycox(tm, ev, x, weights = -w), "must be positive")
  expect_error(morie_svycox(tm, ev + 1, x), "must be 0 or 1")
})

test_that("Svyqtl, Svymed and Svytbl", {
  y <- c(3.2, 1.5, 4.8, 2.2, 5.1, 3.9, 0.7, 2.9)
  w <- c(1, 2, 1.5, 1, 0.5, 2, 1, 1.5)
  q <- Svyqtl(y, w, 0.3)
  o <- order(y)
  cw <- cumsum(w[o]) / sum(w)
  inv <- function(p) y[o][which(cw >= p)[1]]
  est <- inv(0.3)
  Fq <- sum(w[y <= est]) / sum(w)
  se <- sqrt(sum(w^2 * ((y <= est) - Fq)^2)) / sum(w)
  expect_equal(q$estimate, est)
  expect_equal(q$F, Fq, tolerance = 1e-12)
  expect_equal(q$se, se, tolerance = 1e-12)
  expect_equal(q$lower, inv(0.3 - stats::qnorm(0.975) * se))
  expect_equal(q$upper, inv(0.3 + stats::qnorm(0.975) * se))
  expect_equal(q$neff, sum(w)^2 / sum(w^2), tolerance = 1e-12)
  m <- Svymed(y, w)
  expect_equal(m$estimate, inv(0.5))
  expect_equal(m$mean, sum(w * y) / sum(w), tolerance = 1e-12)
  expect_equal(Svymed(y)$estimate, sort(y)[4])
  expect_error(Svyqtl(y, w, 1), "quantile must lie")
  expect_error(Svyqtl(y, w[-1]), "differ in length")
  expect_error(Svyqtl(numeric(0)), "y is empty")
  a <- c("u", "v", "u", "v", "u", "u", "v", "v")
  b <- c(1, 1, 2, 2, 2, 1, 1, 2)
  t <- Svytbl(a, b, w)
  P <- as.matrix(stats::xtabs(w ~ a + b)) / sum(w)
  E <- outer(rowSums(P), colSums(P))
  st <- sum((P - E)^2 / E)
  ne <- sum(w)^2 / sum(w^2)
  expect_equal(t$estimate, ne * st, tolerance = 1e-12)
  expect_equal(t$statistic_naive, 8 * st, tolerance = 1e-12)
  expect_equal(t$p_value, stats::pchisq(ne * st, 1, lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(t$cols, c("1", "2"))
  expect_error(Svytbl(rep("u", 8), b), "at least two rows")
})

test_that("Swiglu and Swinmw", {
  x <- c(0.5, -1, 2)
  W <- rbind(c(1, 0.2), c(-0.5, 0.3), c(0.1, -0.4))
  V <- rbind(c(0.3, 1), c(0.2, -0.1), c(-0.6, 0.5))
  W2 <- rbind(c(1, -1, 0.5), c(0.2, 0.3, 0.4))
  r <- Swiglu(x, W = W, V = V, b = c(0.1, -0.2), c = c(0.05, 0), beta = 1.5, W2 = W2)
  g <- as.numeric(t(W) %*% x) + c(0.1, -0.2)
  gate <- g * stats::plogis(1.5 * g)
  out <- gate * (as.numeric(t(V) %*% x) + c(0.05, 0))
  expect_equal(r$gate, gate, tolerance = 1e-12)
  expect_equal(r$out, out, tolerance = 1e-12)
  expect_equal(r$ffn, as.numeric(t(W2) %*% out), tolerance = 1e-12)
  A <- array(c(0.1, 0.5, -0.2, 0.3, 0.8, -0.4, 0.2, 0.0, 0.6, 0.1, -0.3, 0.7,
               0.4, -0.1, 0.9, 0.2), dim = c(2, 4, 2))
  tab <- matrix(c(0.1, -0.2, 0.3, 0.0, 0.5, -0.1, 0.2, 0.4, -0.3), 3)
  s <- Swinmw(A, 2, relative_bias = tab)
  expect_equal(s$n_windows, 2L)
  B <- matrix(0, 4, 4)
  for (p in 1:4) for (q in 1:4) {
    B[p, q] <- tab[(p - 1) %/% 2 - (q - 1) %/% 2 + 2, (p - 1) %% 2 - (q - 1) %% 2 + 2]
  }
  expect_equal(s$bias, B)
  for (w0 in c(0, 2)) {
    Xw <- t(vapply(1:4, function(p) A[(p - 1) %/% 2 + 1, w0 + (p - 1) %% 2 + 1, ], numeric(2)))
    S <- Xw %*% t(Xw) / sqrt(2) + B
    P <- exp(S) / rowSums(exp(S))
    O <- P %*% Xw
    for (p in 1:4) {
      expect_equal(s$output[(p - 1) %/% 2 + 1, w0 + (p - 1) %% 2 + 1, ], O[p, ], tolerance = 1e-12)
    }
  }
  expect_error(Swinmw(A, 3), "must divide")
  expect_error(Swinmw(matrix(1, 2, 2), 1), "dim c\\(H, W, d\\)")
  expect_error(Swinmw(A, 2, relative_bias = diag(2)), "relative_bias must be")
})

test_that("Sysrs and Sysamp give the systematic-sampling design", {
  y <- c(3, 7, 2, 9, 4, 6, 8, 1, 5, 10, 2, 6)
  r <- Sysrs(y, 3, seed = 17)
  u <- (48271 * 17) %% 2147483647 / 2147483647
  st <- floor(u * 3) + 1
  expect_equal(r$start, st)
  expect_equal(r$sample, y[seq(st, 12, by = 3)])
  mk <- vapply(1:3, function(i) mean(y[seq(i, 12, by = 3)]), 0)
  expect_equal(r$design_se, sqrt(mean((mk - mean(y))^2)), tolerance = 1e-12)
  s <- Sysamp(y, 3)
  V <- mean((mk - mean(y))^2)
  Vs <- (1 - 4 / 12) * stats::var(y) / 4
  expect_equal(s$variance, V, tolerance = 1e-12)
  expect_equal(s$deff, V / Vs, tolerance = 1e-12)
  expect_equal(s$rho, (V * 4 / stats::var(y) - 1) / 3, tolerance = 1e-12)
  expect_error(Sysamp(y, 5), "exact multiple")
  expect_error(Sysrs(y, 12), "at least two units")
  expect_error(Sysamp(y, 0), "at least 1")
})

test_that("morie_svyrcq_survey_quantile_regression approaches the weighted rq fit", {
  skip_if_not_installed("quantreg")
  x <- c(0.5, 1.2, 2.2, 3.1, 4.0, 5.3, 6.1, 7.4, 8.0, 9.2)
  y <- c(1.1, 2.0, 2.4, 4.5, 4.1, 5.9, 9.0, 7.2, 8.8, 9.1)
  w <- c(1, 2, 1, 1.5, 1, 2, 1, 1, 2.5, 1)
  r <- morie_svyrcq_survey_quantile_regression(x, y, tau = 0.3, weights = w)
  f <- quantreg::rq(y ~ x, tau = 0.3, weights = w)
  chk <- function(b) {
    u <- y - b[1] - b[2] * x
    sum(w * u * (0.3 - (u < 0)))
  }
  expect_equal(r$objective, chk(r$coefficients), tolerance = 1e-12)
  # MM smooths |u| below eps = 1e-6, so the fit sits within O(n eps) of the
  # exact LP optimum
  expect_equal(r$objective, chk(stats::coef(f)), tolerance = 1e-5)
  expect_equal(r$coefficients, unname(stats::coef(f)), tolerance = 1e-4)
  expect_true(all(diff(r$objective_path) <= 1e-12))
  expect_equal(r$weighted_fraction_below, sum(w[r$residuals < 0]) / sum(w), tolerance = 1e-12)
  expect_error(morie_svyrcq_survey_quantile_regression(x, y, tau = 1), "tau must lie")
  expect_error(morie_svyrcq_survey_quantile_regression(x, y, weights = -w), "cannot be negative")
  expect_error(morie_svyrcq_survey_quantile_regression(x[1:2], y[1:2]), "cannot identify")
})
