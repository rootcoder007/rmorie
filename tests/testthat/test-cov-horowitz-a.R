# Coverage for Horowitz (2009) additive models, Box-Cox minimum distance,
# maximum score, order-r kernels, bandwidth CV, confidence bands, rates,
# the control function, the empirical characteristic function and the
# generalized likelihood ratio test; each estimator is rebuilt from its
# defining equations with vectorised base R.

gk <- function(u) exp(-0.5 * u^2) / sqrt(2 * pi)
llsm <- function(z, y, zq, h) {
  vapply(zq, function(q) {
    u <- (z - q) / h
    w <- exp(-0.5 * u^2)
    s <- c(sum(w), sum(w * u), sum(w * u^2))
    t0 <- sum(w * y)
    t1 <- sum(w * u * y)
    (s[3] * t0 - s[2] * t1) / (s[1] * s[3] - s[2]^2)
  }, 0)
}
set.seed(21)
xa <- cbind(runif(20), runif(20))
ya <- sin(2 * xa[, 1]) + xa[, 2]^2 + rnorm(20, sd = 0.1)

test_that("Npaddreg and its alias are marginal integration", {
  r <- Npaddreg(xa, ya, h1 = 0.3, h2 = 0.4, ngrid = 5)
  for (j in 1:2) {
    k <- 3 - j
    g <- seq(min(xa[, j]), max(xa[, j]), length.out = 5)
    W2 <- gk(outer(xa[, k], xa[, k], "-") / 0.4)
    K1 <- gk(outer(g, xa[, j], "-") / 0.3)
    m <- vapply(1:5, function(t) {
      Wt <- W2 * matrix(K1[t, ], 20, 20, byrow = TRUE)
      mean(as.numeric(Wt %*% ya) / rowSums(Wt))
    }, 0)
    expect_equal(r$components[[j]], m - mean(m), tolerance = 1e-12)
  }
  fit <- mean(ya) + approx(r$grids[[1]], r$components[[1]], xa[, 1], rule = 2)$y +
    approx(r$grids[[2]], r$components[[2]], xa[, 2], rule = 2)$y
  expect_equal(r$fitted, fit, tolerance = 1e-12)
  expect_equal(r$rss, sum((ya - fit)^2), tolerance = 1e-12)
  expect_equal(morie_horowitz_additive_model(xa, ya, h1 = 0.3, h2 = 0.4, ngrid = 5)$fitted, r$fitted)
  expect_equal(Npaddreg(xa, ya)$h1, 20^-0.2, tolerance = 1e-12)
  expect_error(Npaddreg(xa[, 1], ya), "at least two covariates")
})

test_that("Addlink with the identity link smooths y; the logistic link maps back through G", {
  r <- Addlink(xa, ya, link = "identity", K = 2, h = 0.25, ngrid = 6)
  for (j in 1:2) {
    g <- seq(min(xa[, j]), max(xa[, j]), length.out = 6)
    K <- gk(outer(g, xa[, j], "-") / 0.25)
    m <- as.numeric(K %*% ya) / rowSums(K)
    expect_equal(r$components[[j]], m - mean(m), tolerance = 1e-9)
  }
  expect_equal(r$mu, mean(ya), tolerance = 1e-9)
  expect_equal(r$fitted, r$eta, tolerance = 1e-12)
  yb <- as.numeric(ya > median(ya))
  lg <- Addlink(xa, yb, link = "logistic", K = 2, h = 0.3, ngrid = 6, niter = 30)
  expect_equal(lg$fitted, plogis(lg$eta), tolerance = 1e-12)
  expect_equal(lg$rss, sum((yb - lg$fitted)^2), tolerance = 1e-12)
  expect_equal(vapply(lg$components, sum, 0), c(0, 0), tolerance = 1e-12)
  cu <- Addlink(xa, ya, link = list(function(v) v, function(v) rep(1, length(v))), K = 2, h = 0.25, ngrid = 6)
  expect_equal(cu$fitted, r$fitted, tolerance = 1e-12)
  expect_equal(morie_horowitz_additive_nonid_link(xa, ya, link = "identity", K = 2, h = 0.25, ngrid = 6)$fitted,
               r$fitted)
  expect_error(Addlink(xa, ya, link = "probit"), "link must be one of")
})

test_that("Addquant assembles the check-loss fit from its centred components", {
  r <- Addquant(xa, ya, alpha = 0.3, K = 2, h = 0.3, niter = 20, ngrid = 5)
  expect_equal(vapply(r$components, sum, 0), c(0, 0), tolerance = 1e-12)
  expect_equal(r$checkloss, sum(abs(r$resid) + (2 * 0.3 - 1) * r$resid), tolerance = 1e-12)
  expect_equal(r$checkloss, 2 * sum(r$resid * (0.3 - (r$resid < 0))), tolerance = 1e-12)
  expect_equal(r$resid, ya - r$fitted, tolerance = 1e-12)
  expect_equal(morie_horowitz_additive_quantile(xa, ya, alpha = 0.3, K = 2, h = 0.3, niter = 20, ngrid = 5)$fitted,
               r$fitted)
  expect_error(Addquant(xa, ya, alpha = 1), "alpha")
})

test_that("Hrzaul returns an index normalised by (3.26) and the polynomial link on it", {
  r <- Hrzaul(xa, ya, degree = 2, link_degree = 2, iters = 5)
  n <- 20
  U <- apply(xa, 2, function(v) (rank(v) - 0.5) / n)
  B <- lapply(1:2, function(j) outer(U[, j], 1:2, "^") - matrix(1 / (2:3), n, 2, byrow = TRUE))
  nu <- B[[1]] %*% r$m_coef[[1]] + B[[2]] %*% r$m_coef[[2]]
  expect_equal(r$index, as.numeric(nu), tolerance = 1e-12)
  Om <- outer(1:2, 1:2, function(a, b) 1 / (a + b + 1) - 1 / ((a + 1) * (b + 1)))
  expect_equal(r$scale_norm, sum(vapply(r$m_coef, function(c) sum(outer(c, c) * Om), 0)), tolerance = 1e-12)
  expect_equal(r$scale_norm, 1, tolerance = 1e-12)
  expect_equal(r$G_hat, as.numeric(outer(r$index, 0:2, "^") %*% r$link_coef), tolerance = 1e-12)
  expect_equal(r$rss, sum((ya - r$G_hat)^2), tolerance = 1e-12)
  expect_equal(r$m_j_hats[[2]], as.numeric(B[[2]] %*% r$m_coef[[2]]), tolerance = 1e-12)
  expect_equal(morie_horowitz_additive_unknown_link(xa, ya, degree = 2, link_degree = 2, iters = 5)$rss, r$rss)
  expect_error(Hrzaul(xa[, 1], ya), "at least two")
  expect_error(Hrzaul(xa, ya, iters = 0), "iters")
})

test_that("Hrzboxc minimises the Foster-Tian-Wei distance", {
  set.seed(3)
  X <- cbind(1, runif(10))
  y <- exp(0.5 + X[, 2] + rnorm(10, sd = 0.2))
  r <- Hrzboxc(X, y, a_lo = -1, a_hi = 1, ngrid = 5, refine = 10, nu = 7)
  bc <- function(v, a) if (a == 0) log(v) else (v^a - 1) / a
  crit <- function(a) {
    Ty <- bc(y, a)
    b <- qr.solve(X, Ty)
    uh <- Ty - X %*% b
    ug <- max(y) * (1:7) / 7
    tot <- 0
    for (u in ug) {
      z <- bc(u, a) - X %*% b
      fn <- vapply(z, function(zz) mean(uh < zz), 0)
      tot <- tot + sum(((y < u) - fn)^2) / max(y) * max(y) / 7
    }
    list(v = tot / 10, b = as.numeric(b))
  }
  cf <- crit(r$lambda_hat)
  expect_equal(r$criterion, cf$v, tolerance = 1e-12)
  expect_equal(r$beta_hat, cf$b, tolerance = 1e-9)
  expect_equal(morie_horowitz_box_cox(X, y, a_lo = -1, a_hi = 1, ngrid = 5, refine = 10, nu = 7)$lambda_hat,
               r$lambda_hat)
  expect_error(Hrzboxc(X, -y), "positive Y")
  expect_error(Hrzboxc(X, y, nu = 2), "nu")
})

test_that("Binresp maximises the Manski score over the cut-point cells", {
  set.seed(5)
  X <- cbind(rnorm(30), rnorm(30))
  y <- as.numeric(X[, 1] - 0.5 * X[, 2] + rnorm(30, sd = 0.5) > 0)
  r <- Binresp(X, y)
  cuts <- sort(-X[, 1] / X[, 2])
  cand <- c(cuts[1] - 1, (head(cuts, -1) + tail(cuts, -1)) / 2, cuts[30] + 1)
  sc <- vapply(cand, function(b) mean((2 * y - 1) * (X %*% c(1, b) >= 0)), 0)
  expect_equal(r$score, max(sc), tolerance = 1e-12)
  expect_equal(r$estimate, c(1, cand[which.max(sc)]), tolerance = 1e-12)
  expect_equal(r$correct, mean((X %*% r$estimate >= 0) == y), tolerance = 1e-12)
  X3 <- cbind(X, rnorm(30))
  r3 <- Binresp(X3, y, ngrid = 5, blim = 2)
  expect_equal(r3$ncand, 25L)
  expect_equal(morie_horowitz_binary_response_model(X, y)$score, r$score)
  expect_error(Binresp(X, y + 1), "binary")
})

test_that("Hrzbr5 builds a Gaussian-based order-r kernel", {
  for (r in c(2L, 4L, 6L)) {
    k <- Hrzbr5(0.3, r)
    Kr <- function(u) {
      He <- function(m, u) switch(as.character(m), "0" = 1, "2" = u^2 - 1, "4" = u^4 - 6 * u^2 + 3)
      s <- 0
      for (j in 0:(r / 2 - 1)) s <- s + (-1)^j / (2^j * factorial(j)) * He(2 * j, u)
      s * dnorm(u)
    }
    mom <- vapply(0:r, function(m) integrate(function(u) u^m * Kr(u), -Inf, Inf, rel.tol = 1e-12)$value, 0)
    expect_equal(k$moments, mom, tolerance = 1e-9)
    expect_equal(k$moments[1:r], c(1, rep(0, r - 1)), tolerance = 1e-12)
    expect_equal(k$estimate, 0.3^r, tolerance = 1e-12)
  }
  expect_equal(morie_horowitz_bias_reduction_deconv(0.5, 2)$coefficients, 1)
  expect_error(Hrzbr5(0.3, 3), "even")
  expect_error(Hrzbr5(0, 2), "positive")
})

test_that("Simbwcv, Npconfband, Simgrate-style NW pieces and the rate tables", {
  set.seed(9)
  X <- cbind(rnorm(25), rnorm(25))
  b <- c(1, 0.5)
  y <- sin(X %*% b)[, 1] + rnorm(25, sd = 0.2)
  z <- as.numeric(X %*% b)
  hs <- c(0.2, 0.4, 0.8)
  cv <- vapply(hs, function(h) {
    K <- gk(outer(z, z, "-") / h)
    diag(K) <- 0
    mean((y - K %*% y / rowSums(K))^2)
  }, 0)
  r <- Simbwcv(X, y, b, grid = hs)
  expect_equal(r$cvcurve, cv, tolerance = 1e-12)
  expect_equal(r$bandwidth, hs[which.min(cv)])
  expect_equal(r$hstokerform, 25^(-2 / 8), tolerance = 1e-12)
  expect_equal(morie_horowitz_bw_cv_sim(X, y, b, grid = hs)$cv, r$cv)

  g <- c(-1, 0, 1)
  cb <- Npconfband(z, y, grid = g, h = 0.5, alpha = 0.1)
  K <- gk(outer(g, z, "-") / 0.5)
  gh <- as.numeric(K %*% y) / rowSums(K)
  dens <- rowSums(K) / (25 * 0.5)
  s2 <- rowSums(K * (matrix(y, 3, 25, byrow = TRUE) - gh)^2) / rowSums(K)
  se <- sqrt(s2 / (2 * sqrt(pi)) / (25 * 0.5 * dens))
  expect_equal(cb$ghat, gh, tolerance = 1e-12)
  expect_equal(cb$se, se, tolerance = 1e-12)
  expect_equal(cb$upper, gh + qnorm(0.95) * se, tolerance = 1e-12)
  expect_equal(morie_horowitz_confidence_bands(z, y, grid = g, h = 0.5)$ghat, gh, tolerance = 1e-12)
  expect_error(Npconfband(z, y, alpha = 2), "alpha")

  nr <- Nprate(3, 1000, s = 2)
  expect_equal(nr$rate, 1000^(-2 / 7), tolerance = 1e-12)
  expect_equal(nr$nequiv, 1000^((2 / 5) / (2 / 7)), tolerance = 1e-12)
  expect_equal(morie_horowitz_curse_dimensionality(3, 1000)$penalty, nr$penalty)
  expect_error(Nprate(0, 10), "positive integers")
  dr <- Dimredrate(5, 400, s = 2, M = 1)
  expect_equal(dr$gain, 400^(-2 / 9) / 400^(-2 / 5), tolerance = 1e-12)
  expect_equal(morie_horowitz_dimension_reduction(5, 400)$indexrate, 400^(-0.4), tolerance = 1e-12)
  expect_error(Dimredrate(1, 1, M = 0), "positive integers")
})

test_that("Hrzctrl backfits g(x) and h(v) with local-linear smoothers", {
  set.seed(4)
  n <- 30
  w <- rnorm(n)
  v <- rnorm(n, sd = 0.5)
  x <- w + v
  y <- x^2 / 4 + v + rnorm(n, sd = 0.1)
  r <- Hrzctrl(x, y, w, bandwidth = 0.6, iters = 6)
  vh <- x - llsm(w, x, w, 0.6)
  g <- h <- numeric(n)
  for (it in 1:6) {
    g <- llsm(x, y - mean(y) - h, x, 0.6)
    g <- g - mean(g)
    h <- llsm(vh, y - mean(y) - g, vh, 0.6)
    h <- h - mean(h)
  }
  expect_equal(r$v_hat, vh, tolerance = 1e-12)
  expect_equal(r$g_hat, g, tolerance = 1e-9)
  expect_equal(r$h_hat, h, tolerance = 1e-9)
  expect_equal(r$resid_sd, sqrt(mean((y - mean(y) - g - h)^2)), tolerance = 1e-9)
  expect_equal(morie_horowitz_control_function(x, y, w, bandwidth = 0.6, iters = 6)$g_hat, r$g_hat)
  expect_error(Hrzctrl(x[1:3], y[1:3], w[1:3]), "at least 4")
})

test_that("Hrzecfw is the empirical characteristic function", {
  w <- c(0.3, -1.1, 2.0, 0.7)
  tau <- c(0.5, 1.5)
  r <- Hrzecfw(w, tau)
  cf <- vapply(tau, function(t) mean(exp(1i * t * w)), 0i)
  expect_equal(r$re, Re(cf), tolerance = 1e-12)
  expect_equal(r$im, Im(cf), tolerance = 1e-12)
  expect_equal(r$modulus, Mod(cf), tolerance = 1e-12)
  expect_equal(r$argument, Arg(cf), tolerance = 1e-12)
  expect_equal(morie_horowitz_empirical_cf(w, tau)$estimate, Mod(cf[1]), tolerance = 1e-12)
  expect_error(Hrzecfw(numeric(0), 1), "empty")
})

test_that("Splrtest compares a polynomial fit with a local-linear fit", {
  set.seed(8)
  x <- sort(runif(30))
  y <- sin(3 * x) + rnorm(30, sd = 0.1)
  r <- Splrtest(x, y, h = 0.15, degree = 1)
  f0 <- fitted(lm(y ~ x))
  f1 <- llsm(x, y, x, 0.15)
  lam <- 0.5 * 30 * log(sum((y - f0)^2) / sum((y - f1)^2))
  expect_equal(r$rss0, sum((y - f0)^2), tolerance = 1e-9)
  expect_equal(r$rss1, sum((y - f1)^2), tolerance = 1e-9)
  expect_equal(r$lambdan, lam, tolerance = 1e-9)
  ck <- 1 / sqrt(2 * pi) - 0.5 / (2 * sqrt(pi))
  df <- 2.5375 * ck * (max(x) - min(x)) / 0.15
  expect_equal(r$df, df, tolerance = 1e-12)
  expect_equal(r$p_value, pchisq(2.5375 * lam, df, lower.tail = FALSE), tolerance = 1e-9)
  tb <- Splrtest(x, y, fitted = f0, h = 0.15, kernel = "table")
  expect_equal(tb$ck, 0.7737)
  expect_equal(morie_horowitz_likelihood_ratio_test(x, y, h = 0.15)$statistic, r$statistic)
  expect_error(Splrtest(x[1:4], y[1:4]), "five observations")
  expect_error(Splrtest(x, y, fitted = 1:3), "one entry per observation")
})
