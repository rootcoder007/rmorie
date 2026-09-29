# Coverage for SpatialBootstrap .. SpatialWeightsBuild exports. Every
# expectation is recomputed in the test body.

test_that("BootstrapBands gives pointwise and sup-t simultaneous bands", {
  R <- rbind(c(1.0, 2.1, 0.5), c(1.2, 1.9, 0.7), c(0.8, 2.4, 0.4), c(1.1, 2.0, 0.9), c(0.9, 2.2, 0.6))
  b <- BootstrapBands(R, alpha = 0.2)
  est <- colMeans(R)
  sd <- apply(R, 2, stats::sd)
  tm <- apply(R, 1, function(r) max(abs(r - est) / sd))
  cc <- unname(stats::quantile(tm, 0.8))
  expect_equal(b$critical_value, cc, tolerance = 1e-12)
  expect_equal(b$simultaneous_upper, est + cc * sd, tolerance = 1e-12)
  expect_equal(b$pointwise_lower, apply(R, 2, stats::quantile, 0.1, names = FALSE), tolerance = 1e-12)
})

test_that("SarPoissonLmTest is the score test for a spatial lag in a Poisson GLM", {
  W <- matrix(0, 6, 6)
  for (i in 1:5) W[i, i + 1] <- W[i + 1, i] <- 1
  W <- W / rowSums(W)
  x <- c(0.2, 1.1, -0.5, 0.8, 1.9, -1.0)
  y <- c(1, 3, 0, 2, 6, 0)
  X <- cbind(1, x)
  r <- SarPoissonLmTest(y, X, W)
  f <- stats::glm(y ~ x, family = stats::poisson(), control = list(epsilon = 1e-14))
  mu <- stats::fitted(f)
  dd <- as.numeric(W %*% stats::predict(f, type = "link"))
  s <- sum((y - mu) * dd)
  XDd <- crossprod(X, mu * dd)
  info <- sum(mu * dd^2) - sum(XDd * solve(crossprod(X * mu, X), XDd))
  # glm converges to 1e-14 relative deviance; the score is a small difference
  expect_equal(r$score, s, tolerance = 1e-8)
  expect_equal(r$statistic, s^2 / info, tolerance = 1e-8)
  expect_equal(r$p_value, stats::pchisq(r$statistic, 1, lower.tail = FALSE), tolerance = 1e-12)
  expect_error(SarPoissonLmTest(y - 0.5, X, W), "non-negative counts")
})

test_that("funnel plot control limits and the Ghose filter", {
  n <- c(20, 100, 500)
  f <- FunnelControlLimits(0.1, n)
  z <- c(stats::qnorm(0.975), stats::qnorm(0.999))
  for (k in 1:2) {
    h <- z[k] * sqrt(0.09 / n)
    expect_equal(f$lower[[k]], pmax(0.1 - h, 0), tolerance = 1e-12)
    expect_equal(f$upper[[k]], pmin(0.1 + h, 1), tolerance = 1e-12)
  }
  s <- FunnelControlLimits(1, n, kind = "smr", phi = 2)
  expect_equal(s$upper[[1]], 1 + z[1] * sqrt(2 / n), tolerance = 1e-12)
  expect_equal(GhoseDrugFilter(c(300, 150, 300), c(2, 2, 6), c(80, 80, 80), c(30, 30, 30)),
               c(TRUE, FALSE, FALSE))
})

test_that("SgcRidge, SarCovariance and InverseDistanceWeights", {
  A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
  X <- cbind(c(1, 0.5, -0.3, 2), c(0.2, 1.5, 0.8, -1))
  y <- c(1.1, 0.4, 0.9, 2.3)
  r <- SgcRidge(A, X, y, k = 2, l2 = 0.5)
  At <- A + diag(4)
  S <- At / sqrt(outer(rowSums(At), rowSums(At)))
  Fm <- S %*% S %*% X
  Z <- cbind(1, Fm)
  b <- solve(crossprod(Z) + diag(c(0, 0.5, 0.5)), crossprod(Z, y))
  expect_equal(r$coefficients, as.numeric(b), tolerance = 1e-10)
  expect_equal(r$features, Fm, tolerance = 1e-12)
  Wn <- A / rowSums(A)
  sc <- SarCovariance(Wn, 0.4, sigma2 = 2)
  expect_equal(sc, 2 * solve(t(diag(4) - 0.4 * Wn) %*% (diag(4) - 0.4 * Wn)), tolerance = 1e-12)
  P <- cbind(c(0, 1, 2, 0.5), c(0, 0.5, 0, 1.5))
  D <- as.matrix(stats::dist(P))
  W <- InverseDistanceWeights(P, power = 2)
  W0 <- ifelse(D > 0, D^-2, 0)
  expect_equal(W, unname(W0), tolerance = 1e-12)
  Wr <- InverseDistanceWeights(P, power = 2, row_standardize = TRUE)
  expect_equal(dim(Wr), c(4L, 4L))
  expect_equal(Wr, unname(W0 / rowSums(W0)), tolerance = 1e-12)
  Wd <- InverseDistanceWeights(P, d2 = 1.2)
  expect_equal(Wd, unname(ifelse(D > 0 & D <= 1.2, 1 / D, 0)), tolerance = 1e-12)
})

test_that("SpatialGlmmPredict kriges the random effect and integrates the link", {
  fit <- list(coords = cbind(c(0, 1, 2), c(0, 0.5, 0)), model = "Exp", sigma2 = 0.8, range = 1.5,
              u = c(0.3, -0.2, 0.1), posterior_cov = diag(0.05, 3), beta = c(0.5, 0.2), family = "poisson")
  X0 <- rbind(c(1, 0.4), c(1, -0.2))
  Q <- rbind(c(0.5, 0.2), c(1.5, 0.5))
  r <- SpatialGlmmPredict(fit, X0, Q)
  D <- as.matrix(stats::dist(fit$coords))
  Sg <- 0.8 * exp(-D / 1.5) + diag(0.8e-10, 3)
  C <- 0.8 * exp(-sqrt(outer(Q[, 1], fit$coords[, 1], "-")^2 + outer(Q[, 2], fit$coords[, 2], "-")^2) / 1.5)
  Wt <- C %*% solve(Sg)
  v <- 0.8 - rowSums(Wt * C) + rowSums((Wt %*% fit$posterior_cov) * Wt)
  eta <- as.numeric(X0 %*% fit$beta + Wt %*% fit$u)
  expect_equal(r$eta, eta, tolerance = 1e-10)
  expect_equal(r$variance, v, tolerance = 1e-10)
  expect_equal(r$mean, exp(eta + v / 2), tolerance = 1e-10)
  fit$family <- "binomial"
  rb <- SpatialGlmmPredict(fit, X0, Q)
  # 20-point Gauss-Hermite on a smooth logistic-normal integrand
  ln <- vapply(1:2, function(k) stats::integrate(function(z) stats::plogis(eta[k] + sqrt(v[k]) * z) * stats::dnorm(z),
                                                 -Inf, Inf, rel.tol = 1e-12)$value, 0)
  expect_equal(rb$mean, ln, tolerance = 1e-9)
})

test_that("TemperedSpatial replays its Metropolis and swap moves", {
  lp <- function(x) -0.5 * sum((x - c(1, -1))^2)
  temps <- c(1, 3)
  r <- TemperedSpatial(lp, c(0, 0), n_iter = 15, temps = temps, step = 0.7, seed = 3)
  xs <- list(c(0, 0), c(0, 0))
  l <- c(lp(c(0, 0)), lp(c(0, 0)))
  out <- matrix(0, 15, 2)
  acc <- 0
  for (t in 0:14) {
    base <- t * 5
    for (c in 1:2) {
      z <- .morie_random_normal(2, seed = 3, stream = base + 2 * (c - 1))
      u <- .morie_random_uniform(1, seed = 3, stream = base + 2 * (c - 1) + 1)
      pr <- xs[[c]] + 0.7 * sqrt(temps[c]) * z
      if (log(u) < (lp(pr) - l[c]) / temps[c]) {
        xs[[c]] <- pr
        l[c] <- lp(pr)
      }
    }
    u <- .morie_random_uniform(1, seed = 3, stream = base + 4)
    if (log(u) < (1 - 1 / 3) * (l[2] - l[1])) {
      xs <- xs[2:1]
      l <- l[2:1]
      acc <- acc + 1
    }
    out[t + 1, ] <- xs[[1]]
  }
  expect_equal(r$samples, out, tolerance = 1e-12)
  expect_equal(r$swap_rate, acc / 15)
})

test_that("TurningBandsSpherical uses Fibonacci directions and replays its Poisson bands", {
  P <- cbind(c(0, 0.3, 0.7), c(0, 0.2, 0.5))
  r <- TurningBandsSpherical(P, sill = 2, range_ = 0.5, n_bands = 3, lam = 20, seed = 2)
  ga <- pi * (3 - sqrt(5))
  dirs <- t(vapply(0:2, function(b) {
    zc <- 1 - (2 * b + 1) / 3
    c(sqrt(1 - zc^2) * cos(b * ga), sqrt(1 - zc^2) * sin(b * ga), zc)
  }, numeric(3)))
  expect_equal(r$directions, dirs, tolerance = 1e-12)
  expect_equal(rowSums(dirs^2), rep(1, 3), tolerance = 1e-12)
  U <- .gx_unif(2, 0)
  X3 <- cbind(P, 0)
  field <- numeric(3)
  nrm <- sqrt(2 / 3) / sqrt(20 * 0.125 / 12)
  for (l in 1:3) {
    tt <- as.numeric(X3 %*% dirs[l, ])
    s <- min(tt) - 0.25
    hi <- max(tt) + 0.25
    pk <- ek <- numeric(0)
    repeat {
      s <- s - log(1 - U()) / 20
      if (s > hi) break
      pk <- c(pk, s)
      ek <- c(ek, if (U() < 0.5) 1 else -1)
    }
    for (i in 1:3) {
      d <- tt[i] - pk
      field[i] <- field[i] + nrm * sum((ek * d)[d > -0.25 & d < 0.25])
    }
  }
  expect_equal(r$field, field, tolerance = 1e-12)
})

test_that("PcaIdealPoints scores legislators on the leading components", {
  V <- rbind(c(1, 1, 0, 1, NA), c(1, 0, 0, 1, 1), c(0, 0, 1, 0, 0), c(0, NA, 1, 0, 0), c(1, 1, 1, 1, 0))
  r <- PcaIdealPoints(V, dims = 2)
  M <- V
  for (j in 1:5) M[is.na(M[, j]), j] <- mean(M[, j], na.rm = TRUE)
  X <- sweep(M, 2, colMeans(M))
  s <- svd(X)
  sc <- s$u[, 1:2] %*% diag(s$d[1:2])
  sc <- sweep(sc, 2, ifelse(sc[1, ] > 0, -1, 1), "*")
  expect_equal(r$scores, sc, tolerance = 1e-10)
  expect_equal(r$variance_share, s$d[1:2]^2 / sum(s$d^2), tolerance = 1e-10)
})
