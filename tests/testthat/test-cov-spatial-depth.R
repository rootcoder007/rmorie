# Coverage for bivand201310e3.R, bivand20138e4.R, bivand20138e5.R,
# cokrg.R, euclP.R, distM.R, depthH.R and depthM.R: scan statistics,
# variograms and trend surfaces from their formulas, cokriging from the
# explicit covariance system, polynomial gcds from known factors, and
# data depths against direct enumeration.

test_that("Scanstat is Kulldorff's Poisson log-likelihood ratio", {
  O <- c(5, 1, 8, 2, 3)
  E <- c(2, 2, 3, 2.5, 3.5)
  zones <- list(0L, c(0L, 2L), c(1L, 3L), 0:4)
  r <- Scanstat(O, E, zones)
  llr <- function(z) {
    oz <- sum(O[z + 1])
    ez <- sum(E[z + 1])
    oo <- sum(O) - oz
    eo <- sum(E) - ez
    if (eo <= 0) return(-Inf)
    if (oz / ez <= sum(O) / sum(E)) return(-Inf)
    oz * log(oz / ez) + oo * log(oo / eo)
  }
  ll <- vapply(zones, llr, 0)
  expect_equal(r$loglr, ll, tolerance = 1e-12)
  expect_equal(r$best, which.max(ll) - 1L)
  expect_equal(r$bestzone, c(0L, 2L))
  expect_true(is.na(r$rrout[4]))
  lo <- Scanstat(O, E, zones, highonly = FALSE)
  expect_equal(lo$loglr[3], 3 * log(3 / 4.5) + 16 * log(16 / 8.5), tolerance = 1e-12)
  expect_equal(Scanstat(c(0, 4), c(1, 1), list(0L), highonly = FALSE)$loglr, 4 * log(4), tolerance = 1e-12)
  expect_error(Scanstat(O, E[-1], zones), "same length")
  expect_error(Scanstat(O, E * 0, zones), "strictly positive")
  expect_error(Scanstat(-O, E, zones), "non-negative")
  expect_error(Scanstat(O, E, list()), "no candidate")
  expect_error(Scanstat(O, E, list(7L)), "out of range")
})

test_that("Svariog bins half squared differences", {
  P <- rbind(c(0, 0), c(1, 0), c(0, 2), c(3, 1), c(2, 2), c(1, 3))
  z <- c(1, 2, 0.5, 3, 2.5, 1.5)
  pr <- t(combn(6, 2))
  d <- sqrt(rowSums((P[pr[, 1], ] - P[pr[, 2], ])^2))
  g <- (z[pr[, 1]] - z[pr[, 2]])^2
  br <- c(0, 1.5, 2.5, 4)
  r <- Svariog(P, z, breaks = br)
  bin <- cut(d, br)
  expect_equal(r$gamma, unname(tapply(g, bin, sum) / (2 * table(bin))), ignore_attr = TRUE,
               tolerance = 1e-12)
  expect_equal(r$np, as.integer(table(bin)))
  expect_equal(r$dist, as.numeric(tapply(d, bin, mean)), tolerance = 1e-12)
  a <- Svariog(P, z, nbins = 2)
  expect_equal(a$breaks, max(d) / 3 * c(0, 0.5, 1), tolerance = 1e-12)
  expect_equal(sum(a$np), sum(d <= max(d) / 3))
  e <- Svariog(P, z, breaks = c(0, 0.5, 1.2))
  expect_true(is.na(e$gamma[1]) && e$np[1] == 0L)
  expect_error(Svariog(P, z[-1]), "one value per location")
  expect_error(Svariog(P[1, , drop = FALSE], 1), "at least two")
  expect_error(Svariog(P, z, nbins = 0), "positive")
  expect_error(Svariog(P, z, breaks = c(0, 2, 1)), "strictly increasing")
})

test_that("Spatrend is the least-squares trend surface", {
  X <- cbind(c(0, 1, 2, 0, 1, 2, 0.5), c(0, 0, 0, 1, 1, 1, 2))
  z <- c(1, 1.8, 3.1, 1.4, 2.2, 3.5, 2.6)
  f <- lm(z ~ X)
  r <- Spatrend(X, z)
  expect_equal(r$beta, unname(coef(f)), tolerance = 1e-10)
  expect_equal(r$fitted, unname(fitted(f)), tolerance = 1e-10)
  expect_equal(r$sigma2, summary(f)$sigma^2, tolerance = 1e-10)
  expect_equal(Spatrend(X, z, addintercept = FALSE)$beta, unname(coef(lm(z ~ X - 1))),
               tolerance = 1e-10)
  expect_error(Spatrend(X, z[-1]), "one row per observation")
  expect_error(Spatrend(X[1:3, ], z[1:3]), "more observations")
})

test_that("morie_cokrg solves the cokriging system", {
  co <- rbind(c(0, 0), c(1, 0), c(0, 1.5))
  x <- c(1.2, 0.4, 2)
  y <- c(0.5, 0.1, 0.9)
  tg <- rbind(c(0.5, 0.5), c(1, 1))
  args <- list(sill_p = 2, range_p = 1.5, sill_s = 1, range_s = 0.8, cross_sill = 0.6,
               cross_range = 1.2, nugget = 0.1)
  D <- as.matrix(dist(co))
  cov2 <- function(h, i, j) {
    if (i == 1 && j == 1) return((h == 0) * 0.1 + 1.9 * exp(-h / 1.5))
    if (i == 2 && j == 2) return((h == 0) * 0.1 + 0.9 * exp(-h / 0.8))
    0.6 * exp(-h / 1.2)
  }
  vv <- rep(1:2, each = 3)
  C <- outer(1:6, 1:6, Vectorize(function(a, b) cov2(D[(a - 1) %% 3 + 1, (b - 1) %% 3 + 1], vv[a], vv[b])))
  zz <- c(x, y)
  mu <- c(0.8, 0.3)
  sk <- do.call(morie_cokrg, c(list(x, y, co, tg), args, list(means = mu)))
  ok <- do.call(morie_cokrg, c(list(x, y, co, tg), args, list(means = NULL)))
  Xd <- outer(vv, 1:2, "==") + 0
  for (k in 1:2) {
    h0 <- sqrt(colSums((t(co) - tg[k, ])^2))
    c0 <- c(cov2(h0, 1, 1), cov2(h0, 1, 2))
    w <- solve(C, c0)
    expect_equal(sk$estimate[k], 0.8 + sum(w * (zz - mu[vv])), tolerance = 1e-10)
    expect_equal(sk$se[k], sqrt(2 - sum(w * c0)), tolerance = 1e-10)
    A <- rbind(cbind(C, Xd), cbind(t(Xd), matrix(0, 2, 2)))
    lam <- solve(A, c(c0, 1, 0))[1:6]
    expect_equal(ok$estimate[k], sum(lam * zz), tolerance = 1e-10)
    expect_equal(ok$se[k], sqrt(2 - 2 * sum(lam * c0) + sum(lam * (C %*% lam))), tolerance = 1e-10)
  }
  one <- do.call(morie_cokrg, c(list(x, y, co, c(0.5, 0.5)), args, list(means = mu)))
  expect_equal(one$estimate, sk$estimate[1], tolerance = 1e-12)
  expect_match(ok$method, "Ordinary")
  expect_same_function(morie_cokrg, cokrg)
  expect_error(morie_cokrg(x, y, co, c(1, 2, 3)), "dim mismatch")
  expect_error(morie_cokrg(x, y[-1], co, tg), "matching n")
  expect_error(morie_cokrg(x, y, co, cbind(tg, 1)), "dim mismatch")
})

test_that("EuclP returns the monic polynomial gcd", {
  pm <- function(a, b) {
    out <- numeric(length(a) + length(b) - 1)
    for (i in seq_along(a)) for (j in seq_along(b)) out[i + j - 1] <- out[i + j - 1] + a[i] * b[j]
    out
  }
  g <- pm(c(-1, 1), c(-2, 1))
  p <- pm(g, c(3, 1))
  q <- pm(g, c(-5, 2))
  r <- EuclP(p, q)
  expect_equal(r$gcd, g, tolerance = 1e-9)
  expect_equal(r$degree, 2L)
  expect_equal(EuclP(q, p)$gcd, g, tolerance = 1e-9)
  expect_equal(EuclP(c(1, 1), c(2, 1))$gcd, 1)
  expect_equal(EuclP(0, c(4, 2))$gcd, c(2, 1))
  expect_equal(EuclP(c(4, 2), c(0, 0))$gcd, c(2, 1))
  expect_error(EuclP(0, 0), "undefined")
  expect_error(EuclP(numeric(0), 1), "empty")
  expect_error(EuclP(1, 1, tol = 0), "positive")
})

test_that("DistM scores triples by the trilinear product", {
  tr <- rbind(c(0, 0, 1), c(1, 1, 2), c(2, 0, 0))
  E <- rbind(c(1, 0.5), c(-1, 2), c(0.3, 0.3))
  R <- rbind(c(2, 1), c(0.5, -1))
  r <- DistM(tr, 2, E = E, R = R)
  sc <- vapply(1:3, function(i) sum(E[tr[i, 1] + 1, ] * R[tr[i, 2] + 1, ] * E[tr[i, 3] + 1, ]), 0)
  expect_equal(r$scores, sc, tolerance = 1e-12)
  expect_equal(r$symmetric_gap, 0)
  s <- 3
  z <- numeric(10)
  for (i in 1:10) {
    s <- (48271 * s) %% 2147483647
    z[i] <- qnorm(s / 2147483647)
  }
  Ed <- matrix(z[1:6], 3, 2, byrow = TRUE)
  Rd <- matrix(z[7:10], 2, 2, byrow = TRUE)
  rd <- DistM(tr, 2, seed = 3)
  expect_equal(rd$estimate, mean(vapply(1:3, function(i) {
    sum(Ed[tr[i, 1] + 1, ] * Rd[tr[i, 2] + 1, ] * Ed[tr[i, 3] + 1, ])
  }, 0)), tolerance = 1e-12)
})

test_that("DepthH and Mahaldep", {
  X <- rbind(c(0, 0), c(2, 0), c(1, 2), c(3, 3), c(-1, 1), c(1, 0.5))
  ang <- (seq_len(36000) + 0.123) * 2 * pi / 36000
  U <- cbind(cos(ang), sin(ang))
  brute <- function(th) {
    D <- sweep(X, 2, th)
    min(colSums(D %*% t(U) >= 0))
  }
  for (th in list(c(1, 1), c(1, 0.5), c(5, 5), c(0.5, 0.2))) {
    expect_equal(DepthH(X, th)$count, brute(th))
  }
  expect_equal(DepthH(X, c(5, 5))$estimate, 0)
  X3 <- cbind(X, c(0, 1, 0, 1, 1, 0))
  h3 <- DepthH(X3, c(1, 1, 0.5))
  D3 <- sweep(X3, 2, c(1, 1, 0.5))
  dirs <- rbind(D3, -D3)
  expect_equal(h3$count, min(colSums(D3 %*% t(dirs) >= 0)))
  expect_equal(h3$exact, 0L)
  expect_equal(DepthH(matrix(1, 3, 2), c(1, 1))$estimate, 1)
  m <- Mahaldep(X)
  d2 <- mahalanobis(X, colMeans(X), cov(X))
  expect_equal(m$d2, d2, tolerance = 1e-12)
  expect_equal(m$depth, 1 / (1 + d2), tolerance = 1e-12)
  expect_equal(m$deepest, which.max(1 / (1 + d2)) - 1L)
  given <- Mahaldep(X, mu = c(1, 1), Sigma = diag(c(2, 1)))
  expect_equal(given$d2, (X[, 1] - 1)^2 / 2 + (X[, 2] - 1)^2, tolerance = 1e-12)
  expect_error(Mahaldep(X, mu = 1), "one entry per column")
  expect_error(Mahaldep(X[1, , drop = FALSE]), "at least 2 rows")
  expect_error(Mahaldep(X, Sigma = diag(3)), "p x p")
})
