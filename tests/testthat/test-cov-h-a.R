# Coverage for heritability, coin-flip asymptotics, plasma half-life,
# Hampel psi/weights, haplotype blocks, Matern hard-core processes,
# HDBSCAN*, the HDP mixtures, HPD intervals, Anderson-Darling, the
# Hellinger distance and the Hermite basis; recomputed in the test body.

test_that("H2est and HalfHeads", {
  r <- H2est(c(2, 1, 0), c(1, 3, 4))
  expect_equal(r$h2, c(2, 1, 0) / (c(2, 1, 0) + c(1, 3, 4)), tolerance = 1e-12)
  expect_equal(r$sigma_p2, c(3, 4, 4))
  expect_equal(H2est(1, c(1, 3))$h2, c(0.5, 0.25))
  expect_error(H2est(1:2, 1:3), "incompatible")
  expect_error(H2est(-1, 1), "non-negative")
  expect_error(H2est(0, 0), "phenotypic variance")
  h <- HalfHeads(10)
  expect_equal(h$exact, dbinom(10, 20, 0.5), tolerance = 1e-12)
  expect_equal(h$approx, 1 / sqrt(10 * pi), tolerance = 1e-12)
  expect_equal(h$relative_error, abs(h$approx - h$exact) / h$exact, tolerance = 1e-12)
  expect_error(HalfHeads(0), "integer")
})

test_that("morie_halft covers the one-, two-compartment and effective routes", {
  o <- morie_halft(Vd = 40, Cl = 5, dose = 100)
  expect_equal(o$half_life, log(2) * 40 / 5, tolerance = 1e-12)
  expect_equal(o$auc, 100 / 5, tolerance = 1e-12)
  expect_equal(o$effective_half_life, log(2) * 8, tolerance = 1e-12)
  e <- morie_halft(Vd = 40, Cl = 5, route = "effective")
  expect_equal(e$half_life, o$half_life, tolerance = 1e-12)

  rt <- morie_halft_rates(10, 30, 2, 4)
  k10 <- 0.2
  k12 <- 0.4
  k21 <- 4 / 30
  ev <- sort(eigen(matrix(c(-(k10 + k12), k12, k21, -k21), 2))$values)
  expect_equal(c(rt$alpha, rt$beta), -ev, tolerance = 1e-12)
  expect_error(morie_halft_rates(0, 1, 1, 1), "positive")

  t2 <- morie_halft(Cl = 2, route = "two_compartment", V1 = 10, V2 = 30, Q = 4, dose = 50)
  a <- rt$alpha
  b <- rt$beta
  A <- (a - k21) / (10 * (a - b))
  B <- (k21 - b) / (10 * (a - b))
  expect_equal(t2$terminal_half_life, log(2) / b, tolerance = 1e-12)
  expect_equal(t2$distribution_half_life, log(2) / a, tolerance = 1e-12)
  expect_equal(c(t2$A, t2$B), 50 * c(A, B), tolerance = 1e-12)
  # AUC of the biexponential equals dose / CL
  expect_equal(t2$auc, 50 / 2, tolerance = 1e-12)
  expect_equal(t2$mean_residence_time, 40 / 2, tolerance = 1e-12)
  expect_error(morie_halft(Cl = 1, route = "two_compartment", V1 = 1), "needs V1, V2 and Q")
  expect_error(morie_halft(Vd = 1, Cl = 0), "Cl must")
  expect_error(morie_halft(Vd = 1, Cl = 1, route = "iv"), "route must")
  expect_match(morie_halft_cheatsheet(), "two_compartment")
})

test_that("Hampel and Hampw are the three-part redescender and its weight", {
  x <- c(-9, -5, -3, -1, 0.5, 2, 3.5, 6, 8.5)
  r <- Hampel(x, 2, 4, 8)
  u <- abs(x)
  psi <- sign(x) * ifelse(u <= 2, u, ifelse(u <= 4, 2, ifelse(u <= 8, 2 * (8 - u) / 4, 0)))
  expect_equal(r$psi, psi, tolerance = 1e-12)
  expect_equal(r$psi_deriv, ifelse(u <= 2, 1, ifelse(u <= 4, 0, ifelse(u <= 8, -0.5, 0))))
  expect_equal(r$n_reject, sum(u > 8))
  w <- Hampw(x, 2, 4, 8)
  expect_equal(w$weights, ifelse(u == 0, 1, psi / x), tolerance = 1e-12)
  expect_equal(w$n_zero, sum(u > 8))
  expect_error(Hampel(x, 3, 2, 8), "0 < a <= b < c")
  expect_error(Hampw(numeric(0)), "empty")
})

test_that("morie_hapblk computes D' and the Gabriel confidence bounds", {
  H <- rbind(c(0, 0, 0, 1), c(0, 0, 0, 0), c(1, 1, 1, 0), c(1, 1, 1, 1),
             c(0, 0, 1, 1), c(1, 1, 0, 0), c(0, 0, 0, 1), c(1, 1, 1, 0))
  r <- morie_hapblk(H)
  ci <- function(a, b) {
    h <- c(sum(!H[, a] & !H[, b]), sum(!H[, a] & H[, b] == 1), sum(H[, a] == 1 & !H[, b]), sum(H[, a] & H[, b]))
    n <- sum(h)
    pA <- (h[1] + h[2]) / n
    pB <- (h[1] + h[3]) / n
    D <- h[1] / n - pA * pB
    dm <- if (D > 0) min(pA * (1 - pB), (1 - pA) * pB) else min(pA * pB, (1 - pA) * (1 - pB))
    g <- (0:200) / 200
    ll <- vapply(g, function(dp) {
      Dg <- sign(D + (D == 0)) * dp * dm
      p <- c(pA * pB + Dg, pA * (1 - pB) - Dg, (1 - pA) * pB - Dg, (1 - pA) * (1 - pB) + Dg)
      if (any(p < -1e-12)) -1e18 else sum(h * log(pmax(p, 1e-12)))
    }, 0)
    cdf <- cumsum(exp(ll - max(ll)) / sum(exp(ll - max(ll))))
    c(abs(D) / dm, g[which(cdf >= 0.05)[1]], g[which(cdf >= 0.95)[1]])
  }
  for (ab in list(c(1, 2), c(1, 3), c(3, 4))) {
    v <- ci(ab[1], ab[2])
    expect_equal(c(r$dprime[ab[1], ab[2]], r$ci_lo[ab[1], ab[2]], r$ci_hi[ab[1], ab[2]]), v, tolerance = 1e-12)
  }
  expect_equal(r$dprime[1, 2], 1)
  expect_equal(r$pair_class[1, 2], if (r$ci_hi[1, 2] > 0.98 && r$ci_lo[1, 2] > 0.7) "S" else
    if (r$ci_hi[1, 2] < 0.9) "R" else "U")
  expect_error(morie_hapblk(H[1:3, ]), "4 haplotypes")
})

test_that("Hcoreg gives the Matern type I and II retention probabilities", {
  pts <- rbind(c(0, 0), c(1, 0), c(0, 0.3), c(3, 3))
  r <- Hcoreg(pts, r = 0.5, lam = 2, model = 2)
  x <- 2 * pi * 0.25
  expect_equal(r$alpha_I, exp(-x), tolerance = 1e-12)
  expect_equal(r$alpha_II, (1 - exp(-x)) / x, tolerance = 1e-12)
  expect_equal(r$estimate, 2 * (1 - exp(-x)) / x, tolerance = 1e-12)
  d <- sort(as.numeric(dist(pts)))
  expect_equal(r$d, d, tolerance = 1e-12)
  expect_false(r$feasible)
  expect_equal(r$retained, c(0, 1, 0, 1))
  lens <- function(v) if (v >= 1) 0 else 2 * 0.25 * acos(v) - 0.5 * v * sqrt(1 - v^2)
  U <- vapply(d, function(v) 2 * pi * 0.25 - lens(v), 0)
  expect_equal(r$k_I, ifelse(d < 0.5, 0, exp(-2 * U)), tolerance = 1e-12)
  gm <- pi * 0.25
  k2 <- (2 * U * (1 - exp(-2 * gm)) - 2 * gm * (1 - exp(-2 * U))) / (4 * gm * U * (U - gm))
  expect_equal(r$k_II, ifelse(d < 0.5, 0, k2), tolerance = 1e-12)
  f <- Hcoreg(c(0, 0, 2, 2), 1, 0.5, model = 1)
  expect_true(f$feasible)
  expect_equal(f$log_density, 2 * log(0.5), tolerance = 1e-12)
  expect_equal(f$estimate, 0.5 * exp(-0.5 * pi), tolerance = 1e-12)
  expect_error(Hcoreg(pts, 0, 1), "positive")
  expect_error(Hcoreg(pts, 1, 1, model = 3), "model must be")
  expect_error(Hcoreg(1:3, 1, 1), "even length")
})

test_that("morie_hdbsc matches dbscan::hdbscan on separated blobs", {
  skip_if_not_installed("dbscan")
  set.seed(8)
  X <- rbind(matrix(rnorm(30, 0, 0.3), 15), matrix(rnorm(30, 4, 0.4), 15), matrix(rnorm(24, c(0, 5), 0.3), 12, byrow = TRUE))
  r <- morie_hdbsc(X, min_pts = 5, min_cluster_size = 5)
  ref <- dbscan::hdbscan(X, minPts = 5)
  D <- as.matrix(dist(X))
  expect_equal(r$core_distances, unname(apply(D, 1, function(v) sort(v)[5])), tolerance = 1e-12)
  expect_equal(unname(r$core_distances), unname(dbscan::kNNdist(X, k = 4)), tolerance = 1e-12)
  lab <- ifelse(r$labels < 0, 0L, r$labels + 1L)
  tb <- table(lab, ref$cluster)
  expect_equal(r$n_clusters, length(setdiff(unique(ref$cluster), 0)))
  expect_true(all(rowSums(tb > 0) == 1) && all(colSums(tb > 0) == 1))
  lf <- morie_hdbsc(X, 5, 5, selection = "leaf")
  expect_gte(lf$n_clusters, r$n_clusters)
  expect_error(morie_hdbsc(X, selection = "top"), "selection")
  expect_error(morie_hdbsc(X[1:3, ], min_pts = 5), "min_pts")
})

test_that("Hpdint scans for the shortest window", {
  v <- c(0.2, 1.5, 0.9, 3.8, 1.1, 1.3, 0.7, 2.2, 1.0, 5.0)
  r <- Hpdint(v, alpha = 0.2)
  s <- sort(v)
  j <- ceiling(0.8 * 10) - 1
  w <- s[(j + 1):10] - s[1:(10 - j)]
  i <- which.min(w)
  expect_equal(r$width, w[i], tolerance = 1e-12)
  expect_equal(c(r$lo, r$hi), c(s[i], s[i + j]), tolerance = 1e-12)
  expect_equal(c(r$eq_lo, r$eq_hi), unname(quantile(v, c(0.1, 0.9))), tolerance = 1e-12)
})

test_that("Hdpmix, Hdpgmm and Hdplda follow the HDP posterior-mean updates", {
  beta <- Stickw(1.5, 4)$pi
  beta <- beta / sum(beta)
  z <- c(0, 1, 1, 3, 0, 2, 2, 2)
  g <- c("a", "a", "a", "a", "b", "b", "b", "b")
  m <- Hdpmix(z, g, gamma = 1.5, alpha = 2, truncation = 4)
  ca <- tabulate(z[g == "a"] + 1, 4)
  cb <- tabulate(z[g == "b"] + 1, 4)
  expect_equal(m$beta, beta, tolerance = 1e-12)
  expect_equal(m$pi[[1]], (2 * beta + ca) / (2 + 4), tolerance = 1e-12)
  expect_equal(m$pi[[2]], (2 * beta + cb) / 6, tolerance = 1e-12)
  expect_equal(m$shared, sum(ca > 0 & cb > 0))

  y <- c(-2.1, -1.9, -2.3, 0.1, 2.0, 2.2, 1.8, -0.2)
  one <- Hdpgmm(y, g, gamma = 1, alpha = 1, truncation = 3, max_iter = 1)
  b3 <- Stickw(1, 3)$pi
  b3 <- b3 / sum(b3)
  mu <- -2.3 + 4.5 * (0:2 + 0.5) / 3
  sdv <- rep(4.5 / 3, 3)
  L <- sapply(1:3, function(t) b3[t] * dnorm(y, mu[t], sdv[t]))
  expect_equal(one$loglik, sum(log(rowSums(L))), tolerance = 1e-12)
  R <- L / rowSums(L)
  gi <- match(g, c("a", "b"))
  pin <- t(sapply(1:2, function(j) (b3 + colSums(R[gi == j, ])) / (1 + 4)))
  expect_equal(one$pi, pin, tolerance = 1e-12)
  nk <- colSums(R)
  mu1 <- colSums(R * y) / nk
  expect_equal(one$mu, mu1, tolerance = 1e-12)
  expect_equal(one$sigma, sqrt(colSums(R * outer(y, mu1, "-")^2) / (nk + b3)), tolerance = 1e-12)

  docs <- list(c(0, 1, 1, 2), c(2, 3, 3), c(0, 3))
  lda <- Hdplda(docs, gamma = 1, alpha = 1, truncation = 2, eta = 0.5, max_iter = 1)
  b2 <- Stickw(1, 2)$pi
  b2 <- b2 / sum(b2)
  phi <- outer(0:1, 0:3, function(t, w) 1 + ((t * 7 + w * 3) %% 5))
  phi <- phi / rowSums(phi)
  ll <- 0
  cnt <- matrix(0, 2, 4)
  post <- matrix(0, 3, 2)
  for (j in 1:3) for (w in docs[[j]]) {
    p <- b2 * phi[, w + 1]
    ll <- ll + log(sum(p))
    cnt[, w + 1] <- cnt[, w + 1] + p / sum(p)
    post[j, ] <- post[j, ] + p / sum(p)
  }
  expect_equal(lda$loglik, ll, tolerance = 1e-12)
  expect_equal(lda$phi, (cnt + 0.5) / rowSums(cnt + 0.5), tolerance = 1e-12)
  expect_equal(lda$theta, (matrix(b2, 3, 2, byrow = TRUE) + post) / (1 + lengths(docs)), tolerance = 1e-12)
  expect_equal(lda$n_vocab, 4L)
})

test_that("Adcore and Adstat compute the Anderson-Darling A^2", {
  y <- c(0.3, -1.2, 0.8, 1.9, -0.4, 0.05)
  u <- sort(pnorm(y))
  n <- 6
  a2 <- -n - sum((2 * (1:n) - 1) * (log(u) + log(1 - rev(u)))) / n
  expect_equal(Adcore(pnorm(y)), a2, tolerance = 1e-12)
  s <- Adstat(y, pnorm)
  expect_equal(s$statistic, a2, tolerance = 1e-12)
  expect_equal(s$u, u, tolerance = 1e-12)
  expect_equal(Adcore(c(0, 0.5)), Inf)
  expect_error(Adcore(0.5), "at least 2")
})

test_that("hellie, morie_hellinger_distance and hellngd", {
  p <- c(1, 2, 3, 4)
  q <- c(4, 3, 2, 1)
  pn <- p / 10
  qn <- q / 10
  h <- sqrt(sum((sqrt(pn) - sqrt(qn))^2) / 2)
  r <- hellie(p, q)
  expect_equal(r$estimate, h, tolerance = 1e-12)
  expect_equal(r$bc, sum(sqrt(pn * qn)), tolerance = 1e-12)
  expect_equal(r$h2, 1 - r$bc, tolerance = 1e-12)
  expect_equal(morie_hellinger_distance(p, q)$estimate, h, tolerance = 1e-12)
  expect_equal(hellngd(p, q)$estimate, h, tolerance = 1e-12)
  raw <- hellie(c(0.2, 0.3), c(0.1, 0.1, 0.9), normalise = FALSE)
  expect_equal(raw$n, 2L)
  expect_equal(raw$estimate, sqrt(sum((sqrt(c(0.2, 0.3)) - sqrt(c(0.1, 0.1)))^2) / 2), tolerance = 1e-12)
})

test_that("hermitS and morie_hermite_basis build Hermite polynomials", {
  x <- c(-1.5, 0, 0.7, 2)
  r <- hermitS(x, K = 4)
  H <- cbind(1, 2 * x, 4 * x^2 - 2, 8 * x^3 - 12 * x, 16 * x^4 - 48 * x^2 + 12)
  expect_equal(r$basis, H, tolerance = 1e-12)
  expect_equal(r$top, H[4, 5], tolerance = 1e-12)
  p <- morie_hermite_basis(x, K = 3, kind = "probabilist")
  expect_equal(p$basis, unname(cbind(1, x, x^2 - 1, x^3 - 3 * x)), tolerance = 1e-12)
  expect_equal(hermitS(x, 0)$basis, matrix(1, 4, 1))
})
