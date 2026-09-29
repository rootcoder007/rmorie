# Coverage for the Schabenberger & Gotway spatial fill-ins and their
# non-book companions. Each statistic is recomputed from its printed
# equation with base R (dist, fft, eigen, solve, lm.fit, stats::medpolish)
# on a small deterministic configuration.

.coords <- cbind(c(0, 1, 2, 0.5, 1.5, 2.5, 0.2), c(0, 0.3, 0, 1, 1.2, 0.9, 2))
.z <- c(1.2, 0.4, 2.1, 1.7, 0.9, 2.6, 0.1)
.wrook <- function(n) {
  W <- matrix(0, n, n)
  for (i in seq_len(n - 1)) W[i, i + 1] <- W[i + 1, i] <- 1
  W
}

test_that("SpAcf bins pair products of deviations by distance", {
  r <- SpAcf(.coords, .z, bins = 3)
  d <- .z - mean(.z)
  c0 <- mean(d^2)
  D <- as.matrix(stats::dist(.coords))
  up <- which(upper.tri(D), arr.ind = TRUE)
  h <- D[up]
  pr <- d[up[, 1]] * d[up[, 2]]
  edges <- max(h) * (1:3) / 3
  lo <- c(0, edges[-3])
  cv <- vapply(1:3, function(k) mean(pr[h > lo[k] & h <= edges[k]]), 1)
  expect_equal(r$cov, cv, tolerance = 1e-12)
  expect_equal(r$acf, cv / c0, tolerance = 1e-12)
  expect_equal(r$npairs, vapply(1:3, function(k) sum(h > lo[k] & h <= edges[k]), 1))
  e2 <- SpAcf(.coords, .z, bins = c(0.5, 1, 3))
  expect_equal(e2$lags, c(0.5, 1, 3))
  expect_error(SpAcf(.coords, .z, bins = c(1, 0.5)), "must increase")
  expect_error(SpAcf(.coords, rep(1, 7)), "constant")
})

test_that("LisaI is eq (1.17) and sums to w.. times global I", {
  W <- .wrook(7)
  r <- LisaI(.z, W)
  d <- .z - mean(.z)
  li <- 7 * d * as.numeric(W %*% d) / sum(d^2)
  expect_equal(r$local, li, tolerance = 1e-12)
  gi <- 7 * sum(W * outer(d, d)) / (sum(W) * sum(d^2))
  expect_equal(r$global_i, gi, tolerance = 1e-12)
  expect_equal(sum(r$local), sum(W) * gi, tolerance = 1e-12)
  expect_equal(r$expectation, -rowSums(W) / 6, tolerance = 1e-12)
  W2 <- W
  diag(W2) <- 1
  expect_error(LisaI(.z, W2), "zero diagonal")
})

test_that("MantelM2 and MantelZ: eqs (1.4)-(1.5) and the Gaussian moments", {
  r <- MantelM2(.coords, .z)
  W <- as.matrix(stats::dist(.coords))
  U <- abs(outer(.z, .z, "-"))
  expect_equal(r$m2, sum(W * U), tolerance = 1e-12)
  expect_equal(r$m1, sum((W * U)[upper.tri(W)]), tolerance = 1e-12)
  expect_equal(r$beta, sum(W * U) / sum(W^2), tolerance = 1e-12)
  Wr <- .wrook(7)
  r2 <- MantelM2(NULL, .z, w = Wr)
  expect_equal(r2$m2, sum(Wr * U), tolerance = 1e-12)
  z <- MantelZ(.coords, .z, Wr)
  d <- .z - mean(.z)
  s2 <- sum(d^2) / 6
  P <- diag(7) - 1 / 7
  AM <- Wr %*% P
  ex <- s2 * sum(diag(AM))
  vr <- 2 * s2^2 * sum(diag(AM %*% AM))
  expect_equal(z$m2, sum(Wr * outer(d, d)), tolerance = 1e-12)
  expect_equal(c(z$expectation, z$variance), c(ex, vr), tolerance = 1e-12)
  expect_equal(z$p_value, 2 * stats::pnorm(-abs((z$m2 - ex) / sqrt(vr))), tolerance = 1e-12)
  zu <- MantelZ(.coords, .z, Wr, u = U - diag(diag(U)))
  expect_false(zu$gaussian_moments_apply)
  expect_true(is.na(zu$z))
  expect_error(MantelM2(.coords[1:3, ], .z), "rows")
})

test_that("Pcf differences the border-corrected K and divides by 2 pi h", {
  pts <- cbind(c(0.1, 0.4, 0.8, 0.3, 0.9, 0.55, 0.2, 0.7), c(0.2, 0.7, 0.3, 0.45, 0.85, 0.5, 0.9, 0.1))
  rr <- c(0.05, 0.1, 0.15, 0.2)
  r <- Pcf(pts, region = rbind(c(0, 1), c(0, 1)), r = rr)
  D <- as.matrix(stats::dist(pts))
  bd <- pmin(pts[, 1], 1 - pts[, 1], pts[, 2], 1 - pts[, 2])
  K <- vapply(rr, function(h) {
    keep <- which(bd > h)
    (sum(D[, keep] <= h & D[, keep] > 0) / length(keep)) / 8
  }, 1)
  expect_equal(r$k, K, tolerance = 1e-12)
  der <- c((K[2] - K[1]) / 0.05, (K[3] - K[1]) / 0.1, (K[4] - K[2]) / 0.1, (K[4] - K[3]) / 0.05)
  expect_equal(r$pcf, der / (2 * pi * rr), tolerance = 1e-12)
  rn <- Pcf(pts, region = rbind(c(0, 1), c(0, 1)), r = rr, correction = "none")
  expect_equal(rn$k, vapply(rr, function(h) (sum(D <= h & D > 0) / 8) / 8, 1), tolerance = 1e-12)
  expect_error(Pcf(pts, r = rr, correction = "ripley"), "border")
  expect_error(Pcf(pts, r = 0.1), "at least 2 radii")
})

test_that("SphVario is eq (4.15) with the nugget and gamma(0) = 0", {
  h <- c(0, 0.5, 1, 2, 3.5)
  r <- SphVario(h, c0 = 0.3, c = 2, a = 2)
  u <- pmin(h / 2, 1)
  s <- 1.5 * u - 0.5 * u^3
  expect_equal(r$gamma, ifelse(h == 0, 0, 0.3 + 2 * s), tolerance = 1e-12)
  expect_equal(r$cov, ifelse(h == 0, 2.3, 2 * (1 - s)), tolerance = 1e-12)
  expect_error(SphVario(1, a = 0), "must be positive")
  expect_error(SphVario(-1), "non-negative")
})

test_that("Pgram, SmPgram and CrossSpec agree with stats::fft", {
  y <- c(2, 5, 3, 7, 4, 6, 1, 8, 5)
  r <- Pgram(y)
  d <- y - mean(y)
  js <- setdiff(-4:4, 0)
  expect_equal(r$omega, 2 * pi * js / 9, tolerance = 1e-12)
  I <- Mod(stats::fft(d))^2 / (2 * pi * 9)
  expect_equal(r$periodogram, I[(js %% 9) + 1], tolerance = 1e-12)
  expect_equal(r$from_covariance, r$periodogram, tolerance = 1e-12)
  expect_equal(r$acov[1:3], vapply(0:2, function(k) sum(d[1:(9 - k)] * d[(1 + k):9]) / 9, 1), tolerance = 1e-12)
  s <- SmPgram(y, span = 3)
  raw <- r$periodogram
  expect_equal(s$smoothed, (raw + c(raw[8], raw[-8]) + c(raw[-1], raw[1])) / 3, tolerance = 1e-12)
  expect_error(SmPgram(y, span = 2), "odd positive")
  expect_error(Pgram(rep(3, 5)), "constant")
  x2 <- c(1, 3, 2, 5, 4, 3, 6, 5)
  y2 <- c(2, 2, 4, 3, 6, 5, 5, 8)
  cs <- CrossSpec(x2, y2)
  X <- stats::fft(x2 - mean(x2))
  Y <- stats::fft(y2 - mean(y2))
  S <- (X * Conj(Y))[2:5] / (2 * pi * 8)
  expect_equal(cs$cospectrum, Re(S), tolerance = 1e-12)
  expect_equal(cs$quadrature, -Im(S), tolerance = 1e-12)
  expect_equal(cs$phase, Arg(S), tolerance = 1e-12)
  expect_error(CrossSpec(x2, y2[-1]), "same length")
})

test_that("MsCoh is Welch-averaged magnitude-squared coherence", {
  set.seed(6)
  x <- stats::rnorm(64)
  y <- 0.7 * x + 0.3 * stats::rnorm(64)
  r <- MsCoh(x, y, nperseg = 16, overlap = 0.5)
  win <- 0.5 - 0.5 * cos(2 * pi * (0:15) / 15)
  st <- seq(0, 48, by = 8)
  sxy <- sxx <- syy <- 0
  for (s in st) {
    a <- stats::fft((x[s + 1:16] - mean(x[s + 1:16])) * win)[2:9]
    b <- stats::fft((y[s + 1:16] - mean(y[s + 1:16])) * win)[2:9]
    sxx <- sxx + Mod(a)^2
    syy <- syy + Mod(b)^2
    sxy <- sxy + a * Conj(b)
  }
  expect_equal(r$coherence, Mod(sxy)^2 / (sxx * syy), tolerance = 1e-12)
  expect_equal(r$sxx, sxx / 7, tolerance = 1e-12)
  expect_identical(r$n_segments, 7L)
  expect_error(MsCoh(x, y, nperseg = 64), "fewer than 2 segments")
  expect_error(MsCoh(x, y, overlap = 1), "overlap")
})

test_that("SpecRad and SpecClust: spectral radius and normalised-Laplacian clusters", {
  W <- .wrook(5)
  r <- SpecRad(W)
  ev <- eigen(W, symmetric = TRUE)
  expect_equal(r$rho, max(abs(ev$values)), tolerance = 1e-12)
  expect_equal(r$sar_rho_bound, 1 / max(abs(ev$values)), tolerance = 1e-12)
  expect_equal(abs(r$eigenvector), abs(ev$vectors[, 1]), tolerance = 1e-9)
  expect_error(SpecRad(matrix(c(0, 1, 0, 0), 2)), "symmetric")
  A <- matrix(0, 6, 6)
  A[1:3, 1:3] <- 1
  A[4:6, 4:6] <- 1
  diag(A) <- 0
  A[3, 4] <- A[4, 3] <- 0.1
  sc <- SpecClust(A, 2)
  L <- diag(6) - diag(1 / sqrt(rowSums(A))) %*% A %*% diag(1 / sqrt(rowSums(A)))
  expect_equal(sc$eigenvalues, sort(eigen(L, symmetric = TRUE)$values)[1:2], tolerance = 1e-9)
  expect_true(length(unique(sc$labels[1:3])) == 1 && length(unique(sc$labels[4:6])) == 1 && sc$labels[1] != sc$labels[4])
  expect_equal(sc$sizes, c(3, 3))
  A0 <- A
  A0[6, ] <- A0[, 6] <- 0
  expect_error(SpecClust(A0, 2), "degree 0")
  expect_error(SpecClust(A, 7), "`k` must lie")
})

test_that("SpErrMod minimises the concentrated SAR-error -2 log-likelihood", {
  set.seed(9)
  n <- 12
  W <- .wrook(n)
  X <- cbind(1, stats::rnorm(n))
  e <- solve(diag(n) - 0.35 * W, stats::rnorm(n, sd = 0.5))
  y <- as.numeric(X %*% c(1, 2) + e)
  r <- SpErrMod(X, y, W)
  f <- function(rho) {
    A <- diag(n) - rho * W
    fit <- stats::lm.fit(A %*% X, as.numeric(A %*% y))
    s2 <- sum(fit$residuals^2) / n
    list(v = n * log(2 * pi * s2) + n - 2 * log(abs(det(A))), b = fit$coefficients, s2 = s2)
  }
  fr <- f(r$rho)
  expect_equal(r$neg2loglik, fr$v, tolerance = 1e-9)
  expect_equal(unname(r$beta), unname(fr$b), tolerance = 1e-9)
  expect_equal(r$sigma2, fr$s2, tolerance = 1e-9)
  # golden section leaves rho within its bracket; the objective is flat to
  # 1e-9 there, and no grid point on the admissible interval does better
  grid <- seq(r$rho_bounds[1], r$rho_bounds[2], length.out = 41)
  expect_true(all(vapply(grid, function(g) f(g)$v, 1) >= r$neg2loglik - 1e-9))
  expect_equal(unname(r$ols_beta), unname(stats::lm.fit(X, y)$coefficients), tolerance = 1e-9)
  expect_equal(r$spectral_radius, max(abs(eigen(W)$values)), tolerance = 1e-12)
  expect_error(SpErrMod(X, y, W + diag(0, n) + upper.tri(W) * 0.1), "symmetric")
})

test_that("SpKappa scores agreement over neighbour pairs", {
  x <- c(1, 1, 2, 2, 3, 1)
  y <- c(1, 2, 2, 2, 3, 3)
  W <- .wrook(6)
  r <- SpKappa(x, y, W)
  po <- sum(W * outer(x, y, "==")) / sum(W)
  pe <- sum(vapply(1:3, function(k) sum(rowSums(W)[x == k]) / sum(W) * sum(colSums(W)[y == k]) / sum(W), 1))
  expect_equal(c(r$p_observed, r$p_expected), c(po, pe), tolerance = 1e-12)
  expect_equal(r$kappa, (po - pe) / (1 - pe), tolerance = 1e-12)
  expect_error(SpKappa(x + 0.5, y, W), "integer category codes")
  expect_error(SpKappa(x, y, -W), "non-negative")
})

test_that("MedPolish matches stats::medpolish run for the same sweeps", {
  Y <- matrix(c(3, 5, 9, 2, 8, 7, 4, 6, 1, 12, 5, 7), 3, 4)
  r <- MedPolish(Y, iters = 4)
  # eps = -1 disables the convergence stop, so both run exactly 4 sweeps
  ref <- suppressWarnings(stats::medpolish(Y, eps = -1, maxiter = 4, trace.iter = FALSE))
  expect_equal(r$overall, ref$overall, tolerance = 1e-12)
  expect_equal(r$row, ref$row, tolerance = 1e-12)
  expect_equal(r$col, ref$col, tolerance = 1e-12)
  expect_equal(unname(r$residuals), unname(ref$residuals), tolerance = 1e-12)
  expect_equal(r$fitted + r$residuals, Y, tolerance = 1e-12)
  g <- MedPolish(as.numeric(t(Y)), grid = c(3, 4), iters = 4)
  expect_equal(g$overall, r$overall, tolerance = 1e-12)
  expect_error(MedPolish(1:5, grid = c(2, 3)), "asks for 6")
  expect_error(MedPolish(matrix(1:3, 1)), "2 by 2")
})

test_that("ShrinkPred: cluster-size-specific shrinkage toward the GLS mean", {
  y <- c(3, 4, 5, 10, 12, 1, 2, 2, 3, 2)
  cl <- c(1, 1, 1, 2, 2, 3, 3, 3, 3, 3)
  r <- ShrinkPred(y, cl, sigma2_u = 2, sigma2_e = 4)
  nj <- c(3, 2, 5)
  yb <- c(4, 11, 2)
  lam <- 4 / (4 + nj * 2)
  w <- nj / (4 + nj * 2)
  mu <- sum(w * yb) / sum(w)
  expect_equal(r$lambda, lam, tolerance = 1e-12)
  expect_equal(r$grand_mean, mu, tolerance = 1e-12)
  expect_equal(r$shrunk, mu + (1 - lam) * (yb - mu), tolerance = 1e-12)
  expect_error(ShrinkPred(y, rep(1, 10), 1, 1), "at least 2 clusters")
  expect_error(ShrinkPred(y, cl, 1, 0), "sigma2_e")
})

test_that("SparseVector, SpecDec and SpikeInfo closed forms", {
  q <- c(0.2, 1.5, 0.3, 2.2, 3, 0.1)
  r <- SparseVector(q, threshold = 1, c = 2, epsilon = 0.5, threshold_noise = 0.1, query_noise = c(0, -0.7, 0, 0, 0, 5))
  expect_identical(r$above, c(FALSE, FALSE, FALSE, TRUE, TRUE, NA))
  expect_equal(r$released, c(NA, NA, NA, 2.2, 3, NA))
  expect_identical(r$halted_at, 5L)
  expect_equal(unname(r$noise_scales), c(4, 8))
  expect_error(SparseVector(q, 1, c = 7), "exceeds the number of queries")
  p <- c(0.5, 0.3, 0.2)
  qd <- c(0.2, 0.5, 0.3)
  sd <- SpecDec(qd, p, gamma = 3)
  a <- sum(pmin(p, qd))
  expect_equal(sd$alpha, a, tolerance = 1e-12)
  expect_equal(sd$expected_tokens, (1 - a^4) / (1 - a), tolerance = 1e-12)
  expect_equal(SpecDec(p, p, 3)$expected_tokens, 4)
  expect_error(SpecDec(c(0.5, 0.4), c(0.5, 0.5)), "must sum to 1")
  spike <- c(0, 1, 3, 5, 2, 7, 1, 6, 0, 4)
  stim <- c(0, 0, 1, 1, 0, 1, 0, 1, 0, 1)
  si <- SpikeInfo(spike, stim, nbins = 2)
  edge <- sort(spike)[5]
  code <- as.integer(spike > edge)
  H <- function(v) {
    p <- table(v) / length(v)
    -sum(p * log2(p))
  }
  hn <- sum(vapply(0:1, function(s) mean(stim == s) * H(code[stim == s]), 1))
  expect_equal(si$information, H(code) - hn, tolerance = 1e-12)
  expect_error(SpikeInfo(spike, rep(0, 10)), "2 stimulus classes")
})

test_that("SpecAnom keeps the phase and rebuilds from the residual log-amplitude", {
  x <- c(sin(2 * pi * (0:31) / 8), 0)
  x[20] <- 3
  r <- SpecAnom(x, q = 3)
  X <- stats::fft(x)
  lg <- log(Mod(X))
  n <- length(x)
  sm <- (lg + c(lg[n], lg[-n]) + c(lg[-1], lg[1])) / 3
  res <- lg - sm
  rec <- Re(stats::fft(exp(res) * exp(1i * Arg(X)), inverse = TRUE)) / n
  expect_equal(r$residual, res, tolerance = 1e-10)
  expect_equal(r$saliency, rec^2, tolerance = 1e-10)
  expect_identical(r$peak_index, which.max(rec^2) - 1L)
  expect_error(SpecAnom(x, q = 4), "odd positive")
  expect_error(SpecAnom(1:5), "at least 8")
})

test_that("SpGam solves the thin-plate saddle-point system", {
  cc <- .coords
  y <- .z
  K <- as.matrix(stats::dist(cc))
  K <- ifelse(K > 0, K^2 * log(K), 0)
  Tm <- cbind(1, cc)
  for (lam in c(0, 0.05)) {
    A <- rbind(cbind(K + diag(7 * lam, 7), Tm), cbind(t(Tm), matrix(0, 3, 3)))
    sol <- unname(solve(A, c(y, 0, 0, 0)))
    r <- SpGam(y, NULL, cc, lam = lam)
    expect_equal(r$spline_weights, sol[1:7], tolerance = 1e-9)
    expect_equal(r$coef, sol[8:10], tolerance = 1e-9)
    expect_equal(r$fitted, as.numeric(K %*% sol[1:7] + Tm %*% sol[8:10]), tolerance = 1e-9)
  }
  expect_equal(SpGam(y, NULL, cc)$fitted, y, tolerance = 1e-9)
  xcov <- c(0.3, 1.1, -0.4, 0.8, 0.2, -1, 0.5)
  rx <- SpGam(y, xcov, cc, lam = 0.1)
  expect_length(rx$coef, 4L)
  expect_error(SpGam(y, NULL, cc, lam = -1), "non-negative")
  expect_error(SpGam(y[1:3], NULL, cc[1:3, ]), "more sites")
})

test_that("ShiftInt weights by the back-shifted Gaussian density ratio", {
  y <- c(2.1, 3.4, 1.8, 4.2, 3.9, 2.7, 3.1, 4.8)
  a <- c(1, 2, 0.5, 2.5, 2.2, 1.4, 1.9, 3)
  h <- c(0.2, 0.8, 0.1, 1.1, 0.9, 0.4, 0.7, 1.3)
  r <- ShiftInt(y, a, h, delta = 0.5)
  fit <- stats::lm.fit(cbind(1, h), a)
  tau2 <- sum(fit$residuals^2) / 6
  mu <- as.numeric(cbind(1, h) %*% fit$coefficients)
  w <- stats::dnorm(a - 0.5, mu, sqrt(tau2)) / stats::dnorm(a, mu, sqrt(tau2))
  expect_equal(r$weights, w, tolerance = 1e-12)
  expect_equal(r$psi, mean(w * y), tolerance = 1e-12)
  expect_equal(r$tau2, tau2, tolerance = 1e-12)
  expect_equal(ShiftInt(y, a, h, 0.5, trim = 1.1)$weights, pmin(w, 1.1), tolerance = 1e-12)
  expect_error(ShiftInt(y, a, h, trim = 0), "`trim` must be positive")
  expect_error(ShiftInt(y, a[-1], h), "same length")
})
