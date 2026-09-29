# Coverage for ropedy .. rtwall exports. Every expectation is recomputed in
# the test body.

test_that("Ropedy rotates coordinate pairs by position-dependent angles", {
  q <- c(0.5, -1.2, 0.8, 0.3, -0.4, 1.1)
  r <- Ropedy(NULL, q, m = 7, theta = 100)
  fr <- 100^(-2 * (0:2) / 6)
  z <- complex(real = q[c(1, 3, 5)], imaginary = q[c(2, 4, 6)]) * exp(1i * 7 * fr)
  expect_equal(r$estimate, as.numeric(rbind(Re(z), Im(z))), tolerance = 1e-12)
  expect_equal(r$freqs, fr, tolerance = 1e-12)
  # rotation preserves the norm of every pair
  expect_equal(sum(r$estimate^2), sum(q^2), tolerance = 1e-12)
  s <- Ropedy(NULL, q, m = 7, theta = 100, L_new = 4096, L_train = 1024)
  expect_equal(s$theta_base, 100 * 4^(6 / 4), tolerance = 1e-10)
  expect_equal(Ropedy(NULL, q, m = 0)$estimate, q)
})

test_that("Rosenbaum signed-rank bounds over a Gamma grid", {
  d <- c(1.2, -0.4, 2.1, 0.8, 1.5, -0.9, 3.0, 0.6, 1.1, 0, 2.4, -0.2)
  dd <- d[d != 0]
  rk <- rank(abs(dd))
  W <- sum(rk[dd > 0])
  bound <- function(G) {
    pp <- G / (1 + G)
    pm <- 1 / (1 + G)
    c(stats::pnorm((W - pp * sum(rk)) / sqrt(pp * (1 - pp) * sum(rk^2)), lower.tail = FALSE),
      stats::pnorm((W - pm * sum(rk)) / sqrt(pm * (1 - pm) * sum(rk^2)), lower.tail = FALSE))
  }
  gs <- c(1, 2, 4)
  r <- morie_rosenb(d, Gamma_grid = gs)
  b <- vapply(gs, bound, numeric(2))
  expect_equal(r$p_upper, b[1, ], tolerance = 1e-12)
  expect_equal(r$p_lower, b[2, ], tolerance = 1e-12)
  expect_equal(r$W, W)
  expect_equal(r$n_pairs, 11L)
  expect_equal(r$gamma_critical, max(gs[b[1, ] <= 0.05]))
  r2 <- Rosenb(d, Gamma_grid = gs)
  expect_equal(r2$p_upper, b[1, ], tolerance = 1e-12)
  expect_null(morie_rosenb(d, Gamma_grid = 1, alpha = 1e-9)$gamma_critical)
  expect_error(morie_rosenb(d, Gamma_grid = 0.5), "at least 1")
  expect_error(Rosenb(d, Gamma_grid = numeric(0)), "empty")
  expect_error(morie_rosenb(c(0, 0)), "no non-zero")
})

test_that("Rotatesc is the RotatE distance ||h o r - t||", {
  h <- complex(real = c(0.5, -0.2, 1.0), imaginary = c(0.1, 0.7, -0.3))
  th <- c(0.4, -1.2, 2.0)
  tt <- complex(real = c(0.3, 0.6, -0.8), imaginary = c(0.4, -0.1, 0.2))
  r <- Rotatesc(NULL, h_re = Re(h), h_im = Im(h), theta = th, t_re = Re(tt), t_im = Im(tt), gamma = 3)
  diff <- h * exp(1i * th) - tt
  expect_equal(r$per_dim, Mod(diff), tolerance = 1e-12)
  expect_equal(r$distance, sqrt(sum(Mod(diff)^2)), tolerance = 1e-12)
  expect_equal(r$score, 3 - r$distance, tolerance = 1e-12)
  flat <- Rotatesc(c(Re(h), Im(h), th, Re(tt), Im(tt)))
  expect_equal(flat$distance, r$distance, tolerance = 1e-12)
  expect_true(is.nan(flat$score))
})

test_that("RDP of Gaussian and Laplace mechanisms and the (eps, delta) conversion", {
  expect_equal(morie_rdp_gaussian(5, 2, 1.5), 5 * 2.25 / 8, tolerance = 1e-12)
  a <- 4
  lam <- 0.8
  lap <- log(a / (2 * a - 1) * exp((a - 1) / lam) + (a - 1) / (2 * a - 1) * exp(-a / lam)) / (a - 1)
  expect_equal(morie_rdp_laplace(a, lam), lap, tolerance = 1e-12)
  expect_equal(morie_rdp_laplace(a, 1.6, sensitivity = 2), lap, tolerance = 1e-12)
  expect_error(morie_rdp_laplace(1, 1), "alpha must exceed 1")
  expect_error(morie_rdp_gaussian(2, 0), "sigma must be positive")
  al <- c(2, 4, 8, 16)
  r <- morie_rpgad(al, mechanism = "gaussian", sigma = 3, n_compositions = 10, delta = 1e-6)
  eps <- 10 * al / 18 + log(1e6) / (al - 1)
  expect_equal(r$epsilons, eps, tolerance = 1e-12)
  expect_equal(r$epsilon, min(eps), tolerance = 1e-12)
  expect_equal(r$best_alpha, al[which.min(eps)])
  rs <- morie_rpgad(c(2, 3), epsilon_R = 0.5, delta = 0.01)
  expect_equal(rs$epsilons, 0.5 + log(100) / c(1, 2), tolerance = 1e-12)
  rl <- morie_rpgad(4, mechanism = "laplace", lam = 0.8)
  expect_equal(rl$rdp_epsilons, lap, tolerance = 1e-12)
  expect_error(morie_rpgad(1), "exceed 1")
  expect_error(morie_rpgad(2, delta = 0, epsilon_R = 1), "delta must lie")
  expect_error(morie_rpgad(2), "give either")
  expect_error(morie_rpgad(2, mechanism = "gaussian"), "needs sigma")
  expect_error(morie_rpgad(2, mechanism = "exp", sigma = 1), "gaussian or laplace")
  expect_error(morie_rpgad(c(2, 3), epsilon_R = c(1, 2, 3)), "epsilon_R")
})

test_that("Rpnlt integrates squared derivatives of the basis", {
  tt <- seq(0, 1, length.out = 11)
  h <- 0.1
  trap <- function(f) h * (sum(f) - (f[1] + f[11]) / 2)
  # second differences are exact on cubics, first differences on quadratics
  B2 <- cbind(1, tt, tt^2, tt^3)
  D2 <- cbind(0, 0, 2, 6 * tt)
  r2 <- Rpnlt(B2, 0.5)
  P2 <- outer(1:4, 1:4, Vectorize(function(i, j) trap(D2[, i] * D2[, j])))
  expect_equal(r2$P, P2, tolerance = 1e-9)
  expect_equal(r2$penalty, 0.5 * sum(P2), tolerance = 1e-9)
  B1 <- cbind(1, tt, tt^2)
  D1 <- cbind(0, 1, 2 * tt)
  r1 <- Rpnlt(B1, 2, p = 1)
  expect_equal(r1$P, outer(1:3, 1:3, Vectorize(function(i, j) trap(D1[, i] * D1[, j]))), tolerance = 1e-9)
  expect_error(Rpnlt(B2[1:3, ], 1), "four grid points")
  expect_error(Rpnlt(B2, -1), "non-negative")
  expect_error(Rpnlt(B2, 1, p = 3), "p must be 1 or 2")
  expect_error(Rpnlt(B2, 1, a = 1, b = 0), "positive width")
})

test_that("Rrand is randomized response with a debiased rate", {
  bits <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1)
  q <- 1 / (1 + exp(0.7))
  s <- 1
  rel <- numeric(10)
  for (i in 1:10) {
    s <- (48271 * s) %% 2147483647
    rel[i] <- if (s / 2147483647 < q) 1 - bits[i] else bits[i]
  }
  r <- Rrand(bits, epsilon = 0.7)
  expect_equal(r$released, rel)
  expect_equal(r$estimate, (mean(rel) - q) / (1 - 2 * q), tolerance = 1e-12)
  expect_equal(r$true_rate, 0.6)
})

test_that("Snpblup and RR-BLUP solve Henderson's mixed model equations", {
  M <- rbind(c(0, 1, 2, 1), c(1, 1, 0, 2), c(2, 0, 1, 1), c(1, 2, 1, 0), c(0, 0, 2, 2), c(2, 1, 1, 0),
             c(1, 0, 0, 1))
  X <- cbind(1, c(0, 1, 0, 1, 1, 0, 1))
  y <- c(3.1, 4.2, 2.8, 4.9, 3.3, 3.6, 2.5)
  mme <- function(lam) {
    A <- rbind(cbind(crossprod(X), crossprod(X, M)), cbind(crossprod(M, X), crossprod(M) + diag(lam, 4)))
    solve(A, c(crossprod(X, y), crossprod(M, y)))
  }
  s <- Snpblup(X, y, M, sigma2_m = 0.4, sigma2_e = 1.2)
  sol <- mme(1.2 / 0.4)
  expect_equal(s$beta, sol[1:2], tolerance = 1e-10)
  expect_equal(s$marker_effects, sol[3:6], tolerance = 1e-10)
  expect_equal(s$gebv, as.numeric(M %*% sol[3:6]), tolerance = 1e-10)
  r <- morie_rrblpr_rr_blup(y, M, lam = 2.5, X = X, M_new = M[1:2, ])
  s2 <- mme(2.5)
  expect_equal(r$coefficients, s2[1:2], tolerance = 1e-10)
  expect_equal(r$marker_effects, s2[3:6], tolerance = 1e-10)
  expect_lt(r$kernel_identity_gap, 1e-10)
  expect_equal(r$prediction_new, as.numeric(M[1:2, ] %*% s2[3:6]), tolerance = 1e-10)
  # REML: the profile restricted log-likelihood at the estimated ratio
  re <- morie_rrblpr_rr_blup(y, M, X = X)
  G <- tcrossprod(M)
  prof <- function(lam) {
    V <- G / lam + diag(7)
    Vi <- solve(V)
    XV <- t(X) %*% Vi %*% X
    b <- solve(XV, t(X) %*% Vi %*% y)
    e <- y - X %*% b
    s2 <- sum(e * (Vi %*% e)) / 5
    -0.5 * (5 * log(s2) + log(det(V)) + log(det(XV)) + 5)
  }
  expect_equal(re$reml_loglik, prof(re$lambda), tolerance = 1e-8)
  # the grid search ends within 1/2000 of the log-range of the maximiser
  opt <- stats::optimize(function(t) prof(exp(t)), c(-12, 12), maximum = TRUE, tol = 1e-10)
  expect_lt(abs(log(re$lambda) - opt$maximum), 24 / 200 / 10 / 10 / 10)
  expect_true(re$lambda_estimated)
  expect_error(morie_rrblpr_rr_blup(y, M, lam = -1, X = X), "cannot be negative")
  expect_error(morie_rrblpr_rr_blup(y, cbind(M, M[, 1:2] + 1), lam = 0, X = X), "not identified")
  expect_error(morie_rrblpr_rr_blup(y[-1], M), "marker rows")
  expect_error(morie_rrblpr_rr_blup(y, M, lam = 1, M_new = M[, 1:3]), "4 markers")
})

test_that("morie_rtwall gives Wallinga-Teunis case reproduction numbers", {
  t <- c(0, 1, 2, 2, 3, 4, 5, 5)
  w <- c(0.3, 0.4, 0.2, 0.1)
  r <- morie_rtwall(t, w)
  W <- outer(t, t, function(ti, tj) {
    lag <- ti - tj
    ifelse(lag >= 1 & lag <= 4, w[pmin(pmax(lag, 1), 4)], 0)
  })
  P <- W / ifelse(rowSums(W) > 0, rowSums(W), 1)
  expect_equal(r$r_case, colSums(P), tolerance = 1e-12)
  expect_equal(r$mass_check, sum(colSums(P)) - sum(rowSums(W) > 0), tolerance = 1e-12)
  expect_equal(unname(r$r_daily), as.numeric(tapply(colSums(P), t, mean)), tolerance = 1e-12)
  expect_error(morie_rtwall(1, w), "two cases")
  expect_error(morie_rtwall(t, -w), "non-negative")
})
