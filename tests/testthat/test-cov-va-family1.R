# Coverage for vandIE .. varimp exports. Every expectation is recomputed in
# the test body.

test_that("VandIE gives four-way decomposition proportions", {
  X <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0)
  M <- c(1.2, 0.4, 1.5, 1.1, 0.2, 0.6, 1.3, 0.5, 1.8, 0.7)
  Y <- c(2.1, 1.0, 2.9, 2.2, 0.3, 1.4, 1.9, 0.8, 3.0, 1.2)
  r <- VandIE(Y, X, M, m = 0.5)
  th <- stats::coef(stats::lm(Y ~ X + M + X:M))
  be <- stats::coef(stats::lm(M ~ X))
  cde <- th[["X"]] + th[["X:M"]] * 0.5
  intref <- th[["X:M"]] * (be[1] - 0.5)
  intmed <- th[["X:M"]] * be[["X"]]
  pie <- th[["M"]] * be[["X"]]
  te <- cde + intref + intmed + pie
  expect_equal(r$te, unname(te), tolerance = 1e-10)
  expect_equal(r$p_cde, unname(cde / te), tolerance = 1e-10)
  expect_equal(r$estimate, unname((intref + intmed + pie) / te), tolerance = 1e-10)
  expect_equal(r$p_mediated, unname((intmed + pie) / te), tolerance = 1e-10)
  expect_equal(r$same_sign, as.numeric(all(sign(c(cde, intref, intmed, pie)) == sign(cde))))
})

test_that("morie_vanr1 is the VanRaden method-1 relationship matrix", {
  M <- rbind(c(0, 1, 2, 1), c(2, 1, 0, 1), c(1, 1, 1, 2), c(0, 2, 2, 0))
  r <- morie_vanr1(M)
  p <- colMeans(M) / 2
  Z <- sweep(M, 2, 2 * p)
  G <- Z %*% t(Z) / (2 * sum(p * (1 - p)))
  expect_equal(r$G, G, tolerance = 1e-12)
  expect_equal(r$estimate, mean(diag(G)), tolerance = 1e-12)
  f <- morie_vanr1(M, freq = rep(0.5, 4))
  expect_equal(f$denominator, 2, tolerance = 1e-12)
})

test_that("Varest, VarF, Vardec and Varimp estimate and summarise a VAR", {
  set <- cbind(c(1.0, 1.4, 0.9, 1.7, 2.1, 1.6, 2.4, 2.2, 2.9, 2.6, 3.1, 3.4),
               c(0.5, 0.2, 0.8, 0.6, 1.1, 0.9, 1.3, 1.6, 1.2, 1.8, 2.0, 1.7))
  r <- Varest(set, p = 1)
  Y <- set[-1, ]
  Z <- cbind(1, set[-12, ])
  B <- t(solve(crossprod(Z), crossprod(Z, Y)))
  E <- Y - Z %*% t(B)
  expect_equal(r$coef, B, tolerance = 1e-10)
  expect_equal(r$sigma_u, crossprod(E) / (11 - 3), tolerance = 1e-10)
  Sml <- crossprod(E) / 11
  expect_equal(r$loglik, -0.5 * 11 * 2 * log(2 * pi) - 0.5 * 11 * log(det(Sml)) - 11, tolerance = 1e-10)
  expect_equal(r$aic, log(det(Sml)) + 2 * 6 / 11, tolerance = 1e-10)
  expect_equal(VarF(set, p = 1)$coef, r$coef, tolerance = 1e-12)
  expect_error(Varest(set, p = 0), "at least 1")
  expect_error(Varest(set[1:2, ], p = 1), "fewer observations than regressors")
  A <- B[, 2:3]
  P <- t(chol(r$sigma_u))
  fe <- Vardec(A, r$sigma_u, periods = 3)
  ctr <- Reduce(`+`, lapply(0:3, function(h) {
    Th <- diag(2)
    if (h > 0) for (k in seq_len(h)) Th <- Th %*% A
    (Th %*% P)^2
  }))
  expect_equal(fe$decomposition[4, , ], ctr / rowSums(ctr), tolerance = 1e-10)
  ir <- Varimp(B, r$sigma_u, horizon = 3, shock_var = 1)
  expect_equal(ir$irf[1, ], P[, 2], tolerance = 1e-10)
  expect_equal(ir$irf[4, ], as.numeric(A %*% A %*% A %*% P[, 2]), tolerance = 1e-10)
  expect_error(Varimp(B, r$sigma_u, shock_var = 2), "out of range")
  expect_error(Vardec(A, diag(3)), "k by k")
})

test_that("Varatr fits GARCH(1,1) on the lattice and reads off VaR and ES", {
  y <- c(0.5, -1.2, 0.3, 2.1, -0.8, 0.1, -1.5, 0.9, 0.4, -0.3, 1.8, -2.2, 0.6, 0.2)
  r <- Varatr(y, alpha = 0.05, a_grid = c(0, 0.1), b_grid = c(0, 0.5))
  mu <- mean(y)
  e <- y - mu
  s2u <- mean(e^2)
  fits <- list()
  for (a in c(0, 0.1)) for (b in c(0, 0.5)) {
    om <- s2u * (1 - a - b)
    s2 <- numeric(14)
    s2[1] <- s2u
    for (t in 2:14) s2[t] <- om + a * e[t - 1]^2 + b * s2[t - 1]
    ll <- sum(stats::dnorm(e, 0, sqrt(s2), log = TRUE))
    fits[[length(fits) + 1]] <- list(a = a, b = b, ll = ll, nxt = om + a * e[14]^2 + b * s2[14])
  }
  best <- fits[[which.max(vapply(fits, function(f) f$ll, 0))]]
  sig <- sqrt(best$nxt)
  z <- stats::qnorm(0.05)
  expect_equal(c(r$a, r$b), c(best$a, best$b))
  expect_equal(r$loglik, best$ll, tolerance = 1e-10)
  expect_equal(r$var, -(mu + sig * z), tolerance = 1e-9)
  expect_equal(r$es, -mu + sig * stats::dnorm(z) / 0.05, tolerance = 1e-9)
  expect_error(Varatr(1), "at least two")
  expect_error(Varatr(rep(1, 5)), "zero variance")
  expect_error(Varatr(y, a_grid = 0.6, b_grid = 0.6), "no admissible")
})

test_that("Vargpc starts from the prior and climbs the ELBO", {
  X <- matrix(c(-2, -1.2, -0.5, 0.1, 0.6, 1.3, 2.0, 2.4), ncol = 1)
  y <- c(0, 0, 1, 0, 1, 1, 0, 1)
  r0 <- Vargpc(X, y, m_inducing = 3, variance = 1.5, steps = 0)
  ell <- stats::integrate(function(f) stats::plogis(f, log.p = TRUE) * stats::dnorm(f, sd = sqrt(1.5)), -Inf, Inf,
                          rel.tol = 1e-12)$value
  # at the prior the KL is zero and each point contributes E[log sigmoid(+-f)]
  expect_equal(r0$kl, 0, tolerance = 1e-10)
  expect_equal(r0$elbo, 8 * ell, tolerance = 1e-8)
  expect_equal(r0$prob, rep(0.5, 8), tolerance = 1e-12)
  expect_lt(r0$quad_check, 1e-10)
  r <- Vargpc(X, y, m_inducing = 3, variance = 1.5, steps = 15)
  expect_equal(r$elbo_monotone, 1)
  expect_gte(r$elbo, r0$elbo)
  Kmm <- 1.5 * exp(-0.5 * as.matrix(stats::dist(r$Z))^2) + diag(1e-8, 3)
  kl <- 0.5 * (sum(diag(solve(Kmm, r$S))) + sum(r$m * solve(Kmm, r$m)) - 3 +
                 as.numeric(determinant(Kmm)$modulus) - as.numeric(determinant(r$S)$modulus))
  expect_equal(r$kl, kl, tolerance = 1e-8)
  expect_equal(r$Z[, 1], X[c(1, 5, 8), 1])
  expect_error(Vargpc(X, y + 1), "binary 0/1")
  expect_error(Vargpc(X, y, m_inducing = 9), "m_inducing must lie")
})
