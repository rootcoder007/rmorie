# Coverage for GAIL, GATE, GBLUP, the Goodman-Bacon composition, the GCN
# family, the Gaussian-DP trade-off, GEGLU and generalizability theory;
# every expectation is recomputed in the test body.

test_that("gail_occupancy_measure and gail follow the logistic discriminator ascent", {
  occ <- gail_occupancy_measure(c(1, 1, 2, 1), c("a", "b", "a", "a"))
  expect_equal(unname(occ[c("(1)x(a)", "(1)x(b)", "(2)x(a)")]), c(0.5, 0.25, 0.25),
               tolerance = 1e-12)
  expect_error(gail_occupancy_measure(1:2, 1), "same length")

  es <- c(0, 1, 2, 1)
  ea <- c(1, 1, 0, 1)
  ps <- c(2, 0, 2, 1, 0)
  pa <- c(0, 0, 1, 0, 1)
  ft <- function(s, a) c(s, a, s * a)
  r <- gail(es, ea, ps, pa, features = ft, lr = 0.3, epochs = 25, l2 = 0.05,
            lam = 0.2, policy_entropy = 1.5)
  XE <- cbind(es, ea, es * ea, 1)
  XP <- cbind(ps, pa, ps * pa, 1)
  w <- numeric(4)
  sg <- function(z) 1 / (1 + exp(-z))
  for (k in 1:25) {
    g <- colMeans((1 - sg(XP %*% w))[, 1] * XP) - colMeans(sg(XE %*% w)[, 1] * XE)
    w <- w + 0.3 * (g - 0.05 * w)
  }
  expect_equal(r$weights, unname(w), tolerance = 1e-12)
  dp <- sg(XP %*% w)[, 1]
  de <- sg(XE %*% w)[, 1]
  expect_equal(r$D_policy, dp, tolerance = 1e-12)
  expect_equal(r$cost, log(dp), tolerance = 1e-12)
  expect_equal(r$objective, mean(log(dp)) + mean(log(1 - de)) - 0.2 * 1.5, tolerance = 1e-12)
  expect_equal(r$accuracy, (sum(dp > 0.5) + sum(de <= 0.5)) / 9, tolerance = 1e-12)
  expect_equal(unname(r$Q["(0)x(0)"]), log(dp[2]), tolerance = 1e-12)

  d <- gail(c(1, 2), c(0, 1), c(1, 1), c(0, 0), lr = 0.5, epochs = 3)
  expect_length(d$weights, 3)
  expect_equal(names(d$Q), "(1)x(0)")
  expect_error(gail(1, 1, 1, 1, features = 3), "callable")
  expect_error(gail(numeric(0), numeric(0), 1, 1), "non-empty")
})

test_that("morie_gate runs AIPW inside each group and skips constant-treatment groups", {
  set.seed(11)
  n <- 80
  df <- data.frame(x = rnorm(n), g = rep(c("a", "b"), each = n / 2))
  df$t <- rbinom(n, 1, plogis(0.4 * df$x))
  df$y <- 1 + df$x + 2 * df$t + rnorm(n)
  df$ps <- plogis(0.4 * df$x)
  r <- morie_gate(df, "t", "y", "x", "g", propensity_col = "ps")
  for (gv in c("a", "b")) {
    ref <- morie_estimate_aipw(df[df$g == gv, ], "t", "y", "x", propensity_col = "ps",
                               outcome_model = "linear")
    expect_equal(r$ate[r$group == gv], ref$ate, tolerance = 1e-12)
    expect_equal(r$se[r$group == gv], ref$se, tolerance = 1e-12)
  }
  df2 <- rbind(df, data.frame(x = 0, g = "c", t = 1, y = 1, ps = 0.5))
  expect_warning(r2 <- morie_gate(df2, "t", "y", "x", "g", propensity_col = "ps"), "no variation")
  expect_equal(r2$group, c("a", "b"))
  expect_equal(nrow(morie_gate(df2[df2$g == "c", ], "t", "y", "x", "g")), 0L)
})

test_that("Gblupeq and Gblupr reproduce the Cholesky identity and Henderson's equations", {
  Z <- matrix(c(1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0), 4, 3, byrow = TRUE)
  G <- matrix(c(2, 0.5, 0.2, 0.5, 1.5, 0.3, 0.2, 0.3, 1), 3, 3)
  e <- Gblupeq(Z, G, 0.8)
  L <- t(chol(G))
  expect_equal(e$L, L, tolerance = 1e-12)
  expect_equal(e$Zstar, Z %*% L, tolerance = 1e-12)
  expect_equal(e$V_original, 0.8 * Z %*% G %*% t(Z), tolerance = 1e-12)
  expect_lt(e$max_gap, 1e-12)
  expect_error(Gblupeq(Z, G[1:2, 1:2], 1), "q by q")
  expect_error(Gblupeq(Z, G, -1), "non-negative")

  y <- c(3.1, 2.4, 4.0, 3.3)
  X <- cbind(1, c(0.2, -0.1, 0.5, 0.3))
  r <- Gblupr(y, X, Z, G, var_u = 0.5, var_e = 1.5, ridge = 0)
  Vy <- 0.5 * Z %*% G %*% t(Z) + 1.5 * diag(4)
  Vi <- solve(Vy)
  beta <- solve(t(X) %*% Vi %*% X, t(X) %*% Vi %*% y)
  u <- 0.5 * G %*% t(Z) %*% Vi %*% (y - X %*% beta)
  expect_equal(r$beta, as.numeric(beta), tolerance = 1e-9)
  expect_equal(r$u, as.numeric(u), tolerance = 1e-9)
  expect_equal(r$lambda, 3, tolerance = 1e-12)
  expect_equal(r$residual_ss, sum((y - r$fitted)^2), tolerance = 1e-12)
  expect_error(Gblupr(y, X, Z, G, var_u = 0), "positive")
  expect_error(Gblupr(y, X[1:3, ], Z, G), "one row per observation")
})

test_that("Gbtcom and morie_gbtcom decompose the TWFE coefficient", {
  unit <- rep(1:6, each = 5)
  time <- rep(1:5, 6)
  onset <- c(2, 4, Inf, 3, Inf, 4)[unit]
  D <- as.numeric(time >= onset)
  y <- 0.3 * unit + 0.5 * time + (1 + 0.2 * unit) * D + sin(unit * time)
  r <- morie_gbtcom(y, D, unit, time)
  twfe <- unname(coef(lm(y ~ D + factor(unit) + factor(time)))["D"])
  expect_equal(r$estimate, twfe, tolerance = 1e-9)
  expect_equal(r$weight_sum, 1, tolerance = 1e-9)
  expect_equal(r$identity_residual, 0, tolerance = 1e-9)
  expect_equal(sum(unlist(r$contribution)), twfe, tolerance = 1e-9)
  expect_gt(r$forbidden_weight, 0)
  df <- data.frame(yy = y, dd = D, uu = unit, tt = time)
  r2 <- Gbtcom(df, "yy", "dd", "uu", "tt")
  expect_equal(r2$estimate, r$estimate, tolerance = 1e-12)
  expect_error(morie_gbtcom(y, D[-1], unit, time), "same length")
})

test_that("the GCN layers match their propagation rules", {
  A <- matrix(c(0, 1, 1, 0,
                1, 0, 1, 0,
                1, 1, 0, 1,
                0, 0, 1, 0), 4, byrow = TRUE)
  X <- matrix(c(1, -0.5, 0.3, 2, 0.7, -1.2, 0.4, 0.1), 4, 2)
  W <- matrix(c(0.5, -1, 1.5, 0.25), 2, 2)
  At <- A + diag(4)
  Dm <- diag(1 / sqrt(rowSums(At)))
  An <- Dm %*% At %*% Dm
  r <- GcnL(A, X, W)
  expect_equal(r$preactivation, An %*% X %*% W, tolerance = 1e-12)
  expect_equal(r$H, pmax(An %*% X %*% W, 0), tolerance = 1e-12)
  expect_error(GcnL(A, X, matrix(1, 3, 1)), "one row per input feature")

  H <- X
  for (k in 1:3) H <- pmax(0.8 * An %*% H + 0.2 * X, 0)
  g2 <- GcnII(A, X, alpha = 0.2, beta = 0.5, K = 3)
  expect_equal(g2$H, H, tolerance = 1e-12)
  expect_error(GcnII(A, X, alpha = 2), "alpha")
  expect_error(GcnII(A, X, K = 0), "K must")

  Lap <- diag(rowSums(A)) - A
  lmax <- max(eigen(Lap, symmetric = TRUE)$values)
  Lt <- 2 * Lap / lmax - diag(4)
  T2 <- 2 * Lt %*% Lt %*% X - X
  T3 <- 2 * Lt %*% T2 - Lt %*% X
  th <- c(0.5, -0.3, 0.2, 0.1)
  ch <- Gcnchb(Lap, X, K = 4, theta = th)
  expect_equal(ch$lambda_max, lmax, tolerance = 1e-9)
  expect_equal(ch$H, th[1] * X + th[2] * Lt %*% X + th[3] * T2 + th[4] * T3, tolerance = 1e-9)
  ch1 <- Gcnchb(Lap, X, K = 2)
  expect_equal(ch1$H, X + 0.5 * Lt %*% X, tolerance = 1e-9)
  expect_error(Gcnchb(Lap, X, K = 2, theta = 1), "K entries")
  expect_error(Gcnchb(-diag(2), X[1:2, ]), "positive eigenvalue")
})

test_that("Gdpf gives the mu-GDP trade-off curve and its (eps, delta) dual", {
  r <- Gdpf(mech = c(2, 4), alpha = c(0.1, 0.5), epsilon = 0.7)
  m <- 0.5
  expect_equal(r$mu, m)
  expect_equal(r$trade_off, pnorm(qnorm(1 - c(0.1, 0.5)) - m), tolerance = 1e-12)
  expect_equal(r$delta, pnorm(-0.7 / m + m / 2) - exp(0.7) * pnorm(-0.7 / m - m / 2),
               tolerance = 1e-12)
  expect_equal(Gdpf(mu = 0)$delta, 0)
  expect_equal(Gdpf(mu = 1)$alpha, c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9))
  expect_error(Gdpf(), "give mu")
  expect_error(Gdpf(mu = 1, alpha = 1), "alpha")
})

test_that("Geglu gates with the exact GELU", {
  x <- c(0.5, -1, 2)
  W <- matrix(c(0.2, -0.4, 0.1, 0.3, 0.5, -0.2), 3, 2)
  V <- matrix(c(1, 0, -1, 0.5, 0.5, 0.5), 3, 2)
  W2 <- matrix(c(1, -1, 2, 0.5), 2, 2)
  r <- Geglu(NULL, x = x, W = W, V = V, b = c(0.1, -0.1), c = c(0, 1), W2 = W2)
  gpre <- as.numeric(t(W) %*% x) + c(0.1, -0.1)
  gate <- gpre * pnorm(gpre)
  out <- gate * (as.numeric(t(V) %*% x) + c(0, 1))
  expect_equal(r$gate, gate, tolerance = 1e-12)
  expect_equal(r$out, out, tolerance = 1e-12)
  expect_equal(r$ffn, as.numeric(t(W2) %*% out), tolerance = 1e-12)
  r0 <- Geglu(x, W = W, V = V)
  g0 <- as.numeric(t(W) %*% x)
  expect_equal(r0$out, g0 * pnorm(g0) * as.numeric(t(V) %*% x), tolerance = 1e-12)
  expect_length(r0$ffn, 0)
})

test_that("Genvxt and Genvdm give the G-study components and D-study coefficients", {
  M <- matrix(c(4, 5, 3, 6,
                2, 3, 2, 4,
                5, 6, 6, 7,
                3, 3, 4, 5,
                1, 2, 2, 2), 5, byrow = TRUE)
  np <- 5
  ni <- 4
  gm <- mean(M)
  msp <- ni * sum((rowMeans(M) - gm)^2) / (np - 1)
  msi <- np * sum((colMeans(M) - gm)^2) / (ni - 1)
  R <- M - outer(rowMeans(M), colMeans(M), "+") + gm
  mspi <- sum(R^2) / ((np - 1) * (ni - 1))
  vp <- (msp - mspi) / ni
  vi <- (msi - mspi) / np
  r <- Genvxt(M)
  expect_equal(c(r$var_p, r$var_i, r$var_pi), c(vp, vi, mspi), tolerance = 1e-12)
  expect_equal(r$e_rho2, vp / (vp + mspi / ni), tolerance = 1e-12)
  expect_equal(r$phi, vp / (vp + (vi + mspi) / ni), tolerance = 1e-12)
  r2 <- Genvxt(M, facets = 10)
  expect_equal(r2$e_rho2, vp / (vp + mspi / 10), tolerance = 1e-12)
  expect_error(Genvxt(M[1, , drop = FALSE]), "two persons")

  d <- Genvdm(c(vp, vi, mspi), c(1, 2, 5, 20), target = 0.9)
  ns <- c(1, 2, 5, 20)
  er <- vp / (vp + mspi / ns)
  expect_equal(d$e_rho2, er, tolerance = 1e-12)
  expect_equal(d$phi, vp / (vp + (vi + mspi) / ns), tolerance = 1e-12)
  expect_equal(d$meets_target, as.integer(er >= 0.9))
  expect_equal(d$n_required, if (any(er >= 0.9)) ns[which(er >= 0.9)[1]] else 0L)
  expect_error(Genvdm(c(1, 2), 3), "three variances")
  expect_error(Genvdm(c(1, 1, 1), 3, target = 1), "target")
})
