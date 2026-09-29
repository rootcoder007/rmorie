# Coverage for reglu .. Reportm exports. Every expectation is recomputed in
# the test body.

test_that("Reglu gates a linear unit with a ReLU", {
  x <- c(0.5, -1.2, 0.8)
  W <- matrix(c(1, 0, -1, 0.5, 0.2, -0.3, -0.4, 1, 0.1), 3)
  V <- matrix(c(0.3, -0.2, 0.7, 1, 0.1, 0.5, -0.6, 0.4, 0.2), 3)
  b <- c(0.1, -0.2, 0.3)
  cc <- c(0, 0.5, -0.1)
  W2 <- matrix(c(1, -1, 0.5, 2, 0.3, 0.7), 3)
  r <- Reglu(NULL, x = x, W = W, V = V, b = b, c = cc, W2 = W2)
  gate <- pmax(as.numeric(t(W) %*% x) + b, 0)
  out <- gate * (as.numeric(t(V) %*% x) + cc)
  expect_equal(r$gate, gate, tolerance = 1e-12)
  expect_equal(r$out, out, tolerance = 1e-12)
  expect_equal(r$ffn, as.numeric(t(W2) %*% out), tolerance = 1e-12)
  expect_equal(r$n_dead, sum(gate == 0))
  expect_equal(Reglu(x, W = W, V = V)$out, pmax(as.numeric(t(W) %*% x), 0) * as.numeric(t(V) %*% x),
               tolerance = 1e-12)
})

test_that("morie_robust_se matches sandwich::vcovHC", {
  skip_if_not_installed("sandwich")
  x1 <- c(0.2, 1.1, -0.5, 0.8, 1.9, -1.0, 0.3, 0.6, 1.4, -0.2)
  x2 <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0)
  y <- 1 + 0.5 * x1 - 0.8 * x2 + c(0.3, -0.5, 0.2, 0.9, -1.1, 0.4, -0.2, 0.6, -0.8, 0.1) * (1 + abs(x1))
  fit <- morie_ols(y, cbind(x1, x2))
  f <- stats::lm(y ~ x1 + x2)
  for (k in c("HC0", "HC1", "HC2", "HC3")) {
    r <- morie_robust_se(fit, kind = k)
    V <- sandwich::vcovHC(f, type = k)
    expect_equal(unname(r$vcov), unname(V), tolerance = 1e-10)
    expect_equal(unname(r$t), unname(stats::coef(f) / sqrt(diag(V))), tolerance = 1e-10)
    expect_equal(unname(r$p_value), unname(2 * stats::pt(-abs(stats::coef(f) / sqrt(diag(V))), 7)),
                 tolerance = 1e-10)
  }
  expect_error(morie_robust_se(fit, kind = "HC4"), "HC0, HC1, HC2 or HC3")
})

test_that("morie_reinfc_cheatsheet states the REINFORCE update", {
  s <- morie_reinfc_cheatsheet()
  expect_type(s, "character")
  expect_match(s, "Delta w = alpha \\(r - b\\) dln g/dw")
  expect_match(s, "Williams 1992 eq. 2")
})

test_that("rejct gives Hampel's robustness measures", {
  h <- rejct("huber", 1.5)
  ep <- 2 * stats::pnorm(1.5) - 1
  expect_equal(h$expected_psi_prime, ep, tolerance = 1e-12)
  expect_equal(h$gross_error_sensitivity, 1.5 / ep, tolerance = 1e-12)
  expect_identical(h$estimate, Inf)
  b <- rejct("bisquare", 4.685)
  cc <- 4.685
  dpsi <- function(t) (1 - 6 * t^2 / cc^2 + 5 * t^4 / cc^4) * stats::dnorm(t)
  # Simpson with 2000 panels on a smooth integrand: accurate far below 1e-10
  ep2 <- stats::integrate(dpsi, -cc, cc, rel.tol = 1e-13)$value
  expect_equal(b$expected_psi_prime, ep2, tolerance = 1e-10)
  psi <- function(t) t * (1 - (t / cc)^2)^2
  sup <- stats::optimize(psi, c(0, cc), maximum = TRUE, tol = 1e-12)$objective
  expect_equal(b$sup_psi, sup, tolerance = 1e-10)
  expect_equal(b$gross_error_sensitivity, b$sup_psi / ep2, tolerance = 1e-10)
  expect_same_function(morie_rejct, rejct)
  expect_equal(morie_rejct("tukey")$tuning, 4.685)
  expect_error(rejct("cauchy"), "huber' or 'bisquare")
})

test_that("Relbt and Reluact evaluate their formulas", {
  r <- Relbt(c(0.4, -0.3), 0.36)
  expect_equal(r$reliability, c(0.16, 0.09) / 0.36, tolerance = 1e-12)
  expect_equal(r$accuracy, c(0.4, -0.3) / 0.6, tolerance = 1e-12)
  expect_error(Relbt(1.5, 0.3), "r must lie")
  expect_error(Relbt(0.5, 0), "h2 must lie")
  expect_error(Relbt(c(0.1, 0.2), c(0.3, 0.4, 0.5)), "incompatible lengths")
  expect_error(Relbt(numeric(0), 0.3), "both r and h2")
  z <- c(-2, -0.5, 0, 0.7, 3)
  a <- Reluact(z, slope = 0.1)
  expect_equal(a$activation, pmax(z, 0.1 * z), tolerance = 1e-12)
  expect_equal(a$gradient, ifelse(z > 0, 1, 0.1))
  expect_equal(Reluact(z)$activation, pmax(z, 0))
})

test_that("Remlik is the restricted log-likelihood of the linear mixed model", {
  X <- cbind(1, c(0.2, 1.1, -0.5, 0.8, 1.9, -1.0, 0.3))
  Z <- cbind(c(1, 1, 1, 0, 0, 0, 0), c(0, 0, 0, 1, 1, 0, 0), c(0, 0, 0, 0, 0, 1, 1))
  y <- c(1.2, 2.3, 0.4, 1.9, 3.1, -0.2, 1.0)
  D <- diag(0.7, 3)
  R <- diag(0.5, 7)
  r <- Remlik(X, Z, y, D, R = R)
  V <- Z %*% D %*% t(Z) + R
  Vi <- solve(V)
  A <- t(X) %*% Vi %*% X
  b <- solve(A, t(X) %*% Vi %*% y)
  e <- y - X %*% b
  ll <- -0.5 * (log(det(A)) + log(det(V)) + sum(e * (Vi %*% e)))
  expect_equal(r$loglik, ll, tolerance = 1e-10)
  expect_equal(r$beta, as.numeric(b), tolerance = 1e-10)
  expect_equal(c(r$n, r$p, r$q), c(7L, 2L, 3L))
})

test_that("morie_remlfn gives ANOVA on balanced data and matches lmer otherwise", {
  y <- c(5.1, 4.8, 5.5, 6.2, 6.0, 6.5, 4.2, 4.6, 4.0)
  g <- rep(c("a", "b", "c"), each = 3)
  r <- morie_remlfn(y, g)
  tab <- stats::anova(stats::lm(y ~ factor(g)))
  ms <- tab[["Mean Sq"]]
  expect_true(r$closed_form)
  expect_equal(r$sigma2_a, (ms[1] - ms[2]) / 3, tolerance = 1e-12)
  expect_equal(r$sigma2_e, ms[2], tolerance = 1e-12)
  expect_error(morie_remlfn(y[-9], g[-9], solver = "closed"), "only valid for balanced")
  expect_error(morie_remlfn(y, g, solver = "grid"), "solver must be")
  expect_error(morie_remlfn(y, rep("a", 9)), "two classes")
  yu <- c(5.1, 4.8, 5.5, 6.2, 6.0, 6.5, 6.1, 4.2, 4.6, 7.0, 6.8)
  gu <- c("a", "a", "a", "b", "b", "b", "b", "c", "c", "d", "d")
  ru <- morie_remlfn(yu, gu)
  expect_false(ru$closed_form)
  # stationarity of the restricted likelihood the function maximises
  nll <- function(p) -.remlfn_loglik(split(yu, gu), as.numeric(table(gu)), exp(p[1]), exp(p[2]))$loglik
  best <- stats::optim(log(c(ru$sigma2_a, ru$sigma2_e)), nll, method = "BFGS",
                       control = list(reltol = 1e-15))
  expect_equal(-unname(ru$loglik), unname(best$value), tolerance = 1e-10)
  skip_if_not_installed("lme4")
  m <- lme4::lmer(yu ~ 1 + (1 | gu), REML = TRUE)
  vc <- as.data.frame(lme4::VarCorr(m))$vcov
  # lmer stops at its own optimiser tolerance
  expect_equal(c(ru$sigma2_a, ru$sigma2_e), vc, tolerance = 1e-5)
  expect_same_function(morie_reml_variance_components, morie_remlfn)
})

test_that("Report Noisy Max replays its Lehmer stream", {
  cts <- c(10, 12, 11, 9)
  run <- function(eps, seed) {
    s <- seed
    best <- -Inf
    idx <- -1
    for (i in 1:4) {
      s <- (48271 * s) %% 2147483647
      h <- s / 2147483647 - 0.5
      v <- cts[i] - (1 / eps) * sign(h) * log(1 - 2 * abs(h))
      if (v > best) {
        best <- v
        idx <- i - 1
      }
    }
    c(idx, best)
  }
  for (sd in c(1, 7, 12345)) {
    w <- run(0.5, sd)
    a <- morie_reportm(cts, 0.5, seed = sd)
    b <- Reportm(cts, 0.5, seed = sd)
    expect_equal(c(a$index, a$winner), w, tolerance = 1e-12)
    expect_equal(c(b$index, b$winner), w, tolerance = 1e-12)
  }
  expect_error(morie_reportm(numeric(0), 1), "non-empty")
  expect_error(Reportm(cts, 0), "epsilon must be positive")
  expect_error(morie_reportm(cts, 1, sensitivity = 0), "sensitivity must be positive")
})
