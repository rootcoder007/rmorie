# Coverage for offlrl .. otbar exports. Every expectation is recomputed in
# the test body.

test_that("offlrl_lookup and offlrl_safe_max_key read 's|a' keyed maps", {
  q <- list("s1|a" = 0.5, "s1|b" = 2.5, "s1|c" = 2.5, "s2|a" = -1, "s2|b" = -3, "s2|c" = -0.2)
  expect_identical(offlrl_lookup(q, "s1", "b"), 2.5)
  expect_null(offlrl_lookup(q, "s3", "a"))
  A <- c("a", "b", "c")
  for (s in c("s1", "s2")) {
    v <- vapply(A, function(a) q[[paste0(s, "|", a)]], 0)
    # the first maximiser wins ties
    expect_identical(offlrl_safe_max_key(q, s, A), A[which.max(v)])
  }
})

test_that("Ogkcv builds the orthogonalised Gnanadesikan-Kettenring scatter", {
  M <- cbind(c(1.2, 3.4, 2.2, 5.1, 4.4, 2.9, 3.8, 0.7, 6.0, 3.1),
             c(0.5, 1.9, 1.1, 2.8, 2.6, 1.2, 2.5, 0.1, 3.9, 1.4),
             c(3.0, 2.1, 2.9, 1.0, 1.7, 2.2, 1.1, 3.6, 0.2, 2.4))
  s <- apply(M, 2, stats::mad)
  Y <- sweep(M, 2, s, "/")
  U <- diag(3)
  for (j in 1:2) for (k in (j + 1):3) {
    U[j, k] <- U[k, j] <- (stats::mad(Y[, j] + Y[, k])^2 - stats::mad(Y[, j] - Y[, k])^2) / 4
  }
  E <- eigen(U, symmetric = TRUE)$vectors
  Z <- Y %*% E
  A <- diag(s) %*% E
  S <- A %*% diag(apply(Z, 2, stats::mad)^2) %*% t(A)
  res <- Ogkcv(M)
  # sign flips of eigenvectors cancel in S and in A %*% median(Z)
  expect_equal(res$sigma, S, tolerance = 1e-10)
  expect_equal(res$location, as.numeric(A %*% apply(Z, 2, stats::median)), tolerance = 1e-10)
  expect_equal(res$scales, s, tolerance = 1e-12)
  expect_equal(sort(res$eigenvalues), sort(eigen(U, symmetric = TRUE)$values), tolerance = 1e-10)
  expect_equal(res$det, prod(apply(Z, 2, stats::mad)^2) * prod(s^2), tolerance = 1e-10)
  rx <- Ogkcv(M[, 1], M[, 2:3])
  expect_equal(rx$beta, as.numeric(solve(S[2:3, 2:3], S[2:3, 1])), tolerance = 1e-10)
  expect_error(Ogkcv(M[1, , drop = FALSE]), "at least two")
  expect_error(Ogkcv(cbind(M[, 1], 1)), "zero robust scale")
})

test_that("Olsfit and Olsnormeq agree with lm", {
  X <- cbind(c(0.3, 1.2, 2.5, 3.1, 4.0, 4.4, 5.9, 6.2),
             c(1.0, 0.2, 0.8, -0.5, 1.4, 0.3, -0.9, 0.6))
  y <- c(1.1, 2.0, 3.3, 3.2, 5.1, 4.8, 5.5, 7.0)
  f <- stats::lm(y ~ X)
  r <- Olsfit(X, y)
  expect_equal(r$coefficients, unname(stats::coef(f)), tolerance = 1e-10)
  expect_equal(r$fitted, unname(stats::fitted(f)), tolerance = 1e-10)
  expect_equal(r$rss, sum(stats::residuals(f)^2), tolerance = 1e-10)
  expect_equal(r$r2, summary(f)$r.squared, tolerance = 1e-10)
  expect_equal(r$sigma2, summary(f)$sigma^2, tolerance = 1e-10)
  expect_equal(r$df, 5L)
  r0 <- Olsfit(X, y, intercept = FALSE)
  expect_equal(r0$coefficients, unname(stats::coef(stats::lm(y ~ X - 1))), tolerance = 1e-10)
  expect_true(is.nan(r0$intercept))
  expect_error(Olsfit(X, y[-1]), "same number of rows")
  expect_error(Olsfit(X[1:2, ], y[1:2]), "fewer observations")
  n <- Olsnormeq(X, y)
  expect_equal(n$beta, unname(stats::coef(f)), tolerance = 1e-10)
  expect_equal(n$se, unname(summary(f)$coefficients[, 2]), tolerance = 1e-10)
  expect_equal(n$leverage, unname(stats::hatvalues(f)), tolerance = 1e-10)
  expect_equal(n$sigma2, summary(f)$sigma^2, tolerance = 1e-10)
  n0 <- Olsnormeq(X, y, add_intercept = FALSE)
  expect_equal(n0$beta, unname(stats::coef(stats::lm(y ~ X - 1))), tolerance = 1e-10)
  expect_error(Olsnormeq(X, y[-1]), "one row per entry")
  expect_error(Olsnormeq(X[1:3, ], y[1:3]), "more records than columns")
})

test_that("Omegah is McDonald's omega hierarchical", {
  lg <- c(0.7, 0.6, 0.5, 0.65)
  ls <- c(0.3, 0.2, 0.4, 0.1)
  r <- Omegah(NULL, lg, ls)
  psi <- 1 - lg^2 - ls^2
  vt <- sum(lg)^2 + sum(ls)^2 + sum(psi)
  expect_equal(r$estimate, sum(lg)^2 / vt, tolerance = 1e-12)
  expect_equal(r$omega_total, (sum(lg)^2 + sum(ls)^2) / vt, tolerance = 1e-12)
  expect_equal(r$uniqueness, psi, tolerance = 1e-12)
  # without specific factors omega_h equals omega total
  r1 <- Omegah(NULL, lg)
  expect_equal(r1$estimate, r1$omega_total, tolerance = 1e-12)
})

test_that("Ovbias matches the sensemakr bias and adjusted standard error", {
  r <- Ovbias(estimate = 0.8, se = 0.2, df = 120, r2_yz = 0.05, r2_dz = 0.1)
  b <- 0.2 * sqrt(0.05 * 0.1 / 0.9) * sqrt(120)
  ase <- 0.2 * sqrt(0.95 / 0.9 * 120 / 119)
  expect_equal(r$bias, b, tolerance = 1e-12)
  expect_equal(r$adjusted_se, ase, tolerance = 1e-12)
  expect_equal(r$adjusted_estimate, 0.8 - b, tolerance = 1e-12)
  expect_equal(r$adjusted_t, (0.8 - b) / ase, tolerance = 1e-12)
  expect_equal(r$bias_factor, sqrt(0.05) * sqrt(0.1 / 0.9), tolerance = 1e-12)
  expect_equal(r$relative_bias, r$bias_factor / (4 / sqrt(120)), tolerance = 1e-12)
  if (requireNamespace("sensemakr", quietly = TRUE)) {
    expect_equal(r$bias, sensemakr::bias(se = 0.2, dof = 120, r2dz.x = 0.1, r2yz.dx = 0.05),
                 tolerance = 1e-12)
    expect_equal(r$adjusted_se, sensemakr::adjusted_se(se = 0.2, dof = 120, r2dz.x = 0.1,
                                                      r2yz.dx = 0.05), tolerance = 1e-12)
  }
  # negative estimate: the bias shrinks it toward zero
  expect_equal(Ovbias(estimate = -0.8, se = 0.2, df = 120, r2_yz = 0.05, r2_dz = 0.1)$adjusted_estimate,
               -0.8 + b, tolerance = 1e-12)
  # classical delta * gamma bias
  g <- Ovbias(delta = 0.4, gamma = -0.5, estimate = 1)
  expect_equal(g$bias, -0.2)
  expect_equal(g$adjusted_estimate, 1.2)
  expect_error(Ovbias(se = 1, df = 10, r2_yz = 1, r2_dz = 0.1), "partial R2")
  expect_error(Ovbias(se = 1, df = 1, r2_yz = 0.1, r2_dz = 0.1), "df must exceed 1")
})

test_that("OneMExp compares (1 - x)^n with exp(-n x)", {
  r <- OneMExp(0.01, 50)
  expect_equal(r$exact, 0.99^50, tolerance = 1e-12)
  expect_equal(r$approx, exp(-0.5), tolerance = 1e-12)
  expect_equal(r$rel_error, abs(0.99^50 - exp(-0.5)) / 0.99^50, tolerance = 1e-12)
  expect_error(OneMExp(1, 3), "lam_eps")
  expect_error(OneMExp(0.1, 2.5), "integer")
})

test_that("opthr finds the Huber constant with the target efficiency", {
  are <- function(k) {
    a <- 2 * stats::pnorm(k) - 1
    a^2 / (a - 2 * k * stats::dnorm(k) + 2 * k^2 * stats::pnorm(-k))
  }
  r <- opthr(0.95)
  k <- stats::uniroot(function(k) are(k) - 0.95, c(0.5, 3), tol = 1e-14)$root
  expect_equal(r$estimate, k, tolerance = 1e-9)
  expect_equal(r$achieved, are(r$estimate), tolerance = 1e-12)
  expect_equal(r$asymptotic_variance, 1 / r$achieved, tolerance = 1e-12)
  expect_lt(abs(r$estimate - 1.345), 5e-4)
  expect_same_function(morie_opthr, opthr)
  expect_equal(morie_opthr(0.9)$estimate,
               stats::uniroot(function(k) are(k) - 0.9, c(0.1, 3), tol = 1e-14)$root, tolerance = 1e-9)
})

test_that("Opttre searches IPW values over one split", {
  W <- cbind(c(0.1, 0.9, 0.4, 0.7, 0.2, 0.8, 0.5, 0.3, 0.6, 0.95),
             c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0))
  A <- c(1, 0, 1, 0, 0, 1, 1, 0, 0, 1)
  y <- c(3, 2, 4, 5, 1, 1, 3, 2, 4, 0.5)
  pt <- mean(A)
  pv <- ifelse(A == 1, pt, 1 - pt)
  bc <- function(idx) {
    sc <- function(a) {
      s <- idx[A[idx] == a]
      c(sum(y[s] / pv[s]), sum(1 / pv[s]))
    }
    q1 <- sc(1)
    q0 <- sc(0)
    v1 <- if (q1[2] > 0) q1[1] / q1[2] else -Inf
    v0 <- if (q0[2] > 0) q0[1] / q0[2] else -Inf
    if (v1 > v0) c(1, q1) else c(0, q0)
  }
  val <- function(rule) sum(y * (rule == A) / pv) / sum((rule == A) / pv)
  # depth 0: one treatment for everyone
  r0 <- Opttre(y, A, W, max_depth = 0)
  root <- bc(1:10)
  expect_equal(r0$rule, rep(root[1], 10))
  expect_equal(r0$value, val(rep(root[1], 10)), tolerance = 1e-12)
  expect_equal(r0$value_all_treated, val(rep(1, 10)), tolerance = 1e-12)
  expect_equal(r0$value_all_control, val(rep(0, 10)), tolerance = 1e-12)
  # depth 1: best single split by the pooled IPW value
  best <- NA
  for (j in 1:2) for (i in 1:10) {
    L <- which(W[, j] <= W[i, j])
    R <- which(W[, j] > W[i, j])
    if (!length(L) || !length(R)) next
    bl <- bc(L)
    br <- bc(R)
    v <- (bl[2] + br[2]) / (bl[3] + br[3])
    if (is.na(best) || v > best) {
      best <- v
      bj <- j
      bt <- W[i, j]
      rule <- numeric(10)
      rule[L] <- bl[1]
      rule[R] <- br[1]
    }
  }
  r1 <- Opttre(y, A, W, max_depth = 1)
  expect_gt(best, root[2] / root[3])
  expect_equal(r1$split_var, bj - 1L)
  expect_equal(r1$split_point, bt)
  expect_equal(r1$rule, rule)
  expect_equal(r1$value, val(rule), tolerance = 1e-12)
  expect_equal(r1$n_leaves, 2L)
  # supplied propensities are used as given
  rp <- Opttre(y, A, W, pi = rep(0.5, 10), max_depth = 0)
  expect_equal(rp$value_all_treated, mean(y[A == 1]), tolerance = 1e-12)
  expect_error(Opttre(y, A + 1, W), "binary")
  expect_error(Opttre(y, rep(1, 10), W), "both treatments")
  expect_error(Opttre(y, A, W, max_depth = -1), "non-negative")
  expect_error(Opttre(y, A, W, min_leaf = 0), "at least 1")
  expect_error(Opttre(y, A, W, pi = rep(0, 10)), "pi must lie")
})

test_that("OrdRep and OrdSubs count ordered samples with and without replacement", {
  r <- OrdRep(7, 3)
  expect_equal(r$count, 7^3)
  expect_equal(r$log_count, 3 * log(7), tolerance = 1e-12)
  expect_equal(OrdRep(0, 0)$count, 1)
  expect_error(OrdRep(-1, 2), "non-negative")
  s <- OrdSubs(7, 3)
  expect_equal(s$count, factorial(7) / factorial(4))
  expect_equal(OrdSubs(5, 0)$count, 1)
  expect_error(OrdSubs(3, 4), "n <= N")
})

test_that("Ordprobs differences the link CDF at the thresholds", {
  eta <- c(-0.5, 0.2, 1.3)
  th <- c(-1, 0.4, 1.5)
  for (lk in c("probit", "logit")) {
    F <- if (lk == "probit") stats::pnorm else stats::plogis
    P <- t(vapply(eta, function(e) diff(c(0, F(th + e), 1)), numeric(4)))
    r <- Ordprobs(eta, th, link = lk)
    expect_equal(r$probabilities, P, tolerance = 1e-12)
    expect_equal(rowSums(r$probabilities), rep(1, 3), tolerance = 1e-12)
  }
  expect_equal(c(r$n, r$C), c(3L, 4L))
})

test_that("local_nuisance is weighted least squares with an intercept", {
  W <- cbind(c(0.2, 1.1, -0.4, 0.8, 1.9, -1.0, 0.3), c(1, 0.5, 0.7, -0.2, 0.1, 1.2, -0.8))
  y <- c(1.3, 2.2, 0.1, 1.9, 3.5, -0.2, 0.9)
  w <- c(0.5, 1, 0.2, 0.8, 1.5, 0.3, 0.7)
  r <- local_nuisance(y, W, w, ridge = 0)
  f <- stats::lm.wfit(cbind(1, W), y, w)
  expect_equal(r$coef, unname(f$coefficients), tolerance = 1e-10)
  expect_equal(r$fit, as.numeric(cbind(1, W) %*% f$coefficients), tolerance = 1e-10)
  # exclude is 0-based and zeroes that unit's weight
  w2 <- w
  w2[3] <- 0
  expect_equal(local_nuisance(y, W, w, exclude = 2, ridge = 0)$coef,
               unname(stats::lm.wfit(cbind(1, W), y, w2)$coefficients), tolerance = 1e-10)
  expect_error(local_nuisance(y, W, rep(0, 7)), "neighbourhood is empty")
})

test_that("orthogonal_moment solves the residual-on-residual moment", {
  yr <- c(0.5, -0.3, 1.2, -0.8, 0.1)
  tr <- c(0.4, -0.1, 0.9, -0.7, 0.2)
  w <- c(0.1, 0.3, 0.2, 0.25, 0.15)
  r <- orthogonal_moment(yr, tr, w)
  expect_equal(r$theta, sum(w * tr * yr) / sum(w * tr^2), tolerance = 1e-12)
  expect_equal(r$den, sum(w * tr^2), tolerance = 1e-12)
  expect_error(orthogonal_moment(yr, rep(0, 5), w), "not .*identified")
  expect_error(orthogonal_moment(yr, tr[-1], w), "agree in length")
})

test_that("orthogonal_random_forest recovers a constant effect exactly", {
  n <- 24
  X <- cbind(seq(0.05, 0.95, length.out = n), rep(c(0.2, 0.7, 0.4), 8))
  Wc <- cbind(sin(1:n), cos(2 * (1:n)))
  Tt <- 0.5 * Wc[, 1] + rep(c(1, -1, 0.5, -0.3), 6)
  # Y is exactly linear in T and the controls, so every weighted
  # residualisation leaves Y~ = 2 T~ and theta(x) = 2 wherever identified
  Y <- 1 + 2 * Tt + 0.7 * Wc[, 1] - 0.4 * Wc[, 2]
  for (rs in c("global", "local")) {
    r <- orthogonal_random_forest(Y, Tt, X, Wc, x_eval = X[c(3, 12, 20), ], n_trees = 5,
                                  min_leaf = 2, seed = 7, residualize = rs, ridge = 0,
                                  leave_one_out = FALSE)
    expect_equal(r$theta, rep(2, 3), tolerance = 1e-9)
    expect_equal(r$estimate, 2, tolerance = 1e-9)
  }
  expect_same_function(morie_orfgrf, orthogonal_random_forest)
  expect_error(orthogonal_random_forest(Y[1:5], Tt[1:5], X[1:5, ], Wc[1:5, ]), "at least 8")
  expect_error(orthogonal_random_forest(Y, Tt[-1], X, Wc), "treatments for")
  expect_error(orf_estimate(Y, Tt, X, Wc, X[1, ], NULL, residualize = "nope"),
               "local or global")
  # orf_estimate returns weights that sum to one over the forest
  fr <- list(trees = list(list(leaf = TRUE, value = 0)))
  oe <- orf_estimate(Y, Tt, X, Wc, X[1, ], fr, residualize = "global", ridge = 0)
  expect_equal(oe$w, rep(1 / n, n), tolerance = 1e-12)
  expect_equal(oe$theta, 2, tolerance = 1e-9)
})

test_that("Otbar runs iterative Bregman projections for the entropic barycenter", {
  x <- 1:5
  C <- outer(x, x, function(a, b) (a - b)^2) / 10
  a1 <- c(0.4, 0.3, 0.2, 0.1, 0)
  a2 <- c(0, 0.1, 0.1, 0.3, 0.5)
  eps <- 0.5
  K <- exp(-C / eps)
  # one input: after two sweeps the barycenter is K (a / K'1)
  r1 <- Otbar(cbind(a1), list(C), 1, eps, max_iter = 5)
  expect_equal(r1$bary, as.numeric(K %*% (a1 / colSums(K))), tolerance = 1e-12)
  # two inputs: the Benamou et al. (2015) iteration written with matrix ops
  w <- c(0.25, 0.75)
  V <- matrix(1, 5, 2)
  AA <- cbind(a1, a2)
  for (it in 1:50) {
    KV <- K %*% V
    b <- exp(log(KV) %*% w)[, 1]
    U <- b / KV
    V <- AA / (t(K) %*% U)
  }
  r2 <- Otbar(AA, list(C, C), c(1, 3), eps, max_iter = 50)
  expect_equal(r2$bary, b, tolerance = 1e-12)
  expect_equal(r2$mass, sum(b), tolerance = 1e-12)
  expect_error(Otbar(AA, list(C), w, eps), "one cost matrix per input")
  expect_error(Otbar(AA, list(C, C[-1, ]), w, eps), "n by n")
  expect_error(Otbar(AA, list(C, C), 1, eps), "one weight per input")
  expect_error(Otbar(AA, list(C, C), w, 0), "epsilon must be positive")
})
