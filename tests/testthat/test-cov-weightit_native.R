# The native WeightIt engines (R/weightit_native.R): every value is recomputed
# in the test body from the rule the engine implements.

# the helpers live beside morie_weightit (the namespace, or covr's copy of the file)
.wn <- function(name) get(name, envir = environment(morie_weightit))

.wn_data <- function(n = 120, seed = 3) {
  set.seed(seed)
  x1 <- rnorm(n)
  x2 <- rbinom(n, 1, 0.5)
  g <- sample(c("a", "b", "c"), n, TRUE)
  t <- rbinom(n, 1, stats::plogis(-0.3 + 0.8 * x1 - 0.5 * x2))
  pm <- cbind(1, exp(0.6 * x1), exp(1.2 * x1))
  m <- factor(c("lo", "mid", "hi")[apply(pm, 1L, function(p) sample(3L, 1L, prob = p))],
              levels = c("lo", "mid", "hi"))
  a <- 0.5 * x1 + rnorm(n)
  data.frame(t = t, x1 = x1, x2 = x2, g = g, m = m, a = a, sw = runif(n, 0.5, 2))
}

test_that("every link's second derivative matches a numerical one", {
  wn_link <- .wn(".wn_link")
  eta <- c(-1.5, -0.3, 0.2, 0.9)
  for (nm in .wn(".wn_link_names")) {
    lk <- wn_link(nm)
    e2 <- if (nm %in% c("log", "clog")) -abs(eta) else eta
    h <- 1e-5
    num <- (lk$linkinv(e2 + h) - 2 * lk$linkinv(e2) + lk$linkinv(e2 - h)) / h^2
    expect_equal(lk$d2mu(e2), num, tolerance = 1e-4, info = nm)
    expect_equal(lk$linkinv(lk$linkfun(c(0.2, 0.7))), c(0.2, 0.7), tolerance = 1e-8, info = nm)
  }
  lk <- wn_link("loglog")
  expect_equal(lk$linkinv(0.5), exp(-exp(-0.5)))
  expect_equal(lk$mu.eta(0.5), exp(-0.5 - exp(-0.5)))
  expect_equal(lk$mu.eta(1e4), lk$mu.eta(700))
  expect_true(lk$valideta(1))
  expect_true(wn_link("clog")$valideta(1))
  expect_equal(wn_link("clog")$mu.eta(0.3), exp(-0.3))
})

test_that("the design drops a column collinear with the ones before it", {
  d <- .wn_data(40)
  d$x3 <- 2 * d$x1 + 1
  X <- .wn(".wn_design")(d, c("x1", "x2", "x3"))
  expect_equal(ncol(X), 2L)
  expect_equal(X[, 1L], d$x1)
  expect_equal(ncol(.wn(".wn_design")(d, "g")), 2L)
  expect_equal(ncol(.wn(".wn_design")(d, character())), 0L)
  Z <- .wn(".wn_standardize")(cbind(d$x1, rep(1, 40)), d$sw)
  p <- d$sw / sum(d$sw)
  expect_equal(sum(p * Z[, 1L]), 0, tolerance = 1e-12)
  expect_equal(sum(p * Z[, 1L]^2), 1, tolerance = 1e-12)
  expect_equal(Z[, 2L], rep(0, 40))
})

test_that("glm propensity scores equal glm's for every link and the weights follow get_w_from_ps", {
  d <- .wn_data()
  X <- cbind(d$x1, d$x2)
  ps_binary <- .wn(".wn_ps_binary")
  for (lk in c("logit", "probit", "cloglog", "cauchit")) {
    f <- ps_binary(X, d$t, d$sw, link = lk)
    ref <- stats::glm(t ~ x1 + x2, data = d, weights = sw, family = stats::quasibinomial(lk),
                      control = stats::glm.control(epsilon = 1e-12, maxit = 100L))
    expect_equal(f$ps, unname(ref$fitted.values), tolerance = 1e-6, info = lk)
    # the weighted score equation holds at the fit
    lkf <- .wn(".wn_link")(lk)
    eta <- lkf$linkfun(f$ps)
    sc <- crossprod(cbind(1, X), d$sw * (d$t - f$ps) * lkf$mu.eta(eta) / (f$ps * (1 - f$ps)))
    expect_equal(as.numeric(sc) / nrow(d), rep(0, 3), tolerance = 1e-7, info = lk)
  }
  for (lk in c("log", "loglog", "clog", "identity")) {
    # on these data the clog and identity fits stop at the iteration cap and say so
    w <- testthat::capture_warnings(f <- ps_binary(X, d$t, d$sw, link = lk))
    expect_equal(w, rep("the propensity model did not converge", lk %in% c("clog", "identity")), info = lk)
    expect_true(all(f$ps > 0 & f$ps < 1), info = lk)
    expect_equal(f$link$name, lk)
  }
  expect_warning(ps_binary(X, d$t, d$sw, link = "cauchit", ps_model = "mle"), NA)
  ps <- ps_binary(X, d$t, d$sw)$ps
  w_binary <- .wn(".wn_w_binary")
  t <- d$t
  expect_equal(w_binary(ps, t, "ATE"), ifelse(t == 1, 1 / ps, 1 / (1 - ps)))
  expect_equal(w_binary(ps, t, "ATT"), ifelse(t == 1, 1, ps / (1 - ps)))
  expect_equal(w_binary(ps, t, "ATC"), ifelse(t == 1, (1 - ps) / ps, 1))
  expect_equal(w_binary(ps, t, "ATO"), ifelse(t == 1, 1 - ps, ps))
  atm <- w_binary(ps, t, "ATM")
  expect_equal(atm, ifelse(t == 1, pmin(1, (1 - ps) / ps), pmin(1, ps / (1 - ps))))
  atos <- w_binary(ps, t, "ATOS")
  a <- .wn(".wn_atos_alpha")(ps)
  expect_true(a >= 0 && a < 0.5)
  expect_equal(atos[ps < a | ps > 1 - a], rep(0, sum(ps < a | ps > 1 - a)))
  inside <- ps >= a & ps <= 1 - a
  expect_equal(atos[inside], ifelse(t == 1, 1 / ps, 1 / (1 - ps))[inside])
  # Crump et al.'s alpha: the smallest trimming level whose rule holds
  z <- ps * (1 - ps)
  if (a > 0) expect_true(2 * a * (1 - a) * mean(1 / z[z >= a * (1 - a)]) >= 1)
  expect_error(ps_binary(X, t, d$sw, link = "probit", ps_model = "ridge"), "use link")
  expect_error(ps_binary(X, t, d$sw, ps_model = "bogus"), "ps_model must be")
})

test_that("stabilised weights multiply by the marginal level probability", {
  d <- .wn_data(50)
  w <- rep(1, 50)
  s <- .wn(".wn_stabilize")(w, d$m)
  expect_equal(s, as.numeric(table(d$m)[as.character(d$m)] / 50))
})

test_that("Firth's fit solves the bias-reduced score equation", {
  d <- .wn_data()
  X <- cbind(1, d$x1, d$x2)
  f <- .wn(".wn_firth")(X, d$t, d$sw, "logit")
  eta <- as.numeric(X %*% f$coef)
  mu <- stats::plogis(eta)
  w <- d$sw * mu * (1 - mu)
  H <- X %*% solve(crossprod(X, X * w)) %*% t(X)
  h <- diag(H) * w
  # modified score: X'(sw (t - mu) + h (1/2 - mu))
  score <- crossprod(X, d$sw * (d$t - mu) + h * (0.5 - mu))
  expect_equal(as.numeric(score), rep(0, 3), tolerance = 1e-6)
  expect_warning(.wn(".wn_firth")(X, d$t, d$sw, "logit", max_iter = 1L), "did not converge")
  flog <- .wn(".wn_firth")(X[, 1:2], d$t, d$sw, "log")
  expect_true(all(flog$ps > 0 & flog$ps < 1))
})

test_that("the penalised models satisfy their stationarity conditions", {
  d <- .wn_data()
  pen <- .wn(".wn_penalized_logit")
  X <- cbind(1, .wn(".wn_standardize")(cbind(d$x1, d$x2), d$sw))
  lambda <- 0.7
  # ridge, package scale: gradient of the log-likelihood minus lambda beta_j (j > 1)
  r <- pen(X, d$t, d$sw, lambda, alpha = 0, scale = "sum")
  g <- as.numeric(crossprod(X, d$sw * (d$t - r$ps))) - c(0, lambda * r$coef[-1L])
  expect_equal(g, rep(0, 3), tolerance = 1e-7)
  # lasso, glmnet scale: KKT conditions on the mean log-likelihood
  l <- pen(X, d$t, d$sw, lambda = 0.05, alpha = 1)
  sw <- d$sw / sum(d$sw)
  gl <- as.numeric(crossprod(X, sw * (d$t - l$ps)))
  expect_equal(gl[1L], 0, tolerance = 1e-7)
  for (j in 2:3) {
    if (abs(l$coef[j]) > 1e-10) {
      expect_equal(gl[j], 0.05 * sign(l$coef[j]), tolerance = 1e-6)
    } else {
      expect_lte(abs(gl[j]), 0.05 + 1e-6)
    }
  }
  e <- pen(X, d$t, d$sw, lambda = 0.05, alpha = 0.5)
  ge <- as.numeric(crossprod(X, sw * (d$t - e$ps)))
  for (j in 2:3) {
    if (abs(e$coef[j]) > 1e-10) {
      expect_equal(ge[j] - 0.05 * 0.5 * e$coef[j], 0.05 * 0.5 * sign(e$coef[j]), tolerance = 1e-6)
    }
  }
  big <- pen(X, d$t, d$sw, lambda = 50, alpha = 1)
  expect_equal(big$coef[-1L], c(0, 0))
})

test_that("multinomial and ordinal propensity models match nnet and MASS", {
  d <- .wn_data()
  X <- cbind(d$x1, d$x2)
  P <- .wn(".wn_multinom")(X, d$m, d$sw)
  expect_equal(colnames(P), levels(d$m))
  expect_equal(rowSums(P), rep(1, nrow(d)))
  # the score of the multinomial likelihood is zero at the fit
  Y <- outer(as.integer(d$m), 1:3, "==") * 1
  score <- crossprod(cbind(1, X), d$sw * (Y - P)[, -1L])
  expect_equal(as.numeric(score), rep(0, 6), tolerance = 1e-6)
  if (requireNamespace("nnet", quietly = TRUE)) {
    ref <- utils::capture.output(fit <- nnet::multinom(m ~ x1 + x2, data = d, weights = sw, maxit = 500, reltol = 1e-12))
    expect_equal(unname(P), unname(stats::fitted(fit)), tolerance = 1e-5)
  }
  O <- .wn(".wn_ordinal")(X, d$m, d$sw)
  expect_equal(rowSums(O), rep(1, nrow(d)))
  if (requireNamespace("MASS", quietly = TRUE)) {
    ref <- suppressWarnings(MASS::polr(m ~ x1 + x2, data = d, weights = sw, Hess = FALSE))
    expect_equal(unname(O), unname(stats::fitted(ref)), tolerance = 1e-4)
    refp <- suppressWarnings(MASS::polr(m ~ x1 + x2, data = d, weights = sw, method = "probit"))
    Op <- .wn(".wn_ordinal")(X, d$m, d$sw, link = "probit")
    expect_equal(unname(Op), unname(stats::fitted(refp)), tolerance = 1e-4)
  }
  O0 <- .wn(".wn_ordinal")(X[, 0, drop = FALSE], d$m, d$sw)
  expect_equal(O0[1L, ], as.numeric(tapply(d$sw, d$m, sum) / sum(d$sw)), tolerance = 1e-6, ignore_attr = TRUE)
  w_multi <- .wn(".wn_w_multi")
  idx <- cbind(seq_len(nrow(d)), as.integer(d$m))
  expect_equal(w_multi(P, d$m, "ATE"), 1 / P[idx])
  att <- w_multi(P, d$m, "ATT", focal = "hi")
  expect_equal(att[d$m == "hi"], rep(1, sum(d$m == "hi")))
  expect_equal(att[d$m != "hi"], (P[, "hi"] / P[idx])[d$m != "hi"])
  expect_equal(w_multi(P, d$m, "ATO"), (1 / P[idx]) / rowSums(1 / P))
  expect_equal(w_multi(P, d$m, "ATM"), apply(P, 1, min) / P[idx])
  expect_error(w_multi(P, d$m, "ATOS"), "not available for a multi-category")
})

test_that("continuous-treatment weights are the ratio of two residual densities", {
  d <- .wn_data()
  X <- cbind(d$x1, d$x2)
  wc <- .wn(".wn_w_continuous")
  r <- wc(X, d$a, d$sw)
  sw <- d$sw / mean(d$sw)
  fit <- stats::lm(a ~ x1 + x2, data = d, weights = sw)
  m <- unname(stats::fitted(fit))
  expect_equal(r$fitted, m, tolerance = 1e-8)
  # WeightIt 2.x: the denominator is the density of the standardised residual; the numerator is
  # the marginal density of a, the sampling-weighted mixture of the conditional densities on a
  # 50-point grid of a, splined at the observed values
  s <- sqrt(mean(sw * (d$a - m)^2))
  zd <- (d$a - m) / s
  grid <- seq(min(d$a), max(d$a), length.out = 50L)
  marginal <- function(f) {
    num <- vapply(grid, function(g) sum(sw * f((g - m) / s)) / sum(sw), numeric(1))
    stats::spline(grid, num, xout = d$a)$y
  }
  expect_equal(r$w, marginal(stats::dnorm) / stats::dnorm(zd), tolerance = 1e-8)
  lap <- function(z) exp(-abs(z)) / 2
  expect_equal(wc(X, d$a, d$sw, density = "laplace")$w, marginal(lap) / lap(zd), tolerance = 1e-8)
  t5 <- function(z) stats::dt(z, 5)
  expect_equal(wc(X, d$a, d$sw, density = "t_5")$w, marginal(t5) / t5(zd), tolerance = 1e-8)
  n2 <- function(z) stats::dnorm(z, sd = 2)
  expect_equal(wc(X, d$a, d$sw, density = function(x) stats::dnorm(x, sd = 2))$w, marginal(n2) / n2(zd),
               tolerance = 1e-8)
  expect_silent(k <- wc(X, d$a, d$sw, density = "kernel")$w)
  expect_true(all(is.finite(k) & k > 0))
  k2 <- wc(X, d$a, d$sw, density = "kernel", bw = 0.5, adjust = 2)$w
  expect_false(isTRUE(all.equal(k, k2)))
  expect_error(wc(X, d$a, d$sw, density = "cauchy"), "density must be")
})

test_that("balance terms add powers and centred products and drop the redundant", {
  d <- .wn_data(60)
  X <- cbind(d$x1, d$x2, d$x1 + d$x2)
  terms <- .wn(".wn_terms")
  expect_equal(ncol(terms(X, d$sw)), 2L)
  T2 <- terms(X[, 1:2], d$sw, moments = 2L)
  expect_equal(ncol(T2), 3L)
  expect_equal(T2[, 3L], d$x1^2)
  T3 <- terms(X[, 1:2], d$sw, moments = 1L, int = TRUE)
  expect_equal(ncol(T3), 3L)
  m1 <- sum(d$x1 * d$sw) / sum(d$sw)
  m2 <- sum(d$x2 * d$sw) / sum(d$sw)
  expect_equal(T3[, 3L], (d$x1 - m1) * (d$x2 - m2))
  expect_equal(ncol(terms(X[, 0, drop = FALSE], d$sw)), 0L)
})

test_that("entropy balancing reaches exact moment balance for every estimand", {
  d <- .wn_data()
  X <- cbind(d$x1, d$x2)
  ebal <- .wn(".wn_ebal")
  tr <- factor(d$t)
  wm <- function(v, w) sum(v * w) / sum(w)
  w <- ebal(X, tr, d$sw, "ATT", focal = "1")
  for (j in 1:2) {
    expect_equal(wm(X[d$t == 0, j], (w * d$sw)[d$t == 0]), wm(X[d$t == 1, j], d$sw[d$t == 1]), tolerance = 1e-8)
  }
  expect_equal(w[d$t == 1], rep(1, sum(d$t == 1)))
  w <- ebal(X, tr, d$sw, "ATE")
  for (j in 1:2) for (g in 0:1) {
    expect_equal(wm(X[d$t == g, j], (w * d$sw)[d$t == g]), wm(X[, j], d$sw), tolerance = 1e-8)
  }
  w <- ebal(X, d$m, d$sw, "ATT", focal = "hi")
  for (j in 1:2) for (g in c("lo", "mid")) {
    expect_equal(wm(X[d$m == g, j], (w * d$sw)[d$m == g]), wm(X[d$m == "hi", j], d$sw[d$m == "hi"]), tolerance = 1e-8)
  }
  # base weights q: the solution is q * exp(-C z), so log(w / q) lies in the span of (1, X)
  q <- exp(d$x1 / 2)
  wq <- ebal(X, tr, d$sw, "ATT", focal = "1", q = q)
  for (j in 1:2) {
    expect_equal(wm(X[d$t == 0, j], (wq * d$sw)[d$t == 0]), wm(X[d$t == 1, j], d$sw[d$t == 1]), tolerance = 1e-8)
  }
  res <- stats::lm.fit(cbind(1, X[d$t == 0, ]), log(wq / q)[d$t == 0])$residuals
  expect_equal(max(abs(res)), 0, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(wq[d$t == 0], w[d$t == 0])))
  # an impossible target: a covariate the control group never reaches
  bad <- cbind(ifelse(d$t == 1, 5, 0))
  expect_warning(ebal(bad, tr, d$sw, "ATT", focal = "1"), "exact balance")
})

test_that("continuous entropy balancing zeroes the weighted treatment-covariate correlations", {
  d <- .wn_data()
  X <- cbind(d$x1, d$x2)
  w <- .wn(".wn_ebal_continuous")(X, d$a, d$sw)
  sw <- w * d$sw
  wm <- function(v) sum(v * sw) / sum(sw)
  expect_equal(wm(d$a), mean(d$a * d$sw) / mean(d$sw), tolerance = 1e-7)
  for (j in 1:2) {
    expect_equal(wm(X[, j]), sum(X[, j] * d$sw) / sum(d$sw), tolerance = 1e-7)
    expect_equal(wm((d$a - wm(d$a)) * (X[, j] - wm(X[, j]))), 0, tolerance = 1e-7)
  }
  w2 <- .wn(".wn_ebal_continuous")(X, d$a, d$sw, d_moments = 2L)
  sw2 <- w2 * d$sw
  expect_equal(sum(d$a^2 * sw2) / sum(sw2), sum(d$a^2 * d$sw) / sum(d$sw), tolerance = 1e-6)
})

test_that("inverse probability tilting balances each group to its target exactly", {
  d <- .wn_data()
  X <- cbind(d$x1, d$x2)
  C <- cbind(1, X)
  ipt <- .wn(".wn_ipt")
  r <- ipt(X, d$t, d$sw, "ATE")
  for (g in 0:1) {
    expect_equal(colSums(C[d$t == g, ] * (r$w * d$sw)[d$t == g]) / sum(d$sw), colSums(C * d$sw) / sum(d$sw),
                 tolerance = 1e-7)
  }
  r <- ipt(X, d$t, d$sw, "ATT")
  expect_equal(colSums(C[d$t == 0, ] * (r$w * d$sw)[d$t == 0]), colSums(C[d$t == 1, ] * d$sw[d$t == 1]),
               tolerance = 1e-7)
  r <- ipt(X, d$t, d$sw, "ATC", link = "probit")
  expect_equal(colSums(C[d$t == 1, ] * (r$w * d$sw)[d$t == 1]), colSums(C[d$t == 0, ] * d$sw[d$t == 0]),
               tolerance = 1e-6)
  expect_error(ipt(X, d$t, d$sw, "ATO"), "ipt takes")
  w <- .wn(".wn_ipt_multi")(X, d$m, d$sw, "ATE")
  for (g in levels(d$m)) {
    expect_equal(colSums(C[d$m == g, ] * (w * d$sw)[d$m == g]) / sum(d$sw), colSums(C * d$sw) / sum(d$sw),
                 tolerance = 1e-7)
  }
  w <- .wn(".wn_ipt_multi")(X, d$m, d$sw, "ATT", focal = "hi")
  for (g in c("lo", "mid")) {
    expect_equal(colSums(C[d$m == g, ] * (w * d$sw)[d$m == g]), colSums(C[d$m == "hi", ] * d$sw[d$m == "hi"]),
                 tolerance = 1e-6)
  }
  expect_equal(w[d$m == "hi"], rep(1, sum(d$m == "hi")))
})

test_that("the covariate balancing propensity score solves its balance equations", {
  d <- .wn_data()
  X <- cbind(d$x1, d$x2)
  cbps <- .wn(".wn_cbps")
  Cb <- cbind(1, scale(svd(X)$u))
  t <- d$t
  for (est in c("ATE", "ATT", "ATC", "ATO")) {
    r <- cbps(X, t, d$sw, est)
    p <- r$ps
    f <- switch(est,
      ATE = t / p - (1 - t) / (1 - p), ATT = (t - p) / (1 - p), ATC = (t - p) / p, ATO = t - p)
    expect_equal(as.numeric(colMeans(d$sw * f * Cb)), rep(0, 3), tolerance = 1e-7, info = est)
    expect_equal(r$w, .wn(".wn_w_binary")(p, t, est), info = est)
  }
  o <- cbps(X, t, d$sw, "ATE", over = TRUE)
  expect_true(is.function(o$objective))
  expect_true(is.finite(o$objective(o$ps)))
  expect_lte(o$objective(o$ps), o$objective(cbps(X, t, d$sw, "ATE")$ps) + 1e-8)
  expect_error(cbps(X, t, d$sw, "ATM"), "cbps takes")
})

test_that("the Newton solver halves its step and reports non-convergence", {
  newton <- .wn(".wn_newton")
  psi <- function(B) cbind((B[1] - 2)^3, B[2] - 1)
  jac <- function(B) diag(c(3 * (B[1] - 2)^2 + 1e-12, 1))
  r <- newton(c(5, 5), psi, jac, max_iter = 3L)
  expect_false(r$converged)
  r <- newton(c(5, 5), psi, jac, max_iter = 5000L)
  expect_equal(r$par[2L], 1)
  # a singular Jacobian stops the iteration
  r <- newton(c(1, 1), function(B) cbind(B[1], B[2]), function(B) matrix(0, 2, 2))
  expect_equal(r$par, c(1, 1))
  expect_false(r$converged)
})

test_that("morie_weightit routes every treatment type and method and refuses the rest", {
  d <- .wn_data()
  X <- cbind(d$x1, d$x2)
  r <- morie_weightit(d, "t", c("x1", "x2"))
  expect_s3_class(r, "morie_weightit")
  expect_equal(r$treatment_type, "binary")
  expect_equal(r$weights, .wn(".wn_w_binary")(.wn(".wn_ps_binary")(X, d$t, rep(1, nrow(d)))$ps, d$t, "ATE"))
  expect_equal(r$ess, sum(r$weights)^2 / sum(r$weights^2))
  expect_output(print(r), "morie_weightit: glm, binary treatment, estimand ATE")
  rs <- morie_weightit(d, "t", c("x1", "x2"), sampling_weights = "sw", stabilize = TRUE)
  ps <- .wn(".wn_ps_binary")(X, d$t, d$sw)$ps
  expect_equal(rs$weights, .wn(".wn_stabilize")(.wn(".wn_w_binary")(ps, d$t, "ATE"), factor(d$t)) * d$sw)
  expect_equal(morie_weightit(d, "t", "x1", sampling_weights = d$sw)$weights,
               morie_weightit(d, "t", "x1", sampling_weights = "sw")$weights)
  for (m in c("ebal", "ipt", "cbps")) {
    r <- morie_weightit(d, "t", c("x1", "x2"), method = m, estimand = "ATT")
    expect_equal(r$focal, "1", info = m)
    expect_equal(r$weights[d$t == 1], rep(1, sum(d$t == 1)), info = m)
  }
  expect_equal(morie_weightit(d, "t", "x1", method = "ebal", estimand = "ATC")$weights[d$t == 0],
               rep(1, sum(d$t == 0)))
  expect_equal(morie_weightit(d, "t", "x1", method = "glm", ps_model = "firth", link = "probit")$ps,
               .wn(".wn_firth")(cbind(1, d$x1), d$t, rep(1, nrow(d)), "probit")$ps)
  o <- morie_weightit(d, "t", "x1", method = "cbps", over = TRUE)
  expect_true(is.function(o$objective))
  # multi-category
  r <- morie_weightit(d, "m", c("x1", "x2"), estimand = "ATT", focal = "hi")
  expect_equal(r$treatment_type, "multi")
  expect_equal(dim(r$ps), c(nrow(d), 3L))
  expect_equal(r$weights[d$m == "hi"], rep(1, sum(d$m == "hi")))
  ro <- morie_weightit(d, "m", "x1", ordinal = TRUE)
  expect_equal(ro$ps, .wn(".wn_ordinal")(cbind(d$x1), d$m, rep(1, nrow(d))))
  expect_equal(morie_weightit(d, "m", "x1", method = "ebal", estimand = "ATE")$treatment_type, "multi")
  expect_equal(morie_weightit(d, "m", "x1", method = "ipt", estimand = "ATT", focal = "lo")$focal, "lo")
  expect_equal(morie_weightit(d, "g", "x1", method = "ipt")$treatment_type, "multi")
  expect_error(morie_weightit(d, "m", "x1", method = "cbps"), "binary treatment")
  expect_error(morie_weightit(d, "m", "x1", estimand = "ATT"), "needs focal")
  expect_error(morie_weightit(d, "m", "x1", estimand = "ATT", focal = "nope"), "needs focal")
  expect_error(morie_weightit(d, "m", "x1", method = "ebal", estimand = "ATO"), "ebal takes")
  expect_error(morie_weightit(d, "m", "x1", method = "ipt", estimand = "ATO"), "ipt takes")
  expect_error(morie_weightit(d, "t", "x1", method = "ebal", estimand = "ATO"), "ebal takes")
  # continuous
  r <- morie_weightit(d, "a", c("x1", "x2"))
  expect_equal(r$treatment_type, "continuous")
  expect_equal(r$estimand, "ATE")
  expect_equal(r$weights, .wn(".wn_w_continuous")(X, d$a, rep(1, nrow(d)))$w)
  expect_equal(morie_weightit(d, "a", "x1", method = "ebal", estimand = "ATT")$treatment_type, "continuous")
  expect_error(morie_weightit(d, "a", "x1", method = "ipt"), "binary or multi-category")
  # input checks
  expect_error(morie_weightit(d, "t", "zz"), "Columns missing")
  expect_error(morie_weightit(d, "t", "x1", estimand = "ATZ"), "estimand must be")
  expect_error(morie_weightit(d, "t", "x1", sampling_weights = -d$sw), "sampling_weights")
  expect_error(morie_weightit(transform(d, t = 1), "t", "x1"), "one level")
  expect_error(morie_weightit(transform(d, t = as.Date(t, origin = "2000-01-01")), "t", "x1"), "must be numeric")
  expect_error(morie_weightit(transform(d, a = replace(a, 1, NA)), "a", "x1"), "missing values")
  expect_equal(morie_weightit(transform(d, t = t == 1), "t", "x1")$treatment_type, "binary")
  expect_equal(morie_weightit(transform(d, k = sample(1:3, nrow(d), TRUE)), "k", "x1")$treatment_type, "multi")
  expect_equal(morie_weightit(transform(d, t = factor(t)), "t", "x1", estimand = "ATOS")$treatment_type, "binary")
})

test_that("morie_causal_weighting runs ebal for the ATE and ipt natively", {
  d <- .wn_data(80)
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) package != "WeightIt",
                                  .package = "base")
  r <- morie_causal_weighting(d, "t", "x1", method = "ebal", estimand = "ATE")
  expect_null(r$weightit)
  expect_equal(r$weights, morie_weightit(d, "t", "x1", method = "ebal", estimand = "ATE")$weights)
  r <- morie_causal_weighting(d, "t", "x1", method = "ipt", estimand = "ATT")
  expect_equal(r$weights, morie_weightit(d, "t", "x1", method = "ipt", estimand = "ATT")$weights)
  expect_equal(r$propensity_scores, morie_weightit(d, "t", "x1", method = "ipt", estimand = "ATT")$ps)
  expect_error(morie_causal_weighting(d, "t", "x1", method = "energy"), "WeightIt")
})

test_that("the native engines agree with WeightIt where it is installed", {
  skip_if_not_installed("WeightIt", "1.0.0")
  d <- .wn_data()
  cmp <- function(ours, theirs, tol = 1e-5) expect_equal(unname(ours), unname(as.numeric(theirs)), tolerance = tol)
  w <- WeightIt::weightit(t ~ x1 + x2, data = d, method = "glm", estimand = "ATE")
  cmp(morie_weightit(d, "t", c("x1", "x2"))$weights, w$weights)
  w <- WeightIt::weightit(t ~ x1 + x2, data = d, method = "glm", estimand = "ATT", s.weights = "sw",
                          link = "probit")
  cmp(morie_weightit(d, "t", c("x1", "x2"), estimand = "ATT", sampling_weights = "sw", link = "probit")$weights,
      w$weights * d$sw)
  w <- WeightIt::weightit(t ~ x1 + x2, data = d, method = "ebal", estimand = "ATE")
  cmp(morie_weightit(d, "t", c("x1", "x2"), method = "ebal")$weights, w$weights, 1e-4)
  w <- WeightIt::weightit(t ~ x1 + x2, data = d, method = "ipt", estimand = "ATT")
  cmp(morie_weightit(d, "t", c("x1", "x2"), method = "ipt", estimand = "ATT")$weights, w$weights, 1e-4)
  w <- WeightIt::weightit(t ~ x1 + x2, data = d, method = "cbps", estimand = "ATE")
  cmp(morie_weightit(d, "t", c("x1", "x2"), method = "cbps")$weights, w$weights, 1e-4)
  w <- WeightIt::weightit(m ~ x1 + x2, data = d, method = "glm", estimand = "ATE")
  cmp(morie_weightit(d, "m", c("x1", "x2"))$weights, w$weights, 1e-4)
  w <- WeightIt::weightit(a ~ x1 + x2, data = d, method = "glm")
  cmp(morie_weightit(d, "a", c("x1", "x2"))$weights, w$weights, 1e-5)
  w <- WeightIt::weightit(a ~ x1 + x2, data = d, method = "ebal")
  cmp(morie_weightit(d, "a", c("x1", "x2"), method = "ebal")$weights, w$weights, 1e-4)
})

test_that("the solvers say so when separated data give no balancing solution", {
  set.seed(1)
  n <- 60
  x <- rnorm(n)
  t <- as.numeric(x > 0)
  X <- cbind(x)
  one <- rep(1, n)
  ipt_msg <- "inverse probability tilting did not converge"
  # the ATE tilts each group in turn, so both say so
  expect_equal(testthat::capture_warnings(.wn(".wn_ipt")(X, t, one, "ATE")), rep(ipt_msg, 2))
  expect_equal(testthat::capture_warnings(.wn(".wn_ipt")(X, t, one, "ATT")), ipt_msg)
  expect_equal(testthat::capture_warnings(.wn(".wn_cbps")(X, t, one, "ATE")),
               "the covariate balancing propensity score did not converge")
  m <- factor(c("a", "b", "c")[1 + (x > -0.5) + (x > 0.5)])
  expect_equal(testthat::capture_warnings(.wn(".wn_ipt_multi")(X, m, one, "ATE")), rep(ipt_msg, 2))
  # no unit below one half: Crump's search has nothing to trim and alpha is zero
  expect_equal(.wn(".wn_atos_alpha")(rep(0.7, 10)), 0)
  # a log-link start is the marginal rate, no fit
  st <- .wn(".wn_glm_start")(cbind(1, x), t, one, .wn(".wn_link")("log"))
  expect_equal(st, c(log(mean(t)), 0))
  expect_null(.wn(".wn_density")("kernel"))
})
