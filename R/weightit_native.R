# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native balancing weights under WeightIt's method names and rules (Greifer,
# WeightIt 1.7.0, GPL >= 2; the rules are re-implemented, no code is called):
# the propensity models and their links, the weights each estimand takes
# from a propensity score (get_w_from_ps), and the continuous-treatment
# density ratio. tests/cross/test-weightit-native.R compares every engine
# with WeightIt itself.

# ---- links ------------------------------------------------------------------

.wn_link_names <- c("logit", "probit", "cloglog", "loglog", "identity", "log", "clog", "cauchit")

# a binomial link with its first two derivatives of the inverse link (the
# second for the bias-reduced fit); "loglog" and "clog" are WeightIt's own
.wn_link <- function(link) {
  eps <- .Machine$double.eps
  lk <- switch(link,
    loglog = structure(list(
      linkfun = function(mu) -log(-log(mu)),
      linkinv = function(eta) pmax(exp(-exp(-eta)), eps),
      mu.eta = function(eta) {
        eta <- pmin(eta, 700)
        pmax(exp(-eta - exp(-eta)), eps)
      },
      valideta = function(eta) TRUE, name = "loglog"), class = "link-glm"),
    clog = structure(list(
      linkfun = function(mu) -log(1 - mu),
      linkinv = function(eta) pmin(1 - exp(-eta), 1 - eps),
      mu.eta = function(eta) pmax(exp(-eta), eps),
      valideta = function(eta) TRUE, name = "clog"), class = "link-glm"),
    stats::make.link(link)
  )
  lk$d2mu <- switch(link,
    logit = function(eta) { m <- stats::plogis(eta); m * (1 - m) * (1 - 2 * m) },
    probit = function(eta) -eta * stats::dnorm(eta),
    cloglog = function(eta) { d <- exp(eta - exp(eta)); d * (1 - exp(eta)) },
    loglog = function(eta) { d <- exp(-eta - exp(-eta)); d * (exp(-eta) - 1) },
    cauchit = function(eta) -2 * eta / (pi * (1 + eta^2)^2),
    identity = function(eta) 0 * eta,
    log = function(eta) exp(eta),
    clog = function(eta) -exp(-eta)
  )
  lk
}

# ---- the covariates -----------------------------------------------------------

# model.matrix-style columns (dummies for a factor), less any column that is
# collinear with the intercept and the columns before it (WeightIt's
# make_full_rank; glm would alias it)
.wn_design <- function(data, covariates) {
  X <- .mor_ps_design(data, covariates)
  if (!ncol(X)) return(X)
  q <- qr(cbind(1, X), tol = 1e-7)
  keep <- sort(q$pivot[seq_len(q$rank)])
  X[, keep[keep > 1L] - 1L, drop = FALSE]
}

# standardised with the sampling weights (penalties are scale dependent)
.wn_standardize <- function(X, sw) {
  sw <- sw / sum(sw)
  for (j in seq_len(ncol(X))) {
    m <- sum(sw * X[, j])
    s <- sqrt(sum(sw * (X[, j] - m)^2))
    X[, j] <- (X[, j] - m) / if (s > 0) s else 1
  }
  X
}

# ---- binary propensity models ---------------------------------------------------

# P(treated | X): an unpenalised GLM (any link), ridge, lasso or elastic net
# on standardised covariates (logit), or Firth's bias-reduced GLM (mean bias
# reduction, Kosmidis & Firth 2009; brglm2's default for these models)
.wn_ps_binary <- function(X, t, sw, link = "logit", ps_model = "mle", lambda = 1, alpha = 1) {
  if (ps_model %in% c("ridge", "lasso", "elasticnet") && link != "logit") {
    stop(sprintf("ps_model = \"%s\" is a penalised logistic model: use link = \"logit\"", ps_model), call. = FALSE)
  }
  Xi <- cbind(1, X)
  switch(ps_model,
    mle = {
      lk <- .wn_link(link)
      fam <- stats::quasibinomial(link = lk)
      start <- mustart <- NULL
      if (link %in% c("log", "clog", "identity")) {
        start <- c(lk$linkfun(sum(sw * t) / sum(sw)), rep(0, ncol(X)))
      } else {
        mustart <- 0.25 + 0.5 * t
      }
      fit <- suppressWarnings(stats::glm.fit(Xi, t, weights = sw, start = start, mustart = mustart, family = fam))
      if (!fit$converged) warning("the propensity model did not converge", call. = FALSE)
      list(ps = as.numeric(fit$fitted.values), coef = fit$coefficients, X = Xi, link = lk)
    },
    firth = .wn_firth(Xi, t, sw, link),
    ridge = .wn_penalized_logit(cbind(1, .wn_standardize(X, sw)), t, sw, lambda, alpha = 0, scale = "sum"),
    lasso = .wn_penalized_logit(cbind(1, .wn_standardize(X, sw)), t, sw, lambda, alpha = 1),
    elasticnet = .wn_penalized_logit(cbind(1, .wn_standardize(X, sw)), t, sw, lambda, alpha = alpha),
    stop("ps_model must be one of \"mle\", \"firth\", \"ridge\", \"lasso\", \"elasticnet\"", call. = FALSE)
  )
}

# mean-bias-reduced binomial GLM: the score plus h_i d'_i / (2 d_i) times
# the working weight (h the leverages), solved by Fisher scoring
.wn_firth <- function(X, t, sw, link, max_iter = 200L, tol = 1e-10) {
  lk <- .wn_link(link)
  mu0 <- if (link %in% c("log", "clog", "identity")) sum(sw * t) / sum(sw) else 0.25 + 0.5 * t
  beta <- if (link %in% c("log", "clog", "identity")) c(lk$linkfun(mu0), rep(0, ncol(X) - 1L)) else
    as.numeric(qr.coef(qr(X), lk$linkfun(mu0)))
  beta[is.na(beta)] <- 0
  for (it in seq_len(max_iter)) {
    eta <- as.numeric(X %*% beta)
    mu <- pmin(pmax(lk$linkinv(eta), 1e-12), 1 - 1e-12)
    d <- lk$mu.eta(eta)
    v <- mu * (1 - mu)
    w <- sw * d^2 / v
    XtWX <- crossprod(X, X * w)
    Ri <- solve(XtWX)
    h <- rowSums((X %*% Ri) * X) * w
    score <- crossprod(X, sw * d / v * (t - mu) + h * lk$d2mu(eta) / (2 * d))
    step <- as.numeric(Ri %*% score)
    beta <- beta + step
    if (max(abs(step)) < tol) break
  }
  if (max(abs(step)) >= tol) warning("the bias-reduced propensity model did not converge", call. = FALSE)
  list(ps = lk$linkinv(as.numeric(X %*% beta)), coef = beta, X = X, link = lk)
}

# penalised logistic regression, intercept unpenalised. scale = "sum": the
# log-likelihood minus lambda / 2 * ||beta||^2 (the package's ridge, as
# morie's Python); "mean": glmnet's scale, the mean log-likelihood minus
# lambda * ((1 - alpha) / 2 * ||beta||^2 + alpha * ||beta||_1), by IRLS with
# coordinate descent (Friedman, Hastie & Tibshirani 2010)
.wn_penalized_logit <- function(X, t, sw, lambda, alpha, scale = "mean",
                                max_iter = 500L, tol = 1e-12) {
  n <- nrow(X)
  p <- ncol(X)
  sw <- if (scale == "mean") sw / sum(sw) else sw
  beta <- numeric(p)
  for (it in seq_len(max_iter)) {
    eta <- pmin(pmax(as.numeric(X %*% beta), -30), 30)
    mu <- stats::plogis(eta)
    w <- sw * pmax(mu * (1 - mu), 1e-10)
    z <- eta + (t - mu) / pmax(mu * (1 - mu), 1e-10)
    old <- beta
    if (alpha == 0) {
      beta <- as.numeric(solve(crossprod(X, X * w) + diag(c(0, rep(lambda, p - 1L)), p), crossprod(X, w * z)))
    } else {
      r <- z - as.numeric(X %*% beta)
      for (sweep in seq_len(1000L)) {
        moved <- 0
        for (j in seq_len(p)) {
          bj <- beta[j]
          g <- sum(w * X[, j] * r) + bj * sum(w * X[, j]^2)
          beta[j] <- if (j == 1L) g / sum(w) else
            sign(g) * max(abs(g) - lambda * alpha, 0) / (sum(w * X[, j]^2) + lambda * (1 - alpha))
          if (beta[j] != bj) {
            r <- r - X[, j] * (beta[j] - bj)
            moved <- max(moved, abs(beta[j] - bj))
          }
        }
        if (moved < tol) break
      }
    }
    if (max(abs(beta - old)) < tol) break
  }
  list(ps = stats::plogis(as.numeric(X %*% beta)), coef = beta, X = X, link = .wn_link("logit"))
}

# ---- weights from propensity scores (WeightIt's get_w_from_ps) -------------------

.wn_atos_alpha <- function(ps) {
  ps_sorted <- sort(c(ps, 1 - ps))
  z <- ps * (1 - ps)
  for (i in seq_len(sum(ps < 0.5))) {
    if (i == 1L || abs(ps_sorted[i] - ps_sorted[i - 1L]) > sqrt(.Machine$double.eps)) {
      a <- ps_sorted[i] * (1 - ps_sorted[i])
      if (2 * a * sum(1 / z[z >= a]) / sum(z >= a) >= 1) return(ps_sorted[i])
    }
  }
  0
}

.wn_w_binary <- function(ps, t, estimand) {
  w <- rep(1, length(t))
  c0 <- t == 0
  switch(estimand,
    ATE = {
      w[c0] <- 1 / (1 - ps[c0])
      w[!c0] <- 1 / ps[!c0]
    },
    ATT = w[c0] <- ps[c0] / (1 - ps[c0]),
    ATC = w[!c0] <- (1 - ps[!c0]) / ps[!c0],
    ATO = {
      w[c0] <- ps[c0]
      w[!c0] <- 1 - ps[!c0]
    },
    ATM = {
      lo <- c0 & ps < 0.5
      hi <- !c0 & ps > 0.5
      w[lo] <- ps[lo] / (1 - ps[lo])
      w[hi] <- (1 - ps[hi]) / ps[hi]
    },
    ATOS = {
      w[c0] <- 1 / (1 - ps[c0])
      w[!c0] <- 1 / ps[!c0]
      a <- .wn_atos_alpha(ps)
      w[ps < a | ps > 1 - a] <- 0
    }
  )
  w
}

# ps_mat: n x K, columns the treatment levels; focal a level
.wn_w_multi <- function(ps_mat, treat, estimand, focal = NULL) {
  idx <- cbind(seq_along(treat), match(as.character(treat), colnames(ps_mat)))
  w <- 1 / ps_mat[idx]
  switch(estimand,
    ATE = w,
    ATT = , ATC = {
      f <- treat == focal
      w[f] <- 1
      w[!f] <- w[!f] * ps_mat[!f, as.character(focal)]
      w
    },
    ATO = w / rowSums(1 / ps_mat),
    ATM = w * apply(ps_mat, 1L, min),
    stop(sprintf("estimand = \"%s\" is not available for a multi-category treatment", estimand), call. = FALSE)
  )
}

.wn_stabilize <- function(w, treat) {
  tab <- table(as.character(treat)) / length(treat)
  w * as.numeric(tab[as.character(treat)])
}

# ---- multi-category treatments ---------------------------------------------------

# multinomial logit by Newton-Raphson (the first level the reference), with
# step halving on the weighted log-likelihood
.wn_multinom <- function(X, treat, sw, max_iter = 100L, tol = 1e-10) {
  lev <- levels(treat)
  K <- length(lev)
  X <- cbind(1, X)
  p <- ncol(X)
  Y <- outer(as.integer(treat), seq_len(K), "==") * 1
  B <- matrix(0, p, K - 1L)
  probs <- function(B) {
    E <- cbind(0, X %*% B)
    E <- exp(E - apply(E, 1L, max))
    E / rowSums(E)
  }
  ll <- function(P) sum(sw * rowSums(Y * log(pmax(P, 1e-300))))
  P <- probs(B)
  cur <- ll(P)
  for (it in seq_len(max_iter)) {
    g <- as.numeric(crossprod(X, sw * (Y[, -1L, drop = FALSE] - P[, -1L, drop = FALSE])))
    H <- matrix(0, p * (K - 1L), p * (K - 1L))
    for (a in seq_len(K - 1L)) for (b in seq_len(K - 1L)) {
      wab <- sw * P[, a + 1L] * ((a == b) - P[, b + 1L])
      H[(a - 1L) * p + seq_len(p), (b - 1L) * p + seq_len(p)] <- crossprod(X, X * wab)
    }
    step <- solve(H, g)
    s <- 1
    repeat {
      Bn <- B + matrix(s * step, p)
      Pn <- probs(Bn)
      new <- ll(Pn)
      if (new >= cur - 1e-12 || s < 1e-8) break
      s <- s / 2
    }
    B <- Bn
    P <- Pn
    done <- abs(new - cur) < tol * (abs(cur) + tol) && max(abs(s * step)) < 1e-8
    cur <- new
    if (done) break
  }
  colnames(P) <- lev
  P
}

# cumulative-link (proportional odds) model for an ordered treatment:
# P(A <= j) = F(theta_j - x'b), fitted by maximum likelihood (BFGS with the
# analytic gradient, then Newton steps from a finite-difference Hessian)
.wn_ordinal <- function(X, treat, sw, link = "logit") {
  lk <- .wn_link(link)
  K <- nlevels(treat)
  y <- as.integer(treat)
  p <- ncol(X)
  cdf <- function(e) if (link == "logit") stats::plogis(e) else lk$linkinv(e)
  pdf <- function(e) if (link == "logit") stats::dlogis(e) else lk$mu.eta(e)
  unpack <- function(par) {
    th <- cumsum(c(par[1L], exp(par[seq_len(K - 2L) + 1L])))
    list(th = th, b = par[K - 1L + seq_len(p)])
  }
  cum <- function(par) {
    u <- unpack(par)
    xb <- if (p) as.numeric(X %*% u$b) else rep(0, length(y))
    up <- ifelse(y == K, Inf, u$th[pmin(y, K - 1L)]) - xb
    lo <- ifelse(y == 1L, -Inf, u$th[pmax(y - 1L, 1L)]) - xb
    list(u = u, up = up, lo = lo, pr = pmax(cdf(up) - cdf(lo), 1e-300))
  }
  nll <- function(par) -sum(sw * log(cum(par)$pr))
  grad <- function(par) {
    cc <- cum(par)
    fu <- ifelse(is.finite(cc$up), pdf(cc$up), 0)
    fl <- ifelse(is.finite(cc$lo), pdf(cc$lo), 0)
    gth <- numeric(K - 1L)
    for (j in seq_len(K - 1L)) {
      gth[j] <- sum(sw * ((y == j) * fu - (y == j + 1L) * fl) / cc$pr)
    }
    # d theta / d par: theta_j = par_1 + sum_{k<j} exp(par_k+1)
    dth <- matrix(0, K - 1L, K - 1L)
    dth[, 1L] <- 1
    for (k in seq_len(K - 2L)) dth[(k + 1L):(K - 1L), k + 1L] <- exp(par[k + 1L])
    gb <- if (p) -as.numeric(crossprod(X, sw * (fu - fl) / cc$pr)) else numeric(0)
    -c(as.numeric(crossprod(dth, gth)), gb)
  }
  cp <- cumsum(table(y)) / length(y)
  th0 <- stats::qlogis(pmin(pmax(cp[-K], 1e-4), 1 - 1e-4))
  if (link != "logit") th0 <- lk$linkfun(pmin(pmax(cp[-K], 1e-4), 1 - 1e-4))
  par <- c(th0[1L], log(pmax(diff(th0), 1e-4)), rep(0, p))
  par <- stats::optim(par, nll, grad, method = "BFGS", control = list(reltol = 1e-14, maxit = 2000L))$par
  for (it in seq_len(20L)) {
    g <- grad(par)
    h <- 1e-6
    H <- vapply(seq_along(par), function(j) {
      e <- replace(numeric(length(par)), j, h)
      (grad(par + e) - grad(par - e)) / (2 * h)
    }, numeric(length(par)))
    step <- solve((H + t(H)) / 2, g)
    par <- par - step
    if (max(abs(step)) < 1e-10) break
  }
  cc <- cum(par)
  u <- cc$u
  xb <- if (p) as.numeric(X %*% u$b) else rep(0, length(y))
  Fc <- cbind(0, sapply(u$th, function(th) cdf(th - xb)), 1)
  P <- Fc[, -1L, drop = FALSE] - Fc[, -(K + 1L), drop = FALSE]
  colnames(P) <- levels(treat)
  P
}

# ---- continuous treatments ---------------------------------------------------------

.wn_density <- function(density) {
  if (is.function(density)) return(function(x) log(density(x)))
  switch(density,
    normal = function(x) stats::dnorm(x, log = TRUE),
    laplace = function(x) -abs(x) - log(2),
    kernel = NULL,
    if (startsWith(density, "t_")) {
      df <- as.numeric(sub("^t_", "", density))
      function(x) stats::dt(x, df, log = TRUE)
    } else {
      stop("density must be \"normal\", \"laplace\", \"t_<df>\", \"kernel\" or a function", call. = FALSE)
    }
  )
}

# the stabilised generalised-propensity weight f(A) / f(A | X), both
# densities of standardised residuals (WeightIt's method_glm, continuous)
.wn_w_continuous <- function(X, a, sw, link = "identity", density = "normal", bw = "nrd0", adjust = 1) {
  sw <- sw / mean(sw)
  fam <- stats::gaussian(link = link)
  fit <- stats::glm.fit(cbind(1, X), a, weights = sw, family = fam)
  m <- fit$fitted.values
  un_p <- mean(sw * a)
  un_s2 <- mean(sw * (a - un_p)^2)
  s2 <- mean(sw * (a - m)^2)
  zn <- (a - un_p) / sqrt(un_s2)
  zd <- (a - m) / sqrt(s2)
  if (identical(density, "kernel")) {
    kd <- function(z) {
      d <- stats::density(z, n = 10L * length(z), weights = sw / sum(sw), bw = bw, adjust = adjust)
      log(stats::approxfun(d$x, d$y)(z))
    }
    return(list(w = exp(kd(zn) - kd(zd)), fitted = m))
  }
  f <- .wn_density(density)
  list(w = exp(f(zn) - f(zd)), fitted = m)
}
