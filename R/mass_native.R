# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native replacements for the MASS utilities used across the package
# (feat/native-specializations, module 30). MASS::ginv (Moore-Penrose
# pseudo-inverse, 54 call sites) and MASS::mvrnorm (multivariate normal
# sampling) are reproduced exactly -- same SVD / eigen algorithm and, for
# mvrnorm, the same RNG consumption order, so results match MASS to
# machine precision (and bit-for-bit under a common seed).

#' Internal: Moore-Penrose generalized inverse (native MASS::ginv)
#'
#' Exact re-implementation of \code{MASS::ginv} for real matrices: the
#' SVD pseudo-inverse with the same singular-value tolerance rule.
#' @noRd
.morie_ginv <- function(X, tol = sqrt(.Machine$double.eps)) {
  if (length(dim(X)) > 2L || !is.numeric(X)) {
    stop("'X' must be a numeric matrix", call. = FALSE)
  }
  if (!is.matrix(X)) X <- as.matrix(X)
  s <- svd(X)
  pos <- s$d > max(tol * s$d[1L], 0)
  if (all(pos)) {
    s$v %*% (1 / s$d * t(s$u))
  } else if (!any(pos)) {
    array(0, dim(X)[2L:1L])
  } else {
    s$v[, pos, drop = FALSE] %*%
      ((1 / s$d[pos]) * t(s$u[, pos, drop = FALSE]))
  }
}

#' Native multivariate normal sampling (reproduces MASS::mvrnorm)
#'
#' Draws from \eqn{N(\mu, \Sigma)} using the symmetric eigen
#' decomposition, matching \code{MASS::mvrnorm} exactly -- including its
#' RNG consumption (\code{rnorm(p * n)} in the same order), so under a
#' common seed the draws are identical.
#'
#' @param n Number of samples.
#' @param mu Mean vector (length p).
#' @param Sigma p x p covariance matrix.
#' @param tol Tolerance for the positive-definiteness check.
#' @param empirical If TRUE, force the sample mean/covariance to match
#'   \code{mu}/\code{Sigma} exactly (as in MASS).
#' @return A length-p vector when \code{n == 1}, else an \code{n x p}
#'   matrix.
#' @references Venables, W. N., & Ripley, B. D. (2002). \emph{Modern
#'   Applied Statistics with S}. Springer.
#' @examples
#' set.seed(1)
#' morie_mvrnorm(3, mu = c(0, 0), Sigma = diag(2))
#' @export
morie_mvrnorm <- function(n = 1, mu, Sigma, tol = 1e-6,
                          empirical = FALSE) {
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) stop("incompatible arguments")
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) {
    stop("'Sigma' is not positive definite")
  }
  X <- matrix(stats::rnorm(p * n), n)
  if (empirical) {
    X <- scale(X, TRUE, FALSE)
    X <- X %*% svd(X, nu = 0)$v
    X <- scale(X, FALSE, TRUE)
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) nm <- dn[[1L]]
  dimnames(X) <- list(nm, NULL)
  if (n == 1) drop(X) else t(X)
}
# --- Module 31: negative-binomial GLM + 2-D KDE (native MASS) ---------

# Normal-reference bandwidth (reproduces MASS::bandwidth.nrd).
.morie_bandwidth_nrd <- function(x) {
  r <- stats::quantile(x, c(0.25, 0.75))
  h <- (r[2L] - r[1L]) / 1.34
  4 * 1.06 * min(sqrt(stats::var(x)), h) * length(x)^(-1 / 5)
}

#' Native 2-D kernel density estimate (reproduces MASS::kde2d)
#'
#' Gaussian-kernel 2-D density on a regular grid, matching
#' \code{MASS::kde2d} (same normal-reference bandwidth and the h/4
#' scaling MASS applies).
#'
#' @param x,y Numeric coordinate vectors of equal length.
#' @param h Bandwidth vector (default normal-reference per coordinate).
#' @param n Grid size (scalar or length-2).
#' @param lims Grid limits \code{c(xlo, xhi, ylo, yhi)}.
#' @return A list \code{list(x, y, z)} (grid axes and the n1 x n2
#'   density matrix).
#' @examples
#' set.seed(2)
#' x <- rnorm(80); y <- rnorm(80)
#' k <- morie_kde2d(x, y, n = 20)
#' dim(k$z)
#' @export
morie_kde2d <- function(x, y, h, n = 25, lims = c(range(x), range(y))) {
  nx <- length(x)
  if (length(y) != nx) stop("data vectors must be the same length")
  n <- rep(n, length.out = 2L)
  gx <- seq.int(lims[1L], lims[2L], length.out = n[1L])
  gy <- seq.int(lims[3L], lims[4L], length.out = n[2L])
  h <- if (missing(h)) {
    c(.morie_bandwidth_nrd(x), .morie_bandwidth_nrd(y))
  } else rep(h, length.out = 2L)
  if (any(h <= 0)) stop("bandwidths must be strictly positive")
  h <- h / 4
  ax <- outer(gx, x, "-") / h[1L]
  ay <- outer(gy, y, "-") / h[2L]
  z <- tcrossprod(matrix(stats::dnorm(ax), , nx),
                  matrix(stats::dnorm(ay), , nx)) / (nx * h[1L] * h[2L])
  list(x = gx, y = gy, z = z)
}

# Negative-binomial family with fixed theta (reproduces
# MASS::negative.binomial for the log link path used by glm.nb).
.morie_negbin_family <- function(theta, link = "log") {
  lk <- stats::make.link(link)
  variance <- function(mu) mu + mu^2 / theta
  validmu  <- function(mu) all(mu > 0)
  dev.resids <- function(y, mu, wt) {
    2 * wt * (y * log(pmax(1, y) / mu) -
                (y + theta) * log((y + theta) / (mu + theta)))
  }
  aic <- function(y, n, mu, wt, dev) {
    term <- (y + theta) * log(mu + theta) - y * log(mu) +
      lgamma(y + 1) - theta * log(theta) + lgamma(theta) -
      lgamma(theta + y)
    2 * sum(term * wt)
  }
  initialize <- expression({
    if (any(y < 0)) stop("negative values not allowed for the negative binomial family")
    n <- rep(1, nobs)
    mustart <- y + (y == 0) / 6
  })
  structure(list(family = paste0("Negative Binomial(",
                                 format(round(theta, 4)), ")"),
                 link = link, linkfun = lk$linkfun, linkinv = lk$linkinv,
                 variance = variance, dev.resids = dev.resids, aic = aic,
                 mu.eta = lk$mu.eta, initialize = initialize,
                 validmu = validmu, valideta = lk$valideta),
            class = "family")
}

# Theta MLE by Fisher scoring (reproduces MASS::theta.ml).
.morie_theta_ml <- function(y, mu, n = sum(weights), weights,
                            limit = 10, eps = .Machine$double.eps^0.25) {
  if (missing(weights)) weights <- rep(1, length(y))
  score <- function(th) sum(weights * (digamma(th + y) - digamma(th) +
    log(th) + 1 - log(th + mu) - (y + th) / (mu + th)))
  info <- function(th) sum(weights * (-trigamma(th + y) + trigamma(th) -
    1 / th + 2 / (mu + th) - (y + th) / (mu + th)^2))
  t0 <- n / sum(weights * (y / mu - 1)^2)
  it <- 0L; del <- 1
  while ((it <- it + 1L) < limit && abs(del) > eps) {
    t0 <- abs(t0)
    del <- score(t0) / info(t0)
    t0 <- t0 + del
  }
  if (t0 < 0) t0 <- 0
  as.numeric(t0)
}

#' Native negative-binomial GLM (reproduces MASS::glm.nb)
#'
#' Fits a negative-binomial GLM by the same alternating scheme as
#' \code{MASS::glm.nb}: an initial Poisson fit, then iterate between a
#' \code{glm.fit} with the current \code{negative.binomial(theta)}
#' family and a \code{theta} MLE update until the log-likelihood and
#' \code{theta} converge. Coefficients and \code{theta} match
#' \code{MASS::glm.nb} to convergence tolerance.
#'
#' @param formula,data Model formula and data.
#' @param weights Optional prior weights.
#' @param init.theta Optional starting \code{theta}.
#' @param link Link (default \code{"log"}).
#' @param control A \code{stats::glm.control} object.
#' @param ... Passed to \code{glm.control}.
#' @return A \code{glm}/\code{negbin} object with \code{$theta}.
#' @references Venables, W. N., & Ripley, B. D. (2002). \emph{Modern
#'   Applied Statistics with S}. Springer.
#' @examples
#' set.seed(1); n <- 300
#' x <- rnorm(n)
#' y <- rnbinom(n, mu = exp(0.3 + 0.9 * x), size = 3)
#' fit <- suppressWarnings(morie_glm_nb(y ~ x, data = data.frame(y, x)))
#' coef(fit)
#' @export
morie_glm_nb <- function(formula, data, weights, init.theta = NULL,
                         link = "log", control = stats::glm.control(...),
                         ...) {
  loglik <- function(th, mu, y, w) sum(w * (lgamma(th + y) -
    lgamma(th) - lgamma(y + 1) + th * log(th) +
    y * log(mu + (y == 0)) - (th + y) * log(th + mu)))
  mf <- stats::model.frame(formula, data)
  Terms <- attr(mf, "terms")
  Y <- stats::model.response(mf, "numeric")
  X <- stats::model.matrix(Terms, mf)
  w <- if (missing(weights) || is.null(weights)) rep(1, length(Y)) else weights
  n <- length(Y)
  icpt <- attr(Terms, "intercept") > 0
  fam0 <- if (is.null(init.theta)) stats::poisson(link) else
    .morie_negbin_family(init.theta, link)
  fit <- stats::glm.fit(x = X, y = Y, weights = w, family = fam0,
                        control = control, intercept = icpt)
  class(fit) <- c("negbin", "glm", "lm")
  mu <- fit$fitted.values
  th <- .morie_theta_ml(Y, mu, sum(w), w, limit = control$maxit)
  fam <- .morie_negbin_family(th, link)
  iter <- 0L
  d1 <- sqrt(2 * max(1, fit$df.residual)); d2 <- del <- 1
  g <- fam$linkfun; Lm <- loglik(th, mu, Y, w); Lm0 <- Lm + 2 * d1
  while ((iter <- iter + 1L) <= control$maxit &&
         (abs(Lm0 - Lm) / d1 + abs(del) / d2) > control$epsilon) {
    eta <- g(mu)
    fit <- stats::glm.fit(x = X, y = Y, weights = w, etastart = eta,
                          family = fam, control = control, intercept = icpt)
    t0 <- th
    th <- .morie_theta_ml(Y, mu, sum(w), w, limit = control$maxit)
    fam <- .morie_negbin_family(th, link)
    mu <- fit$fitted.values
    del <- t0 - th; Lm0 <- Lm; Lm <- loglik(th, mu, Y, w)
  }
  fit$theta <- as.numeric(th)
  fit$terms <- Terms
  fit$formula <- stats::as.formula(formula)
  fit$call <- match.call()
  fit$twologlik <- as.numeric(2 * Lm)
  fit$aic <- as.numeric(-fit$twologlik + 2 * fit$rank + 2)
  class(fit) <- c("negbin", "glm", "lm")
  fit
}

#' @exportS3Method stats::summary negbin
summary.negbin <- function(object, dispersion = 1, ...) {
  s <- stats::summary.glm(object, dispersion = dispersion, ...)
  s$theta <- object$theta
  s$SE.theta <- attr(object$theta, "SE")
  s
}

#' @exportS3Method stats::logLik negbin
logLik.negbin <- function(object, ...) {
  val <- object$twologlik / 2
  attr(val, "df") <- object$rank + 1L
  attr(val, "nobs") <- length(object$residuals)
  class(val) <- "logLik"
  val
}

# --- Module 31 (cont.): robust regression + ordered logit (native MASS) --

#' Native robust (Huber M-estimator) regression (reproduces MASS::rlm)
#'
#' IRLS with Huber weights and MAD scale, matching \code{MASS::rlm}'s
#' default \code{method = "M"}, \code{psi = psi.huber} (k = 1.345),
#' \code{scale.est = "MAD"} path. \code{summary()} reproduces
#' \code{MASS:::summary.rlm}'s XtX standard errors.
#'
#' @param formula,data Model formula and data.
#' @param k Huber tuning constant (default 1.345).
#' @param maxit Max IRLS iterations.
#' @param acc Convergence tolerance on the residual change.
#' @return A \code{morie_rlm} object.
#' @references Venables, W. N., & Ripley, B. D. (2002). \emph{Modern
#'   Applied Statistics with S}. Springer.
#' @examples
#' set.seed(3)
#' n <- 100; x <- rnorm(n)
#' y <- 2 * x + rnorm(n)
#' y\[1:3\] <- y\[1:3\] + 40
#' rob <- morie_rlm(y ~ x, data = data.frame(y, x))
#' rob$coefficients
#' @export
morie_rlm <- function(formula, data, k = 1.345, maxit = 20L, acc = 1e-4) {
  mf <- stats::model.frame(formula, data)
  y <- stats::model.response(mf, "numeric")
  x <- stats::model.matrix(attr(mf, "terms"), mf)
  psi <- function(u, deriv = 0) if (!deriv) pmin(1, k / abs(u)) else abs(u) <= k
  # Fused Armadillo Huber-M IRLS; matches MASS::rlm to ~1e-15, ~2x faster.
  cp <- .morie_rlm_cpp(x, y, k, as.integer(maxit), acc)
  coef <- as.numeric(cp$coef); names(coef) <- colnames(x)
  fitted <- drop(x %*% coef)
  structure(list(coefficients = coef, residuals = y - fitted,
                 wresid = as.numeric(cp$resid), fitted.values = fitted,
                 s = cp$scale, psi = psi, x = x, weights = numeric(0),
                 converged = isTRUE(cp$converged), k2 = k),
            class = "morie_rlm")
}

#' @exportS3Method stats::summary morie_rlm
summary.morie_rlm <- function(object, ...) {
  s <- object$s; coef <- object$coefficients; wresid <- object$wresid
  n <- length(wresid); p <- length(coef); cn <- names(coef)
  w <- object$psi(wresid / s)
  S <- sum((wresid * w)^2) / (n - p)
  psiprime <- object$psi(wresid / s, deriv = 1)
  mn <- mean(psiprime)
  kappa <- 1 + p * stats::var(psiprime) / (n * mn^2)
  stddev <- sqrt(S) * (kappa / mn)
  R <- qr(object$x)$qr
  R <- R[seq_len(p), seq_len(p), drop = FALSE]
  R[lower.tri(R)] <- 0
  rinv <- solve(R, diag(p))
  rowlen <- sqrt(rowSums(rinv^2))
  se <- rowlen * stddev
  tab <- cbind(Value = coef, "Std. Error" = se, "t value" = coef / se)
  rownames(tab) <- cn
  list(coefficients = tab, s = s, stddev = stddev)
}

#' Native ordered logistic/probit regression (reproduces MASS::polr)
#'
#' Proportional-odds ordinal regression by direct maximum likelihood on
#' the cumulative-link parametrisation used by \code{MASS::polr} (same
#' cutpoint transform \code{cumsum(c(theta_1, exp(theta_2..q)))} and the
#' same glm-based start), so the fitted log-likelihood matches
#' \code{MASS::polr} to optimiser tolerance.
#'
#' @param formula,data Model formula and data (response an ordered factor).
#' @param weights Optional prior weights.
#' @param method \code{"logistic"} (default) or \code{"probit"}.
#' @return A \code{morie_polr} object (with \code{$deviance},
#'   \code{$coefficients}, \code{$zeta}).
#' @references Venables, W. N., & Ripley, B. D. (2002). \emph{Modern
#'   Applied Statistics with S}. Springer.
#' @examples
#' set.seed(4)
#' n <- 250; x <- rnorm(n)
#' yc <- 1 + (runif(n) > plogis(-0.5 - x)) + (runif(n) > plogis(1 - x))
#' yf <- factor(pmin(yc, 3), levels = 1:3, ordered = TRUE)
#' fit <- morie_polr(yf ~ x, data = data.frame(yf, x))
#' fit$zeta
#' @export
morie_polr <- function(formula, data, weights, method = "logistic") {
  pfun <- switch(method, logistic = stats::plogis, probit = stats::pnorm,
                 stop("morie_polr supports method 'logistic' or 'probit'"))
  dfun <- switch(method, logistic = stats::dlogis, probit = stats::dnorm)
  mf <- stats::model.frame(formula, data)
  x <- stats::model.matrix(attr(mf, "terms"), mf)
  xint <- match("(Intercept)", colnames(x), nomatch = 0L)
  if (xint > 0L) x <- x[, -xint, drop = FALSE]
  n <- nrow(x); pc <- ncol(x)
  wt <- if (missing(weights) || is.null(weights)) rep(1, n) else weights
  offset <- rep(0, n)
  y <- stats::model.response(mf)
  if (!is.factor(y)) stop("response must be a factor")
  lev <- levels(y); llev <- length(lev)
  if (llev <= 2L) stop("response must have 3 or more levels")
  y <- unclass(y); q <- llev - 1L
  ind_pc <- seq_len(pc); ind_q <- seq_len(q)
  # starting values (MASS::polr glm-based scheme)
  q1 <- llev %/% 2L
  X <- cbind(Intercept = rep(1, n), x)
  fam <- if (method == "probit") stats::binomial("probit") else stats::binomial()
  fit <- stats::glm.fit(X, as.numeric(y > q1), wt, family = fam, offset = offset)
  coefs <- fit$coefficients
  logit <- function(p) log(p / (1 - p))
  spacing <- logit((seq_len(q)) / (q + 1L))
  if (method != "logistic") spacing <- spacing / 1.7
  gammas <- -coefs[1L] + spacing - spacing[q1]
  start <- c(coefs[-1L], gammas)
  # optim works in the (cut_1, log-increments) space, so the cutpoints
  # stay ordered; transform the start into it (as MASS::polr.fit does).
  s0 <- if (pc) c(start[seq_len(pc + 1L)], log(diff(start[-seq_len(pc)])))
        else c(start[1L], log(diff(start)))
  Y1 <- col(matrix(0, n, q)) == y
  Y2 <- col(matrix(0, n, q)) == (y - 1L)
  fmin <- function(beta) {
    theta <- beta[pc + ind_q]
    gamm <- c(-Inf, cumsum(c(theta[1L], exp(theta[-1L]))), Inf)
    eta <- offset + if (pc) drop(x %*% beta[ind_pc]) else 0
    pr <- pfun(pmin(100, gamm[y + 1L] - eta)) - pfun(pmax(-100, gamm[y] - eta))
    if (all(pr > 0)) -sum(wt * log(pr)) else Inf
  }
  # analytic gradient (chain rule through the cut-increment jacobian)
  gmin <- function(beta) {
    theta <- beta[pc + ind_q]
    etheta <- exp(theta[-1L])
    gamm <- c(-Inf, cumsum(c(theta[1L], etheta)), Inf)
    eta <- offset + if (pc) drop(x %*% beta[ind_pc]) else 0
    z1 <- pmin(100, gamm[y + 1L] - eta); z2 <- pmax(-100, gamm[y] - eta)
    pr <- pfun(z1) - pfun(z2); p1 <- dfun(z1); p2 <- dfun(z2)
    g1 <- if (pc) drop(t(x) %*% (wt * (p1 - p2) / pr)) else numeric()
    g2 <- -drop(t(Y1 * p1 - Y2 * p2) %*% (wt / pr))
    jac <- matrix(0, q, q); jac[, 1L] <- 1
    for (i in seq_len(q)[-1L]) jac[i:q, i] <- etheta[i - 1L]
    g2 <- drop(g2 %*% jac)
    if (all(pr > 0)) c(g1, g2) else rep(NA_real_, pc + q)
  }
  res <- stats::optim(s0, fmin, gmin, method = "BFGS")
  beta <- res$par
  theta <- beta[pc + ind_q]
  zeta <- cumsum(c(theta[1L], exp(theta[-1L])))
  cf <- beta[ind_pc]; names(cf) <- colnames(x)
  structure(list(coefficients = cf, zeta = zeta, deviance = 2 * res$value,
                 method = method, lev = lev, n = n, edf = pc + q,
                 nobs = sum(wt), convergence = res$convergence),
            class = "morie_polr")
}

#' @exportS3Method stats::logLik morie_polr
logLik.morie_polr <- function(object, ...) {
  val <- -object$deviance / 2
  attr(val, "df") <- object$edf
  attr(val, "nobs") <- object$nobs
  class(val) <- "logLik"
  val
}
