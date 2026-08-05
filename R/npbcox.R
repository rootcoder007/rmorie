# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cox regression with a gamma-process baseline hazard
#'
#' Kalbfleisch put a gamma process \code{H ~ GP(c, H_0)} on the
#' baseline cumulative hazard and left the regression coefficient
#' unconstrained. The posterior-mean increment at an observed event
#' time pools the prior increment and the observed count by their
#' weights,
#' \code{dH(t_k) = (c dH_0(t_k) + dN_k) / (c + sum_{j in R_k} exp(x_j'b))},
#' with \code{R_k} the risk set. As \code{c -> 0} this is exactly the
#' Breslow estimator; with \code{b = 0} as well it is Nelson-Aalen.
#'
#' \code{b} maximises the Breslow partial log-likelihood
#' \code{l(b) = sum_k [sum_{i in D_k} x_i'b - d_k log sum_{j in R_k}
#' exp(x_j'b)]}, which is concave, so the Newton step is taken against
#' the observed information and no line search is needed.
#'
#' @param time Observation times.
#' @param event 1 = event, 0 = right-censored.
#' @param X Covariates, no intercept.
#' @param c Gamma-process prior weight; \code{c = 0} gives Breslow.
#' @param lam0 Constant base hazard rate; \code{1 / mean(time)} if
#'   \code{NULL}.
#' @param n_iter Newton iteration cap.
#' @param tol Gradient-norm stopping rule.
#' @return List with \code{beta}, \code{se}, \code{estimate},
#'   \code{loglik}, \code{times}, \code{dH}, \code{H}, \code{S},
#'   \code{grad_norm}, \code{iterations}, \code{converged}, \code{n},
#'   \code{n_events}, \code{c}, \code{lam0}.
#' @references Kalbfleisch, J. D. (1978). Non-parametric Bayesian
#'   analysis of survival time data. Journal of the Royal Statistical
#'   Society Series B, 40(2), 214-221. Cox, D. R. (1972). Regression
#'   models and life-tables. JRSS B, 34(2), 187-220.
#' @export
Npbcox <- function(time, event, X, c = 1.0, lam0 = NULL, n_iter = 50L,
                   tol = 1e-12) {
  t <- as.numeric(time)
  n <- length(t)
  if (n == 0L) stop("Npbcox: time is empty")
  d <- as.numeric(as.numeric(event) != 0)
  if (length(d) != n) stop("Npbcox: time and event have different lengths")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) stop("Npbcox: X and time have different lengths")
  p <- ncol(Xm)
  cc <- as.numeric(c)
  if (cc < 0) stop("Npbcox: c must be non-negative")
  l0 <- if (is.null(lam0)) 1 / mean(t) else as.numeric(lam0)
  ev <- sort(unique(t[d == 1]))
  if (!length(ev)) stop("Npbcox: no events")

  parts <- function(b) {
    eta <- as.numeric(Xm %*% b)
    w <- exp(eta)
    ll <- 0; g <- numeric(p); H <- matrix(0, p, p)
    for (tk in ev) {
      R <- which(t >= tk)
      D <- which(t == tk & d == 1)
      s0 <- sum(w[R])
      s1 <- as.numeric(crossprod(Xm[R, , drop = FALSE], w[R]))
      dk <- length(D)
      ll <- ll + sum(eta[D]) - dk * log(s0)
      g <- g + colSums(Xm[D, , drop = FALSE]) - dk * s1 / s0
      s2 <- crossprod(Xm[R, , drop = FALSE], Xm[R, , drop = FALSE] * w[R])
      H <- H + dk * (s2 / s0 - outer(s1, s1) / (s0 * s0))
    }
    list(ll = ll, g = g, H = H, w = w)
  }

  beta <- numeric(p)
  it <- 0L
  pr <- parts(beta)
  gn <- sqrt(sum(pr$g^2))
  while (it < as.integer(n_iter) && gn > as.numeric(tol)) {
    beta <- beta + as.numeric(solve(pr$H, pr$g))
    it <- it + 1L
    pr <- parts(beta)
    gn <- sqrt(sum(pr$g^2))
  }
  # A covariate collinear with the baseline (a constant, say) leaves the
  # information matrix singular. That is a real answer -- the coefficient
  # is not identified -- not a reason to abort, so the standard errors come
  # back NaN and the estimates stand.
  cov <- try(solve(pr$H), silent = TRUE)
  se <- if (inherits(cov, "try-error")) rep(NaN, p) else
    ifelse(diag(cov) > 0, sqrt(diag(cov)), NaN)

  w <- pr$w
  dH <- numeric(length(ev)); Hc <- numeric(length(ev)); S <- numeric(length(ev))
  acc <- 0; surv <- 1; prev <- 0
  for (k in seq_along(ev)) {
    tk <- ev[k]
    risk <- sum(w[t >= tk])
    dk <- sum(t == tk & d == 1)
    dh0 <- l0 * (tk - prev)
    prev <- tk
    inc <- (cc * dh0 + dk) / (cc + risk)
    dH[k] <- inc
    acc <- acc + inc
    Hc[k] <- acc
    f <- max(1 - inc, 1e-15)
    surv <- surv * f
    S[k] <- surv
  }
  .t1_result(beta = beta, se = se, estimate = beta[1], loglik = pr$ll,
             times = ev, dH = dH, H = Hc, S = S, grad_norm = gn,
             iterations = it, converged = if (gn <= as.numeric(tol)) 1 else 0,
             n = n, n_events = sum(d), c = cc, lam0 = l0,
             method = "Cox model with a gamma-process baseline (Kalbfleisch 1978)")
}
