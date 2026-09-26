# SPDX-License-Identifier: AGPL-3.0-or-later

#' Parametric accelerated failure time models
#'
#' \eqn{\log T = X\beta + \sigma W}, with \eqn{W} standard extreme value
#' (Weibull; exponential fixes \eqn{\sigma = 1}), normal (lognormal) or
#' logistic (loglogistic), fitted by maximum likelihood with right
#' censoring: events contribute \eqn{\log f(w) - \log\sigma - \log t},
#' censored units \eqn{\log S(w)}, \eqn{w = (\log t - X\beta)/\sigma}.
#' The optimum is found with analytic gradients (BFGS) and polished by
#' Newton steps on the analytic Hessian; the covariance is the inverse observed information in
#' \eqn{(\beta, \log\sigma)}, as \code{survival::survreg} reports it.
#'
#' @param time Positive follow-up times.
#' @param event Event indicator (1 = event, 0 = censored).
#' @param X Covariate matrix (an intercept is added), or \code{NULL} for
#'   an intercept-only model.
#' @param dist One of \code{"weibull"}, \code{"lognormal"},
#'   \code{"loglogistic"}, \code{"exponential"}.
#' @return list with \code{coefficients} (intercept first), \code{scale},
#'   \code{loglik}, \code{vcov} (of \eqn{\beta} and \eqn{\log\sigma}),
#'   \code{dist}, \code{n}, \code{n_events}.
#' @references Kalbfleisch JD, Prentice RL (2002). \emph{The Statistical
#'   Analysis of Failure Time Data}, 2nd ed., ch. 2-3. Reference
#'   implementation: survival::survreg.
#' @examples
#' tt <- c(2, 3, 3, 5, 8, 9, 4, 6, 7, 10)
#' ee <- c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1)
#' morie_aft(tt, ee, dist = "weibull")$scale
#' @export
morie_aft <- function(time, event, X = NULL,
                      dist = c("weibull", "lognormal", "loglogistic", "exponential")) {
  dist <- match.arg(dist)
  time <- as.numeric(time)
  event <- as.numeric(event)
  if (any(time <= 0)) stop("times must be positive", call. = FALSE)
  Xm <- if (is.null(X)) matrix(1, length(time), 1L) else cbind(1, as.matrix(X))
  p <- ncol(Xm)
  y <- log(time)
  fixed_scale <- dist == "exponential"
  fam <- if (dist %in% c("weibull", "exponential")) "ev" else if (dist == "lognormal") "norm" else "logis"
  lf <- function(w) switch(fam, ev = w - exp(w), norm = stats::dnorm(w, log = TRUE),
                           logis = stats::dlogis(w, log = TRUE))
  lS <- function(w) switch(fam, ev = -exp(w), norm = stats::pnorm(w, lower.tail = FALSE, log.p = TRUE),
                           logis = stats::plogis(w, lower.tail = FALSE, log.p = TRUE))
  dlf <- function(w) switch(fam, ev = 1 - exp(w), norm = -w, logis = 1 - 2 * stats::plogis(w))
  dlS <- function(w) switch(fam, ev = -exp(w),
                            norm = -exp(stats::dnorm(w, log = TRUE) -
                                          stats::pnorm(w, lower.tail = FALSE, log.p = TRUE)),
                            logis = -stats::plogis(w))
  d2lf <- function(w) switch(fam, ev = -exp(w), norm = rep(-1, length(w)),
                             logis = -2 * stats::dlogis(w))
  d2lS <- function(w) switch(fam, ev = -exp(w),
                             norm = {
                               lam <- exp(stats::dnorm(w, log = TRUE) -
                                            stats::pnorm(w, lower.tail = FALSE, log.p = TRUE))
                               -lam * (lam - w)
                             },
                             logis = -stats::dlogis(w))
  unpack <- function(th) list(b = th[seq_len(p)], ls = if (fixed_scale) 0 else th[p + 1L])
  negll <- function(th) {
    u <- unpack(th)
    s <- exp(u$ls)
    w <- (y - drop(Xm %*% u$b)) / s
    -sum(event * (lf(w) - u$ls - y) + (1 - event) * lS(w))
  }
  grad <- function(th) {
    u <- unpack(th)
    s <- exp(u$ls)
    w <- (y - drop(Xm %*% u$b)) / s
    g <- event * dlf(w) + (1 - event) * dlS(w)     # d ll_i / d w
    gb <- -colSums(Xm * (g / s))                   # dw/dbeta = -x / s
    out <- -gb
    if (!fixed_scale) out <- c(out, -sum(-event + g * (-w)))   # dw/dlog s = -w
    out
  }
  ## analytic Hessian of negll in (beta, log sigma)
  hess <- function(th) {
    u <- unpack(th)
    s <- exp(u$ls)
    w <- (y - drop(Xm %*% u$b)) / s
    g1 <- event * dlf(w) + (1 - event) * dlS(w)
    g2 <- event * d2lf(w) + (1 - event) * d2lS(w)
    Hbb <- crossprod(Xm * (g2 / s^2), Xm)
    if (fixed_scale) return(-Hbb)
    Hbl <- colSums(Xm * ((g2 * w + g1) / s))
    Hll <- sum(g2 * w^2 + g1 * w)
    -rbind(cbind(Hbb, Hbl), c(Hbl, Hll))
  }
  ev <- event == 1
  b0 <- if (sum(ev) > p) stats::lm.fit(Xm[ev, , drop = FALSE], y[ev])$coefficients else c(mean(y), rep(0, p - 1))
  b0[is.na(b0)] <- 0
  th <- if (fixed_scale) b0 else c(b0, log(max(stats::sd(y), 0.1)))
  fit <- stats::optim(th, negll, grad, method = "BFGS",
                      control = list(maxit = 1000, reltol = 1e-15))
  th <- fit$par
  for (k in 1:20) {        # Newton polish on the analytic gradient
    H <- hess(th)
    step <- tryCatch(solve(H, grad(th)), error = function(e) rep(0, length(th)))
    if (max(abs(step)) < 1e-13) break
    th_new <- th - step
    if (negll(th_new) <= negll(th) + 1e-12 * abs(negll(th))) th <- th_new else break
  }
  H <- hess(th)
  u <- unpack(th)
  cn <- c("(Intercept)", if (is.null(colnames(Xm)[-1])) if (p > 1) paste0("x", seq_len(p - 1)) else NULL
          else colnames(Xm)[-1])
  list(coefficients = stats::setNames(u$b, cn), scale = unname(exp(u$ls)), loglik = -negll(th),
       vcov = tryCatch(solve(H), error = function(e) H * NA), dist = dist,
       n = length(time), n_events = sum(event))
}
