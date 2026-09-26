# SPDX-License-Identifier: AGPL-3.0-or-later

## J_k = int_0^L s^k exp(b s) ds for k = 0, 1, 2, stable as b -> 0
.morie_jnp_jk <- function(b, L) {
  x <- b * L
  if (abs(x) < 1e-3) {
    return(vapply(0:2, function(k) {
      acc <- 0
      term <- 1
      for (m in 0:11) {
        acc <- acc + term * L^(m + k + 1) / (m + k + 1)
        term <- term * b / (m + 1)
      }
      acc
    }, numeric(1)))
  }
  e <- exp(x)
  j0 <- expm1(x) / b
  j1 <- (L * e - j0) / b
  j2 <- (L * L * e - 2 * j1) / b
  c(j0, j1, j2)
}

## continuous piecewise log-linear hazard, log h(u) = a + b u +
## sum_k d_k (u - tau_k)_+, by Newton-Raphson on the exact concave
## log-likelihood sum_events log h(t_i) - sum_i H(t_i)
.morie_jnp_fit <- function(time, event, taus, n_iter = 100L) {
  taus <- sort(as.numeric(taus))
  p <- 2L + length(taus)
  x_at <- function(u) c(1, u, pmax(u - taus, 0))
  ev <- time[event > 0]
  sx <- numeric(p)
  for (t in ev) sx <- sx + x_at(t)
  theta <- c(log(length(ev) / sum(time)), numeric(p - 1L))
  llgh <- function(th, need = TRUE) {
    ll <- sum(sx * th)
    g <- sx
    H <- matrix(0, p, p)
    for (t in time) {
      cuts <- c(0, taus[taus < t], t)
      for (q in seq_len(length(cuts) - 1L)) {
        lo <- cuts[q]
        hi <- cuts[q + 1L]
        if (hi <= lo) next
        alpha <- c(1, 0, ifelse(lo >= taus, -taus, 0))
        beta <- c(0, 1, ifelse(lo >= taus, 1, 0))
        cc <- sum(alpha * th)
        bb <- sum(beta * th)
        E <- exp(cc + bb * lo)
        J <- .morie_jnp_jk(bb, hi - lo)
        i0 <- E * J[1L]
        ll <- ll - i0
        if (!need) next
        i1 <- E * (lo * J[1L] + J[2L])
        i2 <- E * (lo * lo * J[1L] + 2 * lo * J[2L] + J[3L])
        g <- g - (alpha * i0 + beta * i1)
        H <- H - (outer(alpha, alpha) * i0 + (outer(alpha, beta) + outer(beta, alpha)) * i1 +
                    outer(beta, beta) * i2)
      }
    }
    list(ll = ll, g = g, H = H)
  }
  cur <- llgh(theta)
  for (it in seq_len(n_iter)) {
    step <- solve(cur$H, -cur$g)
    dec <- sum(cur$g * step)
    s <- 1
    repeat {
      cand <- theta + s * step
      llc <- tryCatch(llgh(cand, need = FALSE)$ll, error = function(e) -Inf)
      if (is.nan(llc)) llc <- -Inf
      if (llc >= cur$ll + 1e-4 * s * dec || s < 1e-12) break
      s <- s / 2
    }
    theta <- cand
    cur <- llgh(theta)
    if (dec < 1e-13) break
  }
  list(theta = theta, ll = cur$ll)
}

#' Joinpoint regression for survival hazard trends
#'
#' The log-hazard is continuous and piecewise linear in time,
#' log h(t) = a + b t + sum_k d_k (t - tau_k)_+ (the joinpoint model of
#' Kim, Fay, Feuer and Midthune 2000 applied to the hazard), fitted by
#' exact maximum likelihood with closed-form cumulative hazards.
#' Joinpoints are searched over a grid of event-time percentiles and
#' their number is chosen by BIC, -2 log L + (2 + 2k) log n.
#'
#' @param time Observed event or censoring times.
#' @param event Event indicator (1 event, 0 censored).
#' @param max_joinpoints Maximum number of joinpoints tried.
#' @return List with \code{n_joinpoints}, \code{joinpoints}, \code{slopes}
#'   and \code{intercepts} per segment, \code{bic}, \code{log_likelihood},
#'   \code{n_obs}, \code{n_events}.
#' @examples
#' set.seed(1)
#' tt <- rexp(60, 0.3)
#' r <- morie_jnpnt(tt, rep(1, 60), max_joinpoints = 1)
#' r$n_joinpoints
#' @export
morie_jnpnt <- function(time, event, max_joinpoints = 3L) {
  time <- as.numeric(time)
  event <- as.numeric(event)
  if (length(time) != length(event)) stop("time and event must have the same length", call. = FALSE)
  if (any(time < 0)) stop("times must be non-negative", call. = FALSE)
  n <- length(time)
  ev <- sort(time[event > 0])
  d <- length(ev)
  if (d < 4L) {
    return(list(n_joinpoints = 0L, joinpoints = numeric(0), slopes = 0,
                intercepts = log(d / sum(time) + 1e-300), bic = Inf,
                log_likelihood = 0, n_obs = n, n_events = d))
  }
  m <- min(10L, d)
  cand <- sort(unique(stats::quantile(ev, seq(0.1, 0.9, length.out = m), names = FALSE, type = 7)))
  best <- NULL
  for (k in 0:max_joinpoints) {
    combos <- if (k == 0L) list(numeric(0)) else utils::combn(cand, k, simplify = FALSE)
    for (jp in combos) {
      fit <- .morie_jnp_fit(time, event, jp)
      bic <- -2 * fit$ll + (2 + 2 * k) * log(n)
      if (is.null(best) || bic < best$bic) best <- list(k = k, jp = jp, theta = fit$theta, ll = fit$ll, bic = bic)
    }
  }
  a <- best$theta[1L]
  b <- best$theta[2L]
  ints <- a
  slopes <- b
  for (k in seq_along(best$jp)) {
    a <- a - best$theta[2L + k] * best$jp[k]
    b <- b + best$theta[2L + k]
    ints <- c(ints, a)
    slopes <- c(slopes, b)
  }
  list(n_joinpoints = best$k, joinpoints = best$jp, slopes = slopes, intercepts = ints,
       bic = best$bic, log_likelihood = best$ll, n_obs = n, n_events = d)
}
