# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mixture cure model
#'
#' Sy and Taylor (2000), Estimation in a Cox proportional hazards cure
#' model, Biometrics 56(1), 227-236, and Kuk and Chen (1992), Biometrika
#' 79(3), 531-541: S(t | X, Z) = (1 - pi(Z)) + pi(Z) S_0(t | X) with
#' pi(Z) = expit(gamma' Z) the probability of being UNCURED, so that as t
#' -> infinity S -> 1 - pi and 1 - pi is the cure fraction.  Both papers
#' are paywalled; the mixture is quoted in its standard published form.
#' Estimation is Sy and Taylor's EM, and their zero-tail constraint -- S_0
#' is zero beyond the largest UNCENSORED time -- is applied, since without
#' it the cure fraction is not identified.
#'
#' @param time,event follow-up times and event indicators.
#' @param X latency covariates (currently entering through the weights).
#' @param Z incidence covariates.
#' @param max_iter,tol EM controls.
#' @return list: estimate, cure_fraction, pi, gamma, S0, times, weights, n,
#'   method.
#' @keywords internal
#' @examples
#' Curemod(c(1, 2, 3, 4, 5, 6), c(1, 0, 1, 0, 1, 0))$cure_fraction
#' @export
Curemod <- function(time, event, X = NULL, Z = NULL, max_iter = 200,
                    tol = 1e-12) {
  t <- .s03vec(time); e <- .s03vec(event); n <- length(t)
  Zd <- .s03design(Z, n)
  times <- sort(unique(t[e > 0.5]))
  tmax <- if (length(times)) max(times) else Inf
  w <- ifelse(e > 0.5, 1, 0.5)
  gam <- numeric(ncol(Zd)); S0 <- rep(1, length(times))
  for (it in seq_len(as.integer(max_iter))) {
    gam <- .s03logit(Zd, w, 40L)
    pi_ <- vapply(.s03matvec(Zd, gam), .s03sigmoid, 0)
    surv <- 1; S0 <- numeric(length(times))
    for (ti in seq_along(times)) {
      tt <- times[ti]
      d <- 0; r <- 0
      for (i in seq_len(n)) {
        if (t[i] >= tt) r <- r + w[i]
        if (abs(t[i] - tt) < 1e-12 && e[i] > 0.5) d <- d + 1
      }
      surv <- surv * (if (r > 0) 1 - d / r else 1)
      S0[ti] <- surv
    }
    neww <- numeric(n)
    for (i in seq_len(n)) {
      if (e[i] > 0.5) { neww[i] <- 1; next }
      s <- 1
      for (j in seq_along(times)) if (times[j] <= t[i]) s <- S0[j]
      if (t[i] >= tmax) s <- 0
      num <- pi_[i] * s
      den <- (1 - pi_[i]) + num
      neww[i] <- if (den > 0) num / den else 0
    }
    delta <- 0
    for (i in seq_len(n)) {
      dd <- abs(neww[i] - w[i])
      if (dd > delta) delta <- dd
    }
    w <- neww
    if (delta < tol) break
  }
  pi_ <- vapply(.s03matvec(Zd, gam), .s03sigmoid, 0)
  cure <- 1 - .s03mean(pi_)
  list(estimate = cure, cure_fraction = cure, pi = pi_, gamma = gam,
       S0 = S0, times = times, weights = w, n = n,
       method = "Sy and Taylor (2000) EM cure model with the zero-tail constraint")
}
