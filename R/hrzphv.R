# SPDX-License-Identifier: AGPL-3.0-or-later
#' Proportional hazards with a gamma frailty, by the EM algorithm
#'
#' Horowitz, J. L. (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Springer, Section 6.2.2, pages 205-208 (volume
#' [Pages 189-232], read as rendered page images).  With Z = exp(-V) gamma
#' with mean 1 and unknown variance theta (p. 205), the marginal density of Y
#' is (6.39), and the log likelihood over the jumps of Lambda_0 is (6.40).
#' Nielsen et al. (1992) and Petersen et al. (1996) replace the
#' (n+1)-dimensional maximisation by an EM algorithm: (6.41) is the M step for
#' Lambda_0, (6.42) the E step
#' E(Z_nj | Y_1, ..., Y_n) = (1 + theta_n) / (1 + theta_n Lambda_n0(Y_j)), and
#' a one-dimensional maximisation over theta closes the cycle.  Steps 1 to 4'
#' on p. 208 give the version with covariates: everywhere Z_nj appears it is
#' replaced by Z_nj exp(-X_j b_n), and Lambda_n0 by Lambda_n0 exp(-X_i b_n).
#'
#' THREE BOOK NOTES, all settled against equations on the facing pages.
#'
#' 1. (6.41) prints the denominator as sum over the risk set of exp(-Z_nj).
#'    But Z = exp(-V) on p. 205 is already the frailty multiplier, and (6.42)
#'    returns a posterior mean of that multiplier, not of a log frailty; and
#'    step 4' asks for "Z_nj exp(-X_j b)" in place of Z_nj, which under the
#'    printed reading would be exp(-Z_nj exp(-X_j b)).  The consistent
#'    reading, and the one used here, is the sum of Z_nj exp(-X_j b_n) over
#'    the risk set.
#'
#' 2. (6.40) prints the second term as (1 + 1/t) log[1 + t dA(Y_i)], with the
#'    jump dA rather than the level A.  (6.39), directly above it, has
#'    [1 + theta Lambda_0(y)]^(1 + 1/theta) with the level.  The level is used
#'    here.
#'
#' 3. (6.42) is written for the uncensored case the section discusses.  The
#'    posterior mean of a gamma frailty given d events is
#'    (1 + theta d) / (1 + theta Lambda_0(Y)), which reduces to the printed
#'    formula at d = 1; the d factor is carried here so that right-censored
#'    observations, which contribute no event, get the correct E step.
#'
#' The likelihood maximised in step 2' is therefore the sum over i of
#' log dA(Y_i) - X_i b - (1 + 1/t) log[1 + t A(Y_i) exp(-X_i b)], maximised
#' over (log t, b) by cyclic coordinate golden-section search.  Nothing is
#' random.
#'
#' @param t Observed durations Y.
#' @param x n-by-p covariate matrix WITHOUT an intercept.
#' @param event Optional, 1 for an event and 0 for right censoring; all ones
#'   if omitted.
#' @param frailty_dist Only "gamma" is offered: it is the distribution p. 205
#'   assumes.
#' @param theta Optional, pin the frailty variance instead of estimating it.
#'   theta near 0 is the no-frailty limit, in which the M step is the Tsiatis
#'   baseline and beta is the partial-likelihood estimate.
#' @param em_iter,cycles,gs_iter,tol Deterministic optimiser controls.
#' @return list: estimate, sigma2_hat, theta_hat, beta_hat, h0_hat, Lambda0,
#'   event_times, frailty, n, n_events, method.
#' @keywords internal
#' @examples
#' Hrzphv(1:8, cbind(c(0, 1, 0, 1, 0, 1, 0, 1)), theta = 1e-8)$beta_hat
#' @export
Hrzphv <- function(t, x, event = NULL, frailty_dist = "gamma", theta = NULL,
                   em_iter = 60L, cycles = 8L, gs_iter = 44L, tol = 1e-11) {
  GR <- 0.6180339887498949
  tt <- .s03vec(t)
  XX <- .s03mat(x)
  n <- length(tt)
  if (n == 0L) stop("horowitz_ph_heterogeneity: t is empty")
  if (nrow(XX) != n) {
    stop("horowitz_ph_heterogeneity: x has a different number of rows than t")
  }
  if (!identical(frailty_dist, "gamma")) {
    stop("horowitz_ph_heterogeneity: only the gamma frailty of p.205 is offered")
  }
  p <- ncol(XX)
  if (is.null(event)) {
    ev <- rep(1, n)
  } else {
    ev <- .s03vec(event)
    if (length(ev) != n) {
      stop("horowitz_ph_heterogeneity: event has a different length than t")
    }
  }
  ord <- order(tt, seq_len(n))
  evt <- ord[ev[ord] != 0]
  if (length(evt) == 0L) stop("horowitz_ph_heterogeneity: no events")
  mstep <- function(Z, b) {
    jm <- numeric(length(evt))
    for (idx in seq_along(evt)) {
      i <- evt[idx]
      s <- 0
      for (j in seq_len(n)) {
        if (tt[j] < tt[i]) next
        e <- 0
        for (k in seq_len(p)) e <- e + XX[j, k] * b[k]
        s <- s + Z[j] * exp(min(max(-e, -300), 300))
      }
      jm[idx] <- if (s > 0) 1 / s else 0
    }
    jm
  }
  cumulative <- function(jm) {
    A <- numeric(n)
    run <- 0
    pos <- 1L
    for (i in ord) {
      if (ev[i] != 0) {
        run <- run + jm[pos]
        pos <- pos + 1L
      }
      A[i] <- run
    }
    A
  }
  negll <- function(par, A, dA) {
    tau <- par[1]
    b <- par[-1]
    th <- exp(min(max(tau, -30), 30))
    tot <- 0
    for (idx in seq_along(evt)) {
      if (dA[idx] > 0) tot <- tot + log(dA[idx])
    }
    for (i in seq_len(n)) {
      e <- 0
      for (k in seq_len(p)) e <- e + XX[i, k] * b[k]
      w <- exp(min(max(-e, -300), 300))
      if (ev[i] != 0) tot <- tot - e
      tot <- tot - (1 + 1 / th) * log(1 + th * A[i] * w)
    }
    -tot
  }
  b <- numeric(p)
  th <- if (is.null(theta)) 1 else as.numeric(theta)
  if (th <= 0) stop("horowitz_ph_heterogeneity: theta must be positive")
  Z <- rep(1, n)
  jumps <- mstep(Z, b)
  A <- cumulative(jumps)
  par <- c(log(th), b)
  for (ite in seq_len(as.integer(em_iter))) {
    prev <- par
    cur <- negll(par, A, jumps)
    for (itc in seq_len(as.integer(cycles))) {
      for (cc in seq_along(par)) {
        if (cc == 1L && !is.null(theta)) next
        lo <- par[cc] - 1.5
        hi <- par[cc] + 1.5
        a1 <- hi - GR * (hi - lo)
        a2 <- lo + GR * (hi - lo)
        q <- par
        q[cc] <- a1
        f1 <- negll(q, A, jumps)
        q[cc] <- a2
        f2 <- negll(q, A, jumps)
        for (itg in seq_len(as.integer(gs_iter))) {
          if (f1 < f2) {
            hi <- a2
            a2 <- a1
            f2 <- f1
            a1 <- hi - GR * (hi - lo)
            q[cc] <- a1
            f1 <- negll(q, A, jumps)
          } else {
            lo <- a1
            a1 <- a2
            f1 <- f2
            a2 <- lo + GR * (hi - lo)
            q[cc] <- a2
            f2 <- negll(q, A, jumps)
          }
        }
        newv <- 0.5 * (lo + hi)
        q[cc] <- newv
        fv <- negll(q, A, jumps)
        if (fv < cur) {
          par[cc] <- newv
          cur <- fv
        }
      }
    }
    th <- exp(min(max(par[1], -30), 30))
    b <- par[-1]
    for (i in seq_len(n)) {
      e <- 0
      for (k in seq_len(p)) e <- e + XX[i, k] * b[k]
      w <- exp(min(max(-e, -300), 300))
      Z[i] <- (1 + th * (if (ev[i] != 0) 1 else 0)) / (1 + th * A[i] * w)
    }
    jumps <- mstep(Z, b)
    A <- cumulative(jumps)
    if (max(abs(par - prev)) < tol) break
  }
  list(estimate = th, sigma2_hat = th, theta_hat = th, beta_hat = b,
       h0_hat = jumps, Lambda0 = A[evt], event_times = tt[evt], frailty = Z,
       n = n, n_events = length(evt),
       method = paste0("Horowitz (2009) Sec. 6.2.2 (6.40)-(6.42) EM, ",
                       "steps 1-4' p.208 with covariates"))
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrzphv
#' @keywords internal
#' @export
morie_horowitz_ph_heterogeneity <- Hrzphv
