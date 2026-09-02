# SPDX-License-Identifier: AGPL-3.0-or-later
#' Joint shared-frailty model for recurrent events and a terminal event
#'
#' A single cluster-level frailty w drives both processes:
#' \code{lambda_R(t|w) = w lambda_0R(t)} and
#' \code{lambda_T(t|w) = w^alpha lambda_0T(t)}, with
#' \code{w ~ Gamma(1/theta, 1/theta)} so that E[w] = 1 and Var[w] =
#' theta.  alpha is the association parameter: 0 makes the terminal
#' event independent of the recurrent process, 1 is the ordinary shared
#' frailty, and a negative alpha means clusters with many recurrences
#' die later.  Baselines are constant (exponential).  Because w^alpha
#' appears in the terminal hazard the frailty cannot be integrated out
#' in closed form, so the expectation is taken on a 16-point generalised
#' Gauss-Laguerre rule matched to the frailty density (Golub-Welsch), for
#' which E[w] = 1 and E[w^2] = 1 + theta hold to ten digits;
#' maximisation is a fixed-length coordinate golden-section search with a
#' guarded comparison, so a flat optimum cannot send the two language arms
#' into different sub-intervals.
#'
#' Formula: lambda_R(t|w) = w lambda_0R, lambda_T(t|w) = w^alpha lambda_0T.
#'
#' @param time Follow-up time of each record; must be positive.
#' @param event Number of recurrent events for that record.
#' @param terminal Terminal event indicator, 0 or 1.
#' @param cluster Cluster (subject) label.
#' @param sweeps Coordinate-ascent sweeps.
#' @return List with \code{estimate} (alpha), \code{alpha},
#'   \code{theta}, \code{lambda_r}, \code{lambda_t}, \code{loglik},
#'   \code{n_clusters}, \code{n_recurrent}, \code{n_terminal},
#'   \code{exposure}, \code{naive_lambda_r}, \code{naive_lambda_t},
#'   \code{n}, \code{method}.
#' @references Liu, Wolfe and Huang (2004), Shared frailty models for
#'   recurrent events and a terminal event, Biometrics 60(3):747-756.
#'   \doi{10.1111/j.0006-341X.2004.00225.x}
#' @export
#' @examples
#' set.seed(1)
#' Jntfr(time = rexp(30), event = rbinom(30, 1, 0.7),
#'       terminal = rbinom(30, 1, 0.3), cluster = rep(1:10, 3))
Jntfr <- function(time, event, terminal, cluster, sweeps = 4) {
  tv <- .s03vec(time); n <- length(tv)
  if (n == 0L) stop("joint_frailty: time is empty")
  ev <- .s03vec(event); te <- .s03vec(terminal); cl <- .s03vec(cluster)
  if (length(ev) != n || length(te) != n || length(cl) != n)
    stop("joint_frailty: time, event, terminal and cluster have different lengths")
  if (any(tv <= 0)) stop("joint_frailty: time must be positive")
  if (any(te != 0 & te != 1)) stop("joint_frailty: terminal must be 0 or 1")
  if (any(ev < 0)) stop("joint_frailty: event counts must be non-negative")
  labels <- sort(unique(cl)); g <- length(labels)
  N <- numeric(g); A <- numeric(g); dl <- numeric(g); Tt <- numeric(g)
  for (i in seq_len(n)) {
    j <- match(cl[i], labels)
    N[j] <- N[j] + ev[i]; A[j] <- A[j] + tv[i]
    dl[j] <- dl[j] + te[i]; Tt[j] <- Tt[j] + tv[i]
  }
  if (sum(N) <= 0 || sum(dl) <= 0)
    stop("joint_frailty: need at least one recurrent and one terminal event")
  lamR <- sum(N) / sum(A); lamT <- sum(dl) / sum(Tt)
  theta <- 0.5; alpha <- 1
  for (sw in seq_len(as.integer(sweeps))) {
    lamR <- .jntfr_golden(function(v) .jntfr_loglik(v, lamT, theta, alpha, N, A, dl, Tt), 1e-4, 10 * sum(N) / sum(A))
    lamT <- .jntfr_golden(function(v) .jntfr_loglik(lamR, v, theta, alpha, N, A, dl, Tt), 1e-4, 10 * sum(dl) / sum(Tt))
    theta <- .jntfr_golden(function(v) .jntfr_loglik(lamR, lamT, v, alpha, N, A, dl, Tt), 1e-3, 5)
    alpha <- .jntfr_golden(function(v) .jntfr_loglik(lamR, lamT, theta, v, N, A, dl, Tt), -3, 3)
  }
  ll <- .jntfr_loglik(lamR, lamT, theta, alpha, N, A, dl, Tt)
  .t1_result(estimate = alpha, alpha = alpha, theta = theta, lambda_r = lamR,
             lambda_t = lamT, loglik = ll, n_clusters = g,
             n_recurrent = sum(N), n_terminal = sum(dl), exposure = sum(A),
             naive_lambda_r = sum(N) / sum(A), naive_lambda_t = sum(dl) / sum(Tt),
             n = n,
             method = "lambda_R = w lambda_0R, lambda_T = w^alpha lambda_0T, w ~ Gamma(1/theta, 1/theta), Liu, Wolfe & Huang (2004)")
}

#' @keywords internal
#' @noRd
.jntfr_cache <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.jntfr_nodes <- function(theta) {
  key <- sprintf("%.17g", theta)
  if (!is.null(.jntfr_cache[[key]])) return(.jntfr_cache[[key]])
  NQ <- 16L
  k <- 1 / theta
  al <- k - 1
  J <- matrix(0, NQ, NQ)
  for (i in seq_len(NQ)) J[i, i] <- 2 * (i - 1L) + al + 1
  for (i in seq(2L, NQ)) {
    b <- sqrt((i - 1L) * ((i - 1L) + al))
    J[i, i - 1L] <- b; J[i - 1L, i] <- b
  }
  e <- .s03jacobi(J)
  out <- list(x = e$values / k, w = e$vectors[1, ]^2)
  if (length(ls(.jntfr_cache)) > 4096L) rm(list = ls(.jntfr_cache), envir = .jntfr_cache)
  assign(key, out, envir = .jntfr_cache)
  out
}

#' @keywords internal
#' @noRd
.jntfr_loglik <- function(lamR, lamT, theta, alpha, N, A, dl, Tt) {
  nd <- .jntfr_nodes(theta)
  xs <- nd$x; ws <- nd$w; NQ <- length(xs)
  tot <- 0
  for (i in seq_along(N)) {
    acc <- 0
    for (q in seq_len(NQ)) {
      w <- xs[q]
      lp <- N[i] * log(lamR * w) - lamR * w * A[i]
      wa <- w^alpha
      lp <- lp + dl[i] * log(lamT * wa) - lamT * wa * Tt[i]
      acc <- acc + ws[q] * exp(lp)
    }
    if (acc <= 0) acc <- 1e-300
    tot <- tot + log(acc)
  }
  tot
}

#' @keywords internal
#' @noRd
.jntfr_golden <- function(f, lo, hi, iters = 40L) {
  g <- 0.6180339887498949
  cc <- hi - g * (hi - lo); dd <- lo + g * (hi - lo)
  fc <- f(cc); fd <- f(dd)
  for (i in seq_len(iters)) {
    # Guarded comparison: near the optimum the objective is flat and the two
    # language arms differ by an ulp or two, which would otherwise send them
    # into different sub-intervals.  Anything inside the guard is a tie and
    # is resolved the same way in both arms.
    if (fc - fd > 1e-10 * (1 + abs(fc) + abs(fd))) {
      hi <- dd; dd <- cc; fd <- fc
      cc <- hi - g * (hi - lo); fc <- f(cc)
    } else {
      lo <- cc; cc <- dd; fc <- fd
      dd <- lo + g * (hi - lo); fd <- f(dd)
    }
  }
  0.5 * (lo + hi)
}
