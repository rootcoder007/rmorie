# SPDX-License-Identifier: AGPL-3.0-or-later
#' Proportional hazards with gamma heterogeneity, Y observed in intervals
#'
#' Horowitz, J. L. (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Springer, Section 6.2.3, pages 208-209 (volume
#' [Pages 189-232], read as rendered page images).  When Y is observed only
#' through the interval (y_{j-1}, y_j] that contains it, Lambda_0 is
#' identified at the K boundaries only, so the model of Section 6.2.2 -- a
#' proportional hazard with a gamma frailty of variance theta -- becomes
#' finite-dimensional.  With y_0 = 0 the book gives, for 1 <= j <= K,
#' P(y_{j-1} < Y <= y_j | X = x) equal to
#' [1 + theta Lambda_0(y_{j-1}) exp(-x b)]^(-1/theta) minus
#' [1 + theta Lambda_0(y_j) exp(-x b)]^(-1/theta), and
#' P(Y > y_K | X = x) = [1 + theta Lambda_0(y_K) exp(-x b)]^(-1/theta), and
#' then the log likelihood of a random sample of (Y, X).
#'
#' BOOK NOTE (sign).  The two displayed probabilities carry the exponent
#' -1/theta; the log likelihood printed immediately below them on p. 208
#' carries +1/t in all three places.  With A_{j-1} < A_j the +1/t reading
#' makes the bracketed difference negative and its logarithm undefined, while
#' the -1/t reading is exactly the probability displayed two lines above.  The
#' exponent is -1/t; that is what is implemented.  This is the same dropped
#' minus this book is already known for.
#'
#' Estimation is by direct maximisation of the log likelihood over theta > 0,
#' beta and the K jumps of Lambda_0, using theta = exp(tau) and
#' Lambda_0(y_j) = sum_{l <= j} exp(c_l) so the parameters are unconstrained
#' and Lambda_0 is automatically nonnegative and increasing.  The maximiser is
#' cyclic coordinate golden-section search from a fixed start: deterministic,
#' no random restarts.
#'
#' @param t_discrete For an observed event, the index j in 1, ..., K of the
#'   interval containing Y.  Ignored where event = 0.
#' @param x n-by-p covariate matrix WITHOUT an intercept.
#' @param event Optional, 1 if Y fell in interval t_discrete and 0 if
#'   Y > y_K; all ones if omitted.
#' @param K Optional number of intervals; the largest observed index if
#'   omitted.
#' @param cycles,gs_iter Coordinate-descent controls.
#' @return list: estimate, theta_hat, beta_hat, h_j_hat, Lambda0, loglik,
#'   cell_probs, K, n, method.
#' @keywords internal
#' @examples
#' Hrzphd(c(1, 2, 3, 2, 1, 3), cbind(c(0, 1, 0, 1, 0, 1)))$theta_hat
#' @export
Hrzphd <- function(t_discrete, x, event = NULL, K = NULL, cycles = 40L,
                   gs_iter = 48L) {
  GR <- 0.6180339887498949
  jj <- .s03vec(t_discrete)
  XX <- .s03mat(x)
  n <- length(jj)
  if (n == 0L) stop("horowitz_ph_discrete_obs: t_discrete is empty")
  if (nrow(XX) != n) {
    stop("horowitz_ph_discrete_obs: x has a different number of rows than t_discrete")
  }
  p <- ncol(XX)
  if (is.null(event)) {
    ev <- rep(1, n)
  } else {
    ev <- .s03vec(event)
    if (length(ev) != n) {
      stop("horowitz_ph_discrete_obs: event has a different length than t_discrete")
    }
  }
  kk <- 0L
  for (i in seq_len(n)) {
    if (ev[i] != 0) {
      v <- as.integer(jj[i])
      if (v < 1L || v != jj[i]) {
        stop("horowitz_ph_discrete_obs: interval indices must be integers >= 1")
      }
      if (v > kk) kk <- v
    }
  }
  if (!is.null(K)) kk <- as.integer(K)
  if (kk < 1L) stop("horowitz_ph_discrete_obs: no interval events, K is undefined")
  cumA <- function(cc) {
    A <- numeric(kk + 1L)
    s <- 0
    for (l in seq_len(kk)) {
      s <- s + exp(min(max(cc[l], -300), 300))
      A[l + 1L] <- s
    }
    A
  }
  negll <- function(par) {
    tau <- par[1]
    b <- par[seq_len(p) + 1L]
    A <- cumA(par[seq_len(kk) + 1L + p])
    th <- exp(min(max(tau, -30), 30))
    tot <- 0
    for (i in seq_len(n)) {
      e <- 0
      for (k in seq_len(p)) e <- e + XX[i, k] * b[k]
      w <- exp(min(max(-e, -300), 300))
      if (ev[i] != 0) {
        j <- as.integer(jj[i])
        lo <- (1 + th * A[j] * w)^(-1 / th)
        hi <- (1 + th * A[j + 1L] * w)^(-1 / th)
        d <- lo - hi
        tot <- tot + (if (d > 1e-300) log(d) else -1e300)
      } else {
        tot <- tot + (-1 / th) * log(1 + th * A[kk + 1L] * w)
      }
    }
    -tot
  }
  npar <- 1L + p + kk
  par <- numeric(npar)
  for (l in seq_len(kk)) par[1L + p + l] <- -1
  cur <- negll(par)
  for (itc in seq_len(as.integer(cycles))) {
    moved <- 0
    for (cc in seq_len(npar)) {
      lo <- par[cc] - 2
      hi <- par[cc] + 2
      a1 <- hi - GR * (hi - lo)
      a2 <- lo + GR * (hi - lo)
      q <- par
      q[cc] <- a1
      f1 <- negll(q)
      q[cc] <- a2
      f2 <- negll(q)
      for (itg in seq_len(as.integer(gs_iter))) {
        if (f1 < f2) {
          hi <- a2
          a2 <- a1
          f2 <- f1
          a1 <- hi - GR * (hi - lo)
          q[cc] <- a1
          f1 <- negll(q)
        } else {
          lo <- a1
          a1 <- a2
          f1 <- f2
          a2 <- lo + GR * (hi - lo)
          q[cc] <- a2
          f2 <- negll(q)
        }
      }
      newv <- 0.5 * (lo + hi)
      q[cc] <- newv
      fv <- negll(q)
      if (fv < cur) {
        moved <- max(moved, abs(newv - par[cc]))
        par[cc] <- newv
        cur <- fv
      }
    }
    if (moved < 1e-10) break
  }
  th <- exp(min(max(par[1], -30), 30))
  beta <- par[seq_len(p) + 1L]
  A <- cumA(par[seq_len(kk) + 1L + p])
  jumps <- numeric(kk)
  for (l in seq_len(kk)) jumps[l] <- A[l + 1L] - A[l]
  e <- 0
  for (k in seq_len(p)) e <- e + XX[1, k] * beta[k]
  w <- exp(min(max(-e, -300), 300))
  cells <- numeric(kk + 1L)
  for (j in seq_len(kk)) {
    cells[j] <- (1 + th * A[j] * w)^(-1 / th) - (1 + th * A[j + 1L] * w)^(-1 / th)
  }
  cells[kk + 1L] <- (1 + th * A[kk + 1L] * w)^(-1 / th)
  list(estimate = th, theta_hat = th, beta_hat = beta, h_j_hat = jumps,
       Lambda0 = A[-1], loglik = -cur, cell_probs = cells, K = kk, n = n,
       method = paste0("Horowitz (2009) Sec. 6.2.3 pp.208-209, interval ",
                       "likelihood with exponent -1/theta"))
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrzphd
#' @keywords internal
#' @export
morie_horowitz_ph_discrete_obs <- Hrzphd
