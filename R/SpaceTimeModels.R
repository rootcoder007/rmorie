# SPDX-License-Identifier: AGPL-3.0-or-later
# Space-time regression and hidden Markov state models.
# Identical to the Python arm morie.fn.stmodels.

#' Space-time models: GTWR, multiscale GWR by backfitting, Gaussian hidden Markov models
#'
#' \code{GtwrFit}: local weighted least squares with space-time kernel
#' weights; distance \code{sqrt(lam d_S^2 + mu d_T^2)} (Huang et al. 2010) or
#' \code{lam d_S + (1 - lam) d_T + 2 sqrt(lam (1 - lam) d_S d_T) cos(ksi)}
#' (\code{distance = "fotheringham"}, GWmodel::gtwr, which also uses
#' \code{past_only = TRUE}); bisquare, gaussian or exponential kernels.
#' \code{MgwrBackfit}: multiscale GWR with fixed per-covariate bandwidths by
#' backfitting from OLS until \code{sqrt(|RSS1 - RSS0| / RSS1) < threshold};
#' centred predictors with intercepts reported on the uncentred scale (as
#' GWmodel::gwr.multiscale with fixed bandwidths). \code{GaussianHmm}:
#' Baum-Welch EM with scaled forward-backward recursions for one or several
#' series sharing parameters, then Viterbi paths (0-based states).
#'
#' @param y Response.
#' @param X Regressor matrix (an intercept is added).
#' @param coords Coordinate matrix (rows are observations).
#' @param times Observation times.
#' @param bandwidth Space-time bandwidth.
#' @param lam,mu Space and time distance weights.
#' @param kernel "bisquare", "gaussian" or "exponential".
#' @param distance "huang" or "fotheringham".
#' @param ksi Angle of the Fotheringham distance.
#' @param past_only Give weight only to observations at or before the focal time.
#' @param bandwidths Bandwidths for the intercept and each covariate.
#' @param center Centre the covariates.
#' @param threshold Backfitting convergence threshold.
#' @param max_iter Maximum iterations.
#' @param series Numeric vector, or list of vectors.
#' @param n_states Number of hidden states.
#' @param means,sds,trans,init Optional starting values.
#' @param tol Log-likelihood tolerance.
#' @return A list.
#' @references Huang, B., Wu, B. and Barry, M. (2010). IJGIS 24, 383-401.
#'   Fotheringham, A. S., Crespo, R. and Yao, J. (2015). Geographical Analysis
#'   47, 431-452. Fotheringham, A. S., Yang, W. and Kang, W. (2017). Annals AAG
#'   107, 1247-1265. Rabiner, L. R. (1989). Proc. IEEE 77, 257-286.
#' @examples
#' GaussianHmm(c(0.1, -0.2, 0, 5.1, 4.9, 5.2, 0.2, -0.1), 2)$states
#' @export
GtwrFit <- function(y, X, coords, times, bandwidth, lam = 1, mu = 1, kernel = "bisquare", distance = "huang", ksi = 0,
                    past_only = FALSE) {
  y <- as.numeric(y)
  n <- length(y)
  Xr <- cbind(1, unname(as.matrix(X)) * 1)
  coords <- matrix(as.numeric(unlist(coords)), nrow = n, byrow = is.list(coords))
  betas <- matrix(0, n, ncol(Xr))
  fitted <- numeric(n)
  for (i in seq_len(n)) {
    ds <- sqrt((coords[, 1] - coords[i, 1])^2 + (coords[, 2] - coords[i, 2])^2)
    dt <- abs(times - times[i])
    d <- if (distance == "fotheringham") lam * ds + (1 - lam) * dt + 2 * sqrt(lam * (1 - lam) * ds * dt) * cos(ksi) else
      sqrt(lam * ds^2 + mu * dt^2)
    w <- .stm_kernel(d, bandwidth, kernel)
    if (past_only) w[times > times[i]] <- 0
    b <- as.numeric(solve(crossprod(Xr, Xr * w), crossprod(Xr, w * y)))
    betas[i, ] <- b
    fitted[i] <- sum(Xr[i, ] * b)
  }
  res <- y - fitted
  list(beta = betas, fitted = fitted, residuals = res, rss = sum(res^2))
}

.stm_kernel <- function(d, bw, kernel) {
  if (kernel == "gaussian") return(exp(-0.5 * (d / bw)^2))
  if (kernel == "exponential") return(exp(-d / bw))
  ifelse(d < bw, (1 - (d / bw)^2)^2, 0)
}

#' @rdname GtwrFit
#' @export
MgwrBackfit <- function(y, X, coords, bandwidths, kernel = "bisquare", center = TRUE, threshold = 1e-10,
                        max_iter = 5000) {
  y <- as.numeric(y)
  n <- length(y)
  X <- unname(as.matrix(X)) * 1
  means <- colSums(X) / n
  cols <- cbind(1, if (center) sweep(X, 2, means) else X)
  p <- ncol(cols)
  coords <- matrix(as.numeric(unlist(coords)), nrow = n, byrow = is.list(coords))
  D <- as.matrix(stats::dist(coords))
  Wt <- lapply(seq_len(p), function(k) .stm_kernel(D, bandwidths[k], kernel))
  b0 <- as.numeric(solve(crossprod(cols), crossprod(cols, y)))
  beta <- matrix(rep(b0, each = n), n, p)
  f <- cols * beta
  resid <- y - rowSums(f)
  rss0 <- sum(resid^2)
  it <- 0
  crit <- Inf
  while (it < max_iter && crit > threshold) {
    it <- it + 1
    for (k in seq_len(p)) {
      yk <- resid + f[, k]
      x <- cols[, k]
      bk <- as.numeric(Wt[[k]] %*% (x * yk)) / as.numeric(Wt[[k]] %*% (x * x))
      beta[, k] <- bk
      f[, k] <- x * bk
      resid <- yk - f[, k]
    }
    rss1 <- sum(resid^2)
    crit <- if (rss1 > 0) sqrt(abs(rss1 - rss0) / rss1) else 0
    rss0 <- rss1
  }
  if (center && p > 1) beta[, 1] <- beta[, 1] - as.numeric(beta[, -1, drop = FALSE] %*% means)
  list(beta = beta, fitted = y - resid, residuals = resid, rss = rss0, iterations = it)
}

.stm_q7 <- function(s, prob) {
  h <- (length(s) - 1) * prob
  lo <- floor(h)
  hi <- min(lo + 1, length(s) - 1)
  w <- h - lo
  if (w > 0) (1 - w) * s[lo + 1] + w * s[hi + 1] else s[lo + 1]
}

#' @rdname GtwrFit
#' @export
GaussianHmm <- function(series, n_states, means = NULL, sds = NULL, trans = NULL, init = NULL, max_iter = 1000,
                        tol = 1e-12) {
  seqs <- if (is.list(series)) lapply(series, as.numeric) else list(as.numeric(series))
  K <- n_states
  pooled <- sort(unlist(seqs))
  N <- length(pooled)
  gm <- sum(pooled) / N
  mu <- if (!is.null(means)) as.numeric(means) else vapply(seq_len(K) - 1, function(k) .stm_q7(pooled, (k + 0.5) / K), 0)
  sd_ <- if (!is.null(sds)) as.numeric(sds) else rep(sqrt(sum((pooled - gm)^2) / N), K)
  A <- if (!is.null(trans)) as.matrix(trans) else {
    m <- matrix(0.1 / (K - 1), K, K)
    diag(m) <- 0.9
    m
  }
  pi_ <- if (!is.null(init)) as.numeric(init) else rep(1 / K, K)
  ldn <- function(x, m, s) -0.5 * ((x - m) / s)^2 - log(s) - 0.5 * log(2 * pi)
  fb <- function(x) {
    T_ <- length(x)
    LB <- outer(x, seq_len(K), function(v, k) ldn(v, mu[k], sd_[k]))
    mx <- apply(LB, 1, max)
    B <- exp(LB - mx)
    al <- matrix(0, T_, K)
    cc <- numeric(T_)
    a <- pi_ * B[1, ]
    cc[1] <- sum(a)
    al[1, ] <- a / cc[1]
    if (T_ > 1) for (t in 2:T_) {
      a <- B[t, ] * as.numeric(al[t - 1, ] %*% A)
      cc[t] <- sum(a)
      al[t, ] <- a / cc[t]
    }
    be <- matrix(1, T_, K)
    if (T_ > 1) for (t in (T_ - 1):1) be[t, ] <- as.numeric(A %*% (B[t + 1, ] * be[t + 1, ])) / cc[t + 1]
    list(B = B, al = al, be = be, c = cc, mx = mx)
  }
  ll_old <- -Inf
  it <- 0
  for (iter in seq_len(max_iter)) {
    it <- it + 1
    num_pi <- numeric(K)
    num_A <- matrix(0, K, K)
    g_sum <- g_x <- g_xx <- numeric(K)
    ll <- 0
    for (x in seqs) {
      r <- fb(x)
      ll <- ll + sum(log(r$c)) + sum(r$mx)
      g <- r$al * r$be
      g_sum <- g_sum + colSums(g)
      g_x <- g_x + colSums(g * x)
      g_xx <- g_xx + colSums(g * x * x)
      num_pi <- num_pi + g[1, ]
      if (length(x) > 1) for (t in 2:length(x)) num_A <- num_A + outer(r$al[t - 1, ], r$B[t, ] * r$be[t, ]) * A / r$c[t]
    }
    pi_ <- num_pi / length(seqs)
    A <- num_A / rowSums(num_A)
    mu <- g_x / g_sum
    sd_ <- sqrt(pmax(g_xx / g_sum - mu * mu, 0))
    if (ll - ll_old < tol) break
    ll_old <- ll
  }
  ll <- 0
  states <- list()
  for (x in seqs) {
    r <- fb(x)
    ll <- ll + sum(log(r$c)) + sum(r$mx)
    T_ <- length(x)
    d <- ifelse(pi_ > 0, log(pi_) + ldn(x[1], mu, sd_), -Inf)
    bp <- matrix(0L, T_, K)
    if (T_ > 1) for (t in 2:T_) {
      nd <- numeric(K)
      for (k in seq_len(K)) {
        cand <- d + ifelse(A[, k] > 0, log(A[, k]), -Inf)
        i <- which.max(cand)
        nd[k] <- cand[i] + ldn(x[t], mu[k], sd_[k])
        bp[t, k] <- i
      }
      d <- nd
    }
    path <- which.max(d)
    if (T_ > 1) for (t in T_:2) path <- c(bp[t, path[1]], path)
    states[[length(states) + 1]] <- path - 1L
  }
  list(means = mu, sds = sd_, trans = A, init = pi_, loglik = ll, states = states, iterations = it)
}
