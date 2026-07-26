#' Largest Lyapunov exponent (Rosenstein) -- Rangayyan Ch 7
#'
#' Rosenstein et al. (1993) algorithm via delay embedding and
#' nearest-neighbour divergence tracking.
#'
#' @param x Numeric vector.
#' @param m Embedding dimension (default 3).
#' @param tau Embedding lag (default 1).
#' @param max_t Maximum forward step (default `min(M/4, 100)`).
#' @param theiler Theiler-window exclusion (default 10).
#' @param dt Sampling period, the \eqn{\Delta t} of Rosenstein eq (13). The
#'   returned exponent is per unit of `dt`: leave it at 1 for a map (Table 1
#'   uses \eqn{\Delta t = 1} for the logistic and Henon maps) and set it to
#'   `1/fs` for a sampled flow.
#' @return Named list `lyapunov`, `divergence_curve`, `t`.
#' @references Rosenstein et al. (1993), Physica D 65:117.
#' @export
#' @examples
#' set.seed(0)
#' rglyp(rnorm(200), m = 3, tau = 1, max_t = 20)$lyapunov
rglyp <- function(x, m = 3L, tau = 1L, max_t = NULL, theiler = 10L, dt = 1.0) {
  N <- length(x)
  M <- N - (m - 1L) * tau
  if (M < 10) stop("Series too short for embedding.")
  dt <- as.numeric(dt)
  if (dt <= 0) stop(sprintf("`dt` must be positive, got %g.", dt))
  ## The Theiler window excludes temporally close pairs. If it swallows every
  ## candidate, max.col over an all-Inf row silently returns the first column
  ## -- a "nearest neighbour" that is nothing of the kind.
  if (2L * as.integer(theiler) + 1L >= M) {
    stop(sprintf(
      "Theiler window %d excludes every neighbour for %d embedded points; need 2*theiler + 1 < %d.",
      as.integer(theiler), M, M
    ))
  }
  Y <- matrix(0, nrow = M, ncol = m)
  for (i in seq_len(m)) Y[, i] <- x[((i - 1L) * tau + 1L):((i - 1L) * tau + M)]
  if (is.null(max_t)) max_t <- min(as.integer(M / 4), 100L)
  d <- as.matrix(stats::dist(Y))
  iv <- seq_len(M)
  mask <- abs(outer(iv, iv, "-")) <= theiler
  d[mask] <- Inf
  nn <- max.col(-d, ties.method = "first")
  div <- rep(NA_real_, max_t)
  for (t in seq_len(max_t)) {
    t0 <- t - 1L
    ok <- (iv + t0 <= M) & (nn + t0 <= M)
    if (!any(ok)) next
    diffs <- sqrt(rowSums((Y[iv[ok] + t0, , drop = FALSE] -
      Y[nn[ok] + t0, , drop = FALSE])^2))
    diffs <- diffs[diffs > 0]
    if (length(diffs)) div[t] <- mean(log(diffs))
  }
  ts <- which(is.finite(div))
  if (length(ts) < 3) {
    lam <- NA_real_
  } else {
    half <- max(3L, length(ts) %/% 2L)
    ## Rosenstein eq (13): y(i) = (1/dt) <ln d_j(i)>, so lambda_1 is a rate
    ## per unit TIME. Fitting <ln d> against the index and dividing the slope
    ## by dt is the same line. Without this the result was a per-SAMPLE rate,
    ## wrong by a factor of fs for any sampled flow.
    lam <- stats::coef(stats::lm(div[ts[seq_len(half)]] ~ ts[seq_len(half)]))[2] / dt
  }
  list(
    lyapunov = unname(lam), divergence_curve = div,
    t = seq_len(max_t), dt = dt
  )
}

#' @rdname rglyp
#' @keywords internal
#' @export
morie_rangayyan_lyapunov <- rglyp
