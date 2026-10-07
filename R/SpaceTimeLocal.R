#' Space-time Gi*, bivariate Moran and space-time trend surfaces
#'
#' \code{StGetisOrd}: Getis-Ord \eqn{G_i^*} z-scores of a space-time cube
#' (rows are times) with binary neighbourhoods of locations within
#' \code{distance} (self included) and times within \code{time_window}
#' (Ord and Getis 1995), as \code{spdep::localG} on the space-time
#' neighbours. \code{BivariateMoran}: \eqn{I_B = \sum x_i (Wy)_i / \sum x_i^2}
#' of standardised variables with local terms and a permutation p-value, as
#' \code{spdep::moran_bv}. \code{StTrendSurface}: least-squares polynomial
#' trend in centred coordinates and time. Identical to the Python arm
#' \code{morie.fn.stlocal}.
#'
#' @param z Matrix (times by locations) or vector (\code{StTrendSurface}).
#' @param coords Two-column coordinates.
#' @param distance Spatial neighbourhood radius.
#' @param time_window Temporal neighbourhood half-width.
#' @param x,y Variables.
#' @param W Spatial weights matrix.
#' @param nsim Permutations.
#' @param seed Philox seed.
#' @param times Times.
#' @param degree,time_degree Polynomial degrees in space and time.
#' @return List.
#' @references Ord, J. K. and Getis, A. (1995). Local spatial
#'   autocorrelation statistics: distributional issues and an application.
#'   Geographical Analysis 27, 286-306.
#'
#'   Wartenberg, D. (1985). Multivariate spatial correlation: a method for
#'   exploratory geographical analysis. Geographical Analysis 17, 263-283.
#'
#'   Krumbein, W. C. (1959). Trend surface analysis of contour-type maps with
#'   irregular control-point spacing. Journal of Geophysical Research 64,
#'   823-834.
#' @examples
#' W <- rbind(c(0, 1, 0, 0), c(0.5, 0, 0.5, 0), c(0, 0.5, 0, 0.5), c(0, 0, 1, 0))
#' BivariateMoran(1:4, 1:4, W, nsim = 9)$statistic
#' @export
StGetisOrd <- function(z, coords, distance, time_window = 1L) {
  .morie_arg(z, "n")
  Z <- as.matrix(z) * 1
  P <- as.matrix(coords)
  Tn <- nrow(Z)
  N <- ncol(Z)
  x <- as.vector(t(Z))
  n <- length(x)
  xbar <- mean(x)
  S <- sqrt(sum(x^2) / n - xbar^2)
  D <- as.matrix(stats::dist(P))
  out <- matrix(0, Tn, N)
  for (t in seq_len(Tn)) {
    tt <- max(1, t - time_window):min(Tn, t + time_window)
    for (i in seq_len(N)) {
      vals <- Z[tt, D[i, ] <= distance, drop = FALSE]
      W <- length(vals)
      out[t, i] <- (sum(vals) - xbar * W) / (S * sqrt((n * W - W^2) / (n - 1)))
    }
  }
  list(z = out, mean = xbar, sd = S)
}

#' @rdname StGetisOrd
#' @export
BivariateMoran <- function(x, y, W, nsim = 499L, seed = 1) {
  W <- as.matrix(W)
  n <- length(x)
  xs <- (x - mean(x)) / stats::sd(x)
  ys <- (y - mean(y)) / stats::sd(y)
  sxx <- sum(xs^2)
  stat <- function(yy) sum(xs * as.vector(W %*% yy)) / sxx
  I <- stat(ys)
  sims <- vapply(seq_len(nsim) - 1, function(s) {
    u <- .morie_random_uniform(n, seed = seed, stream = s)
    p <- ys
    for (t in seq_len(n - 1)) {
      k <- t + floor(u[t] * (n - t + 1))
      tmp <- p[t]
      p[t] <- p[k]
      p[k] <- tmp
    }
    stat(p)
  }, 0)
  list(statistic = I, local = xs * as.vector(W %*% ys), simulated = sims,
       p_value = (1 + sum(abs(sims) >= abs(I))) / (1 + nsim))
}

#' @rdname StGetisOrd
#' @export
StTrendSurface <- function(z, coords, times, degree = 2L, time_degree = 1L) {
  P <- as.matrix(coords)
  xc <- P[, 1] - mean(P[, 1])
  yc <- P[, 2] - mean(P[, 2])
  tc <- times - mean(times)
  terms <- do.call(rbind, lapply(0:time_degree, function(c) do.call(rbind, lapply(0:degree, function(tot) {
    cbind(tot:0, 0:tot, c)
  }))))
  X <- sapply(seq_len(nrow(terms)), function(k) xc^terms[k, 1] * yc^terms[k, 2] * tc^terms[k, 3])
  X <- matrix(X, length(z))
  beta <- as.vector(solve(crossprod(X), crossprod(X, z)))
  fit <- as.vector(X %*% beta)
  res <- z - fit
  list(coefficients = beta, terms = unname(terms), fitted = fit, residuals = res,
       r2 = 1 - sum(res^2) / sum((z - mean(z))^2), sigma2 = sum(res^2) / (length(z) - ncol(X)))
}
