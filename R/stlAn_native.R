# STL: seasonal-trend decomposition by loess, plus MAD outlier flags.
# Source: Cleveland, R. B., Cleveland, W. S., McRae, J. E. and
# Terpenning, I. (1990), STL: a seasonal-trend decomposition procedure
# based on loess, Journal of Official Statistics 6(1), 3-73.  The
# inner loop is their Sec. 2.2, steps 1-6:
#   1. detrending, Y - T;
#   2. cycle-subseries smoothing by loess with q = n_s, extended one
#      period before and after, giving C of length N + 2 n_p;
#   3. low-pass filter of C: MA(n_p), MA(n_p), MA(3), then loess with
#      q = n_l;
#   4. S = C - L;
#   5. deseasonalising, Y - S;
#   6. trend smoothing by loess with q = n_t.
# The outer loop is their Sec. 2.4 robustness iteration with bisquare
# weights rho = B(|R| / (6 median|R|)).  Default windows follow their
# Sec. 3.4: n_t = next odd >= 1.5 n_p / (1 - 1.5 / n_s), n_l = next
# odd >= n_p.
#
# NOTE ON COMPARISON WITH stats::stl: R's stl() is the original
# Fortran, which evaluates the loess smoothers only every s.jump /
# t.jump / l.jump points (by default ceiling(window/10)) and linearly
# interpolates between them.  This implementation, like the Python
# arm, evaluates at EVERY point, i.e. jump = 1.  Comparisons against
# stats::stl must therefore pass s.jump = t.jump = l.jump = 1, or a
# discrepancy of order 1e-3 is expected and is the interpolation, not
# an error.
#
# Native implementation mirroring Python morie.fn.stlAn exactly: same
# neighbour ordering (distance, then x), same lambda_q scaling when
# q >= n, same degenerate-weight fallbacks.

#' .mor_stl_tricube
#'
#' A step of the stlAn_native implementation. Called by \code{.mor_stl_loess_at}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param u Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.mor_stl_tricube <- function(u) {
  if (u >= 1) return(0)
  t <- 1 - u * u * u
  t * t * t
}

# loess fitted value at x0 (Cleveland et al. 1990, Sec. 2.1)
#' Loess fitted value at x0 (Cleveland et al. 1990, Sec. 2.1)
#'
#' A step of the stlAn_native implementation. Called by \code{morie_stl_decompose}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param xs A vector; its length is taken and its elements indexed.
#' @param ys A vector; indexed elementwise.
#' @param x0 Numeric; combined arithmetically in the body.
#' @param q A count; the body uses it as \code{seq_len(...)}.
#' @param degree See Usage.
#' @param rho Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @return A numeric value.
#' @export
.mor_stl_loess_at <- function(xs, ys, x0, q, degree, rho = NULL) {
  n <- length(xs)
  if (q >= n) {
    lam <- max(abs(xs - x0)) * q / n
    idx <- seq_len(n)
  } else {
    idx <- order(abs(xs - x0), xs)[seq_len(q)]
    lam <- abs(xs[idx[q]] - x0)
  }
  if (lam <= 0) lam <- 1
  sw <- 0; sxw <- 0; syw <- 0; sxxw <- 0; sxyw <- 0
  for (i in idx) {
    w <- .mor_stl_tricube(abs(xs[i] - x0) / lam)
    if (!is.null(rho)) w <- w * rho[i]
    if (w <= 0) next
    dx <- xs[i] - x0
    sw <- sw + w
    sxw <- sxw + w * dx
    syw <- syw + w * ys[i]
    sxyw <- sxyw + w * dx * ys[i]
    sxxw <- sxxw + w * dx * dx
  }
  if (sw <= 0) return(sum(ys[idx]) / length(idx))
  if (degree == 0) return(syw / sw)
  den <- sw * sxxw - sxw * sxw
  if (abs(den) < 1e-300) return(syw / sw)
  beta <- (sw * sxyw - sxw * syw) / den
  (syw - beta * sxw) / sw
}

# moving average of length k; output length length(v) - k + 1
#' Moving average of length k; output length length(v) - k + 1
#'
#' A step of the stlAn_native implementation. Called by \code{morie_stl_decompose}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @param k Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.mor_stl_ma <- function(v, k) {
  cs <- cumsum(c(0, v))
  (cs[(k + 1L):length(cs)] - cs[seq_len(length(v) - k + 1L)]) / k
}

#' .mor_stl_next_odd
#'
#' A step of the stlAn_native implementation. Called by \code{morie_stl_decompose}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.mor_stl_next_odd <- function(v) {
  v <- as.integer(ceiling(v))
  if (v %% 2L == 1L) v else v + 1L
}

#' .mor_stl_median
#'
#' A step of the stlAn_native implementation. Called by \code{morie_stl_decompose}, \code{morie_stlAn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{sort}.
#' @return One of two values, depending on the branch taken.
#' @export
.mor_stl_median <- function(v) {
  s <- sort(v); n <- length(s); mid <- n %/% 2L
  if (n %% 2L == 1L) s[mid + 1L] else 0.5 * (s[mid] + s[mid + 1L])
}

#' STL seasonal-trend decomposition by loess
#'
#' The decomposition of Cleveland et al. (1990): an inner loop that
#' alternates cycle-subseries smoothing and trend smoothing, and an
#' optional outer loop of bisquare robustness weights that makes the
#' fit resistant to outliers.
#'
#' @param x Numeric series.
#' @param period Number of observations per cycle, at least 2; the
#'   series needs at least two full cycles.
#' @param s_window Seasonal loess span \eqn{n_s}, forced odd.
#' @param t_window,l_window Trend and low-pass spans; \code{NULL} uses
#'   the Sec. 3.4 defaults.
#' @param s_degree,t_degree,l_degree Local polynomial degrees.
#' @param inner Inner-loop iterations.
#' @param outer Robustness iterations; 0 gives the non-robust fit.
#'   Both routes the paper defines are available.
#' @return A list with \code{seasonal}, \code{trend},
#'   \code{remainder}, \code{weights}, \code{s_window},
#'   \code{t_window}, \code{l_window}.
#' @references Cleveland, R. B., Cleveland, W. S., McRae, J. E. and
#'   Terpenning, I. (1990). STL: a seasonal-trend decomposition
#'   procedure based on loess. Journal of Official Statistics, 6(1),
#'   3-73.
#' @export
morie_stl_decompose <- function(x, period, s_window = 7L, t_window = NULL,
                                l_window = NULL, s_degree = 1L,
                                t_degree = 1L, l_degree = 1L,
                                inner = 2L, outer = 0L) {
  ys <- as.numeric(x)
  N <- length(ys)
  np_ <- as.integer(period)
  if (np_ < 2L || N < 2L * np_)
    stop("need period >= 2 and at least two full cycles")
  n_s <- as.integer(s_window)
  if (n_s %% 2L == 0L) n_s <- n_s + 1L
  n_t <- if (!is.null(t_window)) as.integer(t_window) else
    .mor_stl_next_odd(1.5 * np_ / (1 - 1.5 / n_s))
  if (n_t %% 2L == 0L) n_t <- n_t + 1L
  n_l <- if (!is.null(l_window)) as.integer(l_window) else
    .mor_stl_next_odd(np_)
  if (n_l %% 2L == 0L) n_l <- n_l + 1L
  T <- numeric(N); S <- numeric(N); R <- numeric(N)
  rho <- rep(1, N)
  for (it_outer in seq_len(as.integer(outer) + 1L)) {
    use_rho <- if (it_outer > 1L) rho else NULL
    for (it in seq_len(as.integer(inner))) {
      det <- ys - T
      C <- numeric(N + 2L * np_)
      for (p in seq_len(np_) - 1L) {
        pos <- seq.int(p + 1L, N, by = np_)
        ncs <- length(pos)
        sxs <- as.numeric(seq_len(ncs))
        sys_ <- det[pos]
        srho <- if (!is.null(use_rho)) rho[pos] else NULL
        for (j in 0:(ncs + 1L)) {
          # the subseries loess is indexed on its own scale; srho is
          # indexed the same way, so it is passed unchanged
          C[p + j * np_ + 1L] <- .mor_stl_loess_at(sxs, sys_, j, n_s,
                                                   s_degree, srho)
        }
      }
      L1 <- .mor_stl_ma(C, np_)
      L2 <- .mor_stl_ma(L1, np_)
      L3 <- .mor_stl_ma(L2, 3L)
      lxs <- as.numeric(seq_len(length(L3)))
      L <- vapply(seq_len(N), function(v)
        .mor_stl_loess_at(lxs, L3, v, n_l, l_degree), numeric(1))
      S <- C[np_ + seq_len(N)] - L
      des <- ys - S
      txs <- as.numeric(seq_len(N))
      T <- vapply(seq_len(N), function(v)
        .mor_stl_loess_at(txs, des, v, n_t, t_degree, use_rho), numeric(1))
    }
    R <- ys - T - S
    if (it_outer <= as.integer(outer)) {
      h <- 6 * .mor_stl_median(abs(R))
      if (h <= 0) {
        rho <- rep(1, N)
      } else {
        u <- abs(R) / h
        rho <- ifelse(u >= 1, 0, (1 - u * u)^2)
      }
    }
  }
  list(seasonal = S, trend = T, remainder = R, weights = rho,
       s_window = n_s, t_window = n_t, l_window = n_l)
}

#' STL decomposition with MAD outlier flags
#'
#' Runs \code{\link{morie_stl_decompose}} and flags observations whose
#' remainder lies more than \code{k} robust standard deviations from
#' the remainder median, the scale being \eqn{1.4826\,\mathrm{MAD}}.
#'
#' @inheritParams morie_stl_decompose
#' @param k Threshold in robust standard deviations, default 3.
#' @return A list with \code{seasonal}, \code{trend},
#'   \code{remainder}, \code{outliers} (1-based indices),
#'   \code{threshold}, \code{sigma_hat}, \code{s_window},
#'   \code{t_window}, \code{l_window}, \code{estimate}, \code{n},
#'   \code{method}.
#' @references Cleveland, R. B. et al. (1990). STL. Journal of
#'   Official Statistics, 6(1), 3-73.
#' @export
morie_stlAn <- function(x, period, s_window = 7L, k = 3, inner = 2L,
                        outer = 0L, t_window = NULL, l_window = NULL) {
  fit <- morie_stl_decompose(x, period, s_window = s_window,
                             t_window = t_window, l_window = l_window,
                             inner = inner, outer = outer)
  R <- fit$remainder
  N <- length(R)
  med <- .mor_stl_median(R)
  mad <- .mor_stl_median(abs(R - med))
  sigma <- 1.4826 * mad
  thr <- k * sigma
  outl <- if (sigma > 0) which(abs(R - med) > thr) else integer(0)
  list(seasonal = fit$seasonal, trend = fit$trend, remainder = R,
       outliers = as.numeric(outl), threshold = thr, sigma_hat = sigma,
       s_window = fit$s_window, t_window = fit$t_window,
       l_window = fit$l_window, estimate = as.numeric(outl), n = N,
       method = "STL + MAD residual outliers (Cleveland et al. 1990)")
}
