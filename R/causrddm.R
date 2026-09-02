# SPDX-License-Identifier: AGPL-3.0-or-later
# weighted least squares of y on (1, x); returns the intercept
#' SPDX-License-Identifier: AGPL-3.0-or-later
#'
#' weighted least squares of y on (1, x); returns the intercept
#'
#' @param xs Numeric; combined arithmetically in the body.
#' @param ys Numeric; combined arithmetically in the body.
#' @param ws Numeric; passed to \code{sqrt}.
#' @return The value of \code{[}.
#' @export
.t4_wls_int <- function(xs, ys, ws) {
  sw <- sqrt(ws)
  X <- cbind(sw, sw * xs)
  .t4_olsfit(X, sw * ys)$beta[1]
}

# degree-4 polynomial fit; coefficients and residual MSE
#' Degree-4 polynomial fit; coefficients and residual MSE
#'
#' A step of the causrddm implementation. Called by \code{Rddmanip}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param mp A vector; its length is taken.
#' @param val Passed to \code{.t4_olsfit}.
#' @return A list with \code{beta}, \code{mse}.
#' @export
.t4_poly4 <- function(mp, val) {
  X <- cbind(1, mp, mp^2, mp^3, mp^4)
  fit <- .t4_olsfit(X, val)
  dof <- length(mp) - 5L
  if (dof <= 0L) stop("too few bins on one side to fit the degree-4 pilot")
  list(beta = fit$beta, mse = sum(fit$resid^2) / dof)
}

#' McCrary's test for a discontinuity in the density at the cutoff
#'
#' The running variable is binned, the bin heights become a density, and
#' a local linear regression with triangular kernel is run on each side
#' and evaluated at the cutoff.  The statistic is
#' \eqn{\theta = \log \hat f_+(c) - \log \hat f_-(c)} with
#' \eqn{se(\theta) = \sqrt{(1/(nh))(24/5)(1/\hat f_+ + 1/\hat f_-)}} and
#' \eqn{z = \theta/se(\theta)} referred to the normal.  The 24/5 is the
#' triangular-kernel constant for a boundary local linear estimator and
#' is not interchangeable with the interior constant.  Defaults follow
#' McCrary's own code: bin width \eqn{2 s n^{-1/2}} and a per-side
#' bandwidth \eqn{h = 3.348 (mse_4 \cdot range / \sum f''^2)^{1/5}}
#' averaged over the two sides, with the binned densities zero-padded
#' out one bandwidth so the boundary regressions see the empty tail.
#' A rejection says the density jumps; failing to reject is not evidence
#' that assignment is as good as random.
#'
#' @param x Running variable.
#' @param cutoff Threshold, strictly inside the range of x.
#' @param bw Bandwidth; McCrary's automatic rule if NULL.
#' @param binsize Bin width; 2 s n^(-1/2) if NULL.
#' @return List with \code{estimate} (theta), \code{se},
#'   \code{statistic}, \code{p_value}, \code{fhat_left},
#'   \code{fhat_right}, \code{bw}, \code{binsize}, \code{n},
#'   \code{method}.
#' @references McCrary (2008), Journal of Econometrics 142:698-714.  Paywalled at Elsevier; the coded form was read from McCrary's own implementation as distributed in Dimmery's rdd package, R/DCdensity.R (fetched from the CRAN GitHub mirror), which gives the binning, the 3.348 pilot bandwidth rule, the zero padding, the triangular weights and sethetahat verbatim.
#' @export
#' @examples
#' set.seed(1)
#' x <- c(runif(100, -1, 0), runif(120, 0, 1))
#' Rddmanip(x)
Rddmanip <- function(x, cutoff = 0, bw = NULL, binsize = NULL) {
  x <- .t4_vec(x)
  rn <- length(x)
  if (rn < 20L) stop("need at least 20 observations")
  cutoff <- as.numeric(cutoff)
  rsd <- stats::sd(x)
  rmin <- min(x)
  rmax <- max(x)
  if (cutoff <= rmin || cutoff >= rmax) stop("cutoff must lie strictly within the range of x")
  b <- if (!is.null(binsize)) as.numeric(binsize) else 2 * rsd * rn^(-1 / 2)
  lo <- floor((rmin - cutoff) / b) * b + b / 2 + cutoff
  hi <- floor((rmax - cutoff) / b) * b + b / 2 + cutoff
  j <- as.integer(floor((rmax - rmin) / b)) + 2L
  cellval <- numeric(j)
  mids <- floor((x - cutoff) / b) * b + b / 2 + cutoff
  idx <- as.integer(round((mids - lo) / b)) + 1L
  idx[idx < 1L] <- 1L
  idx[idx > j] <- j
  for (i in idx) cellval[i] <- cellval[i] + 1
  cellval <- cellval / rn / b
  cellmp <- vapply(seq_len(j), function(i) {
    v <- lo + (i - 1) * b
    floor((v - cutoff) / b) * b + b / 2 + cutoff
  }, numeric(1))
  if (is.null(bw)) {
    mpl <- cellmp[cellmp < cutoff]
    mpr <- cellmp[cellmp >= cutoff]
    vl <- cellval[cellmp < cutoff]
    vr <- cellval[cellmp >= cutoff]
    L <- .t4_poly4(mpl, vl)
    R <- .t4_poly4(mpr, vr)
    fppl <- 2 * L$beta[3] + 6 * L$beta[4] * mpl + 12 * L$beta[5] * mpl^2
    fppr <- 2 * R$beta[3] + 6 * R$beta[4] * mpr + 12 * R$beta[5] * mpr^2
    hleft <- 3.348 * (L$mse * (cutoff - lo) / sum(fppl^2))^(1 / 5)
    hright <- 3.348 * (R$mse * (hi - cutoff) / sum(fppr^2))^(1 / 5)
    bw <- 0.5 * (hleft + hright)
  }
  bw <- as.numeric(bw)
  if (!any(x > cutoff - bw & x < cutoff) || !any(x >= cutoff & x < cutoff + bw))
    stop("insufficient data within the bandwidth")
  pad <- as.integer(ceiling(bw / b))
  cmp <- c(lo - (pad:1) * b, cellmp, hi + (1:pad) * b)
  cval <- c(numeric(pad), cellval, numeric(pad))
  jp <- j + 2L * pad
  dist <- cmp - cutoff
  fhat <- numeric(2)
  names(fhat) <- c("left", "right")
  for (side in c("left", "right")) {
    w <- 1 - abs(dist / bw)
    keep <- if (side == "left") cmp < cutoff else cmp >= cutoff
    w <- ifelse(w > 0, w * keep, 0)
    w <- (w / sum(w)) * jp
    fhat[side] <- .t4_wls_int(dist, cval, w)
  }
  fl <- fhat[["left"]]
  fr <- fhat[["right"]]
  if (fl <= 0 || fr <= 0) stop("non-positive density estimate at the cutoff")
  theta <- log(fr) - log(fl)
  se <- sqrt((1 / (rn * bw)) * (24 / 5) * (1 / fr + 1 / fl))
  z <- theta / se
  p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
  .t4_result(estimate = theta, se = se, statistic = z, p_value = p,
             fhat_left = fl, fhat_right = fr, bw = bw, binsize = b,
             n = as.integer(rn), method = "McCrary density discontinuity test")
}
