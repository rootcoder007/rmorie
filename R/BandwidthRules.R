.bwr_canonical_ratio <- function(kernel) {
  tab <- list(
    gaussian = c(1 / (2 * sqrt(pi)), 1),
    epanechnikov = c(3 / 5, 1 / 5),
    biweight = c(5 / 7, 1 / 7),
    triweight = c(350 / 429, 1 / 9),
    uniform = c(1 / 2, 1 / 3)
  )
  if (!kernel %in% names(tab)) {
    stop(sprintf("Unknown kernel '%s'. Choose from %s.", kernel, paste(names(tab), collapse = ", ")))
  }
  d0 <- function(k) (tab[[k]][1] / tab[[k]][2]^2)^0.2
  d0(kernel) / d0("gaussian")
}

.bwr_q7 <- function(xs, p) {
  h <- (length(xs) - 1) * p
  lo <- floor(h)
  hi <- min(lo + 1, length(xs) - 1)
  xs[lo + 1] + (h - lo) * (xs[hi + 1] - xs[lo + 1])
}

.bwr_moments <- function(x) {
  xs <- sort(as.numeric(x))
  n <- length(xs)
  if (n < 2) stop("Need at least 2 observations.")
  m <- sum(xs) / n
  list(xs = xs, n = n, sigma = sqrt(sum((xs - m)^2) / (n - 1)),
       iqr = .bwr_q7(xs, 0.75) - .bwr_q7(xs, 0.25))
}

#' Rule-of-thumb kernel bandwidths
#'
#' \code{kbwrt}: Silverman's rule (3.31),
#' \eqn{h = 0.9 \min(s, IQR/1.34) n^{-1/5}}
#' (the same as \code{stats::bw.nrd0}). \code{bwrot}: the 1.06
#' normal-reference rule with the same robust spread (Silverman) or Scott's
#' \eqn{1.059 s n^{-1/5}}. For a non-Gaussian kernel the Gaussian bandwidth
#' is multiplied by the ratio of canonical bandwidths
#' \eqn{\delta_0(K)/\delta_0(\phi)}, \eqn{\delta_0(K) = (R(K)/\mu_2(K)^2)^{1/5}},
#' so that the two kernels smooth equivalently (2.2138 Epanechnikov, 2.6226
#' biweight, 2.9780 triweight, 1.7400 uniform). Identical to the Python arms
#' \code{morie.fn.kbwrt} and \code{morie.fn.bwrot}.
#'
#' @param data,x Numeric sample, at least two values.
#' @param kernel One of \code{"gaussian"}, \code{"epanechnikov"},
#'   \code{"biweight"}, \code{"triweight"}, \code{"uniform"}.
#' @param method \code{"silverman"} or \code{"scott"}.
#' @return A list: \code{bw} (\code{kbwrt}) or \code{bandwidth}
#'   (\code{bwrot}), the sample standard deviation \code{sigma}, the type-7
#'   interquartile range \code{iqr}, the sample size and the kernel ratio.
#' @references Silverman, B. W. (1986). Density Estimation for Statistics and
#'   Data Analysis. Chapman and Hall.
#'
#'   Scott, D. W. (1992). Multivariate Density Estimation. Wiley.
#'
#'   Marron, J. S. and Nolan, D. (1988). Canonical kernels for density
#'   estimation. Statistics and Probability Letters 7, 195-199.
#' @examples
#' kbwrt(c(1, 2, 4, 7, 11))$bw
#' bwrot(c(1, 2, 4, 7, 11), kernel = "epanechnikov")$bandwidth
#' @export
kbwrt <- function(data, kernel = "gaussian") {
  ratio <- .bwr_canonical_ratio(kernel)
  mo <- .bwr_moments(data)
  s <- if (mo$iqr > 0) min(mo$sigma, mo$iqr / 1.34) else mo$sigma
  s <- max(s, 1e-10)
  list(bw = 0.9 * s * mo$n^(-0.2) * ratio, sigma = mo$sigma, iqr = mo$iqr,
       n = mo$n, kernel = kernel, ratio = ratio)
}

#' @rdname kbwrt
#' @export
bwrot <- function(x, kernel = "gaussian", method = "silverman") {
  if (!method %in% c("silverman", "scott")) {
    stop(sprintf("Unknown method '%s'. Choose from silverman, scott.", method))
  }
  ratio <- .bwr_canonical_ratio(kernel)
  mo <- .bwr_moments(x)
  h <- if (method == "silverman") {
    spread <- if (mo$iqr > 0) min(mo$sigma, mo$iqr / 1.34) else mo$sigma
    1.06 * spread * mo$n^(-1 / 5)
  } else {
    1.059 * mo$sigma * mo$n^(-1 / 5)
  }
  list(bandwidth = h * ratio, sigma = mo$sigma, iqr = mo$iqr, method = method,
       n_obs = mo$n, kernel_ratio = ratio)
}
