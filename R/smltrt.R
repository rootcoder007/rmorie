# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ratio estimate of a mean or total, using an auxiliary variable.
#'
#' The estimator is biased; the bias is O(1/n) and is not corrected here,
#' matching Cochran's Chapter 6 treatment.
#'
#' Formula: Rhat = ybar/xbar; Yhat_R = Rhat X;
#'   v(Rhat) = (1 - f)/(n xbar^2) * sum (y_i - Rhat x_i)^2/(n - 1)
#'
#' @param y Sample values of the variable of interest.
#' @param x Sample values of the auxiliary variable; xbar non-zero.
#' @param X Known population TOTAL of x (optional).
#' @param N Population size, for the finite population correction.
#' @param level Confidence level.
#' @return List with \code{ratio}, \code{se_ratio}, \code{ci_lower},
#'   \code{ci_upper}, \code{total}, \code{se_total}, \code{residual_var},
#'   \code{fpc}, \code{n}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Chapter
#'   6. Chapter 6 was NOT in the scanned excerpt available to this batch,
#'   so the standard published form is used; the finite-population factor
#'   matches the samplingbook 1.2.4 convention (N - n)/N used throughout
#'   the sibling Cochran modules.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ratioest(V, V)
Ratioest <- function(y, x, X = NULL, N = Inf, level = 0.95) {
  y <- .t1_vec(y); x <- .t1_vec(x); n <- length(y)
  if (length(x) != n) stop("y and x must have the same length")
  if (n < 2L) stop("a variance needs at least two observations")
  xb <- mean(x)
  if (xb == 0) stop("the auxiliary mean xbar must be non-zero")
  R <- mean(y) / xb
  d <- y - R * x
  sd2 <- sum(d^2) / (n - 1)
  N <- as.numeric(N)
  k <- if (is.infinite(N)) 1 else (N - n) / N
  vR <- k * sd2 / (n * xb^2)
  seR <- sqrt(vR)
  z <- stats::qnorm((1 + level) / 2)
  tot <- if (is.null(X)) NaN else R * as.numeric(X)
  seT <- if (is.null(X)) NaN else abs(as.numeric(X)) * seR
  .t1_result(ratio = R, se_ratio = seR, ci_lower = R - z * seR,
             ci_upper = R + z * seR, total = tot, se_total = seT,
             residual_var = sd2, fpc = k, n = n,
             method = "Ratio estimator, Cochran Chapter 6")
}
