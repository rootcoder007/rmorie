# SPDX-License-Identifier: AGPL-3.0-or-later

#' CLUB upper bound on mutual information
#'
#' Formula: I_CLUB = E[log q(y|x)] - E_marg[log q(y|x)]
#'
#' The second term averages the SAME conditional density over
#' mismatched pairs, so the bound is the average log-ratio between
#' matched and mismatched likelihoods.  It is an upper bound on I(X;Y)
#' whenever q equals the true conditional.  With a Gaussian q fitted by
#' least squares the value has the closed form b^2 var(x) / sigma2.
#'
#' @param x Observations of X.
#' @param y Observations of Y.
#' @param q Optional (a, b, sigma2) of y | x ~ N(a + b x, sigma2).
#' @return List with \code{estimate}, \code{club}, \code{positive},
#'   \code{negative}, \code{a}, \code{b}, \code{sigma2}, \code{rho},
#'   \code{mi_gauss}, \code{n}, \code{method}.
#' @references Cheng, Hao, Dai, Liu, Gan & Carin (2020), CLUB, ICML
#'   119:1779-1788.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' Clbuvc(V, D)
Clbuvc <- function(x, y, q = NULL) {
  xs <- .s03vec(x); ys <- .s03vec(y)
  n <- length(xs)
  if (n < 3L) stop("need at least three observations")
  if (length(ys) != n) stop("x and y must have the same length")
  if (is.null(q)) {
    mx <- sum(xs) / n
    my <- sum(ys) / n
    sxx <- 0; sxy <- 0
    for (i in seq_len(n)) {
      sxx <- sxx + (xs[i] - mx)^2
      sxy <- sxy + (xs[i] - mx) * (ys[i] - my)
    }
    if (sxx <= 0) stop("x has zero variance; the conditional is undefined")
    b <- sxy / sxx
    a <- my - b * mx
    s2 <- 0
    for (i in seq_len(n)) s2 <- s2 + (ys[i] - a - b * xs[i])^2
    s2 <- s2 / n
  } else {
    qq <- .s03vec(q)
    if (length(qq) != 3L) stop("q must be (a, b, sigma2)")
    a <- qq[1]; b <- qq[2]; s2 <- qq[3]
  }
  if (!(s2 > 0)) stop("sigma2 must be strictly positive")
  lp <- function(yi, xi) -0.5 * (log(2 * pi * s2) + (yi - a - b * xi)^2 / s2)
  pos <- 0
  for (i in seq_len(n)) pos <- pos + lp(ys[i], xs[i])
  pos <- pos / n
  neg <- 0
  for (i in seq_len(n)) for (j in seq_len(n)) neg <- neg + lp(ys[j], xs[i])
  neg <- neg / (n * n)
  mx <- sum(xs) / n; my <- sum(ys) / n
  sx <- 0; sy <- 0; sxy <- 0
  for (i in seq_len(n)) {
    sx <- sx + (xs[i] - mx)^2
    sy <- sy + (ys[i] - my)^2
    sxy <- sxy + (xs[i] - mx) * (ys[i] - my)
  }
  sx <- sqrt(sx / n); sy <- sqrt(sy / n)
  rho <- if (sx > 0 && sy > 0) (sxy / n) / (sx * sy) else 0
  mi <- if (abs(rho) < 1) -0.5 * log(1 - rho * rho) else Inf
  .t1_result(estimate = pos - neg, club = pos - neg, positive = pos,
             negative = neg, a = a, b = b, sigma2 = s2, rho = rho,
             mi_gauss = mi, n = n,
             method = "CLUB contrastive log-ratio upper bound on MI")
}
