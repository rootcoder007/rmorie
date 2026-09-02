# SPDX-License-Identifier: AGPL-3.0-or-later
#' Normalise a phenotype, then flag what is still extreme
#'
#' Order matters: flagging outliers first would discard the right tail of
#' a skewed phenotype, so the transform is chosen on all the data and the
#' fences are applied afterwards. Box-Cox is fitted by profile likelihood
#' over a fixed grid rather than an optimiser, which keeps the answer
#' reproducible.
#'
#' Formula: \code{y(lambda) = (y^lambda - 1)/lambda}, \code{log y} at
#' zero, lambda maximising
#' \code{-n/2 log(sigma^2(lambda)) + (lambda - 1) sum log y}; then Tukey
#' fences on the transformed values.
#'
#' @param y Strictly positive phenotype values.
#' @param k Tukey fence multiplier.
#' @param lambdas Grid of lambda values; -2 to 2 by 0.05 by default.
#' @return List with \code{estimate}, \code{loglik}, \code{n_out},
#'   \code{flags}, \code{lower}, \code{upper}, \code{transformed}, \code{n}.
#' @references Tukey, J. W. (1977). Exploratory Data Analysis; Box,
#'   G. E. P. & Cox, D. R. (1964). JRSS B 26:211-252, equation (9).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Pheno2(V)
Pheno2 <- function(y, k = 1.5, lambdas = NULL) {
  v <- as.numeric(unlist(y)); n <- length(v)
  if (min(v) <= 0) stop("phenotype values must be strictly positive")
  if (is.null(lambdas)) lambdas <- (-40:40) * 0.05
  slog <- sum(log(v))
  best_l <- 0; best_ll <- -Inf
  for (lam in lambdas) {
    z <- if (lam == 0) log(v) else (v^lam - 1) / lam
    m <- sum(z) / n
    s2 <- sum((z - m)^2) / n
    ll <- -0.5 * n * log(s2) + (lam - 1) * slog
    if (ll > best_ll) { best_ll <- ll; best_l <- lam }
  }
  lam <- best_l
  z <- if (lam == 0) log(v) else (v^lam - 1) / lam
  s <- sort(z)
  n4 <- floor((n + 3) / 2) / 2
  at <- function(d) 0.5 * (s[floor(d)] + s[ceiling(d)])
  hl <- at(n4); hu <- at(n + 1 - n4)
  spread <- hu - hl
  lo <- hl - k * spread; hi <- hu + k * spread
  flags <- as.numeric(z < lo | z > hi)
  .t1_result(estimate = lam, loglik = best_ll, n_out = sum(flags),
             flags = flags, lower = lo, upper = hi, transformed = z, n = n,
             method = "Box-Cox transform then Tukey fences")
}
