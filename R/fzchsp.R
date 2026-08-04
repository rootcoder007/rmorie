# SPDX-License-Identifier: AGPL-3.0-or-later

#' Chung-Smirnov statistic for the kernel distribution function estimator
#'
#' Sec. 2.1 records the Chung-Smirnov property of the KDFE:
#' \deqn{\limsup_n \sqrt{\frac{2n}{\log\log n}}\sup_x|\hat F_h(x) - F_X(x)| = 1\ \mathrm{a.s.}}{limsup_n sqrt(2n/log log n) sup_x |Fhat_h(x) - F(x)| = 1 a.s.}
#'
#' A law of the iterated logarithm, not a distributional limit: it pins the
#' ALMOST-SURE fluctuation of the uniform error at exactly
#' `sqrt(log log n / (2n))`, with constant 1.
#'
#' This returns the normalised statistic
#' `sqrt(2n / log log n) sup_x |Fhat_h(x) - F(x)|`, whose limsup the theorem
#' sets to 1. Values persistently above 1 are evidence against `F`; a single
#' value above 1 is not, since a limsup is exceeded infinitely often on the way.
#'
#' Undefined for `n <= 15`, where `log log n <= 1` makes the normaliser
#' meaningless; the function says so rather than returning a number.
#'
#' @param x Sample.
#' @param cdf The hypothesised `F(t)`.
#' @param h Bandwidth; defaults to `4^(1/3) sigma n^(-1/3)`.
#' @param grid Points at which the supremum is taken; defaults to the sample.
#' @return Named list with ``statistic``, ``supdiff``, ``scale``, ``h``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Sec. 2.1, the Chung-Smirnov display.
#' @examples
#' Chungsmir(1:50, cdf = function(t) pmin(pmax((t - 1) / 49, 0), 1))
#' @export
Chungsmir <- function(x, cdf, h = NULL, grid = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (n <= 15L) {
    stop("the Chung-Smirnov normaliser needs log log n > 1, i.e. n > 15.")
  }
  if (is.null(h)) h <- .morie_kdfe_h(x)
  if (h <= 0) stop("bandwidth must be positive.")
  g <- if (is.null(grid)) sort(x) else as.numeric(grid)
  khat <- vapply(g, function(t) mean(stats::pnorm((t - x) / h)), numeric(1))
  fv <- vapply(g, function(t) as.numeric(cdf(t)), numeric(1))
  sup <- max(abs(khat - fv))
  scl <- sqrt(2 * n / log(log(n)))
  list(statistic = scl * sup, supdiff = sup, scale = scl, h = h, n = n,
       method = "Chung-Smirnov normalised uniform error of the KDFE")
}

# CANONICAL TEST
# r <- Chungsmir(1:50, cdf = function(t) pmin(pmax((t - 1)/49, 0), 1))
# stopifnot(abs(r$statistic - r$scale * r$supdiff) < 1e-12)

#' @rdname Chungsmir
#' @keywords internal
#' @export
morie_fauzi_chung_smirnov <- Chungsmir
