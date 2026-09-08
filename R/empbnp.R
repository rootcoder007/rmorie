# SPDX-License-Identifier: AGPL-3.0-or-later

#' Robbins' nonparametric empirical Bayes
#'
#' Formula: E\[theta | y = k\] = (k + 1) f(k + 1) / f(k)
#'
#' For Poisson observations the posterior mean of the rate needs no
#' parametric prior at all: it is recovered from the marginal counts
#' alone.  The gaussian branch is Tweedie's formula
#' E\[theta | y\] = y + d/dy log f(y), estimated from a kernel-smoothed
#' marginal.
#'
#' @param y Observed counts (poisson) or values (gaussian).
#' @param prior_family "poisson" for Robbins' rule, "gaussian" for
#'   Tweedie's.
#' @return List with \code{estimate}, \code{theta_hat}, \code{support},
#'   \code{counts}, \code{n}, \code{method}.
#' @references Robbins (1956), Proc. 3rd Berkeley Symp. 1:157-163;
#'   Efron (2011), JASA 106(496):1602-1614.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Empbnp(V)
Empbnp <- function(y, prior_family = "poisson") {
  y <- .s03vec(y)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  fam <- tolower(as.character(prior_family))
  if (!(fam %in% c("poisson", "gaussian")))
    stop("prior_family must be 'poisson' or 'gaussian'")
  if (fam == "poisson") {
    ks <- as.integer(round(y))
    if (any(ks < 0L)) stop("poisson counts must be non-negative")
    top <- max(ks)
    cnt <- integer(top + 2L)
    for (v in ks) cnt[v + 1L] <- cnt[v + 1L] + 1L
    support <- 0:top
    theta <- numeric(top + 1L)
    for (k in support)
      theta[k + 1L] <- if (cnt[k + 1L] > 0L)
        (k + 1) * cnt[k + 2L] / cnt[k + 1L] else NaN
    per <- theta[ks + 1L]
    good <- per[!is.nan(per)]
    est <- if (length(good)) sum(good) / length(good) else NaN
    return(.t1_result(estimate = est, theta_hat = theta, support = support,
                      counts = cnt[seq_len(top + 1L)], n = n,
                      method = "Robbins nonparametric empirical Bayes (Poisson)"))
  }
  s <- .s03sd(y, 1L)
  if (s <= 0) stop("y has zero spread; Tweedie's formula is undefined")
  h <- 1.06 * s * n^(-0.2)
  theta <- numeric(n)
  for (i in seq_len(n)) {
    f <- 0
    fp <- 0
    for (j in seq_len(n)) {
      u <- (y[i] - y[j]) / h
      k <- exp(-0.5 * u * u)
      f <- f + k
      fp <- fp + k * (-u / h)
    }
    theta[i] <- if (f > 0) y[i] + fp / f else y[i]
  }
  .t1_result(estimate = sum(theta) / n, theta_hat = theta, support = y,
             counts = rep(1L, n), n = n,
             method = "Tweedie nonparametric empirical Bayes (Gaussian)")
}
