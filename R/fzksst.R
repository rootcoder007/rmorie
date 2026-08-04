# SPDX-License-Identifier: AGPL-3.0-or-later

#' Kolmogorov-Smirnov statistic against a fully specified distribution
#'
#' The classical statistic the chapter's smoothed versions are compared
#' against: \deqn{KS_n = \sup_x |F_n(x) - F(x)|,}{KS_n = sup_x |F_n(x) - F(x)|,}
#' with `F_n` the empirical distribution function.
#'
#' Computed as `max(D+, D-)` over the order statistics, which is exact: the
#' supremum of a step function against a continuous one is always attained at a
#' jump, so no grid search is needed and none is done.
#'
#' Sec. 5.1 gives the motivation for replacing `F_n` here: its lack of
#' smoothness makes the test over-sensitive near the centre of the distribution
#' and inflates the type-I error above the nominal `alpha` at small `n`.
#' Theorems 5.1 and 5.6 then show the smoothed replacements have the SAME
#' limit, so the same critical values apply.
#'
#' Uses the exact one-sample Kolmogorov distribution for `n <= 40` and the
#' standard asymptotic series otherwise.
#'
#' @param x Sample.
#' @param cdf The fully specified null distribution `F(t)`.
#' @return Named list with ``statistic``, ``dplus``, ``dminus``, ``p_value``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Sec. 5.1, the display preceding (5.3).
#' @examples
#' Ksstat(c(0.1, 0.3, 0.5, 0.7, 0.9), cdf = function(t) pmin(pmax(t, 0), 1))
#' @export
Ksstat <- function(x, cdf) {
  xs <- sort(as.numeric(x))
  n <- length(xs)
  if (n < 2L) stop("need at least two observations.")
  if (!is.function(cdf)) stop("cdf must be a function F(t).")
  fv <- vapply(xs, function(t) as.numeric(cdf(t)), numeric(1))
  dplus <- max(seq_len(n) / n - fv)
  dminus <- max(fv - (seq_len(n) - 1) / n)
  stat <- max(dplus, dminus)
  pval <- if (n <= 40L) {
    1 - .morie_fauzi_ksone(stat, n)
  } else {
    lam <- (sqrt(n) + 0.12 + 0.11 / sqrt(n)) * stat
    k <- seq_len(100L)
    min(1, max(0, 2 * sum((-1)^(k - 1) * exp(-2 * k^2 * lam^2))))
  }
  list(statistic = stat, dplus = dplus, dminus = dminus, p_value = pval, n = n,
       method = "Kolmogorov-Smirnov statistic against a specified F")
}

# CANONICAL TEST
# r <- Ksstat(c(0.1, 0.3, 0.5, 0.7, 0.9), cdf = function(t) pmin(pmax(t, 0), 1))
# stopifnot(abs(r$statistic - 0.1) < 1e-12)

#' @rdname Ksstat
#' @keywords internal
#' @export
morie_fauzi_ks_statistic <- Ksstat
