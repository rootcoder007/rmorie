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
    1 - .morie_fauzi_kolm2x(stat, n)
  } else {
    lam <- (sqrt(n) + 0.12 + 0.11 / sqrt(n)) * stat
    k <- seq_len(100L)
    min(1, max(0, 2 * sum((-1)^(k - 1) * exp(-2 * k^2 * lam^2))))
  }
  list(statistic = stat, dplus = dplus, dminus = dminus, p_value = pval, n = n,
       method = "Kolmogorov-Smirnov statistic against a specified F")
}

# Exact two-sided Kolmogorov distribution P(D_n < d) by the Marsaglia,
# Tsang and Wang (2003, J. Stat. Softw. 8(18)) matrix power; the one-sided
# Birnbaum-Tingey sum it replaces gave P(D_n^+ < d), not the two-sided law.
.morie_fauzi_kolm2x <- function(d, n) {
  if (d <= 0) return(0)
  if (d >= 1) return(1)
  k <- floor(n * d) + 1
  m <- 2 * k - 1
  h <- k - n * d
  hm <- outer(seq_len(m), seq_len(m), function(i, j) as.numeric(i - j + 1 >= 0))
  hm[, 1] <- hm[, 1] - h^seq_len(m)
  hm[m, ] <- hm[m, ] - h^(m:1)
  if (2 * h - 1 > 0) hm[m, 1] <- hm[m, 1] + (2 * h - 1)^m
  for (i in seq_len(m)) {
    for (j in seq_len(m)) {
      if (i - j + 1 > 0) hm[i, j] <- hm[i, j] / factorial(i - j + 1)
    }
  }
  q <- diag(m)
  lsc <- 0
  for (step in seq_len(n)) {
    q <- q %*% hm
    mx <- max(abs(q))
    q <- q / mx
    lsc <- lsc + log(mx)
  }
  min(1, max(0, exp(log(q[k, k]) + lsc + lfactorial(n) - n * log(n))))
}

# CANONICAL TEST
# r <- Ksstat(c(0.1, 0.3, 0.5, 0.7, 0.9), cdf = function(t) pmin(pmax(t, 0), 1))
# stopifnot(abs(r$statistic - 0.1) < 1e-12)

#' @rdname Ksstat
#' @keywords internal
#' @export
morie_fauzi_ks_statistic <- Ksstat
