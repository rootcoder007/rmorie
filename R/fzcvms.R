# SPDX-License-Identifier: AGPL-3.0-or-later

#' Cramer-von Mises statistic against a fully specified distribution
#'
#' \deqn{CvM_n = n\int \[F_n(x)-F(x)\]^2 dF(x).}{CvM_n = n int \[F_n(x) - F(x)\]^2 dF(x).}
#'
#' Evaluated by its exact closed form, not by quadrature: substituting the
#' empirical df and integrating gives
#' \deqn{CvM_n = \frac{1}{12n} + \sum_i\Big(\frac{2i-1}{2n} - F(X_{(i)})\Big)^2,}{CvM_n =
#' 1/(12n) + sum_i ((2i-1)/(2n) - F(X_(i)))^2,}
#' a finite sum with no discretisation error at all.
#'
#' This module previously carried a Kolmogorov-Smirnov body under the
#' Cramer-von Mises name -- one of six modules in this shelf sharing a single
#' copied KS implementation. It now computes what it says.
#'
#' Where KS uses a supremum and responds to the single worst point, CvM
#' integrates the squared discrepancy against `dF` and responds to sustained
#' departure. That is why Theorems 5.1 and 5.7 must be proved separately: the
#' two statistics are not functions of one another, and smoothing affects them
#' differently.
#'
#' @param x Sample.
#' @param cdf The fully specified null distribution `F(t)`.
#' @return Named list with ``statistic``, ``p_value``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (5.2) and the display defining CvM_n in Sec. 5.1.
#' @examples
#' Cvmstat(c(0.1, 0.3, 0.5, 0.7, 0.9), cdf = function(t) pmin(pmax(t, 0), 1))
#' @export
Cvmstat <- function(x, cdf) {
  xs <- sort(as.numeric(x))
  n <- length(xs)
  if (n < 2L) stop("need at least two observations.")
  if (!is.function(cdf)) stop("cdf must be a function F(t).")
  fv <- vapply(xs, function(t) as.numeric(cdf(t)), numeric(1))
  target <- (2 * seq_len(n) - 1) / (2 * n)
  stat <- 1 / (12 * n) + sum((target - fv)^2)
  pval <- 1
  if (stat > 0) {
    k <- 0:99
    acc <- sum(exp(-((4 * k + 1)^2) * pi^2 / (8 * stat)))
    pval <- min(1, max(0, 1 - acc * sqrt(2 / stat)))
  }
  list(statistic = stat, p_value = pval, n = n,
       method = "Cramer-von Mises statistic against a specified F")
}

# CANONICAL TEST
# r <- Cvmstat(c(0.1, 0.3, 0.5, 0.7, 0.9), cdf = function(t) pmin(pmax(t, 0), 1))
# stopifnot(abs(r$statistic - 1/60) < 1e-12)

#' @rdname Cvmstat
#' @keywords internal
#' @export
morie_fauzi_cvm_statistic <- Cvmstat
