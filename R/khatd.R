# SPDX-License-Identifier: AGPL-3.0-or-later
#' Shape of the importance-weight tail, per observation
#'
#' The estimate is only as good as the importance weights, whose variance
#' is finite only when the tail shape is below one half. k is not a fit
#' statistic: it says whether the computation is entitled to a central
#' limit theorem. Past 0.7 the required sample size grows so fast that
#' the fold should be refitted exactly.
#'
#' Formula: generalised Pareto fitted to the largest
#' \code{M = min(0.2 S, 3 sqrt(S))} weights by the Zhang-Stephens rule;
#' k is its shape.
#'
#' @param log_lik Pointwise log likelihood.
#' @return List with \code{estimate}, \code{k}, \code{n_bad},
#'   \code{n_ok}, \code{S}, \code{n}.
#' @references Vehtari, Simpson, Gelman, Yao & Gabry (2024) JMLR 25:1-58;
#'   Zhang & Stephens (2009) Technometrics 51:316-325.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Khatd(V)
Khatd <- function(log_lik) {
  L <- as.matrix(log_lik); Sn <- nrow(L); n <- ncol(L)
  ks <- vapply(seq_len(n), function(i) .s4_psis(-L[, i])$k, 0)
  bad <- sum(ks > 0.7, na.rm = TRUE)
  .t1_result(estimate = if (any(is.nan(ks))) NaN else max(ks), k = ks, n_bad = bad, n_ok = n - bad,
             S = Sn, n = n, method = "Pareto k importance-weight diagnostic")
}
