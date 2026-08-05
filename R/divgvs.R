# SPDX-License-Identifier: AGPL-3.0-or-later

#' Fornell-Larcker discriminant validity
#'
#' Formula: sqrt(AVE_i) > correlation(F_i, F_j)
#'
#' A construct passes when the square root of its average variance
#' extracted exceeds its correlation with every other construct, i.e. it
#' shares more variance with its own indicators than with any other
#' factor.  The reported margin is the smallest sqrt(AVE_i) - |r_ij|
#' over all off-diagonal pairs: positive means the whole model passes.
#'
#' @param AVE Average variance extracted, one entry per construct.
#' @param factor_correlations k x k matrix of inter-construct
#'   correlations.
#' @return List with \code{estimate} (minimum margin), \code{sqrt_ave},
#'   \code{pass_factor}, \code{n_violations}, \code{discriminant},
#'   \code{k}, \code{method}.
#' @references Fornell & Larcker (1981), J. Marketing Research
#'   18(1):39-50.
#' @export
Divgvs <- function(AVE, factor_correlations) {
  ave <- .s03vec(AVE)
  k <- length(ave)
  if (k == 0L) stop("empty input: no AVE values supplied")
  R <- .s03mat(factor_correlations)
  if (nrow(R) != k || ncol(R) != k)
    stop("factor_correlations must be a k x k matrix")
  if (any(ave < 0 | ave > 1)) stop("AVE must lie in [0, 1]")
  sq <- sqrt(ave)
  margin <- Inf
  viol <- 0L
  pass_factor <- integer(k)
  for (i in seq_len(k)) {
    ok <- 1L
    for (j in seq_len(k)) {
      if (i == j) next
      m <- sq[i] - abs(R[i, j])
      if (m < margin) margin <- m
      if (m <= 0) { ok <- 0L; viol <- viol + 1L }
    }
    pass_factor[i] <- if (k > 1L) ok else 1L
  }
  if (k == 1L) margin <- NaN
  .t1_result(estimate = margin, sqrt_ave = sq, pass_factor = pass_factor,
             n_violations = viol, discriminant = as.integer(viol == 0L), k = k,
             method = "Fornell-Larcker discriminant validity")
}
