# SPDX-License-Identifier: AGPL-3.0-or-later
#' Outbreak detection by online changepoint analysis of counts
#'
#' Formula: same run-length recursion as bocpd with a Gamma-Poisson run model; predictive P(x|a,b) = Gamma(x+a)/(Gamma(a) x!) (b/(b+1))^a (1/(b+1))^x
#'
#' @param counts Non-negative integer case counts per period.
#' @param hazard Constant hazard of the geometric run-length prior.
#' @param a0 Gamma prior shape on the Poisson rate.
#' @param b0 Gamma prior rate on the Poisson rate.

#' @param counts See Usage.
#' @param hazard See Usage.
#' @param a0 See Usage.
#' @param b0 See Usage.
#' @return List with ``cp_prob`` (P(r_t = 1)), ``reset_prob``, ``run_length``, ``max_cp_prob``, ``alarm`` (indices with cp_prob > 0.5), ``n``.
#' @references Adams and MacKay (2007), Bayesian Online Changepoint Detection, arXiv:0710.3742. Equations (2)-(5) for the recursion and the changepoint prior, Section 2.3 and Algorithm 1 for the conjugate-exponential update of the run-specific sufficient statistics. Verified against the paper.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Outbrkdet(V)
Outbrkdet <- function(counts, hazard = 0.01, a0 = 1, b0 = 1) {
  y <- .t1_vec(counts)
  n <- length(y)
  H <- as.numeric(hazard)
  if (any(y < 0)) stop("counts must be non-negative")
  a <- a0
  b <- b0
  R <- 1
  cp_prob <- numeric(n)
  run_len <- integer(n)
  for (t in seq_len(n)) {
    xt <- y[t]
    pi <- exp(lgamma(xt + a) - lgamma(a) - lgamma(xt + 1) +
              a * log(b / (b + 1)) - xt * log(b + 1))
    growth <- R * pi * (1 - H)
    cp <- sum(R * pi * H)
    newR <- c(cp, growth)
    newR <- newR / sum(newR)
    a <- c(a0, a + xt)
    b <- c(b0, b + 1)
    R <- newR
    cp_prob[t] <- if (length(R) > 1) R[2] else NA_real_
    run_len[t] <- which.max(R) - 1L
  }
  .t1_result(cp_prob = cp_prob, run_length = run_len,
             max_cp_prob = max(cp_prob, na.rm = TRUE), reset_prob = H,
             alarm = which(!is.na(cp_prob) & cp_prob > 0.5) - 1L, n = n,
             method = "Outbreak detection (Gamma-Poisson online changepoint)")
}
