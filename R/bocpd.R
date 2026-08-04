# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayesian online changepoint detection (Gaussian run model).
#'
#' Formula: P(r_t=r_{t-1}+1, x_1:t) = P(r_{t-1},x_1:t-1) pi_t^(r) (1-H); P(r_t=0, x_1:t) = sum_r P(r_{t-1},x_1:t-1) pi_t^(r) H
#'
#' @param x Observed univariate series.
#' @param hazard Constant hazard H = 1/lambda of the geometric run-length prior.
#' @param mu0 Prior mean.
#' @param kappa0 Prior mean precision (pseudo-count).
#' @param alpha0 Prior shape of the inverse-gamma variance.
#' @param beta0 Prior scale of the inverse-gamma variance.

#' @return List with ``cp_prob`` (P(r_t = 1) at each t), ``reset_prob``, ``run_length`` (posterior mode), ``max_cp_prob``, ``hazard``, ``n``.
#' @references Adams and MacKay (2007), Bayesian Online Changepoint Detection, arXiv:0710.3742. Equations (2)-(5) for the recursion and the changepoint prior, Section 2.3 and Algorithm 1 for the conjugate-exponential update of the run-specific sufficient statistics. Verified against the paper.
#' @export
Bocpd <- function(x, hazard = 0.004, mu0 = 0, kappa0 = 1, alpha0 = 1, beta0 = 1) {
  x <- .t1_vec(x); n <- length(x); H <- as.numeric(hazard)
  mu <- mu0; kap <- kappa0; al <- alpha0; be <- beta0; R <- 1
  cp_prob <- numeric(n); run_len <- integer(n)
  for (t in seq_len(n)) {
    xt <- x[t]
    df <- 2 * al
    s <- sqrt(be * (kap + 1) / (al * kap))
    pi <- stats::dt((xt - mu) / s, df = df) / s
    growth <- R * pi * (1 - H)
    cp <- sum(R * pi * H)
    newR <- c(cp, growth)
    newR <- newR / sum(newR)
    nmu <- c(mu0, (kap * mu + xt) / (kap + 1))
    nkap <- c(kappa0, kap + 1)
    nal <- c(alpha0, al + 0.5)
    nbe <- c(beta0, be + kap * (xt - mu)^2 / (2 * (kap + 1)))
    R <- newR; mu <- nmu; kap <- nkap; al <- nal; be <- nbe
    cp_prob[t] <- if (length(R) > 1) R[2] else NA_real_
    run_len[t] <- which.max(R) - 1L
  }
  .t1_result(cp_prob = cp_prob, run_length = run_len,
             max_cp_prob = max(cp_prob, na.rm = TRUE), reset_prob = H,
             hazard = H, n = n,
             method = "Bayesian online changepoint detection (Normal-Inverse-Gamma)")
}
