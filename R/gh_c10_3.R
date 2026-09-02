# SPDX-License-Identifier: AGPL-3.0-or-later
#' Adaptation to a parametric truth
#'
#' When f0 sits in a d-dimensional submodel the hierarchical posterior
#' attains the parametric sqrt(d/n) rate: the selected dimension stays
#' near d and the posterior risk falls like d/n, i.e. with log-log slope
#' one against n.  Recovering a slope of one is the whole content of the
#' section -- a nonparametric prior paying no price on a parametric truth.
#'
#' Formula: K-hat = argmax_K [log m_K(y) - lam K log n];
#'   risk = K-hat/(n + 1) + sum_{k=K-hat}^{d-1} theta_k^2.
#'
#' @param d_true Dimension of the true submodel.
#' @param ns Increasing vector of sample sizes.
#' @param lam Complexity-penalty scale.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (log-log slope),
#'   \code{risk_by_n}, \code{parametric}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 10.2.2.
#' @export
#' @examples
#' Ghosalparamrate()
Ghosalparamrate <- function(d_true = 2, ns = c(100, 1000, 10000),
                            lam = 1, seed = 42) {
  ns <- as.numeric(ns)
  if (length(ns) < 2L) stop("ns must have at least two sample sizes")
  if (any(ns <= 1)) stop("every n must exceed 1")
  e <- .ghc_rng(seed)
  risks <- numeric(length(ns))
  for (i in seq_along(ns)) {
    n <- ns[i]
    y <- ifelse(seq_len(10) <= d_true, 0.8, 0) + .ghc_norm(e, 10) / sqrt(n)
    logs <- vapply(0:10, function(K) .ghc_log_evidence_K(y, n, K) -
                     lam * K * log(n), numeric(1))
    k_hat <- which.max(logs) - 1
    zeroed <- if (k_hat < d_true) seq(k_hat, d_true - 1) else integer(0)
    risks[i] <- k_hat * (1 / (n + 1)) + length(zeroed) * 0.64
  }
  if (any(risks <= 0))
    stop("risk is zero at some n; the log-log slope is undefined")
  rate_hat <- log(risks[1] / risks[length(risks)]) /
    log(ns[length(ns)] / ns[1])
  .t1_result(estimate = rate_hat,
             risk_by_n = risks,
             parametric = abs(rate_hat - 1) < 0.25,
             method = "parametric adaptation (GvdV 2017 sec. 10.2.2)")
}
