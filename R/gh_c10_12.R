# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayes-factor model selection consistency
#'
#' BF(H1, H0) = p(X | H1) / p(X | H0) tends to infinity under H1 and to
#' zero under H0.  Both evidences are exact here (normal means, H0
#' keeping no coordinates and H1 keeping two), so the demonstration is
#' arithmetic rather than simulation: only the data are random.
#'
#' Formula: log BF = log m_2(y) - log m_0(y).
#'
#' @param truth_in_H1 If TRUE the truth has nonzero mean, so H1 holds.
#' @param n Precision (sample size) of each coordinate.
#' @param seed Seed for the deterministic draw.
#' @return List with \code{estimate} (log Bayes factor),
#'   \code{supports_H1}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 10.5.
#' @export
#' @examples
#' Ghosalmodselbic()
Ghosalmodselbic <- function(truth_in_H1 = TRUE, n = 2000, seed = 42) {
  if (n <= 0) stop("n must be positive")
  e <- .ghc_rng(seed)
  mu <- if (isTRUE(truth_in_H1)) 0.7 else 0
  y <- mu + .ghc_norm(e, 2) / sqrt(n)
  log_bf <- .ghc_log_evidence_K(y, n, 2) - .ghc_log_evidence_K(y, n, 0)
  .t1_result(estimate = log_bf, supports_H1 = log_bf > 0,
             method = "Bayes factor consistency (GvdV 2017 sec. 10.5)")
}
