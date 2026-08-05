# SPDX-License-Identifier: AGPL-3.0-or-later
#' Two-model adaptation
#'
#' With Pi = pi_0 Pi_0 + (1 - pi_0) Pi_1 the posterior weight of whichever
#' model contains the truth tends to one.  Both evidences are conjugate
#' and exact here, so the weight is a plain logistic transform of their
#' log-ratio: the small model keeps one coordinate, the large model six.
#'
#' Formula: w_0 = 1 / (1 + exp(l_1 - l_0)),
#'   l_j = log m_{K_j}(y) + log prior weight of model j.
#'
#' @param n Precision (sample size) of each coordinate.
#' @param truth_dim Number of nonzero coordinates in the truth.
#' @param pi0 Prior weight of the small model, in (0, 1).
#' @param seed Seed for the deterministic draw.
#' @return List with \code{estimate},
#'   \code{posterior_weight_small_model}, \code{small_model_wins},
#'   \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 10.2.3.
#' @export
Ghosaltwomodeladp <- function(n = 500, truth_dim = 1, pi0 = 0.5,
                              seed = 42) {
  if (pi0 <= 0 || pi0 >= 1) stop("pi0 must lie strictly between 0 and 1")
  if (n <= 0) stop("n must be positive")
  e <- .ghc_rng(seed)
  y <- ifelse(seq_len(6) <= truth_dim, 1, 0) + .ghc_norm(e, 6) / sqrt(n)
  l0 <- .ghc_log_evidence_K(y, n, 1) + log(pi0)
  l1 <- .ghc_log_evidence_K(y, n, 6) + log(1 - pi0)
  w0 <- 1 / (1 + exp(l1 - l0))
  .t1_result(estimate = w0,
             posterior_weight_small_model = w0,
             small_model_wins = w0 > 0.5,
             method = "two-model adaptation (GvdV 2017 sec. 10.2.3)")
}
