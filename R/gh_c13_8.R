# SPDX-License-Identifier: AGPL-3.0-or-later
#' Neutral-to-the-right processes
#'
#' F(t) = 1 - exp(-M(t)) for M an independent-increment process.  Neutral
#' to the right means the relative survival fractions over disjoint
#' intervals are INDEPENDENT, and that independence is what makes the
#' class closed under right censoring -- the property the Dirichlet
#' process itself has and shares with the beta process.
#'
#' Formula: F(t_k) = 1 - exp(-sum_{j <= k} inc_j).
#'
#' @param increments Non-negative increments of M.
#' @param seed Unused; kept for call compatibility.
#' @return List with \code{estimate} (terminal CDF value),
#'   \code{F_path}, \code{nondecreasing}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.4.
#' @export
Ghosalntrdef <- function(increments, seed = 42) {
  inc <- as.numeric(increments)
  if (length(inc) == 0L) stop("increments must be non-empty")
  if (any(inc < 0)) stop("increments must be nonnegative")
  F <- 1 - exp(-cumsum(inc))
  nd <- if (length(F) < 2L) TRUE else all(diff(F) >= -1e-15)
  .t1_result(estimate = F[length(F)], F_path = F, nondecreasing = nd,
             method = "NTR construction (GvdV 2017 sec. 13.4)")
}
