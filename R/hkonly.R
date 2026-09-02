# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hadamard response estimator for local differential privacy
#'
#' Formula: phat(x) = 2(e^eps + 1)/(e^eps - 1) * (phat(C_x) - 1/2)
#'
#' @param counts Per-symbol counts of reports falling in C_x.
#' @param epsilon Local privacy parameter.
#' @param n Number of users; the sum of ``counts`` if omitted.

#' @param counts See Usage.
#' @param epsilon See Usage.
#' @param n See Usage.
#' @return List with ``p``, ``p_set`` (the empirical p(C_x)), ``scale``, ``epsilon``, ``n``, ``k``.
#' @references Acharya, Sun and Zhang (2019), Hadamard Response: Estimating Distributions Privately, Efficiently, and with Little Communication, AISTATS, PMLR 89. Equations (8), (9) and (10). Verified against the paper.
#' @export
#' @examples
#' Ldphr(counts = c(1, 2, 3, 4, 5, 6, 7, 8), epsilon = 5L)
Ldphr <- function(counts, epsilon, n = NULL) {
  counts <- .t1_vec(counts); eps <- as.numeric(epsilon)
  if (eps <= 0) stop("epsilon must be positive")
  total <- if (is.null(n)) sum(counts) else as.numeric(n)
  if (total <= 0) stop("need at least one report")
  e <- exp(eps)
  scale <- 2 * (e + 1) / (e - 1)
  pset <- counts / total
  .t1_result(p = scale * (pset - 0.5), p_set = pset, scale = scale,
             epsilon = eps, n = total, k = length(counts),
             method = "Hadamard response LDP distribution estimator")
}
