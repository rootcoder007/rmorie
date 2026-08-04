# SPDX-License-Identifier: AGPL-3.0-or-later
#' Smallest sketch size meeting an inner-product distortion target
#'
#' The bound says how wide the sketch must be, and the answer does not
#' mention the embedding dimension at all. Cost per token is fixed once
#' the accuracy target is fixed, so a long context does not cost more
#' per key.
#'
#' Formula: with
#' \code{m >= (4/3)(1 + eps)/eps^2 log(2/delta)},
#' \code{Pr[|Prod(q,k) - <q,k>| > eps ||q|| ||k||] <= delta}.
#'
#' @param eps Relative distortion target.
#' @param delta Failure probability.
#' @return List with \code{m_min}, \code{m_real}, \code{estimate}, \code{eps}, \code{delta}.
#' @references Zandieh, A., Daliri, M. & Han, I. (2024). arXiv:2406.03482,
#'   lemma 3.5.
#' @export
Tqdist <- function(eps, delta) {
  eps <- as.numeric(eps); delta <- as.numeric(delta)
  m <- (4 / 3) * (1 + eps) / (eps * eps) * log(2 / delta)
  m_min <- ceiling(m)
  .t1_result(m_min = m_min, m_real = m, estimate = m_min, eps = eps,
             delta = delta, method = "QJL inner-product distortion bound")
}
