# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sketch width that keeps every attention score within 1 +/- 3 eps
#'
#' The per-pair bound is not enough on its own: there are n keys and one
#' bad estimate can move the softmax. The score bound is the pairwise
#' bound plus a union bound, and the price of the union is the log n.
#'
#' Formula: if \code{max ||k_i|| <= r}, \code{||q|| <= r} and
#' \code{m >= 2 r^2 eps^-2 log n}, then
#' \code{|Score_hat(i) - Score(i)| <= 3 eps Score(i)} for all i.
#'
#' @param eps Relative score distortion.
#' @param r Norm bound on key and query embeddings.
#' @param n Context length.
#' @return List with \code{m_min}, \code{m_real}, \code{estimate}, \code{eps}, \code{r}, \code{n}.
#' @references Zandieh, A., Daliri, M. & Han, I. (2024). arXiv:2406.03482,
#'   theorem 3.6.
#' @export
Tqscr <- function(eps, r, n) {
  eps <- as.numeric(eps); r <- as.numeric(r); n <- as.numeric(n)
  m <- 2 * r * r / (eps * eps) * log(n)
  .t1_result(m_min = ceiling(m), m_real = m, estimate = ceiling(m),
             eps = eps, r = r, n = n,
             method = "QJL attention-score distortion bound")
}
