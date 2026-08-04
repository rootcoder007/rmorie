# SPDX-License-Identifier: AGPL-3.0-or-later
#' Three-parameter Indian buffet process
#'
#' Teh and Goeruer (2009), Indian buffet processes with power-law
#' behaviour, NIPS 22, 1838-1846, add a stability exponent sigma to the
#' two-parameter IBP of Ghahramani, Griffiths and Sollich (2007).  The
#' expected feature count is K_n = alpha sum_i Gamma(1+c) Gamma(i-1+c+sigma)
#' / (Gamma(i+c) Gamma(c+sigma)), which grows like O(n^sigma) for sigma in
#' (0, 1) -- the power law of the title -- and reduces to the logarithmic
#' O(alpha log n) at sigma = 0, c = 1.  The dish probability for an
#' already-chosen dish is (m_k - sigma)/(n - 1 + c).  The proceedings were
#' not retrievable here; both are quoted in their standard published form.
#' Both the power-law count and the sigma = 0 reduction are computed so
#' the claim can be checked rather than trusted.
#'
#' @param y the number of customers n, or data of that length.
#' @param sigma stability exponent.
#' @param alpha mass parameter.
#' @param c concentration.
#' @return list: estimate, K_n, K_path, new_dishes, one_param, n, method.
#' @keywords internal
#' @examples
#' Ibp3par(20, 0.5, 1, 1)$K_n
#' @export
Ibp3par <- function(y, sigma = 0.5, alpha = 1, c = 1) {
  n <- if (length(y) == 1L && is.numeric(y)) as.integer(y) else length(.s03vec(y))
  s <- as.numeric(sigma); a <- as.numeric(alpha); cc <- as.numeric(c)
  newd <- numeric(n)
  for (i in seq_len(n)) {
    newd[i] <- a * exp(lgamma(1 + cc) + lgamma(i - 1 + cc + s) -
                         lgamma(i + cc) - lgamma(cc + s))
  }
  path <- numeric(n); acc <- 0
  for (i in seq_len(n)) { acc <- acc + newd[i]; path[i] <- acc }
  one <- 0
  for (i in seq_len(n)) one <- one + a * cc / (i - 1 + cc)
  list(estimate = acc, K_n = acc, K_path = path, new_dishes = newd,
       one_param = one, n = n,
       method = "Three-parameter IBP feature count (Teh and Goeruer 2009)")
}
