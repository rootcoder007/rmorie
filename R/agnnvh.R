# SPDX-License-Identifier: AGPL-3.0-or-later
#' The AlphaZero policy-and-value loss
#'
#' Silver et al. (2018), arXiv:1712.01815 (FETCHED), prints the loss
#' verbatim: l = (z - v)^2 - pi' log p + c ||theta||^2 -- squared error on
#' the value head, cross-entropy between the search policy and the network
#' policy, and an L2 penalty.  Silver et al. (2017), Nature 550, 354-359,
#' give the same expression; AlphaGo Zero used c = 1e-4.  The
#' cross-entropy uses 0 log 0 = 0 and floors p at a tiny epsilon.
#'
#' @param z game outcome in \[-1, 1\].
#' @param v value-head output.
#' @param pi search policy.
#' @param p policy-head output.
#' @param theta optional flattened parameters for the L2 term.
#' @param c L2 coefficient.
#' @return list: estimate, value_loss, policy_loss, l2, sq_norm, method.
#' @keywords internal
#' @examples
#' Azloss(1, 0.5, c(0.7, 0.3), c(0.6, 0.4))$estimate
#' @export
Azloss <- function(z, v, pi, p, theta = NULL, c = 1e-4) {
  zz <- as.numeric(z); vv <- as.numeric(v)
  pp <- .s03vec(pi); qq <- .s03vec(p)
  eps <- 1e-300
  vloss <- (zz - vv)^2
  ploss <- 0
  for (i in seq_along(pp)) {
    if (pp[i] > 0) ploss <- ploss - pp[i] * log(if (qq[i] > eps) qq[i] else eps)
  }
  sq <- 0
  if (!is.null(theta)) for (x in .s03vec(theta)) sq <- sq + x * x
  l2 <- as.numeric(c) * sq
  list(estimate = vloss + ploss + l2, value_loss = vloss, policy_loss = ploss,
       l2 = l2, sq_norm = sq,
       method = "AlphaZero loss (z - v)^2 - pi' log p + c ||theta||^2")
}
