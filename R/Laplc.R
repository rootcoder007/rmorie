# SPDX-License-Identifier: AGPL-3.0-or-later
#' Laplace mechanism (alias)
#'
#' Alias of \code{morie_dp_laplace_mechanism}: releases
#' \eqn{f(D) + Lap(\Delta f / \epsilon)}. The generated stub for module
#' laplc described exactly the mechanism already shipped, so this is an
#' alias, not a second implementation.
#'
#' @param y See Usage.
#' @param sensitivity See Usage.
#' @param epsilon See Usage.
#' @param seed See Usage.
#' @references Dwork, C., McSherry, F., Nissim, K., and Smith, A. (2006).
#'   Calibrating noise to sensitivity in private data analysis. Theory of
#'   Cryptography (TCC 2006), LNCS 3876, 265-284.
#' @references Dwork, C., and Roth, A. (2014). The algorithmic foundations
#'   of differential privacy. Foundations and Trends in Theoretical
#'   Computer Science 9(3-4), 211-487. Definition 3.3 and Theorem 3.6.
#'   Local source: fetched-wave3/dwork-roth-2014-algorithmic-foundations-differential-privacy.pdf
#'
#' Delegating wrapper (not a bare assignment) so that source collation
#' order does not matter at load time; the body is a single call to the
#' target, so outputs are exactly identical.
#' @export
Laplc <- function(y, sensitivity = 1, epsilon = 1, seed = NULL) {
  morie_dp_laplace_mechanism(y, sensitivity = sensitivity,
                             epsilon = epsilon, seed = seed)
}
