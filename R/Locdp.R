# SPDX-License-Identifier: AGPL-3.0-or-later
#' Local differential privacy via randomized response (alias)
#'
#' Alias of \code{Rrand}. The generated stub for module locdp described
#' the local model of differential privacy (each user randomizes before
#' collection, Kasiviswanathan et al. 2011); the canonical local-DP
#' mechanism is Warner randomized response with flip probability
#' \eqn{1/(1 + e^{\epsilon})}, already shipped as \code{Rrand}.
#'
#' @references Kasiviswanathan, S. P., Lee, H. K., Nissim, K.,
#'   Raskhodnikova, S., and Smith, A. (2011). What can we learn
#'   privately? SIAM Journal on Computing 40(3), 793-826.
#' @references Warner, S. L. (1965). Randomized response. JASA 60(309),
#'   63-69.
#' @references Dwork, C., and Roth, A. (2014). FnT-TCS 9(3-4), section 3.2.
#'   Local source: fetched-wave3/dwork-roth-2014-algorithmic-foundations-differential-privacy.pdf
#'
#' Delegating wrapper (not a bare assignment) so that source collation
#' order does not matter at load time; the body is a single call to the
#' target, so outputs are exactly identical.
#' @export
Locdp <- function(bit, epsilon = 1) {
  Rrand(bit, epsilon = epsilon)
}
