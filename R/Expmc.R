# SPDX-License-Identifier: AGPL-3.0-or-later
#' Exponential mechanism (alias)
#'
#' Alias of \code{morie_dp_exponential_mechanism}: selects candidate r
#' with probability proportional to
#' \eqn{\exp(\epsilon u(D, r) / (2 \Delta u))}. The generated stub for
#' module expmc described exactly the mechanism already shipped, so this
#' is an alias.
#'
#' @param candidates See Usage.
#' @param utility See Usage.
#' @param epsilon See Usage.
#' @param sensitivity See Usage.
#' @param seed See Usage.
#' @references McSherry, F., and Talwar, K. (2007). Mechanism design via
#'   differential privacy. FOCS 2007, 94-103.
#' @references Dwork, C., and Roth, A. (2014). FnT-TCS 9(3-4), 211-487.
#'   Definition 3.4.
#'   Local source: fetched-wave3/dwork-roth-2014-algorithmic-foundations-differential-privacy.pdf
#'
#' Delegating wrapper (not a bare assignment) so that source collation
#' order does not matter at load time; the body is a single call to the
#' target, so outputs are exactly identical.
#' @export
Expmc <- function(candidates, utility, epsilon = 1, sensitivity = 1,
                  seed = NULL) {
  morie_dp_exponential_mechanism(candidates, utility,
                                 epsilon = epsilon,
                                 sensitivity = sensitivity, seed = seed)
}
