# SPDX-License-Identifier: AGPL-3.0-or-later
#' Strict semiparametric Bernstein-von Mises conditions
#'
#' The exact semiparametric BvM needs three separate things: prior mass
#' around the least-favourable direction, a LAN remainder tending to
#' zero, and a change-of-measure (prior invariance) condition.  They are
#' independent -- a model can satisfy any two and fail the theorem -- so
#' the three are reported separately rather than as one verdict.
#'
#' Formula: score = 1\{prior mass ok\} - lan_remainder
#'   - change_of_measure_gap; holds iff all three checks pass at tol.
#'
#' @param prior_mass_ok Logical; the prior-mass condition.
#' @param lan_remainder Size of the LAN remainder, non-negative.
#' @param change_of_measure_gap Size of the invariance gap.
#' @param tol Tolerance both gaps must fall under.
#' @return List with \code{estimate} (aggregate score),
#'   \code{bvm_holds}, \code{conditions}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 12.3.2.
#' @export
#' @examples
#' Ghosalstrictsbvm()
Ghosalstrictsbvm <- function(prior_mass_ok = TRUE, lan_remainder = 0.01,
                             change_of_measure_gap = 0.02, tol = 0.05) {
  lan <- as.numeric(lan_remainder)
  com <- as.numeric(change_of_measure_gap)
  tol <- as.numeric(tol)
  if (tol <= 0) stop("tol must be positive")
  cond <- c(isTRUE(prior_mass_ok), lan < tol, com < tol)
  .t1_result(estimate = (if (isTRUE(prior_mass_ok)) 1 else 0) - lan - com,
             bvm_holds = all(cond), conditions = cond,
             method = "strict semiparametric BvM (GvdV 2017 sec. 12.3.2)")
}
