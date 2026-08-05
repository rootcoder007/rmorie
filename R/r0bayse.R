# SPDX-License-Identifier: AGPL-3.0-or-later
#' Basic reproduction number -- alias of \code{\link{R0}}
#'
#' \code{R0} carries both routes to the basic reproduction number. This
#' is the name the backlog asked for, not a second implementation.
#'
#' Formula: \code{R0 = beta / gamma}; or solve \code{1 - AR = exp(-R0 AR)}.
#'
#' @param beta Transmission rate.
#' @param gamma Recovery rate, positive.
#' @param attack_rate Final attack rate in (0, 1).
#' @param tol Newton convergence tolerance.
#' @param max_iter Maximum Newton iterations.
#' @return The value of \code{\link{R0}}.
#' @references Diekmann, O., Heesterbeek, J. A. P. & Metz, J. A. J.
#'   (1990). Journal of Mathematical Biology 28(4):365-382.
#'   \doi{10.1007/BF00178324}.
#' @export
R0bayse <- function(beta = NULL, gamma = NULL, attack_rate = NULL,
                    tol = 1e-8, max_iter = 100) {
  R0(beta = beta, gamma = gamma, attack_rate = attack_rate,
     tol = tol, max_iter = max_iter)
}
