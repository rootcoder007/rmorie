# SPDX-License-Identifier: AGPL-3.0-or-later

#' The (1 - x)^n to e^(-nx) step of the Poisson limit
#'
#' (1 - x)^n against e^(-n x), the step that turns the binomial into
#' the Poisson.
#'
#' @param lam_eps the per-slot probability lambda eps, in [0, 1).
#' @param n number of slots, >= 1.
#' @return list(exact, approx, rel_error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.37).
#' @examples
#' OneMExp(0.001, 500)$rel_error
#' @export
OneMExp <- function(lam_eps, n) {
  x <- as.numeric(lam_eps)
  if (length(x) != 1L || is.na(x) || x < 0 || x >= 1) {
    stop("need 0 <= lam_eps < 1.", call. = FALSE)
  }
  if (length(n) != 1L || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("n must be a single integer >= 1.", call. = FALSE)
  }
  n <- as.integer(n)
  exact <- (1 - x)^n
  approx <- exp(-n * x)
  list(exact = exact, approx = approx,
       rel_error = abs(exact - approx) / max(exact, 1e-300))
}
