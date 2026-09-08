# SPDX-License-Identifier: AGPL-3.0-or-later
#' Basic reproduction number R0
#'
#' Two routes. The direct one is \code{beta / gamma}. The second matters
#' more than it looks: an outbreak reports a final attack rate, not a
#' transmission rate, and the final-size relation
#' \code{1 - AR = exp(-R0 AR)} inverts it without any dynamic model.
#' It is solved by Newton, which is deterministic, so both language arms
#' land on the same digits.
#'
#' Formula: \code{R0 = beta / gamma}; or solve \code{1 - AR = exp(-R0 AR)}.
#'
#' @param beta Transmission rate.
#' @param gamma Recovery rate, positive.
#' @param attack_rate Final attack rate in (0, 1), used when the rates
#'   are unknown.
#' @param tol Newton convergence tolerance.
#' @param max_iter Maximum Newton iterations.
#' @return List with \code{estimate}, \code{R0}, \code{route}.
#' @references Diekmann, O., Heesterbeek, J. A. P. & Metz, J. A. J.
#'   (1990). On the definition and the computation of the basic
#'   reproduction ratio R0 in models for infectious diseases in
#'   heterogeneous populations. Journal of Mathematical Biology
#'   28(4):365-382. \doi{10.1007/BF00178324}.
#' @export
R0 <- function(beta = NULL, gamma = NULL, attack_rate = NULL,
               tol = 1e-8, max_iter = 100) {
  if (!is.null(beta) && !is.null(gamma)) {
    if (gamma <= 0) stop("gamma must be positive")
    val <- beta / gamma
    route <- 1
    meth <- "direct"
  } else if (!is.null(attack_rate)) {
    ar <- as.numeric(attack_rate)
    if (!(ar > 0 && ar < 1)) stop("attack_rate must be in (0, 1)")
    val <- 2
    for (i in seq_len(as.integer(max_iter))) {
      f <- 1 - ar - exp(-val * ar)
      fp <- ar * exp(-val * ar)
      if (abs(fp) < 1e-15) break
      val <- val - f / fp
      if (abs(f) < tol) break
    }
    route <- 2
    meth <- "attack_rate_newton"
  } else {
    stop("Provide (beta, gamma) or attack_rate")
  }
  .t1_result(estimate = val, R0 = val, route = route,
             method = paste0("R0 by ", meth))
}
