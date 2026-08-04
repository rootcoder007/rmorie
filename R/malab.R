# SPDX-License-Identifier: AGPL-3.0-or-later
#' L'Abbe plot coordinates with Mantel-Haenszel reference lines
#'
#' Control-arm risk on x, treated-arm risk on y, one point per trial.  The
#' Mantel-Haenszel risk ratio sum(a n2 / N) / sum(c n1 / N) and risk difference
#' sum((a n2 - c n1)/N) / sum(n1 n2 / N) give the reference lines.  Source
#' consulted: L'Abbe, Detsky and O'Rourke (1987), Annals of Internal Medicine
#' 107, 224-233.
#'
#' @param ai,n1i events and sample size in the treated arm.
#' @param ci,n2i events and sample size in the control arm.
#' @return list: estimate, x, y, size, risk_difference, log_rr, n, method.
#' @keywords internal
#' @examples
#' malab(c(12, 20), c(50, 60), c(7, 15), c(50, 55))$estimate
#' @export
malab <- function(ai, n1i, ci, n2i) {
  a <- as.numeric(ai); n1 <- as.numeric(n1i)
  cc <- as.numeric(ci); n2 <- as.numeric(n2i)
  ntot <- n1 + n2
  p1 <- a / n1; p2 <- cc / n2
  rr <- sum(a * n2 / ntot) / sum(cc * n1 / ntot)
  rd <- sum((a * n2 - cc * n1) / ntot) / sum(n1 * n2 / ntot)
  list(estimate = rr, x = p2, y = p1, size = ntot, risk_difference = rd,
       log_rr = log(rr), n = length(a),
       method = "L'Abbe plot coordinates with Mantel-Haenszel reference lines (L'Abbe, Detsky & O'Rourke 1987)")
}

# CANONICAL TEST
# r <- malab(c(12,20), c(50,60), c(7,15), c(50,55))
# stopifnot(abs(r$x[1] - 0.14) < 1e-15)

#' @rdname malab
#' @keywords internal
#' @export
morie_malab <- malab
