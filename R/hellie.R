# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hellinger distance between two discrete distributions
#'
#' H(P, Q) = (1/sqrt 2) sqrt(sum (sqrt p - sqrt q)^2), which lies in \[0, 1\] and
#' satisfies H^2 = 1 - BC for the Bhattacharyya coefficient BC.  Source
#' consulted: Hellinger (1909), Neue Begruendung der Theorie quadratischer
#' Formen von unendlichvielen Veraenderlichen, Journal fuer die reine und
#' angewandte Mathematik 136, 210-271.
#'
#' @param p,q non-negative masses over a common support.
#' @param normalise rescale each argument to sum to one first.
#' @return list: estimate, h2, bc, affinity, n, method.
#' @keywords internal
#' @examples
#' hellie(c(0.25, 0.75), c(0.25, 0.75))
#' @export
hellie <- function(p, q, normalise = TRUE) {
  pp <- as.numeric(p)
  qq <- as.numeric(q)
  n <- min(length(pp), length(qq))
  pv <- pp[seq_len(n)]
  qv <- qq[seq_len(n)]
  if (normalise) {
    if (sum(pv) > 0) pv <- pv / sum(pv)
    if (sum(qv) > 0) qv <- qv / sum(qv)
  }
  ss <- sum((sqrt(pv) - sqrt(qv))^2)
  h <- sqrt(ss / 2)
  bc <- sum(sqrt(pv * qv))
  list(estimate = as.numeric(h), h2 = as.numeric(h * h), bc = as.numeric(bc),
       affinity = as.numeric(bc), n = as.integer(n),
       method = "Hellinger distance (Hellinger 1909)")
}

# CANONICAL TEST
# r <- hellie(c(0.25, 0.75), c(0.25, 0.75))
# stopifnot(abs(r$estimate) < 1e-12, abs(r$bc - 1) < 1e-12)
# stopifnot(abs(hellie(c(1, 0), c(0, 1))$estimate - 1) < 1e-12)

#' @rdname hellie
#' @keywords internal
#' @export
morie_hellinger_distance <- hellie
